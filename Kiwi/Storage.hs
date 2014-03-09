{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Kiwi.Storage (
  addWiki, addPage, editPage, getPage, getPageNames, getPageVersions,
  createTablesIfNecessary,
  Error(..), Result(..)
  ) where

import Control.Applicative ((<$>))
import Control.Monad (forM_, unless)
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Builder (toLazyByteString, byteString)
import Data.ByteString.Lazy.Builder.ASCII (lazyByteStringHexFixed)
import qualified Data.ByteString.UTF8 as BSU
import Data.ByteString.Internal (w2c)
import qualified Data.Text as T
import Database.HaskellDB
import Database.HaskellDB.Database (GetValue, getString, getValue)
import Database.HaskellDB.DBLayout
import Database.HaskellDB.HDBRec
import Database.HaskellDB.HDBC
import Database.HaskellDB.PrimQuery
import Database.HaskellDB.Query
import Database.HaskellDB.Sql.SQLite
import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3)

import Kiwi.Data

type Result a = Either Error a

data Error = PasswordProtected
           | WrongPassword
           | WikiDoesNotExist
           | PageDoesNotExist
           | VersionDoesNotExist
           | WikiAlreadyExists
           | PageAlreadyExists
           | AbnormalError
             deriving (Show, Eq)

dbPath :: FilePath
dbPath = "wiki.db"

-- | Hash a name, returning its SHA-1 hash encoded in an hexadecimal
-- string
hash :: String -> String
hash s =
    -- TODO: this is really ugly way to convert the bytestring
    -- returned by SHA1.hash into an hexadecimal representation
    map w2c $ BSL.unpack hashed
    where rawHash = SHA1.hash $ BSU.fromString s
          bsHash = toLazyByteString $ byteString rawHash
          hex = lazyByteStringHexFixed bsHash
          hashed = toLazyByteString hex

createTablesIfNecessary :: IO ()
createTablesIfNecessary = do
  db <- connectSqlite3 dbPath
  tables <- getTables db
  unless (("wiki" `elem` tables) &&
          ("page" `elem` tables) &&
          ("pageversion" `elem` tables))
         -- TODO: constraints (foreign key etc)
         $ forM_ [unlines [
                   "create table wiki (wid integer primary key autoincrement,",
                   "                   wname text not null unique)"],
                  unlines [
                   "create table page (wid integer not null,",
                   "                   pid integer primary key autoincrement,",
                   "                   pname text not null,",
                   "                   pversion integer not null,",
                   "                   platestversion integer not null,",
                   "                   foreign key(wid) references wiki(wid))"],
-- TODO: this introduces cyclic constraints (to add a page, we need to
-- already have a version, and to add a version we need to already
-- have a page).
--                   "                   foreign key(pid, pversion) references pageversion(pid, pversion)",
--                   "                   foreign key (pid, platestversion) references pageversion(pid, pversion))"],
                  unlines [
                   "create table pageversion (wid integer not null,",
                   "                          pid integer not null,",
                   "                          pversion integer not null,",
                   "                          pcontent text not null,",
                   "                          foreign key (wid) references wiki(wid),",
                   "                          foreign key (pid) references page(pid))"]]
                 (\table -> handleSqlError $ quickQuery' db table [])
  commit db

-- | The unique identifier of a wiki
data WikiID = WikiID

instance FieldTag WikiID where
    fieldName = const "wid"

wikiId :: Attr WikiID Int
wikiId = mkAttr WikiID

-- | The name of a wiki
data WikiName = WikiName

instance FieldTag WikiName where
    fieldName = const "wname"

wikiName :: Attr WikiName ValidWikiName
wikiName = mkAttr WikiName

instance ShowConstant ValidWikiName where
    showConstant = StringLit . show

instance GetValue ValidWikiName where
    getValue fs s f =
        getString fs s f >>=
        (maybe (fail "Cannot extract valid wiki name")
         ((maybe (fail "Invalid wiki name") return) .
          validateWikiName . T.pack))

-- | The unique identifier of a page
data PageID = PageID

instance FieldTag PageID where
    fieldName = const "pid"

pageId :: Attr PageID Int
pageId = mkAttr PageID

-- | The version of a page
data PageVersion = PageVersion

instance FieldTag PageVersion where
    fieldName = const "pversion"

pageVersion :: Attr PageVersion Int
pageVersion = mkAttr PageVersion

-- | The latest version of a page
data PageLatestVersion = PageLatestVersion

instance FieldTag PageLatestVersion where
    fieldName = const "platestversion"

pageLatestVersion :: Attr PageLatestVersion Int
pageLatestVersion = mkAttr PageLatestVersion

-- | The name of a page
data PageName = PageName

instance FieldTag PageName where
    fieldName = const "pname"

pageName :: Attr PageName ValidPageName
pageName = mkAttr PageName

instance ShowConstant ValidPageName where
    showConstant = StringLit . show

instance GetValue ValidPageName where
    getValue fs s f =
        getString fs s f >>=
        (maybe (fail "Cannot extract valid page name")
         ((maybe (fail "Invalid page name") return) .
          validatePageName . T.pack))

data PageContent = PageContent

instance FieldTag PageContent where
    fieldName = const "pcontent"

instance ShowConstant T.Text where
    showConstant = StringLit . T.unpack

instance GetValue T.Text where
    getValue fs s f =
      getString fs s f >>=
      (maybe (fail "Cannot extract text")
       (return . T.pack))

pageContent :: Attr PageContent T.Text
pageContent = mkAttr PageContent

-- | Wiki table
wikiTable :: Table (RecCons WikiID (Expr Int)
                    (RecCons WikiName (Expr ValidWikiName)
                     RecNil))
-- TODO: WikiAccess, WikiPassword
wikiTable = baseTable "wiki"
            $ hdbMakeEntry WikiID
            # hdbMakeEntry WikiName

-- | Page table
pageTable :: Table (RecCons WikiID (Expr Int)
                    (RecCons PageID (Expr Int)
                     (RecCons PageName (Expr ValidPageName)
                      (RecCons PageVersion (Expr Int)
                        (RecCons PageLatestVersion (Expr Int)
                         RecNil)))))
pageTable = baseTable "page"
            $ hdbMakeEntry WikiID
            # hdbMakeEntry PageID
            # hdbMakeEntry PageName
            # hdbMakeEntry PageVersion
            # hdbMakeEntry PageLatestVersion

-- | PageVersion table
pageVersionTable :: Table (RecCons WikiID (Expr Int)
                           (RecCons PageID (Expr Int)
                            (RecCons PageVersion (Expr Int)
                             (RecCons PageContent (Expr T.Text)
                              RecNil))))
pageVersionTable = baseTable "pageversion"
                   $ hdbMakeEntry WikiID
                   # hdbMakeEntry PageID
                   # hdbMakeEntry PageVersion
                   # hdbMakeEntry PageContent

-- | Get the last inserted id
lastInsertedId :: Database -> IO Int
lastInsertedId db = do
  fmap extract $
       query db $ do
         -- We use wikiId but it is really an integer that can
         -- represent anything
         project $ wikiId << Expr (ConstExpr (OtherLit "last_insert_rowid()"))
    where extract = (\res -> res ! wikiId) . head

-- | Try to get a wiki id given its name
getWikiId :: Database -> ValidWikiName -> IO (Maybe Int)
getWikiId db wname = do
  fmap extract $
       query db $ do
         wikis <- table wikiTable
         restrict $ wikis ! wikiName .==. constant wname
         project $ wikiId << wikis ! wikiId
    where extract [] = Nothing
          extract (hd:_) = Just (hd ! wikiId)

-- | Try to get a page id given a wiki id and the page name
getPageId :: Database -> Int -> ValidPageName -> IO (Maybe Int)
getPageId db wid pname = do
  fmap extract $
       query db $ do
         pages <- table pageTable
         restrict $ pages ! wikiId .==. constant wid .&&.
                    pages ! pageName .==. constant pname
         project $ pageId << pages ! pageId
       where extract [] = Nothing
             extract (hd:_) = Just (hd ! pageId)

-- | Get the latest version of a page given a wiki id and page
-- id. Assume the page exists (fails if it is not the case)
getPageLatestVersion :: Database -> Int -> Int -> IO Int
getPageLatestVersion db wid pid = do
  fmap extract $
       query db $ do
         pages <- table pageTable
         restrict $ pages ! wikiId .==. constant wid .&&.
                    pages ! pageId .==. constant pid
         project $ pageLatestVersion << pages ! pageLatestVersion
      where extract [] = error "getPageLatestVersion called with a non existent page"
            extract (hd:_) = hd ! pageLatestVersion

-- | Execute an action inside a db transaction
withDB :: (Database -> IO a) -> IO a
withDB action = hdbcConnect generator (connectSqlite3 dbPath)
                (\db -> transaction db (action db))

-- | Create a new wiki, if it does not exists yet
-- TODO: catch error
addWiki :: ValidWikiName -> IO (Result ())
addWiki wname = do
  withDB (\db ->
          getWikiId db wname >>=
          (maybe
           (do
             insert db wikiTable
                    ( wikiId << _default
                    # wikiName <<- wname
                    )
             wid <- lastInsertedId db
             putStrLn $ ("wid: " ++ show wid)
             insert db pageTable
                    ( wikiId <<- wid
                    # pageId << _default
                    # pageName <<- indexPageName
                    # pageVersion <<- 0
                    # pageLatestVersion <<- 0
                    )
             pid <- lastInsertedId db
             insert db pageVersionTable
                    ( wikiId << constant wid
                    # pageId << constant pid
                    # pageVersion <<- 0
                    # pageContent <<- "Empty page"
                    )
             return (Right ()))
           (\_ -> return (Left WikiAlreadyExists))))

-- | Add a wiki page
addPage :: ValidWikiName -> Page -> IO (Result ())
addPage wname page = do
  withDB (\db ->
            getWikiId db wname >>=
              (maybe (return (Left WikiDoesNotExist))
                     (\wid ->
                        (getPageId db wid $ pName page) >>=
                          (maybe (do
                                   insert db pageTable
                                          ( wikiId <<- wid
                                          # pageId << _default
                                          # pageName <<- pName page
                                          # pageVersion <<- 0
                                          # pageLatestVersion <<- 0
                                          )
                                   pid <- lastInsertedId db
                                   insert db pageVersionTable
                                          ( wikiId <<- wid
                                          # pageId <<- pid
                                          # pageVersion <<- 0
                                          # pageContent <<- pContent page
                                          )
                                   return (Right ()))
                                 (\_ -> return (Left PageAlreadyExists))))))

-- | Edit a wiki page
editPage :: ValidWikiName -> Page -> IO (Result ())
editPage wname page = do
  withDB (\db ->
            getWikiId db wname >>=
              (maybe (return (Left WikiDoesNotExist))
                     (\wid ->
                        (getPageId db wid $ pName page) >>=
                          (maybe (return (Left PageDoesNotExist))
                                 (\pid -> do
                                    version <- succ <$> getPageLatestVersion db wid pid
                                    insert db pageVersionTable
                                           ( wikiId <<- wid
                                           # pageId <<- pid
                                           # pageVersion <<- version
                                           # pageContent <<- pContent page
                                           )
                                    update db pageTable
                                           (\page -> page ! wikiId .==. constant wid .&&.
                                                     page ! pageId .==. constant pid)
                                           (\page -> pageVersion << constant version
                                                   # pageLatestVersion << constant version)
                                    return (Right ()))))))


-- | Get the current version of a wiki page
getPage :: ValidWikiName -> ValidPageName -> IO (Result Page)
getPage wname pname = do
  withDB (\db ->
          getWikiId db wname >>=
          (maybe (return (Left WikiDoesNotExist))
                 (\wid ->
                  getPageId db wid pname >>=
                  (maybe (return (Left PageDoesNotExist))
                   (\pid -> fmap extract $
                            query db $ do
                              page <- table pageTable
                              version <- table pageVersionTable
                              wiki <- table wikiTable
                              restrict $ (wiki ! wikiId .==. constant wid .&&.
                                          page ! wikiId .==. constant wid .&&.
                                          version ! wikiId .==. constant wid .&&.
                                          page ! pageId .==. constant pid .&&.
                                          version ! pageId .==. constant pid .&&.
                                          version ! pageVersion .==. page ! pageVersion)
                              project $ wikiName << wiki ! wikiName
                                      # pageLatestVersion << page ! pageLatestVersion
                                      # pageVersion << page ! pageVersion
                                      # pageName << page ! pageName
                                      # pageContent << version ! pageContent)))))
      where extract [] = Left AbnormalError
            extract (hd:_) = Right (Page
                                    { pVersion = hd ! pageVersion
                                    , pLatestVersion = hd ! pageLatestVersion
                                    , pName = hd ! pageName
                                    , pWikiName = hd ! wikiName
                                    , pContent = hd ! pageContent
                                    })

-- | Return the list of page names for a wiki
getPageNames :: ValidWikiName -> IO (Result [ValidPageName])
getPageNames wname = do
  withDB (\db ->
          getWikiId db wname >>=
          (maybe (return (Left WikiDoesNotExist))
           (\wid ->
            fmap extract $
            query db $ do
              page <- table pageTable
              restrict $ (page ! wikiId .==. constant wid)
              project $ pageName << page ! pageName)))
      where extract = Right . map (! pageName)

-- | Get a given version of a page
getPageVersion :: ValidWikiName -> ValidPageName -> Int -> IO (Result Page)
getPageVersion wname pname v = do
  withDB (\db ->
          getWikiId db wname >>=
          (maybe (return (Left WikiDoesNotExist))
           (\wid ->
            getPageId db wid pname >>=
            (maybe (return (Left PageDoesNotExist))
             (\pid -> fmap extract $
                      query db $ do
                        page <- table pageTable
                        version <- table pageVersionTable
                        wiki <- table wikiTable
                        restrict $ (wiki ! wikiId .==. constant wid .&&.
                                    page ! wikiId .==. constant wid .&&.
                                    version ! wikiId .==. constant wid .&&.
                                    page ! pageId .==. constant pid .&&.
                                    version ! pageId .==. constant pid .&&.
                                    version ! pageVersion .==. constant v)
                        project $ wikiName << wiki ! wikiName
                                # pageName << page ! pageName
                                # pageLatestVersion << page ! pageLatestVersion
                                # pageContent << version ! pageContent)))))
      where extract [] = Left VersionDoesNotExist
            extract (hd:_) = Right (Page
                                    { pVersion = v
                                    , pLatestVersion = hd ! pageLatestVersion
                                    , pName = hd ! pageName
                                    , pWikiName = hd ! wikiName
                                    , pContent = hd ! pageContent
                                    })

-- | Get the existing version numbers of a page
getPageVersions :: ValidWikiName -> ValidPageName -> IO (Result [Int])
getPageVersions wname pname = do
  withDB (\db ->
          getWikiId db wname >>=
          (maybe (return (Left WikiDoesNotExist))
           (\wid ->
            getPageId db wid pname >>=
            (maybe (return (Left PageDoesNotExist))
             (\pid -> fmap extract $
                      query db $ do
                        page <- table pageTable
                        version <- table pageVersionTable
                        wiki <- table wikiTable
                        restrict $ (wiki ! wikiId .==. constant wid .&&.
                                    page ! wikiId .==. constant wid .&&.
                                    version ! wikiId .==. constant wid .&&.
                                    page ! pageId .==. constant pid .&&.
                                    version ! pageId .==. constant pid)
                        project $ pageVersion << version ! pageVersion)))))
      where extract = Right . map (! pageVersion)
