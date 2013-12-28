module Kiwi.Storage where

import Data.ByteString.Char8 as BS
import Data.Digest.Pure.SHA (sha1)
import Data.Time.LocalTime
import Database.Redis (connect, ConnectInfo, defaultConnectInfo,
                       get, set, incr, RedisCtx, runRedis)

import Kiwi.Data

data Result = Success
            | PasswordProtected
            | WrongPassword
            | PageDontExists
            | WikiDontExists
            | AlreadyExists
            | ReturnPage Page
            | ReturnPageNames [ValidPageName]
            | InvalidDatabase
            deriving (Show, Eq)

hash :: String -> Int
hash s = sha1 . BS.pack

connectInfo :: ConnectInfo
connectInfo = defaultConnectInfo

getPageNames :: ValidWikiName -> IO Result
getPageNames name = do
  conn <- connect connectInfo
  runRedis conn $ do
    getWikiId name >>=
      (maybe (return WikiDontExists)
       (\wid -> aux wid 0))
  where aux wid pid = pname wid pid >>= maybe (return []) (\p -> p : (aux (succ pid)))
        pname wid pid = get $ BS.pack $
                        "wiki." ++ show wid ++ ".pages." ++ show pid ++ ".name"

addWiki :: ValidWikiName -> IO Result
addWiki name = do
  conn <- connect connectInfo
  date <- getZonedTime
  runRedis conn $ do
    wid <- nextWikiId conn
    let prefix = "wiki." ++ show wid
    let (==>) suffix value = set (BS.pack $ prefix ++ suffix) (BS.pack value)
    set (BS.pack $ "wiki.hashes." ++ hash (show name)) ==> (BS.pack $ show wid)
    ".name" ==> show name
    ".date" ==> show date
    ".pages.nextpid" ==> "1"
    ".pages.hashes." ++ hash "index" ==> "1"
    ".pages.0.name" ==> "index"
    ".pages.0.version.0.content" ==> "Hello!"
    ".pages.0.version.0.date" ==> show date
    ".pages.0.current" ==> "0"
    ".pages.0.latest" ==> "0"

getPage :: ValidWikiName -> ValidPageName -> IO Result
getPage wname pname = do
  conn <- connect connectInfo
  runRedis conn $ do
    -- Looks like this need some kind of refactoring
    getWikiId wname >>=
      (maybe (return WikiDontExists)
       (\wid ->
         getPageId pname >>=
         (maybe (return PageDontExists)
          (\pid ->
            let prefix = "wiki." ++ show wid ++ ".pages." ++ show pid in
            get (prefix ++ ".version") >>=
            (maybe (return InvalidDatabase)
             (\version ->
               get (prefix ++ ".content") >>=
               (maybe (return InvalidDatabase)
                (\content ->
                  return Page { pVersion = version
                              , pName = pname
                              , pContent = content }))))))))
                                           

editPage :: ValidWikiName -> Page -> IO Result
editPage name page = do
  conn <- connect connectInfo
  runRedis conn $ do
    getWikiId name >>=
      (maybe (return WikiDontExists) $
       (\wid -> getPageId (pName page) >>=
                (maybe increasePageId id) >>=
                editPage' wid))
  where editPage' wid pid = do
          let prefix = "wiki." ++ (show wid) ++ ".pages." ++ (show pid)
          let (==>) suffix value = set (BS.pack $ prefix ++ suffix) (BS.pack value)
          date <- getZonedTime
          version <- nextPageVersion wid pid
          ".name" ==> pName page
          ".version." ++ show version ++ ".content" ==> pContent page
          ".version." ++ show version ++ ".date" ==> date
          ".current" ==> show version
          ".latest" ==> show version


nextWikiId :: RedisCtx m f => m (f Integer)
nextWikiId =
  get (BS.pack "wiki.nextwid") >>= return . maybe 0 read

nextPageVersion :: RedisCtx m f => Int -> Int -> m (f Integer)
nextPageVersion wid pid = do
  latest <- get (BS.pack $ "wiki." ++ show wid ++ ".pages." ++ show pid ++ ".latest")
  return $ maybe 0 ((1+) . read) latest

getWikiId :: RedisCtx m f => ValidWikiName -> m (f (Maybe Integer))
getWikiId name =
  get (BS.pack $ "wiki.hashes" ++ hash $ show name) >>= fmap read

getPageId :: RedisCtx m f => Int -> ValidPageName -> m (f (Maybe Integer))
getPageId wid name =
  get (BS.pack $ "wiki." ++ show wid ++ ".hashes." ++ hash $ show name) >>= fmap read

increasePageId :: RedisCtx m f => Int -> m (f Int)
increasePageId wid =
  incr (BS.pack $ "wiki." ++ show wid ++ ".pages.nextpid")
