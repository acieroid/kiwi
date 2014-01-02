{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
-- TODO: this module really needs some refactoring
module Kiwi.Storage (
  addWiki, editPage, getPage, getPageNames, Result(..)
  ) where

import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Builder (toLazyByteString, byteString)
import Data.ByteString.Lazy.Builder.ASCII (lazyByteStringHexFixed)
import Data.ByteString.Internal (c2w, w2c)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Crypto.Hash.SHA1 as SHA1
import Control.Applicative ((<$>), (<*>), pure)
import Data.Maybe (mapMaybe)
import Data.Time.LocalTime (getZonedTime)
import Database.Redis (connect, ConnectInfo, defaultConnectInfo,
                       get, set, incr, Redis, RedisCtx, runRedis,
                       Reply, Status)

import Kiwi.Data

data Result = Success
            | PasswordProtected
            | WrongPassword
            | PageDoesNotExists
            | WikiDoesNotExists
            | AlreadyExists
            | ReturnPage Page
            | ReturnPageNames [ValidPageName]
            | Error String
            deriving (Show, Eq)

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

-- | Encode a bunch of strings into a bytestring
enc :: [String] -> BSU.ByteString
enc = TE.encodeUtf8 . T.concat . map T.pack

-- | Information to connect to the Redis server
connectInfo :: ConnectInfo
connectInfo = defaultConnectInfo

-- | Get the next version of a wiki page
nextPageVersion :: Functor f => RedisCtx m f => Integer -> Integer -> m (f Integer)
nextPageVersion wid pid = do
  latest <- get $ enc ["wiki.", show wid, ".pages.", show pid, ".latest"]
  return $ maybe 0 ((1+) . read. BSU.toString) <$> latest

-- | Get the id of a wiki, given its name
getWikiId :: Functor f => RedisCtx m f => ValidWikiName -> m (f (Maybe Integer))
getWikiId name = do
  wid <- get $ enc ["wiki.hashes.", (hash $ show name)]
  return $ fmap (read . BSU.toString) <$> wid

-- | Get the id of a wiki page, given its name
getPageId :: Functor f => RedisCtx m f => Integer -> ValidPageName -> m (f (Maybe Integer))
getPageId wid name = do
  pid <- get $ enc ["wiki.", show wid, ".pages.hashes.", (hash $ show name)]
  return $ fmap (read . BSU.toString) <$> pid

-- | Increase the id that will be used for the next wiki, and return
-- its previous value
increaseWikiId :: Functor f => RedisCtx m f => m (f Integer)
increaseWikiId =
  incr "wiki.nextwid" >>= (return . fmap pred)

-- | Increase the id that will be used for the next page for a given
-- wiki, and return its previous value
increasePageId :: Functor f => RedisCtx m f => Integer -> m (f Integer)
increasePageId wid =
  incr (enc ["wiki.", show wid, ".pages.nextpid"]) >>= (return . fmap pred)

-- | Check if a wiki exists
doesWikiExists :: Functor f => RedisCtx m f => ValidWikiName -> m (f Bool)
doesWikiExists name = do
  wid <- getWikiId name
  return $ fmap (maybe False (\_ -> True)) wid

-- | Extract the errors from an Either and put it inside a Result
ret :: Either Reply Result -> Result
ret (Right x) = x
ret (Left err) = Error (show err)

-- | Return the list of page names for a wiki
getPageNames :: ValidWikiName -> IO Result
getPageNames name = do
  conn <- connect connectInfo
  fmap ret $ runRedis conn $ do
    wid <- getWikiId name
    let f :: Maybe Integer -> Redis (Either Reply Result)
        f = maybe (return $ (return WikiDoesNotExists))
                      (\wid -> do res <- aux wid 0
                                  return $ fmap (ReturnPageNames . mapMaybe validatePageName) res)
    case wid of
      Right w -> f w
      Left err -> return $ Left err
  where pname :: Integer -> Integer -> Redis (Either Reply (Maybe T.Text))
        pname wid pid = do
                      n <- get $ enc ["wiki.", show wid, ".pages.", show pid, ".name"]
                      return $ maybe Nothing (return . TE.decodeUtf8) <$> n
        aux :: Integer -> Integer -> Redis (Either Reply [T.Text])
        aux wid pid = do
                      pn <- pname wid pid
                      case pn of
                        Right (Just pn) ->
                            do rest <- aux wid (succ pid)
                               return $ pure (pn:) <*> rest
                        Right Nothing -> return $ Right []
                        Left err -> return $ Left err

-- | Create a new wiki, if it does not exists yet
addWiki :: ValidWikiName -> IO Result
addWiki name = do
  conn <- connect connectInfo
  date <- getZonedTime
  fmap ret $ runRedis conn $ do
    exists <- doesWikiExists name
    case exists of
      Right True -> return $ Right AlreadyExists
      Right False -> do
          wid <- increaseWikiId
          let f :: Integer -> Redis (Either Reply Result)
              f wid = do let prefix :: String
                             prefix = "wiki." ++ show wid
                         let s :: String -> String -> Redis (Either Reply Status)
                             s k v = set (BSU.fromString k) (BSU.fromString v)
                         let (==>) :: String -> String -> Redis (Either Reply Status)
                             (==>) suffix value = s (prefix ++ suffix) value
                         s ("wiki.hashes." ++ (hash $ show name)) (show wid)
                         ".name" ==> show name
                         ".date" ==> show date
                         ".pages.nextpid" ==> "1"
                         (".pages.hashes." ++ (hash "index")) ==> "0"
                         ".pages.0.name" ==> "index"
                         ".pages.0.version.0.content" ==> "Hello!"
                         ".pages.0.version.0.date" ==> show date
                         ".pages.0.current" ==> "0"
                         ".pages.0.latest" ==> "0"
                         return $ Right Success
          case wid of
            Right w -> f w
            Left err -> return $ Left err
      Left err -> return $ Left err

-- | Edit a wiki page
editPage :: ValidWikiName -> Page -> IO Result
editPage name page = do
  conn <- connect connectInfo
  date <- getZonedTime
  fmap ret $ runRedis conn $ do
    wid <- getWikiId name
    let editPage' :: Integer -> Integer -> Redis (Either Reply Result)
        editPage' wid pid = do
          let prefix = "wiki." ++ (show wid) ++ ".pages." ++ (show pid)
          let (==>) suffix value = set (BSU.fromString $ prefix ++ suffix) (BSU.fromString value)
          let (==>>) suffix value = set (BSU.fromString $ prefix ++ suffix) (TE.encodeUtf8 value)
          version <- nextPageVersion wid pid
          case version of
            Right v ->
                do ".name" ==> show (pName page)
                   (".version." ++ show v ++ ".content") ==>> pContent page
                   (".version." ++ show v ++ ".date") ==> show date
                   ".current" ==> show v
                   ".latest" ==> show v
            Left err -> return $ Left err
          return $ Right Success
    case wid of
      Right Nothing -> return $ Right WikiDoesNotExists
      Right (Just wid') ->
          do (pid :: Either Reply (Maybe Integer)) <- getPageId wid' (pName page)
             (pid' :: Either Reply Integer) <- case pid of
                                    Right Nothing -> increasePageId wid'
                                    Right (Just x) -> return $ Right x
                                    Left err -> return $ Left err
             case pid' of
               Right p -> editPage' wid' p
               Left err -> return $ Left err
      Left err -> return $ Left err

-- | Get the latest version of a wiki page
getPage :: ValidWikiName -> ValidPageName -> IO Result
getPage wname pname = do
  conn <- connect connectInfo
  fmap ret $ runRedis conn $ do
    wid <- getWikiId wname
    case wid of
      Right Nothing -> return $ Right WikiDoesNotExists
      Right (Just wid) ->
          do pid <- getPageId wid pname
             case pid of
               Right Nothing -> return $ Right PageDoesNotExists
               Right (Just pid) ->
                   do let prefix = "wiki." ++ show wid ++ ".pages." ++ show pid
                      let build :: Int -> Maybe BSU.ByteString -> Result
                          build version content =
                              let c = maybe T.empty TE.decodeUtf8 content in
                              ReturnPage (Page
                                          { pVersion = version
                                          , pName = pname
                                          , pContent = c })
                      version <- get $ enc [prefix, ".current"]
                      let v = case version of
                                Right (Just x) -> read $ BSU.toString x
                                -- Should probably transmit the error if Left
                                _ -> 0
                      content <- get $ enc [prefix, ".version.", show v, ".content"]
                      return $ pure (build v) <*> content
               Left err -> return $ Left err
      Left err -> return $ Left err
