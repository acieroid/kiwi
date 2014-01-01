{-# LANGUAGE ScopedTypeVariables #-}
-- TODO: this module really needs some refactoring
module Kiwi.Storage where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Builder (toLazyByteString, stringUtf8)
import Data.ByteString.Lazy.Builder.ASCII (lazyByteStringHexFixed)
import Data.ByteString.Internal (c2w, w2c)
import qualified Crypto.Hash.SHA1 as SHA1
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Debug.Trace
import Data.Time.LocalTime
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

hash :: String -> String
hash s =
    -- TODO: this is really ugly way to convert the bytestring
    -- returned by SHA1.hash into an hexadecimal representation
    map w2c $ BSL.unpack hashed
    where rawHash = BS.unpack $ SHA1.hash $ BS.pack s
          bsHash = toLazyByteString $ stringUtf8 rawHash
          hex = lazyByteStringHexFixed bsHash
          hashed = toLazyByteString hex

connectInfo :: ConnectInfo
connectInfo = defaultConnectInfo

nextPageVersion :: Functor f => RedisCtx m f => Integer -> Integer -> m (f Integer)
nextPageVersion wid pid = do
  latest <- get (BS.pack $ "wiki." ++ show wid ++ ".pages." ++ show pid ++ ".latest")
  return $ maybe 0 ((1+) . read. BS.unpack) <$> latest

getWikiId :: Functor f => RedisCtx m f => ValidWikiName -> m (f (Maybe Integer))
getWikiId name = do
  wid <- get (BS.pack $ "wiki.hashes." ++ (hash $ show name))
  return $ fmap (read . BS.unpack) <$> wid

getPageId :: Functor f => RedisCtx m f => Integer -> ValidPageName -> m (f (Maybe Integer))
getPageId wid name = do
  pid <- get (BS.pack $ "wiki." ++ show wid ++ ".pages.hashes." ++ (hash $ show name))
  return $ fmap (read . BS.unpack) <$> pid

increaseWikiId :: Functor f => RedisCtx m f => m (f Integer)
increaseWikiId =
  incr (BS.pack $ "wiki.nextwid") >>= (return . fmap pred)

increasePageId :: Functor f => RedisCtx m f => Integer -> m (f Integer)
increasePageId wid =
  incr (BS.pack $ "wiki." ++ show wid ++ ".pages.nextpid") >>= (return . fmap pred)

-- TODO: generalize to Functor instead of Either Reply
ret :: Either Reply Result -> Result
ret (Right x) = x
ret (Left err) = Error (show err)

getPageNames :: ValidWikiName -> IO Result
getPageNames name = do
  conn <- connect connectInfo
  fmap ret $ runRedis conn $ do
    (wid :: Either Reply (Maybe Integer)) <- getWikiId name
    let f :: Maybe Integer -> Redis (Either Reply Result)
        f = maybe (return $ (return WikiDoesNotExists))
                      (\wid -> do res <- aux wid 0
                                  return $ fmap (ReturnPageNames . mapMaybe validatePageName) res)
    case wid of
      Right w -> f w
      Left err -> return $ Left err
  where pname :: Integer -> Integer -> Redis (Either Reply (Maybe String))
        pname wid pid = do
                      n <- get $ BS.pack $
                           "wiki." ++ show wid ++ ".pages." ++ show pid ++ ".name"
                      return $ maybe Nothing (return . BS.unpack) <$> n
        aux :: Integer -> Integer -> Redis (Either Reply [String])
        aux wid pid = do
                      pn <- pname wid pid
                      case pn of
                        Right (Just pn) ->
                            do rest <- aux wid (succ pid)
                               return $ pure (pn:) <*> rest
                        Right Nothing -> return $ Right []
                        Left err -> return $ Left err

addWiki :: ValidWikiName -> IO Result
addWiki name = do
  -- TODO: check that no wiki with this name already exists
  conn <- connect connectInfo
  date <- getZonedTime
  fmap ret $ runRedis conn $ do
    wid <- increaseWikiId
    let f :: Integer -> Redis (Either Reply Result)
        f wid = do let prefix :: String
                       prefix = "wiki." ++ show wid
                   let s :: String -> String -> Redis (Either Reply Status)
                       s k v = set (BS.pack k) (BS.pack v)
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

editPage :: ValidWikiName -> Page -> IO Result
editPage name page = do
  conn <- connect connectInfo
  date <- getZonedTime
  fmap ret $ runRedis conn $ do
    wid <- getWikiId name
    let editPage' :: Integer -> Integer -> Redis (Either Reply Result)
        editPage' wid pid = do
          let prefix = "wiki." ++ (show wid) ++ ".pages." ++ (show pid)
          let (==>) suffix value = set (BS.pack $ prefix ++ suffix) (BS.pack value)
          version <- nextPageVersion wid pid
          case version of
            Right v ->
                do ".name" ==> show (pName page)
                   (".version." ++ show v ++ ".content") ==> pContent page
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

getPage :: ValidWikiName -> ValidPageName -> IO Result
getPage wname pname = do
  conn <- connect connectInfo
  fmap ret $ runRedis conn $ do
    -- Looks like this need some kind of refactoring
    wid <- getWikiId wname
    case wid of
      Right Nothing -> return $ Right WikiDoesNotExists
      Right (Just wid) ->
          do pid <- getPageId wid pname
             case pid of
               Right Nothing -> return $ Right PageDoesNotExists
               Right (Just pid) ->
                   do let prefix = "wiki." ++ show wid ++ ".pages." ++ show pid
                      let build :: Int -> Maybe BS.ByteString -> Result
                          build version content =
                              let c = maybe "" BS.unpack content in
                              ReturnPage (Page
                                          { pVersion = version
                                          , pName = pname
                                          , pContent = c })
                      version <- get $ BS.pack $ prefix ++ ".version"
                      let v = case version of
                                Right (Just x) -> read $ BS.unpack x
                                -- Should probably transmit the error if Left
                                _ -> 0
                      content <- get $ BS.pack $ prefix ++ ".version." ++ show v ++ ".content"
                      return $ pure (build v) <*> content
               Left err -> return $ Left err
      Left err -> return $ Left err
