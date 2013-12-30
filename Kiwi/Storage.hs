{-# LANGUAGE ScopedTypeVariables #-}
-- TODO: this module really needs some refactoring
module Kiwi.Storage where

import qualified Data.ByteString.Char8 as BS
import qualified Crypto.Hash.SHA1 as SHA1
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Time.LocalTime
import Database.Redis (connect, ConnectInfo, defaultConnectInfo,
                       get, set, incr, Redis, RedisCtx, runRedis,
                       Reply, Status)

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
            | Error String
            deriving (Show, Eq)

hash :: String -> String
hash s = BS.unpack $ SHA1.hash $ BS.pack s

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
  pid <- get (BS.pack $ "wiki." ++ show wid ++ ".hashes." ++ (hash $ show name))
  return $ fmap (read . BS.unpack) <$> pid

increaseWikiId :: RedisCtx m f => m (f Integer)
increaseWikiId =
  incr (BS.pack $ "wiki.nextwid")

increasePageId :: Functor f => RedisCtx m f => Integer -> m (f Integer)
increasePageId wid =
  incr (BS.pack $ "wiki." ++ show wid ++ ".pages.nextpid")

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
        f = maybe (return $ (return WikiDontExists))
                      (\wid -> do res <- aux wid 0
                                  return $ fmap (ReturnPageNames . mapMaybe validatePageName) res)
    case wid of
      Right w -> f w
      Left err -> return $ Left err
  where pname :: Integer -> Integer -> Redis (Either Reply String)
        pname wid pid = do
                      n <- get $ BS.pack $
                           "wiki." ++ show wid ++ ".pages." ++ show pid ++ ".name"
                      return $ maybe "" BS.unpack <$> n
        aux :: Integer -> Integer -> Redis (Either Reply [String])
        aux wid pid = do
                      pn <- pname wid pid
                      rest <- aux wid (succ pid)
                      return $ pure (:) <*> pn <*> rest

addWiki :: ValidWikiName -> IO Result
addWiki name = do
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
                   (".pages.hashes." ++ (hash "index")) ==> "1"
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
          ".name" ==> show (pName page)
          (".version." ++ show version ++ ".content") ==> pContent page
          (".version." ++ show version ++ ".date") ==> show date
          ".current" ==> show version
          ".latest" ==> show version
          return $ Right Success
    case wid of
      Right Nothing -> return $ Right WikiDontExists
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
      Right Nothing -> return $ Right WikiDontExists
      Right (Just wid) ->
          do pid <- getPageId wid pname
             case pid of
               Right Nothing -> return $ Right PageDontExists
               Right (Just pid) ->
                   do let prefix = "wiki." ++ show wid ++ ".pages." ++ show pid
                      let build :: Maybe BS.ByteString -> Maybe BS.ByteString -> Result
                          build version content =
                              let v = maybe 0 (read . BS.unpack) version
                                  c = maybe "" BS.unpack content in
                              ReturnPage (Page
                                          { pVersion = v
                                          , pName = pname
                                          , pContent = c })
                      version <- get $ BS.pack $ prefix ++ ".version"
                      content <- get $ BS.pack $ prefix ++ ".content"
                      return $ pure build <*> version <*> content
               Left err -> return $ Left err
      Left err -> return $ Left err
