{-# LANGUAGE OverloadedStrings #-}
module Kiwi.QueryAPI where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson (fromJSON, decode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Http.Client (get)
import qualified System.IO.Streams as Streams

import Kiwi.Serialization
import Kiwi.Data

readAll :: Streams.InputStream B.ByteString -> IO BL.ByteString
readAll i = BL.concat <$> applyWhileJust Streams.read i
    where applyWhileJust :: (a -> IO (Maybe B.ByteString)) -> a -> IO [BL.ByteString]
          applyWhileJust f x = f x >>=
                               maybe (return [])
                                     (\y -> applyWhileJust f x >>=
                                            return . ((BL.fromStrict y):))

getPage :: String -> String -> String -> IO (Maybe Page)
getPage wname pname api = do
  get url (\_ i -> decode <$> readAll i)
  where url = TE.encodeUtf8 $ T.concat [T.pack api, "/wiki/",
                                          T.pack wname, T.pack pname]

getPages :: String -> [String] -> String -> IO [Page]
getPages wname [] api = return []
getPages wname (pname:pnames) api = do
  page <- getPage wname pname api
  pages <- getPages wname pnames api
  return $ maybe pages (:pages) page

getWiki :: String -> String -> IO (Maybe Wiki)
getWiki wname api = do
  pageNames <- get url (\_ i -> fmap decode $ readAll i)
  pages <- extract $ (\ps -> getPages wname ps api) <$> pageNames
  return $ build <$> (validateWikiName $ T.pack wname) <*> pages
  where build wname pages = Wiki { wName = wname
                                 , wPages = map (\p -> (pName p, p)) pages
                                 , wAccess = ReadWrite -- TODO
                                 , wPassword = Nothing -- TODO
                                 }
        extract = maybe (return Nothing) (\x -> x >>= return . Just)
        url = TE.encodeUtf8 $ T.concat [T.pack api, "/wiki/", T.pack wname]
