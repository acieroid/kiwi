{-# LANGUAGE OverloadedStrings #-}
module Kiwi.QueryAPI where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson (decode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Http.Client (openConnection, buildRequest, http, Method(..), sendRequest, emptyBody, receiveResponse, closeConnection, setAccept)
import qualified System.IO.Streams as Streams

import qualified Kiwi.Config as Config
import Kiwi.Data
import Kiwi.Serialization ()

readAll :: Streams.InputStream B.ByteString -> IO BL.ByteString
readAll i = BL.concat <$> applyWhileJust Streams.read i
    where applyWhileJust :: (a -> IO (Maybe B.ByteString)) -> a -> IO [BL.ByteString]
          applyWhileJust f x = f x >>=
                               maybe (return [])
                                     (\y -> applyWhileJust f x >>=
                                            return . ((BL.fromStrict y):))

toB :: String -> B.ByteString
toB s = TE.encodeUtf8 $ T.pack s

get :: String -> IO BL.ByteString
get path = do
  port <- Config.port
  host <- Config.host
  c <- openConnection (toB $ host) (fromInteger $ port)
  q <- buildRequest $ do
                 http GET (toB path)
                 setAccept "application/json"
  sendRequest c q emptyBody
  res <- receiveResponse c (\_ i -> readAll i)
  closeConnection c
  return res

getPage :: String -> String -> IO (Maybe Page)
getPage wname pname =
  decode <$> get ("/wiki/" ++ wname ++ "/" ++ pname)

getPages :: String -> [String] -> IO [Page]
getPages _ [] = return []
getPages wname (pname:pnames) = do
  page <- getPage wname pname
  pages <- getPages wname pnames
  return $ maybe pages (:pages) page

getWiki :: String -> IO (Maybe Wiki)
getWiki wname = do
  pageNames <- decode <$> get ("/wiki/" ++ wname)
  pages <- extract $ (getPages wname) <$> pageNames
  return $ build <$> (validateWikiName $ T.pack wname) <*> pages
  where build name pages = Wiki { wName = name
                                , wPages = map (\p -> (pName p, p)) pages
                                , wAccess = ReadWrite -- TODO
                                , wPassword = Nothing -- TODO
                                }
        extract = maybe (return Nothing) (>>= return . Just)

getWikiNames :: IO [String]
getWikiNames =
  (decode <$> get "/wiki/") >>= (maybe (return []) return)
