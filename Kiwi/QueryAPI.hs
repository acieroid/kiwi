{-# LANGUAGE OverloadedStrings #-}
module Kiwi.QueryAPI where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson (fromJSON, decode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Http.Client (openConnection, buildRequest, http, Method(..), sendRequest, emptyBody, receiveResponse, closeConnection, setAccept)
import qualified System.IO.Streams as Streams

import Kiwi.Serialization
import Kiwi.Data

data API = API {
      apiUrl :: String
    , apiPort :: Integer
    } deriving (Show)

readAll :: Streams.InputStream B.ByteString -> IO BL.ByteString
readAll i = BL.concat <$> applyWhileJust Streams.read i
    where applyWhileJust :: (a -> IO (Maybe B.ByteString)) -> a -> IO [BL.ByteString]
          applyWhileJust f x = f x >>=
                               maybe (return [])
                                     (\y -> applyWhileJust f x >>=
                                            return . ((BL.fromStrict y):))

toB :: String -> B.ByteString
toB s = TE.encodeUtf8 $ T.pack s

get :: API -> String -> IO BL.ByteString
get api path = do
  c <- openConnection (toB $ apiUrl api) (fromInteger $ apiPort api)
  q <- buildRequest $ do
                 http GET (toB path)
                 setAccept "application/json"
  sendRequest c q emptyBody
  res <- receiveResponse c (\_ i -> readAll i)
  closeConnection c
  return res

getPage :: String -> String -> API -> IO (Maybe Page)
getPage wname pname api =
  decode <$> get api ("/wiki/" ++ wname ++ "/" ++ pname)

getPages :: String -> [String] -> API -> IO [Page]
getPages wname [] api = return []
getPages wname (pname:pnames) api = do
  page <- getPage wname pname api
  pages <- getPages wname pnames api
  return $ maybe pages (:pages) page

getWiki :: String -> API -> IO (Maybe Wiki)
getWiki wname api = do
  pageNames <- decode <$> get api ("/wiki/" ++ wname)
  pages <- extract $ (\ps -> getPages wname ps api) <$> pageNames
  return $ build <$> (validateWikiName $ T.pack wname) <*> pages
  where build wname pages = Wiki { wName = wname
                                 , wPages = map (\p -> (pName p, p)) pages
                                 , wAccess = ReadWrite -- TODO
                                 , wPassword = Nothing -- TODO
                                 }
        extract = maybe (return Nothing) (\x -> x >>= return . Just)
