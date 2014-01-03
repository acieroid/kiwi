{-# LANGUAGE OverloadedStrings #-}
module Kiwi.QueryAPI where

import Network.Http.Client (get)
import Data.Aeson (fromJSON, decode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Functor ((<$>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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

getPage :: String -> String -> IO (Maybe Page)
getPage api name =
    get url (\_ i -> fmap decode $ readAll i)
    where url = TE.encodeUtf8 $ T.concat [T.pack api, "/wiki/", T.pack name]
