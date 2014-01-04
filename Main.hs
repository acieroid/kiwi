{-# LANGUAGE ScopedTypeVariables #-}
import Control.Applicative ((<$>))
import qualified Data.Text as T
import System.Environment (getArgs, getEnv)
import Control.Exception (catch)

import Kiwi.Data
import Kiwi.Render
import Kiwi.QueryAPI

param :: String -> String -> IO String
param name def = getEnv name `catch` \(_ :: IOError) -> return def

api :: IO API
api = do
  url <- param "KIWI_API_URL" "http://127.0.0.1"
  port <- read <$> param "KIWI_API_PORT" "8000"
  return $ API { apiUrl = url, apiPort = port }

destDir :: IO String
destDir = param "DEST" "./wiki"

renderWiki :: String -> IO ()
renderWiki wname = do
  dir <- destDir
  api >>= getWiki wname >>=
      maybe (putStrLn "No such wiki")
            (render dir)

renderPage :: String -> String -> IO ()
renderPage wname pname = do
  dir <- destDir
  api >>= getPage wname pname >>=
      maybe (putStrLn "No such page")
            (render dir)

usage :: IO ()
usage = putStrLn "usage: kiwi wiki [page]"

main :: IO ()
main = do
  args <- getArgs
  case args of
    wiki:[] -> renderWiki wiki
    wiki:page:[] -> renderPage wiki page
    _ -> usage
