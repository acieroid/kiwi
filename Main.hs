{-# LANGUAGE ScopedTypeVariables #-}
import qualified Data.Text as T
import System.Environment (getArgs, getEnv)
import Control.Exception (catch)

import Kiwi.Data
import Kiwi.Render
import Kiwi.QueryAPI

param :: String -> String -> IO String
param name def = getEnv name `catch` \(_ :: IOError) -> return def

apiUrl :: IO String
apiUrl = param "KIWI_API" "http://localhost:8000"

destDir :: IO String
destDir = param "DEST" "./wiki"

renderWiki :: String -> IO ()
renderWiki wname = do
  dir <- destDir
  apiUrl >>= getWiki wname >>=
         maybe (putStrLn "No such wiki")
               (render dir)

renderPage :: String -> String -> IO ()
renderPage wname pname = do
  dir <- destDir
  apiUrl >>= getPage wname pname >>=
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
