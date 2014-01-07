{-# LANGUAGE ScopedTypeVariables #-}
module Kiwi.Generate where

import Control.Applicative ((<$>))
import qualified Data.Text as T
import System.Environment (getArgs, getEnv)
import Control.Exception (catch)

import qualified Kiwi.Config as Config
import Kiwi.Data
import Kiwi.Render
import Kiwi.QueryAPI

renderWiki :: String -> IO ()
renderWiki wname = do
  dir <- Config.wikiDir
  getWiki wname >>=
          maybe (putStrLn "No such wiki")
                (render dir)

renderPage :: String -> String -> IO ()
renderPage wname pname = do
  dir <- Config.wikiDir
  getPage wname pname >>=
          maybe (putStrLn "No such page")
                (render dir)

usage :: IO ()
usage =
    putStrLn "usage: generate wiki [page] [options]" >>
    putStrLn "Run generate --help for the list of options"

main :: IO ()
main = do
  args <- getArgs
  case args of
    wiki:[] -> renderWiki wiki
    wiki:page:[] -> renderPage wiki page
    _ -> usage
