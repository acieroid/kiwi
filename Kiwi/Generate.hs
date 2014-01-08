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
                (render (dir ++ "/" ++ wname))

main :: IO ()
main =
  Config.wiki >>=
        maybe (putStrLn "No wiki specified, use --wiki")
              (\w -> Config.page >>= maybe (renderWiki w)
                                           (renderPage w))
