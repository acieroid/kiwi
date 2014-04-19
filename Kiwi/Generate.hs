{-# LANGUAGE ScopedTypeVariables #-}
module Kiwi.Generate where

import Control.Monad (forM_)

import qualified Kiwi.Config as Config
import Kiwi.Data
import Kiwi.Render
import Kiwi.QueryAPI

renderAll :: IO ()
renderAll =
  getWikiNames >>= (\wikis -> forM_ wikis renderWiki)

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

renderIndex :: IO ()
renderIndex = do
  dir <- Config.wikiDir
  render dir index

main :: IO ()
main =
  Config.wiki >>=
        maybe (renderIndex >> renderAll)
              (\w -> Config.page >>= maybe (renderWiki w)
                                           (renderPage w))
