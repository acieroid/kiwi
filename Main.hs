module Main where

import System.Environment (getProgName)

import qualified Kiwi.API as API
import qualified Kiwi.Config as Config
import qualified Kiwi.Generate as Generate
import qualified Kiwi.Server as Server
import qualified Kiwi.ServerAndAPI as ServerAndAPI
import Kiwi.Data
import qualified Kiwi.Storage as Storage
import qualified Data.Text as T


main :: IO ()
main = do
  target <- Config.target
  case target of
    Config.API -> API.main
    Config.Server -> Server.main
    Config.ServerAndAPI -> ServerAndAPI.main
    Config.Generate -> Generate.main
