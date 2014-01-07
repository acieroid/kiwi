module Main where

import System.Environment (getProgName)

import qualified Kiwi.API as API
import qualified Kiwi.Generate as Generate
import qualified Kiwi.Server as Server

main :: IO ()
main = do
  name <- getProgName
  case name of
    "api" -> API.main
    "generate" -> Generate.main
    "server" -> Server.main
    _ -> error ("Unknown action for program: " ++ name)
