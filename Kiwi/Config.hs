{-# LANGUAGE DeriveDataTypeable #-}
module Kiwi.Config where

import System.Console.CmdArgs (cmdArgs, Data, Typeable)

data Args = Args
    { wikidir :: FilePath
    , port :: Int
    , host :: String
    }
    deriving (Show, Data, Typeable)

defaultArgs :: Args
defaultArgs = Args
              { wikidir = "./wiki"
              , port = 8000
              , host = "localhost"
              }

args :: IO Args
args = cmdArgs defaultArgs
