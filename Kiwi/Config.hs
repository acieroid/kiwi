{-# LANGUAGE DeriveDataTypeable #-}
module Kiwi.Config where

import System.Environment
import System.Console.CmdArgs ((&=), cmdArgs, Data, details, help, program,
                               summary, Typeable)

data Args = Args
    { wikidir :: FilePath
    , port :: Int
    , host :: String
    }
    deriving (Show, Data, Typeable)

defaultArgs :: Args
defaultArgs = Args
              { wikidir = "./wiki" &= help "Wiki directory"
              , port = 8000 &= help "Listening port"
              , host = "localhost" &= help "Listening host"
              }
              &= summary "kiwi v0.1"
              &= details ["More info at http://github.com/acieroid/kiwi"]

args :: IO Args
args = getProgName >>= (\p -> cmdArgs (defaultArgs &= program p))
