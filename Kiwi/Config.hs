{-# LANGUAGE DeriveDataTypeable #-}
module Kiwi.Config where

import Data.Functor ((<$>))
import System.Environment
import System.Console.CmdArgs ((&=), cmdArgs, Data, details, help, program,
                               summary, Typeable)

data Args = Args
    { argWikiDir :: FilePath
    , argPort :: Integer
    , argHost :: String
    }
    deriving (Show, Data, Typeable)

defaultArgs :: Args
defaultArgs = Args
              { argWikiDir = "./wiki" &= help "Wiki directory"
              , argPort = 8000 &= help "Listening port"
              , argHost = "localhost" &= help "Listening host"
              }
              &= summary "kiwi v0.1"
              &= details ["More info at http://github.com/acieroid/kiwi"]

args :: IO Args
args = getProgName >>= (\p -> cmdArgs (defaultArgs &= program p))

progName :: IO String
progName = getProgName

wikiDir :: IO String
wikiDir = argWikiDir <$> args

port :: IO Integer
port = argPort <$> args

host :: IO String
host = argHost <$> args


