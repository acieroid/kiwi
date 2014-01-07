{-# LANGUAGE DeriveDataTypeable #-}
module Kiwi.Config where

import Data.Functor ((<$>))
import System.Environment
import System.Console.CmdArgs ((&=), cmdArgs, Data, details, explicit, help,
                               name, program, summary, Typeable)

data Target = ServerAndAPI | Server | API | Generate
            deriving (Show, Data, Typeable)

-- TODO: see http://hackage.haskell.org/package/cmdargs-0.1/src/cmdargs.htm
data Args = Args
    { argWikiDir :: FilePath
    , argPort :: Integer
    , argHost :: String
    , argWiki :: Maybe String
    , argPage :: Maybe String
    , argTarget :: Target
    }
            deriving (Show, Data, Typeable)

defaultArgs :: Args
defaultArgs = Args
              { argWikiDir = "./wiki" &= explicit &= name "wikidir"
                             &= help "Wiki directory"
              , argPort = 8000 &= explicit &= name "port"
                          &= help "Listening port"
              , argHost = "localhost" &= explicit &= name "host"
                          &= help "Listening host"
              , argWiki = Nothing &= explicit &= name "wiki"
                          &= help "Wiki to generate"
              , argPage = Nothing &= explicit &= name "page"
                          &= help "Page to generate"
              , argTarget = ServerAndAPI &= explicit &= name "target"
                            &= help "What to do"
              }
              &= summary "kiwi v0.1"
              &= details ["More information at http://github.com/acieroid/kiwi"]

args :: IO Args
args = getProgName >>= (\p -> cmdArgs (defaultArgs &= program p))

wikiDir :: IO String
wikiDir = argWikiDir <$> args

port :: IO Integer
port = argPort <$> args

host :: IO String
host = argHost <$> args

wiki :: IO (Maybe String)
wiki = argWiki <$> args

page :: IO (Maybe String)
page = argPage <$> args

target :: IO Target
target = argTarget <$> args
