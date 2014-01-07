{-# LANGUAGE OverloadedStrings #-}
module Kiwi.Server where

import Data.Functor ((<$>))
import Data.Maybe (mapMaybe)
import Network.Wai.Application.Static (staticApp, defaultFileServerSettings)
import Network.Wai
import Network.Wai.Handler.Warp
import WaiAppStatic.Types (ssIndices, ssGetMimeType, toPiece)

import qualified Kiwi.API as API
import qualified Kiwi.Config as Config

main :: IO ()
main = do
  port <- Config.port <$> Config.args
  putStrLn $ "Server listening on port " ++ show port
  run port app

app :: Application
app req = case pathInfo req of
            "wiki":_ -> API.app req
            _ -> static req

-- TODO: not found page, no listing
static :: Application
static = staticApp (defaultFileServerSettings "./wiki")
         { ssGetMimeType = (\_ -> return "html")
         , ssIndices = mapMaybe toPiece ["index"]
         }

