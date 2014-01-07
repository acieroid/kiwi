{-# LANGUAGE OverloadedStrings #-}
module Kiwi.Server where

import Network.Wai.Application.Static (staticApp, defaultFileServerSettings)
import Network.Wai
import Network.Wai.Handler.Warp
import WaiAppStatic.Types (ssIndices, ssGetMimeType, toPiece)
import Data.Maybe (mapMaybe)

import qualified Kiwi.API as API

-- TODO: System.Console.CmdArgs

main :: IO ()
main = do
    let port = 8000
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

