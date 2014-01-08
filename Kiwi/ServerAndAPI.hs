{-# LANGUAGE OverloadedStrings #-}
module Kiwi.ServerAndAPI where

import Network.Wai
import Network.Wai.Handler.Warp

import qualified Kiwi.API as API
import qualified Kiwi.Config as Config
import qualified Kiwi.Server as Server

main :: IO ()
main = do
  port <- Config.port
  putStrLn $ "Server and API listening on port " ++ show port
  run (fromInteger port) app

app :: Application
app req = case pathInfo req of
            "wiki":_ -> API.app req
            _ -> Server.static req
