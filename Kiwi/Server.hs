{-# LANGUAGE OverloadedStrings #-}
import Network.Wai.Application.Static (staticApp, defaultFileServerSettings)
import Network.Wai
import Network.Wai.Handler.Warp

import qualified Kiwi.API as API

-- TODO: System.Console.CmdArgs

main :: IO ()
main = do
    let port = 8000
    putStrLn $ "Server listening on port " ++ show port
    run port app

app :: Application
app req = case pathInfo req of
            "api":_ -> API.app req
            "wiki":_ -> static req
            _ -> return API.notFound

static :: Application
static = staticApp (defaultFileServerSettings "wiki/")

