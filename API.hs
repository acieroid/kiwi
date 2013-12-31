{-# LANGUAGE OverloadedStrings #-}

import Blaze.ByteString.Builder (copyByteString)
import Data.Monoid
import Data.Text (Text)
import qualified Data.ByteString.UTF8 as BU
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (ResponseHeaders, Status, status200, status404)

main :: IO () 
main = do
    let port = 8000
    putStrLn $ "API listening on port " ++ show port
    run port app

app :: Application
app req = return $
    case (requestMethod req, pathInfo req) of
        ("GET", ["help"]) -> help
        ("GET", ["about"]) -> about
        ("POST", ["wiki"]) -> addWiki req
        ("GET", ["wiki", wname]) -> getWikiPages wname
        ("GET", ["wiki", wname, pname]) -> getWikiPage wname pname
        ("POST", ["wiki", wname, pname]) -> editWikiPage wname pname req
        _ -> notFound

headers :: ResponseHeaders
headers = [("Content-Type", "text/plain")]

build :: Status -> BU.ByteString -> Response
build status response =
    responseBuilder status404 headers $
                    mconcat $ map copyByteString [response]

notFound :: Response
notFound = build status404 "Not Found"

notImplemented :: Response
notImplemented = build status404 "Not Implemented (...yet)"

help :: Response
help = notImplemented -- TODO

about :: Response
about = notImplemented -- TODO

addWiki :: Request -> Response
addWiki req = notImplemented -- TODO

getWikiPages :: Text -> Response
getWikiPages wname = notImplemented -- TODO

getWikiPage :: Text -> Text -> Response
getWikiPage wname pname = notImplemented -- TODO

editWikiPage :: Text -> Text -> Request -> Response
editWikiPage wname pname req = notImplemented -- TODO
