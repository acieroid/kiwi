{-# LANGUAGE OverloadedStrings #-}

import Blaze.ByteString.Builder (copyByteString)
import Data.Monoid
import qualified Data.Text as T
import qualified Data.ByteString.UTF8 as BU
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (ResponseHeaders, Status(..), status200, status404)

import Kiwi.Data
import qualified Kiwi.Storage as S

main :: IO () 
main = do
    let port = 8000
    putStrLn $ "API listening on port " ++ show port
    run port app

app :: Application
app req =
    case (requestMethod req, pathInfo req) of
        ("GET", ["help"]) -> return help
        ("GET", ["about"]) -> return about
        ("POST", ["wiki", wname]) -> addWiki (T.unpack wname) req
        ("GET", ["wiki", wname]) -> getWikiPages (T.unpack wname)
        ("GET", ["wiki", wname, pname]) -> getWikiPage (T.unpack wname) (T.unpack pname)
        ("POST", ["wiki", wname, pname]) -> editWikiPage (T.unpack wname) (T.unpack pname) req
        _ -> return notFound

headers :: ResponseHeaders
headers = [("Content-Type", "text/plain")]

statusInvalidName :: Status
statusInvalidName =
    Status { statusCode = 401
           , statusMessage = "Invalid Name" }

statusPasswordProtected :: Status
statusPasswordProtected =
    Status { statusCode = 401
           , statusMessage = "Password Protected" }

statusAlreadyExists :: Status
statusAlreadyExists =
    Status { statusCode = 409
           , statusMessage = "Already Exists" }

build :: Status -> String -> Response
build status response =
    responseBuilder status headers $
                    mconcat $ map copyByteString [BU.fromString response]

buildResult :: S.Result -> Response
buildResult S.Success =
    build status200 "Success"
buildResult S.PasswordProtected =
    build statusPasswordProtected "Missing Password"
buildResult S.WrongPassword =
    build statusPasswordProtected "Incorrect Password"
buildResult S.PageDoesNotExists =
    build status404 "Page Not Found"
buildResult S.WikiDoesNotExists =
    build status404 "Wiki Not Found"
buildResult S.AlreadyExists =
    build statusAlreadyExists "Already Exists"
buildResult (S.ReturnPage page) =
    error "TODO"
buildResult (S.ReturnPageNames names) =
    error "TODO"
buildResult (S.Error err) =
    error err -- TODO

notFound :: Response
notFound = build status404 "Not Found"

notImplemented :: Response
notImplemented = build status404 "Not Implemented (...yet)"

help :: Response
help = notImplemented -- TODO

about :: Response
about = notImplemented -- TODO

addWiki :: String -> Request -> IO Response
addWiki wname req =
    maybe (return $ build statusInvalidName ("Invalid wiki name: " ++ wname))
          (\name -> S.addWiki name >>= (return . buildResult))
          (validateWikiName wname)

getWikiPages :: String -> IO Response
getWikiPages wname = return notImplemented -- TODO

getWikiPage :: String -> String -> IO Response
getWikiPage wname pname = return notImplemented -- TODO

editWikiPage :: String -> String -> Request -> IO Response
editWikiPage wname pname req = return notImplemented -- TODO
