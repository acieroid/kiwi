{-# LANGUAGE OverloadedStrings #-}

import Blaze.ByteString.Builder (fromLazyByteString)
import qualified Data.ByteString.UTF8 as BU
import Data.Monoid (mconcat)
import Data.Aeson
import qualified Data.Text as T
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
headers = [("Content-Type", "application/json")]

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

build :: ToJSON a => Status -> a -> Response
build status response =
    responseBuilder status headers $ fromLazyByteString $ encode response

instance ToJSON Page where
    toJSON page = object [ "version" .= (show $ pVersion page)
                         , "name" .= (show $ pName page)
                         , "content" .= pContent page ]

instance ToJSON ValidPageName where
    toJSON = toJSON . show

success :: Value
success = object ["result" .= ("success" :: String)]

failure :: String -> Value
failure reason = object [ "result" .= ("failure" :: String)
                        , "reason" .= reason ] 

buildResult :: S.Result -> Response
buildResult S.Success =
    build status200 success
buildResult S.PasswordProtected =
    build statusPasswordProtected $ failure "Missing Password"
buildResult S.WrongPassword =
    build statusPasswordProtected $ failure "Incorrect Password"
buildResult S.PageDoesNotExists =
    build status404 $ failure "Page Not Found"
buildResult S.WikiDoesNotExists =
    build status404 $ failure "Wiki Not Found"
buildResult S.AlreadyExists =
    build statusAlreadyExists $ failure "Already Exists"
buildResult (S.ReturnPage page) =
    build status200 page
buildResult (S.ReturnPageNames names) =
    build status200 names
buildResult (S.Error err) =
    error err -- TODO

notFound :: Response
notFound = build status404 $ failure "Not Found"

notImplemented :: Response
notImplemented = build status404 $ failure "Not Implemented (...yet)"

help :: Response
help = notImplemented -- TODO

about :: Response
about = notImplemented -- TODO

withWikiName :: (ValidWikiName -> IO S.Result) -> String -> IO Response
withWikiName action wname =
    maybe (return $ build statusInvalidName $
                  failure ("Invalid wiki name: " ++ wname))
          (\name -> action name >>= (return . buildResult))
          (validateWikiName wname)

addWiki :: String -> Request -> IO Response
addWiki wname req =
    withWikiName S.addWiki wname

getWikiPages :: String -> IO Response
getWikiPages wname =
    withWikiName S.getPageNames wname

getWikiPage :: String -> String -> IO Response
getWikiPage wname pname = return notImplemented -- TODO

editWikiPage :: String -> String -> Request -> IO Response
editWikiPage wname pname req = return notImplemented -- TODO
