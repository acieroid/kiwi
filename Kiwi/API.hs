{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Kiwi.API where

import Blaze.ByteString.Builder (fromLazyByteString)
import Data.Aeson
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Monoid (mconcat)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (ResponseHeaders, Status(..), status200, status404, status500)

import Kiwi.Data
import Kiwi.Serialization
import qualified Kiwi.Storage as S

main :: IO () 
main = do
    let port = 8000
    putStrLn $ "API listening on port " ++ show port
    run port app

app :: Application
app req =
    case (requestMethod req, simplify $ pathInfo req) of
        ("POST", ["wiki", wname]) -> addWiki wname req
        ("GET", ["wiki", wname]) -> getWikiPages wname
        ("GET", ["wiki", wname, pname]) -> getWikiPage wname pname
        ("POST", ["wiki", wname, pname]) -> editWikiPage wname pname req
        _ -> return notFound
    where dropEmpty = filter (not . T.null)
          dropApi ("api":rest) = rest
          dropApi url = url
          simplify = dropApi . dropEmpty

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

success :: Value
success = object ["result" .= ("success" :: T.Text)]

failure :: T.Text -> Value
failure reason = object [ "result" .= ("failure" :: T.Text)
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
    build status500 $ failure $ T.pack err

notFound :: Response
notFound = build status404 $ failure "Not Found"

notImplemented :: Response
notImplemented = build status404 $ failure "Not Implemented (...yet)"

withWikiName :: (ValidWikiName -> IO S.Result) -> T.Text -> IO Response
withWikiName action wname =
    maybe (return $ build statusInvalidName $
                  failure $ T.concat ["Invalid wiki name: ", wname])
          (\name -> action name >>= (return . buildResult))
          (validateWikiName wname)

withPageName :: (ValidWikiName -> ValidPageName -> IO S.Result) -> T.Text -> T.Text -> IO Response
withPageName action wname pname =
    maybe (return $ build statusInvalidName $
                  failure $ T.concat ["Invalid wiki name: ", wname])
          (\w -> maybe (return $ build statusInvalidName $
                               failure $ T.concat ["Invalid page name: ", pname])
                       (\p -> action w p >>= (return . buildResult))
                       (validatePageName pname))
          (validateWikiName wname)

addWiki :: T.Text -> Request -> IO Response
addWiki wname req =
    withWikiName S.addWiki wname

getWikiPages :: T.Text -> IO Response
getWikiPages wname =
    withWikiName S.getPageNames wname

getWikiPage :: T.Text -> T.Text -> IO Response
getWikiPage wname pname =
    withPageName S.getPage wname pname

editWikiPage :: T.Text -> T.Text -> Request -> IO Response
editWikiPage wname pname req = do
  content <- lazyRequestBody req
  let strictContent = B.concat $ BL.toChunks content
  withPageName (\w p -> S.editPage w 
                        (Page { pVersion = 0
                              , pName = p
                              , pWikiName = w
                              , pContent = TE.decodeUtf8 strictContent }))
               wname pname
