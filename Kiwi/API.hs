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

import qualified Kiwi.Config as Config
import Kiwi.Data
import Kiwi.Generate
import Kiwi.Serialization
import qualified Kiwi.Storage as S

main :: IO ()
main = do
  port <- Config.port
  putStrLn $ "API listening on port " ++ show port
  run (fromInteger port) app

app :: Application
app req =
    case (requestMethod req, simplify $ pathInfo req) of
        ("POST", ["wiki", wname]) ->
            addWiki wname req
        ("GET", ["wiki", wname]) ->
            getWikiPages wname
        ("GET", "wiki":wname:pname) ->
            getWikiPage wname (T.intercalate "/" pname)
        ("POST", "wiki":wname:pname) ->
            editWikiPage wname (T.intercalate "/" pname) req
        _ -> (return unsupported)
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

buildResult :: ToJSON a => S.Result a -> Response
buildResult (Right x) =
    build status200 x
buildResult (Left err) =
    build s $ failure msg
    where (s, msg) =
              case err of
                S.PasswordProtected -> (statusPasswordProtected, "Missing Password")
                S.WrongPassword -> (statusPasswordProtected, "Incorrect Password")
                S.PageDoesNotExist -> (status404, "Page Not Found")
                S.WikiDoesNotExist -> (status404, "Wiki Not Found")
                S.WikiAlreadyExists -> (statusAlreadyExists, "Wiki Already Exists")
                S.PageAlreadyExists -> (statusAlreadyExists, "Page Already Exists")
                S.AbnormalError -> (status500, "Unexpected Error")

notFound :: Response
notFound = build status404 $ failure "Not Found"

unsupported :: Response
unsupported = build status404 $ failure "Unsupported Operation"

notImplemented :: Response
notImplemented = build status404 $ failure "Not Implemented (...yet)"

withWikiName :: ToJSON a => (ValidWikiName -> IO (S.Result a)) -> T.Text -> IO Response
withWikiName action wname =
    maybe (return $ build statusInvalidName $
                  failure $ T.concat ["Invalid wiki name: ", wname])
          (\name -> action name >>= (return . buildResult))
          (validateWikiName wname)

withPageName :: ToJSON a => (ValidWikiName -> ValidPageName -> IO (S.Result a)) -> T.Text -> T.Text -> IO Response
withPageName action wname pname =
    maybe (return $ build statusInvalidName $
                  failure $ T.concat ["Invalid wiki name: ", wname])
          (\w -> maybe (return $ build statusInvalidName $
                               failure $ T.concat ["Invalid page name: ", pname])
                       (\p -> action w p >>=
                              (return . buildResult))
                       (validatePageName pname))
          (validateWikiName wname)

ifNecessary :: Response -> IO () -> IO ()
ifNecessary response action | responseStatus response == status200 = do
  target <- Config.target
  case target of
    Config.ServerAndAPI -> action
    _ -> return ()
ifNecessary _ _ =
    -- Don't do anything if we don't return 200
    return ()

toSuccess :: S.Result a -> S.Result Value
toSuccess = fmap (const success)

addWiki :: T.Text -> Request -> IO Response
addWiki wname req = do
    response <- withWikiName ((fmap toSuccess) . S.addWiki) wname
    ifNecessary response (renderWiki $ T.unpack wname)
    return response

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
  if B.null strictContent then
      -- New page
      do response <- withPageName (\w p -> S.addPage w
                                           (Page { pVersion = 0
                                                 , pLatestVersion = 0
                                                 , pName = p
                                                 , pWikiName = w
                                                 , pContent = T.pack "This page is currently empty" }))
                                  wname pname
         -- TODO: only wiki index & new page are necessary
         ifNecessary response (renderWiki (T.unpack wname))
         return response
  else
      do response <- withPageName (\w p -> S.editPage w
                                           (Page { pVersion = 0
                                                 , pLatestVersion = 0
                                                 , pName = p
                                                 , pWikiName = w
                                                 , pContent = TE.decodeUtf8 strictContent }))
                                  wname pname
         ifNecessary response (renderPage (T.unpack wname) (T.unpack pname))
         return response
