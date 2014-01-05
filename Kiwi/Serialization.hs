{-# LANGUAGE OverloadedStrings #-}
module Kiwi.Serialization () where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Text as T

import Kiwi.Data

instance ToJSON Page where
    toJSON page = object [ "version" .= pVersion page
                         , "name" .= (show $ pName page)
                         , "content" .= pContent page ]

instance FromJSON Page where
    parseJSON (Object v) = (validatePageName <$> v .: "name") >>=
                           (maybe mzero
                                  (\name -> build name <$>
                                                  v .: "version" <*>
                                                  v .: "content"))
                           
        where build name version content = Page { pVersion = version
                                                , pName = name
                                                , pContent = content }
    parseJSON _ = mzero

instance ToJSON ValidPageName where
    toJSON = toJSON . show

instance FromJSON ValidPageName where
    parseJSON (Data.Aeson.String s) = maybe mzero return (validatePageName s)
