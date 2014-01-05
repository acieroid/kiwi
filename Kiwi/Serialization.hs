{-# LANGUAGE OverloadedStrings #-}
module Kiwi.Serialization () where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Text as T

import Kiwi.Data

instance ToJSON Page where
    toJSON page = object [ "wiki" .= pWikiName page
                         , "name" .= pName page
                         , "content" .= pContent page
                         , "version" .= pVersion page
                         ]

instance FromJSON Page where
    parseJSON (Object v) = build <$>
                           v .: "wiki" <*>
                           v .: "name" <*>
                           v .: "content" <*>
                           v .: "version"
        where build wname pname version content =
                  Page { pVersion = version
                       , pName = pname
                       , pWikiName = wname
                       , pContent = content }
    parseJSON _ = mzero

instance ToJSON ValidPageName where
    toJSON = toJSON . show

instance FromJSON ValidPageName where
    parseJSON (String s) = maybe mzero return (validatePageName s)

instance ToJSON ValidWikiName where
    toJSON = toJSON . show

instance FromJSON ValidWikiName where
    parseJSON (String s) = maybe mzero return (validateWikiName s)
