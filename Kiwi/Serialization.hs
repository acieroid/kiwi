{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module defines the ToJSON and FromJSON instances for our types. Once
-- it is imported, Data.Aeson's encode and decode functions will work on Page,
-- ValidPageName and ValidWikiName
module Kiwi.Serialization () where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson ((.=), (.:), FromJSON(..), object, ToJSON(..), Value(..))

import Kiwi.Data

instance ToJSON Page where
    toJSON page = object [ "wiki" .= pWikiName page
                         , "name" .= pName page
                         , "content" .= pContent page
                         , "version" .= pVersion page
                         , "latestversion" .= pLatestVersion page
                         ]

instance FromJSON Page where
    parseJSON (Object v) = build <$>
                           v .: "wiki" <*>
                           v .: "name" <*>
                           v .: "content" <*>
                           v .: "version" <*>
                           v .: "latestversion"
        where build wname pname content version latestVersion =
                  Page { pVersion = version
                       , pLatestVersion = latestVersion
                       , pName = pname
                       , pWikiName = wname
                       , pContent = content }
    parseJSON _ = mzero

instance ToJSON ValidPageName where
    toJSON = toJSON . show

instance FromJSON ValidPageName where
    parseJSON (String s) = maybe mzero return (validatePageName s)
    parseJSON _ = mzero

instance ToJSON ValidWikiName where
    toJSON = toJSON . show

instance FromJSON ValidWikiName where
    parseJSON (String s) = maybe mzero return (validateWikiName s)
    parseJSON _ = mzero
