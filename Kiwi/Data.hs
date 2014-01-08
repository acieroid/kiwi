{-# LANGUAGE OverloadedStrings #-}

module Kiwi.Data (
  ValidPageName, validatePageName, Page(..), examplePage,
  AccessMode(..), ValidWikiName, validateWikiName, Wiki(..), exampleWiki,
  Index(..), index
  ) where

import qualified Data.Text as T
import Text.Regex (Regex, mkRegex, matchRegex)

-- | An opaque type representing a valid name of a page (that
-- can be used in an URL)
data ValidPageName = ValidPageName T.Text
                     deriving Eq

instance Show ValidPageName where
    show (ValidPageName s) = T.unpack s

-- | Regex that valid page names should match
validPageNameRegex :: Regex
validPageNameRegex = mkRegex "^[a-zA-Z0-9/-]+$"

-- | Construct a valid page name
validatePageName :: T.Text -> Maybe ValidPageName
validatePageName s =
    matchRegex validPageNameRegex (T.unpack s) >> Just (ValidPageName s)

-- | Represents a page of the wiki
data Page = Page {
      pVersion  :: Int           -- ^ Version number of this version of the page
    , pName     :: ValidPageName -- ^ Name of the page
    , pWikiName :: ValidWikiName -- ^ Name of the wiki that contains this page
      -- TODO: validate page contents
    , pContent  :: T.Text        -- ^ Raw content of the page
  }
            deriving (Show, Eq)

-- | Example page
examplePage :: Page
examplePage =
    Page { pVersion = 1
         , pName = case validatePageName "index" of
                     Just n -> n
                     Nothing -> error "Invalid page name: index"
         , pWikiName = case validateWikiName "example" of
                         Just n -> n
                         Nothing -> error "Invalid wiki name: example"
         , pContent = "Hello, *world* !"
         }

-- | Possible access modes of a wiki
data AccessMode = ReadWrite     -- ^ Read and write for everybody
                | ReadNoWrite   -- ^ Read for everybody, write protected by password
                | NoReadNoWrite -- ^ Read and write protected by password
                deriving (Show, Eq)

-- | An opaque type representing a valid name of a wiki
data ValidWikiName = ValidWikiName T.Text
                     deriving Eq

instance Show ValidWikiName where
    show (ValidWikiName s) = T.unpack s

-- | Regex that valid wiki names should match
validWikiNameRegex :: Regex
validWikiNameRegex = mkRegex "^[a-zA-Z0-9-]+$"

-- | Construct a valid wiki name
validateWikiName :: T.Text -> Maybe ValidWikiName
validateWikiName s =
    matchRegex validWikiNameRegex (T.unpack s) >> Just (ValidWikiName s)

-- | Represents a wiki
data Wiki = Wiki {
      wName :: ValidWikiName            -- ^ Name of the wiki
    , wPages :: [(ValidPageName, Page)] -- ^ Pages contained in this wiki
    , wAccess :: AccessMode             -- ^ Access mode of this wiki
    , wPassword :: Maybe T.Text         -- ^ Password protecting this wiki
    }
            deriving (Show, Eq)

-- | Example wiki
exampleWiki :: Wiki
exampleWiki =
    Wiki { wName = case validateWikiName "example" of
                     Just n -> n
                     Nothing -> error "Invalid wiki name: example"
         , wPages = [(pName examplePage, examplePage)]
         , wAccess = ReadWrite
         , wPassword = Nothing
         }

-- | Index page
data Index = Index {
      iTitle :: String -- ^ Title of the index page
    , iInfos :: String -- ^ Infos to put on the index page
    }
             deriving (Show, Eq)

-- | The actual index page
index :: Index
index =
    Index { iTitle = "Kiwi!"
          , iInfos = unlines ["### Choose a name and click *create* to add a new wiki",
                              "",
                              "This service is currently highly alpha.",
                              "",
                              "Source code available [here](https://github.com/acieroid/kiwi). Any contribution is welcome!"]
          }
