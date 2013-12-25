module Kiwi.Data where

data AccessMode = ReadWrite | ReadNoWrite | NoReadNoWrite
                deriving (Show, Eq)

data Page = Page {
    version :: Int
  , name :: String
  , content :: String
  }
          deriving (Show, Eq)

data Wiki = Wiki {
    pages :: [(String, Page)]
  , access :: AccessMode
  , password :: Maybe String
  }
          deriving (Show, Eq)

examplePage :: Page
examplePage =
  Page { version = 1
       , name = "index"
       , content = "Hello, *world* !"
       }

exampleWiki :: Wiki
exampleWiki =
  Wiki { pages = [(name examplePage, examplePage)]
       , access = ReadWrite
       , password = Nothing
       }
