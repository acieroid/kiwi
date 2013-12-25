import Data.Set as Set
import Text.Pandoc.Options
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML
import Text.Blaze.Renderer.String

data AccessMode = ReadWrite | ReadNoWrite | NoReadNoWrite
                deriving (Show, Eq)

class Renderable a where
  render :: a -> String

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

instance Renderable Page where
  render page = renderMarkup $ writeHtml def $ readMarkdown def $ content page

main :: IO ()
main = putStrLn (render examplePage)
