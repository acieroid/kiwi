module Kiwi.Render where

import Data.Set as Set
import Data.Text.Lazy.IO as TextIO
import Text.Pandoc.Options (def)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.HTML (writeHtml)
import Text.Blaze.Renderer.Text (renderMarkup)
import Kiwi.Data

class Renderable a where
  render :: FilePath -> a -> IO ()

instance Renderable Page where
  render file page =
    -- TODO: add page skeleton with Text.Blaze.Html before rendering
    TextIO.writeFile file content
    where content = renderMarkup $ writeHtml def $ readMarkdown def $ pContent page
