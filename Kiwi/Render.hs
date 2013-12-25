module Kiwi.Render where

import Data.Set as Set
import Text.Pandoc.Options (def)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.HTML (writeHtml)
import Text.Blaze.Renderer.String (renderMarkup)
import Kiwi.Data

class Renderable a where
  render :: a -> String

instance Renderable Page where
  render page = renderMarkup $ writeHtml def $ readMarkdown def $ content page
