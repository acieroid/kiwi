module Kiwi.Render where

import qualified Data.Set as Set
import qualified Data.Text.Lazy.IO as TextIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix ((</>), FilePath)
import Text.Pandoc.Options (def)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.HTML (writeHtml)
import Text.Blaze.Renderer.Text (renderMarkup)
import Kiwi.Data

class Renderable a where
  render :: FilePath -> a -> IO ()

instance Renderable Page where
  render dir page = do
    putStrLn ("Generating page " ++ (show $ pName page) ++
              " in " ++ show pageFile)
    -- TODO: add page skeleton with Text.Blaze.Html before rendering
    TextIO.writeFile pageFile content
    where content = renderMarkup $ writeHtml def $ readMarkdown def $ pContent page
          pageFile = dir </> (show $ pName page)

instance Renderable Wiki where
  render dir wiki = do
    putStrLn ("Generating wiki " ++ (show $ wName wiki) ++
              " in " ++ show wikiDir)
    createDirectoryIfMissing False wikiDir
    mapM_ (\(name, page) -> render wikiDir page) $ wPages wiki
    where wikiDir = dir </> (show $ wName wiki)
