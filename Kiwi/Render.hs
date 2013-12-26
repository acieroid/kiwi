{-# LANGUAGE OverloadedStrings #-}
module Kiwi.Render where

import qualified Data.Set as Set
import qualified Data.Text.Lazy.IO as TextIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix ((</>), FilePath)
import Text.Pandoc.Options (def)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.HTML (writeHtml)
import Text.Blaze.Renderer.Text (renderMarkup)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Kiwi.Data

-- | Create the full content of a wiki page, given the content to put
-- inside.
wikiPage :: String -> H.Html -> H.Html
wikiPage name content =
  H.docTypeHtml $ do
    H.head $ do
      H.meta H.! HA.charset "utf-8"
      H.title $ H.toHtml $ name
    H.body $ do H.h1 $ H.toHtml $ name
                content

class Renderable a where
  render :: FilePath -> a -> IO ()

instance Renderable Page where
  render dir page = do
    putStrLn ("Generating page " ++ (show $ pName page) ++
              " in " ++ show pageFile)
    TextIO.writeFile pageFile $ renderMarkup $ wikiPage (show $ pName page) content
    where content = writeHtml def $ readMarkdown def $ pContent page
          pageFile = dir </> (show $ pName page)

instance Renderable Wiki where
  render dir wiki = do
    putStrLn ("Generating wiki " ++ (show $ wName wiki) ++
              " in " ++ show wikiDir)
    createDirectoryIfMissing False wikiDir
    mapM_ (\(name, page) -> render wikiDir page) $ wPages wiki
    where wikiDir = dir </> (show $ wName wiki)
