{-# LANGUAGE OverloadedStrings #-}
module Kiwi.Render where

import Control.Monad (forM_)
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
import qualified Text.Blaze.Internal as HI
import Kiwi.Data

-- TODO: factor out the html part, to have a more generic rendering
-- engine (that can eg. output pdf, etc.)

-- | Skeleton for HTML output. The argument will be put inside the body
skeleton :: String -> H.Html -> H.Html
skeleton name body =
  H.docTypeHtml $ do
    H.head $ do
      H.meta H.! HA.charset "utf-8"
      H.title $ H.toHtml $ name
    H.body body

-- | Something is Renderable if it can be rendered in a file
class Renderable a where
  render :: FilePath -> a -> IO ()

-- | Create the full content of a wiki page
wikiPage :: Page -> H.Html
wikiPage page =
  skeleton name $ do
    H.h1 $ H.toHtml $ name
    content
  where name = show $ pName page
        content = writeHtml def $ readMarkdown def $ pContent page

-- | A wiki page is renrderable
instance Renderable Page where
  render dir page = do
    putStrLn ("Generating page " ++ (show $ pName page) ++
              " in " ++ show pageFile)
    TextIO.writeFile pageFile $ renderMarkup $ wikiPage page
    where pageFile = dir </> (show $ pName page)

-- | Create a page listing all the pages contained in a wiki
wikiPageList :: Wiki -> H.Html
wikiPageList wiki =
  skeleton name $ do
    H.h1 $ H.toHtml $ name
    H.ul $ forM_ pages (\p -> H.li $ H.a H.! HA.href (HI.stringValue $ show p) $
                              H.toHtml $ show p)
  where name = show $ wName wiki
        pages = map fst $ wPages wiki

-- | A wiki is renderable
instance Renderable Wiki where
  render dir wiki = do
    putStrLn ("Generating wiki " ++ (show $ wName wiki) ++
              " in " ++ show wikiDir)
    createDirectoryIfMissing False wikiDir
    TextIO.writeFile pageListFile $ renderMarkup $ wikiPageList wiki
    mapM_ (\(name, page) -> render wikiDir page) $ wPages wiki
    where wikiDir = dir </> (show $ wName wiki)
          pageListFile = wikiDir </> "_pages"
