{-# LANGUAGE OverloadedStrings #-}
module Kiwi.Render where

import Control.Monad (forM_)
import qualified Data.Set as Set
import qualified Data.Text as T
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

-- | Skeleton for HTML output, given: its title, its header and its body
skeleton :: String -> H.Html -> H.Html -> H.Html
skeleton title header body =
    H.docTypeHtml $ do
      H.head $ do
        H.meta H.! HA.charset "utf-8"
        H.link H.! HA.rel "stylesheet" H.! HA.type_ "text/css" H.! HA.href "../style.css"
        -- TODO: host this file too
        H.script H.! HA.src "http://code.jquery.com/jquery-1.10.1.min.js" $ ""
        H.script H.! HA.src "../kiwi.js" $ ""
        H.title $ H.toHtml $ title
      H.body $ do
        H.div H.! HA.class_ "content" H.! HA.id "banner" $ header
        H.div H.! HA.class_ "content" H.! HA.id "main" $ body
        H.hr
        H.footer $ do
          "Generated with luvz by "
          H.a "awesom" H.! HA.href "http://awesom.eu"
          ". Get your own instance! Use the "
          H.a "source" H.! HA.href "https://github.com/acieroid/kiwi"
          "."

-- | Something is Renderable if it can be rendered in a file
class Renderable a where
    render :: FilePath -> a -> IO ()

-- | Create the full content of a wiki page
wikiPage :: Page -> H.Html
wikiPage page =
    skeleton (wname ++ "/" ++ pname)
             (do H.h1 $ do
                   H.a (H.toHtml wname) H.! HA.href "./"
                   H.toHtml ("/" ++ pname)
                 H.a "pages" H.! HA.href (HI.stringValue pages)
                 " - "
                 H.a "versions" H.! HA.href (HI.stringValue versions)
                 " - "
                 H.a "edit" H.! HA.id "action" H.! HA.href "#" H.! HA.onclick (HI.stringValue editFn))
             content
    where wname = show $ pWikiName page
          pname = show $ pName page
          pages = "_pages"
          versions = pname ++ "_versions"
          content = writeHtml def $ readMarkdown def $ T.unpack $ pContent page
          editFn = "edit(\"" ++ wname ++ "\",\"" ++ pname ++ "\");"

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
    skeleton name (H.h1 $ H.toHtml name)
             (H.ul $ forM_ pages pageLink)
    where name = show $ wName wiki
          pages = map fst $ wPages wiki
          pageLink p = H.li $ H.a H.! HA.href (HI.stringValue $ show p) $
                                  H.toHtml $ show p

-- | A wiki is renderable
instance Renderable Wiki where
    render dir wiki = do
      putStrLn ("Generating wiki " ++ (show $ wName wiki) ++
                " in " ++ show wikiDir)
      createDirectoryIfMissing True wikiDir
      TextIO.writeFile pageListFile $ renderMarkup $ wikiPageList wiki
      mapM_ (\(name, page) -> render wikiDir page) $ wPages wiki
      where wikiDir = dir </> (show $ wName wiki)
            pageListFile = wikiDir </> "_pages"

-- | Generate a page for the index
indexPage :: Index -> H.Html
indexPage content =
    skeleton (iTitle content) (do H.h1 $ H.toHtml $ iTitle content
                                  H.input H.! HA.type_ "text" H.! HA.id "name"
                                  " "
                                  H.a "create" H.! HA.href "#" H.! HA.onclick "create();")
             pageContent
    where pageContent = writeHtml def $ readMarkdown def $ iInfos content

-- | The index is renderable
instance Renderable Index where
    render dir content = do
      putStrLn "Generating index"
      createDirectoryIfMissing True dir
      TextIO.writeFile (dir </> "index") $ renderMarkup $ indexPage content
