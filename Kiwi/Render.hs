{-# LANGUAGE OverloadedStrings #-}
module Kiwi.Render where

import Control.Monad (forM_, when)
import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TextIO
import System.Directory (createDirectoryIfMissing, copyFile, doesFileExist, removeFile)
import System.FilePath.Posix ((</>), dropFileName)
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
        H.link H.! HA.rel "stylesheet" H.! HA.type_ "text/css" H.! HA.href "/style.css"
        -- TODO: host this file too
        H.script H.! HA.src "http://code.jquery.com/jquery-1.10.1.min.js" $ ""
        H.script H.! HA.src "/kiwi.js" $ ""
        H.title $ H.toHtml title
      H.body $ do
        H.div H.! HA.class_ "content" H.! HA.id "banner" $ header
        H.div H.! HA.class_ "content" H.! HA.id "dialog" $ ""
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

-- | Header of a page
wikiPageHeader :: Page -> H.Html
wikiPageHeader page = do
  H.h1 $ do
    H.a (H.toHtml wname) H.!
     HA.href (HI.stringValue ("/" ++ wname ++ "/"))
    H.toHtml ("/" ++ pname)
  H.a "pages" H.! HA.href (HI.stringValue pages)
  " - "
  H.a "versions" H.! HA.href (HI.stringValue versions)
  " - "
  H.a "new page" H.! HA.href "#" H.! HA.onclick (HI.stringValue newPageFn)
  " - "
  H.a "edit" H.! HA.id "action" H.! HA.href "#" H.! HA.onclick (HI.stringValue editFn)
    where wname = show $ pWikiName page
          pname = show $ pName page
          pages = "/" ++ wname ++ "/_pages"
          versions = "/" ++ wname ++ "/" ++ pname ++ ".versions"
          editFn = "edit(\"" ++ wname ++ "\",\"" ++ pname ++ "\");"
          newPageFn = "newpage(\"" ++ wname ++ "\");"

-- | Create the full content of a wiki page
wikiPage :: Page -> H.Html
wikiPage page =
    skeleton (wname ++ "/" ++ pname)
             (wikiPageHeader page)
             content
    where wname = show $ pWikiName page
          pname = show $ pName page
          content = writeHtml def $ readMarkdown def $ T.unpack $ pContent page

pageFile :: FilePath -> Page -> FilePath
pageFile dir page =
    dir </> (foldl1 (</>) $ pageNameComponents $ pName page)

pageVersionFile :: FilePath -> Page -> FilePath
pageVersionFile dir page =
    (pageFile dir page) ++ "." ++ (show $ pVersion page)

pageVersionListFile :: FilePath -> Page -> FilePath
pageVersionListFile dir page =
    (pageFile dir page) ++ ".versions"

wikiPageVersionList :: Page -> H.Html
wikiPageVersionList page =
    skeleton (wname ++ "/" ++ pname)
             (wikiPageHeader page)
             (H.ul $ forM_ [latestVersion,latestVersion-1..0] pageVersionLink)
    where wname = show $ pWikiName page
          pname = show $ pName page
          latestVersion = pLatestVersion page
          pageVersionLink version =
              H.li (H.a (H.toHtml (if version == pVersion page then
                                         show version ++ " (current)"
                                     else
                                         show version))
                    H.!
                     HA.href (HI.stringValue ("/" ++ wname ++ "/" ++
                                                  pname ++ "." ++ (show version))))

-- | A wiki page is renderable (generate only the given version, and
-- assume that it is the current one)
instance Renderable Page where
    render dir page = do
      putStrLn ("Generating page " ++ (show $ pName page) ++
                " in " ++ show (pageVersionFile dir page))
      createDirectoryIfMissing True $ dropFileName vf
      TextIO.writeFile vf $ renderMarkup $ wikiPage page
      doesFileExist vlf >>= (\e -> when e (removeFile vlf))
      TextIO.writeFile vlf $ renderMarkup $ wikiPageVersionList page
      -- This it now the current version
      copyFile (pageVersionFile dir page) (pageFile dir page)
      where vlf = pageVersionListFile dir page
            vf = pageVersionFile dir page

-- | Create a page listing all the pages contained in a wiki
wikiPageList :: Wiki -> H.Html
wikiPageList wiki =
    skeleton name (do H.h1 $ H.toHtml name
                      H.a "new page" H.! HA.href "#" H.! HA.onclick (HI.stringValue newPageFn))
             (H.ul $ forM_ pages pageLink)
    where name = show $ wName wiki
          pages = sort $ map fst $ wPages wiki
          pageLink p = H.li $ H.a H.! HA.href (HI.stringValue $ show p) $
                                  H.toHtml $ show p
          newPageFn = "newpage(\"" ++ name ++ "\");"

-- | A wiki is renderable
instance Renderable Wiki where
    render dir wiki = do
      putStrLn ("Generating wiki " ++ (show $ wName wiki) ++
                " in " ++ show wikiDir)
      createDirectoryIfMissing True wikiDir
      TextIO.writeFile pageListFile $ renderMarkup $ wikiPageList wiki
      mapM_ (\(_, page) -> render wikiDir page) $ wPages wiki
      where wikiDir = dir </> (show $ wName wiki)
            pageListFile = wikiDir </> "_pages"

-- | Generate a page for the index
indexPage :: Index -> H.Html
indexPage content =
    skeleton (iTitle content) (do H.h1 $ H.toHtml $ iTitle content
                                  H.a "create" H.! HA.href "#" H.! HA.onclick "create();")
             pageContent
    where pageContent = writeHtml def $ readMarkdown def $ iInfos content

-- | The index is renderable
instance Renderable Index where
    render dir content = do
      putStrLn "Generating index"
      createDirectoryIfMissing True dir
      TextIO.writeFile (dir </> "index") $ renderMarkup $ indexPage content
