import qualified Data.Text as T
import Kiwi.Storage
import Kiwi.Data
import Kiwi.Render

main :: IO ()
main = do
  let wiki = validateWikiName (T.pack "foo")
  maybe (return ())
        (\wiki -> addWiki wiki >>
                  getPageNames wiki >>=
                  putStrLn . show) wiki
  -- render "/tmp/" exampleWiki
