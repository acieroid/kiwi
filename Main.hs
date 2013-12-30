import Kiwi.Storage
import Kiwi.Data
import Kiwi.Render

main :: IO ()
main = do
  let wiki = validateWikiName "foo"
  maybe (return ())
        (\wiki -> addWiki wiki >>
                  getPageNames wiki >>=
                  putStrLn . show) wiki
  -- render "/tmp/" exampleWiki
