import Kiwi.Storage
import Kiwi.Data
import Kiwi.Render

main :: IO ()
main = putStrLn $ show getPageNames (validateWikiName "foo")
  -- render "/tmp/" exampleWiki
