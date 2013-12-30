import Kiwi.Storage
import Kiwi.Data
import Kiwi.Render

main :: IO ()
main = maybe (return ())
       (\wname -> getPageNames wname >>= putStrLn . show) (validateWikiName "foo")
  -- render "/tmp/" exampleWiki
