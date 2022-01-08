import Lib
import Data.Foldable
import qualified Data.Text.IO as T

main :: IO ()
main = do
  contents <- T.readFile "test/testinput.xhtml"
  foldrM (\c () -> print c >> T.putStrLn "") () (xmlToSrt contents)
