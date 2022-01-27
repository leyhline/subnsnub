module Main where

import System.IO
import System.Environment
import XmlExtract
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main =
  let f hdl = TIO.putStrLn . T.intercalate "\n\n" . extractParagraphs =<< TIO.hGetContents hdl
  in getArgs >>= \(path:_) -> withFile path ReadMode f
