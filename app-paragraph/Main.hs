module Main where

import System.IO
import System.Environment
import XmlExtract
import qualified Data.Text.IO as T

main :: IO ()
main =
  let f hdl = mapM_ T.putStrLn . extractParagraphs =<< T.hGetContents hdl
  in getArgs >>= \(path:_) -> withFile path ReadMode f
