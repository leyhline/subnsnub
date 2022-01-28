module Main where

import System.IO
import System.Environment
import Subtitle
import qualified Data.Text.IO as TIO

main :: IO ()
main =
  let f hdl = TIO.putStrLn . captionsToXml . readSrt =<< TIO.hGetContents hdl
  in getArgs >>= \(path:_) -> withFile path ReadMode f
