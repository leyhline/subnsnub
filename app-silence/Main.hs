module Main where

import System.Environment
import SilenceDetect

main :: IO ()
main = do
  (path:_) <- getArgs
  intervals <- detectSilence path
  mapM_ print intervals
