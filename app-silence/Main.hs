module Main where

import qualified Data.List as L
import System.Environment
import SilenceDetect
import Subtitle

main :: IO ()
main = do
  (path:_) <- getArgs
  intervals <- detectSilence path
  let captions = intervalsToCaptions intervals
      captionStrs = map show captions
      captionStrsWithSep = L.intercalate "\n\n" captionStrs
  putStrLn captionStrsWithSep

intervalsToCaptions :: [SilenceInterval] -> [Caption]
intervalsToCaptions ivls = zipWith3 f [1..] ivls (tail ivls)
  where
    f i (SilenceInterval _ stop) (SilenceInterval start _) =
      Caption i (secondsToTime stop) (secondsToTime start) "_"
