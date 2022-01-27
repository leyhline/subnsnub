module Main where

import qualified Data.Text.IO as T
import System.Environment
import SilenceDetect
import Subtitle

silenceDuration :: Double
silenceDuration = 0.6

main :: IO ()
main = do
  (path:_) <- getArgs
  intervals <- detectSilence silenceDuration path
  let captions = intervalsToCaptions intervals
  T.putStrLn $ srtSubtitles captions

intervalsToCaptions :: [SilenceInterval] -> [Caption]
intervalsToCaptions ivls = zipWith3 f [1..] ivls (tail ivls)
  where
    f i (SilenceInterval _ stop) (SilenceInterval start _) =
      Caption i (secondsToTime $ stop - (silenceDuration*2/3)) (secondsToTime $ start + (silenceDuration/3)) ""
