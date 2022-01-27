module Subtitle
  ( secondsToTime
  , srtSubtitles
  , vttSubtitles
  , Caption(..)
  , Time(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf

data Caption = Caption
  { counter   :: Integer
  , appear    :: Time
  , disappear :: Time
  , text      :: Text
  }
  deriving (Eq, Show)

--               hour    min     sec     msec
data Time = Time Integer Integer Integer Integer
  deriving (Eq, Ord, Show)

showTime :: Char -> Time -> Text
showTime mssep (Time h m s ms) = T.pack $ printf ("%02d:%02d:%02d" ++ mssep:"%03d") h m s ms

secondsToTime :: Double -> Time
secondsToTime d = Time h m s ms
  where
    (h, rest) = floor d `divMod` 3600
    (m, s) = rest `divMod` 60
    ms = round (1000 * d) `mod` 1000

srtSubtitles :: [Caption] -> Text
srtSubtitles = T.intercalate "\n\n" . map (showCaption ',')

vttSubtitles :: [Caption] -> Text
vttSubtitles = T.append header . T.intercalate "\n\n" . map (showCaption '.')
  where header = "WEBVTT\n\n"

showCaption :: Char -> Caption -> Text
showCaption mssep (Caption c a da t) = T.concat [T.pack $ show c, "\n", toText a, " --> ", toText da, "\n", t]
  where toText = showTime mssep
