module Subtitle
  ( secondsToTime
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
  deriving Eq

instance Show Caption where
  show (Caption c a da t) = concat [show c, "\n", show a, " --> ", show da, "\n", T.unpack t]

--               hour    min     sec     msec
data Time = Time Integer Integer Integer Integer
  deriving (Eq, Ord)

instance Show Time where
  show (Time h m s ms) = printf "%02d:%02d:%02d,%03d" h m s ms

secondsToTime :: Double -> Time
secondsToTime d = Time h m s ms
  where
    (h, rest) = floor d `divMod` 3600
    (m, s) = rest `divMod` 60
    ms = floor (1000 * d) `mod` 1000
