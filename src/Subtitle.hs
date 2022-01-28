module Subtitle
  ( secondsToTime
  , srtSubtitles
  , vttSubtitles
  , readSrt
  , captionsToXml
  , Caption(..)
  , Time(..)
  ) where

import Control.Applicative
import Control.Exception
import Text.Printf
import Data.Char
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text
import Text.XML.Light

data SubtitleException = ParserException String
  deriving (Show, Eq)

instance Exception SubtitleException

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

readSrt :: Text -> [Caption]
readSrt input = either (throw . ParserException) id (parseOnly (many' captionParser) input)

captionParser :: Parser Caption
captionParser = do
  skipWhile (not . isDigit)
  i <- decimal
  endOfLine
  a <- time
  skipArrow
  da <- time
  endOfLine
  ts <- many' textLine
  return $ Caption i a da (T.concat ts)
  where
    time :: Parser Time
    time = do
      h <- decimal
      char ':'
      m <- decimal
      char ':'
      s <- decimal
      char ','
      Time h m s <$> decimal
    skipArrow :: Parser ()
    skipArrow = char ' ' >> skip ('-' ==) >> skip ('-' ==) >> skip ('>' ==) >> char ' ' >> return ()
    textLine :: Parser Text
    textLine = do
      t <- takeWhile1 (\c -> c /= '\r' && c /= '\n')
      endOfInput <|> endOfLine
      return t

captionsToXml :: [Caption] -> Text
captionsToXml cs = "<!DOCTYPE html>\n" `T.append` T.pack (ppElement eHtml)
  where
    eHtml = node (unqual "html") ([Attr (unqual "lang") "ja"], [eHead, eBody])
    eHead = node (unqual "head") [eMetaCharset, eMeta]
    eMetaCharset = node (unqual "meta") [Attr (unqual "charset") "utf-8"]
    eMeta = node (unqual "meta") [Attr (unqual "name") "viewport", Attr (unqual "content") "width=device-width,initial-scale=1.0"]
    eBody = node (unqual "body") (mapMaybe toPElement cs)

toPElement :: Caption -> Maybe Element
toPElement (Caption c _ _ t) = if t == ""
  then Nothing
  else Just $ node (unqual "p") ([aId], parseXML t)
    where aId = Attr (unqual "id") ("p" ++ show c)
