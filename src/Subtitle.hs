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

timeToSeconds :: Time -> Double
timeToSeconds (Time h m s ms) = (fromIntegral h) * 3600.0 + (fromIntegral m) * 60 + (fromIntegral s) + (fromIntegral ms) / 1000

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
    (spanElems, intervals) = unzip $ mapMaybe toSpanElement cs
    eHtml = node (unqual "html") ([Attr (unqual "lang") "ja"], [eHead, eBody])
    eHead = node (unqual "head") [eMetaCharset, eMeta, eStyle, script]
    eMetaCharset = node (unqual "meta") [Attr (unqual "charset") "utf-8"]
    eMeta = node (unqual "meta") [Attr (unqual "name") "viewport", Attr (unqual "content") "width=device-width,initial-scale=1.0"]
    eStyle = node (unqual "style") (CData CDataText css Nothing)
    script = node (unqual "script") (CData CDataText ("var intervals = " ++ show intervals) Nothing)
    eBody = node (unqual "body") (foldr spanToPs [] spanElems)
    spanToPs :: Element -> [Element] -> [Element]
    spanToPs eSpan [] = [node (unqual "p") (classAttrFromElemText eSpan, eSpan)]
    spanToPs eSpan (Element {
      elName = QName { qName = "p" },
      elContent = (Elem (Element QName { qName = "span" } attrs (Text (CData a ('_':b) c):scs) _
      ):spans) }:ps) = node (unqual "p") (classAttrFromElemText eSpan, Elem eSpan : Elem (node (unqual "span") (attrs,Text (CData a b c):scs)) : spans) : ps
    spanToPs eSpan ps = node (unqual "p") (classAttrFromElemText eSpan, eSpan) : ps
    classAttrFromElemText :: Element -> [Attr]
    classAttrFromElemText e = maybe [] (\c -> if c == '「' then [aClsDlg] else [aClsNrr]) (maybeHead (strContent e))
    aClsNrr = Attr (unqual "class") "nrr"
    aClsDlg = Attr (unqual "class") "dlg"

toSpanElement :: Caption -> Maybe (Element, (Double, Double))
toSpanElement (Caption _ a da t) = if t == ""
  then Nothing
  else Just (node (unqual "span") ([aClsAud], parseXML t), (timeToSeconds a, timeToSeconds da))
    where aClsAud = Attr (unqual "class") "aud"

css :: String
css = "\
\.nrr { padding-left: 1em; } \
\.aud:hover { background-color: lavender; }"

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x
