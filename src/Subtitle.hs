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
    eHead = node (unqual "head") [eMetaCharset, eMeta, eStyle, eScript]
    eMetaCharset = node (unqual "meta") [Attr (unqual "charset") "utf-8"]
    eMeta = node (unqual "meta") [Attr (unqual "name") "viewport", Attr (unqual "content") "width=device-width,initial-scale=1.0"]
    eStyle = node (unqual "style") (CData CDataText css Nothing)
    eScript = node (unqual "script") (CData CDataRaw
      (js ++ "\nvar gIntervals = " ++ show (map (\(x,y) -> [x,y]) intervals) ++ ";\nwindow.onload = main;") Nothing)
    eBody = node (unqual "body") (eAudioSrc : foldr spanToPs [] spanElems)
    eAudioSrc = node (unqual "audio") ([Attr (unqual "id") audioSourceId, Attr (unqual "src") "subnsnub.ogg"], ""::String) -- TODO: .ogg needs to be variable
    spanToPs :: Element -> [Element] -> [Element]
    spanToPs eSpan [] = [node (unqual "p") (classAttrFromElemText eSpan, eSpan)]
    spanToPs eSpan (Element {
      elName = QName { qName = "p" },
      elContent = (Elem (Element QName { qName = "span" } attrs (Text (CData a ('_':b) c):scs) _
      ):spans) }:ps) = node (unqual "p") (classAttrFromElemText eSpan, Elem eSpan : Elem (node (unqual "span") (attrs,Text (CData a b c):scs)) : spans) : ps
    spanToPs eSpan ps = node (unqual "p") (classAttrFromElemText eSpan, eSpan) : ps
    classAttrFromElemText :: Element -> [Attr]
    classAttrFromElemText e = maybe [] (\c -> if c == '「' then [aClsDlg] else [aClsNrr]) (maybeHead (strContent e))
    aClsNrr = Attr (unqual "class") narrationClassVal
    aClsDlg = Attr (unqual "class") dialogueClassVal

toSpanElement :: Caption -> Maybe (Element, (Double, Double))
toSpanElement (Caption _ a da t) = if t == ""
  then Nothing
  else Just (node (unqual "span") ([aClsAud], parseXML t), (timeToSeconds a, timeToSeconds da))
    where aClsAud = Attr (unqual "class") audibleClassVal

audibleClassVal :: String
audibleClassVal = "aud"
narrationClassVal :: String
narrationClassVal = "nrr"
dialogueClassVal :: String
dialogueClassVal = "dlg"
audioSourceId :: String
audioSourceId = "audsrc"

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

css :: String
css = "\
\." ++ narrationClassVal ++ "{ padding-left: 1em; } \
\." ++ audibleClassVal ++ ":hover { background-color: lavender; cursor: pointer; }"

js :: String
js = "\
\var gAudioStopTime = 0;\n\
\function main() {\n\
\  const audioElem = document.getElementById('" ++ audioSourceId ++ "');\n\
\  const spans = document.getElementsByClassName('" ++ audibleClassVal ++ "');\n\
\  console.assert(spans.length == gIntervals.length, 'number of span.aud elements must match length of intervals array');\n\
\  audioElem.addEventListener('timeupdate', (ev) => {\n\
\    if (audioElem.currentTime > gAudioStopTime) audioElem.pause();\n\
\  });\n\
\  for (let i = 0; i < spans.length; i++) {\n\
\    addAudioPlayOnClick(spans[i], audioElem, gIntervals[i][0], gIntervals[i][1]);\n\
\  }\n\
\}\n\
\function addAudioPlayOnClick(spanElem, audioElem, start, stop) {\n\
\  spanElem.addEventListener('click', (mouseEv) => {\n\
\    gAudioStopTime = stop;\n\
\    audioElem.currentTime = start;\n\
\    audioElem.play();\n\
\  })\n\
\}"
