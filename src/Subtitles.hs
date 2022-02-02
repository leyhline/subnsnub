{-
subnsnub - helps to create subtitles from and for audiobooks
Copyright (C) 2022  Thomas Leyh <thomas.leyh@mailbox.org>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

{-|
Module      : Subtitles
Description : Creation and conversion of subtitle data.

Subtitle datatype and conversion to SRT, VTT and basic HTML with
audio support.
-}

module Subtitles
  ( secondsToTime
  , srtSubtitles
  , vttSubtitles
  , readSrt
  , subtitlesToXml
  , Subtitle(..)
  , Time(..)
  ) where

import SubtitleMarkup
import Control.Applicative
import Control.Exception
import Text.Printf
import Data.Char
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text
import Text.XML.Light

newtype SubtitleException = ParserException String
  deriving (Show, Eq)

instance Exception SubtitleException

data Subtitle = Subtitle
  { counter   :: Integer
  , appear    :: Time
  , disappear :: Time
  , markup    :: SubtitleMarkup
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
timeToSeconds (Time h m s ms) = fromIntegral h * 3600.0 + fromIntegral m * 60 + fromIntegral s + fromIntegral ms / 1000

srtSubtitles :: [Subtitle] -> Text
srtSubtitles = T.intercalate "\n\n" . map (showSubtitle ',')

vttSubtitles :: [Subtitle] -> Text
vttSubtitles = T.append header . T.intercalate "\n\n" . map (showSubtitle '.')
  where header = "WEBVTT\n\n"

showSubtitle :: Char -> Subtitle -> Text
showSubtitle mssep (Subtitle c a da cs) = T.concat [T.pack $ show c, "\n", toText a, " --> ", toText da, "\n", showSubtitleMarkup cs]
  where toText = showTime mssep

readSrt :: Text -> [Subtitle]
readSrt input = either (throw . ParserException) id (parseOnly (many' subtitleParser) input)

subtitleParser :: Parser Subtitle
subtitleParser = do
  skipWhile (not . isDigit)
  i <- decimal
  endOfLine
  a <- time
  skipArrow
  da <- time
  endOfLine
  mtext <- many' textLine
  return $ Subtitle i a da (readSubtitleMarkup $ T.concat mtext)
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

subtitlesToXml :: [Subtitle] -> Text
subtitlesToXml cs = "<!DOCTYPE html>\n" `T.append` T.pack (ppElement eHtml)
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
    spanToPs eSpan (p@Element
      { elName = QName { qName = "p" }
      , elContent = (Elem eSpan'@(Element QName { qName = "span" } _ ((Text cdata@(CData _ ('_':b) _)):scs) _):spans)
      } : ps) = p
        { elContent = Elem eSpan : Elem (eSpan' { elContent = Text cdata { cdData = b } : scs }) : spans
        , elAttribs = classAttrFromElemText eSpan
        } : ps
    spanToPs eSpan ps = node (unqual "p") (classAttrFromElemText eSpan, eSpan) : ps
    classAttrFromElemText :: Element -> [Attr]
    classAttrFromElemText e = maybe [] (\c -> if c == '「' then [aClsDlg] else [aClsNrr]) (maybeHead (strContent e))
    aClsNrr = Attr (unqual "class") narrationClassVal
    aClsDlg = Attr (unqual "class") dialogueClassVal

toSpanElement :: Subtitle -> Maybe (Element, (Double, Double))
toSpanElement (Subtitle _ a da cs) = if null cs
  then Nothing
  else Just (node (unqual "span") ([aClsAud], subtitleMarkupToXml cs), (timeToSeconds a, timeToSeconds da))
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
\  console.assert(spans.length == gIntervals.length, 'number of span.aud elements need to match length of intervals array');\n\
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
