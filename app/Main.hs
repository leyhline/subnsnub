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

module Main where

import System.IO
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import Subtitles
import SubtitleMarkup
import XmlExtract
import SilenceDetect

data Options = Options
  { xmlExtractCommand :: Command
  , audioToSubCommand :: Command
  , subToHtmlCommand  :: Command
  }

data Command
 = XmlExtract XmlExtractOptions
 | AudioToSub AudioToSubOptions
 | SubToHtml  SubToHtmlOptions

data XmlExtractOptions = XmlExtractOptions
  { xmlInputFile  :: FilePath
  , txtOutputFile :: Maybe FilePath
  }

data AudioToSubOptions = AudioToSubOptions
  { audioInputFile  :: FilePath
  , subOutputFile   :: Maybe FilePath
  , noiseTolerance  :: Integer
  , silenceDuration :: Double
  }

data SubToHtmlOptions  = SubToHtmlOptions
  { subInputFile   :: FilePath
  , htmlOutputFile :: Maybe FilePath
  }

main :: IO ()
main = execCommand =<< execParser opts
  where
    opts = info (commandParser <**> helper)
      (  fullDesc
      <> progDesc "Parses ebook XML. Maps silence intervals in audio to subtitle templates. Creates HTML from subtitles."
      <> header "subnsnub - helps to create subtitles from and for audiobooks")
    execCommand cmd = case cmd of
      XmlExtract o -> xmlExtract o
      AudioToSub o -> audioToSub o
      SubToHtml  o -> subToHtml  o

commandParser :: Parser Command
commandParser = hsubparser
  (  command "xmlextract" (info
    (XmlExtract <$> xmlExtractOptions)
    (progDesc "Extracts paragraphs from ebook XML and outputs them by line"))
  <> command "audio2sub"  (info
    (AudioToSub <$> audioToSubOptions)
    (progDesc "Call FFmpeg to detect silence intervals and create subtitle template"))
  <>  command "sub2html"  (info
    (SubToHtml  <$> subToHtmlOptions)
    (progDesc "Convert a subtitle SRT file to a HTML page with audio support")))

subToHtmlOptions :: Parser SubToHtmlOptions
subToHtmlOptions = SubToHtmlOptions
  <$> strArgument (metavar "FILE")
  <*> optional (strOption
    (  long "output"
    <> short 'o'
    <> metavar "FILE"
    <> help "HTML file destination path, needs subnsnub.ogg audio in same directory (default: stdout)" ))

subToHtml :: SubToHtmlOptions -> IO ()
subToHtml (SubToHtmlOptions input output) =
  withFile input ReadMode (\hdl -> do
    contents <- TIO.hGetContents hdl
    let html = subtitlesToXml $ readSrt contents
    maybe (TIO.putStrLn html) (`writeToFile` html) output)

audioToSubOptions :: Parser AudioToSubOptions
audioToSubOptions = AudioToSubOptions
  <$> strArgument (metavar "FILE")
  <*> optional (strOption
    (  long "output"
    <> short 'o'
    <> metavar "FILE"
    <> help "SRT file destination path (default: stdout)" ))
  <*> option auto
    (  long "noise"
    <> short 'n'
    <> metavar "INT"
    <> value (-60)
    <> help "Noise tolerance (default: -60dB)")
  <*> option auto
    (  long "duration"
    <> short 'd'
    <> metavar "FLOAT"
    <> value 0.6
    <> help "Minimal duration of silence between subtitles; gets passed to FFmpeg (default: 0.6)")

audioToSub :: AudioToSubOptions -> IO ()
audioToSub (AudioToSubOptions input output noise duration) = do
  intervals <- detectSilence noise duration input
  let subtitles = intervalsToSubtitles duration intervals
      srt = srtSubtitles subtitles
  maybe (TIO.putStrLn srt) (`writeToFile` srt) output

intervalsToSubtitles :: Double -> [SilenceInterval] -> [Subtitle]
intervalsToSubtitles d ivls = zipWith3 f [1..] ivls (tail ivls)
  where
    f i (SilenceInterval _ stop) (SilenceInterval start _) =
      Subtitle i (secondsToTime $ stop - (d/2)) (secondsToTime $ start + (d/2)) []

xmlExtractOptions :: Parser XmlExtractOptions
xmlExtractOptions = XmlExtractOptions
  <$> strArgument (metavar "FILE")
  <*> optional (strOption
    (  long "output"
    <> short 'o'
    <> metavar "FILE"
    <> help "Text file destination path (default: stdout)" ))

xmlExtract :: XmlExtractOptions -> IO ()
xmlExtract (XmlExtractOptions input output) =
  withFile input ReadMode (\hdl -> do
    contents <- TIO.hGetContents hdl
    let ps = extractParagraphs contents
        txt = T.intercalate "\n\n" (map showSubtitleMarkup ps)
    maybe (TIO.putStrLn txt) (`writeToFile` txt) output)

writeToFile :: FilePath -> Text -> IO ()
writeToFile path t = withFile path WriteMode (`TIO.hPutStr` t)
