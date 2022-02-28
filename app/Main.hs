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
import AnkiConnect
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
 | ToAnki     ToAnkiOptions

data XmlExtractOptions = XmlExtractOptions
  { xmlInputFile  :: FilePath
  , txtOutputFile :: Maybe FilePath
  }

data AudioToSubOptions = AudioToSubOptions
  { audioInputFile  :: FilePath
  , subOutputFile   :: Maybe FilePath
  , noiseTolerance  :: Integer
  , silenceDuration :: Double
  , subDefault      :: Text
  , useVtt          :: Bool
  }

data SubToHtmlOptions  = SubToHtmlOptions
  { subInputFile   :: FilePath
  , audioSrcPath   :: FilePath
  , htmlOutputFile :: Maybe FilePath
  }

data ToAnkiOptions = ToAnkiOptions
  { ankiSubInputFile   :: FilePath
  , ankiAudioInputFile :: FilePath
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
      ToAnki     o -> toAnki o

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
    (progDesc "Convert a subtitle SRT file to a HTML page with audio support"))
  <>  command "send2anki"    (info
    (ToAnki     <$> toAnkiOptions)
    (progDesc "Creates Anki flash cards from subtitle fragments with audio (requires anki-connect)")))

subToHtmlOptions :: Parser SubToHtmlOptions
subToHtmlOptions = SubToHtmlOptions
  <$> strArgument (metavar "FILE")
  <*> strArgument (metavar "FILE")
  <*> optional (strOption
    (  long "output"
    <> short 'o'
    <> metavar "FILE"
    <> help "HTML file destination path, needs subnsnub.ogg audio in same directory (default: stdout)" ))

subToHtml :: SubToHtmlOptions -> IO ()
subToHtml (SubToHtmlOptions input audSrcPath output) =
  withFile input ReadMode (\hdl -> do
    contents <- TIO.hGetContents hdl
    let html = subtitlesToXml audSrcPath $ readSrt contents
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
  <*>  strOption
    (  long "subdefault"
    <> short 's'
    <> metavar "STRING"
    <> value ""
    <> help "Default text for each subtitle output (default: <blank>)")
  <*>  switch
    (  long "vtt"
    <> help "Use WebVTT format instead of SRT")

audioToSub :: AudioToSubOptions -> IO ()
audioToSub (AudioToSubOptions input output noise duration subdefault vtt) = do
  intervals <- detectSilence noise duration input
  let subtitles = intervalsToSubtitles duration subdefault intervals
      sub = if vtt then vttSubtitles subtitles else srtSubtitles subtitles
  maybe (TIO.putStrLn sub) (`writeToFile` sub) output

intervalsToSubtitles :: Double -> Text -> [SilenceInterval] -> [Subtitle]
intervalsToSubtitles d subdefault ivls = zipWith3 f [1..] ivls (tail ivls)
  where
    f i (SilenceInterval _ stop) (SilenceInterval start _) =
      Subtitle i (secondsToTime $ stop - (d/2)) (secondsToTime $ start + (d/2)) [SubText subdefault]

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

toAnkiOptions :: Parser ToAnkiOptions
toAnkiOptions = ToAnkiOptions
  <$> strArgument (metavar "FILE")
  <*> strArgument (metavar "FILE")

toAnki :: ToAnkiOptions -> IO ()
toAnki (ToAnkiOptions subsFile audioFile) =
  withFile subsFile ReadMode (\hdl -> do
    contents <- TIO.hGetContents hdl
    let subs = readSrt contents
    subtitlesToAnki subs audioFile)

writeToFile :: FilePath -> Text -> IO ()
writeToFile path t = withFile path WriteMode (`TIO.hPutStr` t)
