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
Module      : SilenceDetect
Description : Calls FFmpeg with silencedetect filter and parses output
-}
module SilenceDetect
  ( detectSilence
  , parseSilence
  , SilenceInterval(..)
  ) where

import Control.Exception
import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T
import System.IO
import System.Process
import System.Exit

data SilenceDetectException = ParserException String
  | ProcessException String
  deriving (Show, Eq)

instance Exception SilenceDetectException

data SilenceInterval = SilenceInterval Double Double
 deriving (Eq, Show)

detectSilence :: Integer -> Double -> FilePath -> IO [SilenceInterval]
detectSilence n d audioFile = do
  let
    args = ["-i", audioFile, "-af", "silencedetect=n=" ++ show n ++ "dB:d=" ++ show d, "-f", "null", "-"]
    cmd = (proc "ffmpeg" args){ std_err = CreatePipe }
    cmdStr = "'ffmpeg " ++ unwords args ++ "'"
  (_, _, Just errHdl, p) <- createProcess cmd
  exitCode <- waitForProcess p
  case exitCode of
    ExitSuccess -> parseSilence . T.pack <$> hGetContents errHdl
    ExitFailure code -> throw $ ProcessException $ cmdStr ++ " quit with exit code " ++ show code

parseSilence :: Text -> [SilenceInterval]
parseSilence input = either (throw . ParserException) id (parseOnly silenceParser input)

silenceParser :: Parser [SilenceInterval]
silenceParser = many' intervalParser
  where
    intervalParser :: Parser SilenceInterval
    intervalParser = do
      skipHeader
      _ <- string "silence_start: "
      startTime <- double
      skipHeader
      _ <- string "silence_end: "
      SilenceInterval startTime <$> double
    skipHeader :: Parser ()
    skipHeader = do
      _ <- manyTill (skipWhile ('[' /=) >> anyChar) (string "silencedetect")
      _ <- skipWhile (']' /=) >> anyChar
      skipSpace
