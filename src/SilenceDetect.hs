module SilenceDetect
  ( detectSilence
  ) where

import Data.Attoparsec.Text
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.IO
import System.Process

data SilenceInterval = SilenceInterval Double Double
 deriving (Eq, Show)

detectSilence :: FilePath -> IO [SilenceInterval]
detectSilence audioFile =
  let
    createProc = proc "ffmpeg" args
    args = ["-i", audioFile, "-af", "silencedetect=d=1", "-f", "null"]
    f _ _ stderrHandle _ = maybe reportFailure runParser stderrHandle
    reportFailure = return []
    runParser = fmap (parseSilences . T.pack) . hGetContents
  in withCreateProcess createProc f

parseSilences :: Text -> [SilenceInterval]
parseSilences input = fromMaybe [] (maybeResult $ parse silenceParser input)

silenceParser :: Parser [SilenceInterval]
silenceParser = many' intervalParser
  where
    intervalParser :: Parser SilenceInterval
    intervalParser = do
      headerParser
      string "silence_start: "
      startTime <- double
      headerParser
      string "silence_end: "
      stopTime <- double
      return $ SilenceInterval startTime stopTime
    headerParser :: Parser ()
    headerParser = do
      string "[silencedetect"
      takeWhile1 (']' ==)
      skipSpace
