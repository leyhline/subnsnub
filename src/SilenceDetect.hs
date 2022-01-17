module SilenceDetect
  ( detectSilence
  , parseSilences
  , SilenceInterval(..)
  ) where

import Control.Exception
import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T
import System.IO
import System.Process

data SilenceDetectException = ParserException String
  | ProcessException String
  deriving (Show, Eq)

instance Exception SilenceDetectException

data SilenceInterval = SilenceInterval Double Double
 deriving (Eq, Show)

detectSilence :: FilePath -> IO [SilenceInterval]
detectSilence audioFile =
  let
    createProc = proc "ffmpeg" args
    args = ["-i", audioFile, "-af", "silencedetect=d=1", "-f", "null"]
    f _ _ stderrHandle _ = maybe reportFailure runParser stderrHandle
    reportFailure = throw $ ProcessException $ "ffmpeg " ++ unwords args
    runParser = fmap (parseSilences . T.pack) . hGetContents
  in withCreateProcess createProc f

parseSilences :: Text -> [SilenceInterval]
parseSilences input = either (throw . ParserException) id (parseOnly silenceParser input)

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
      skipWhile ('[' /=)
      string "[silencedetect"
      skipWhile (']' /=)
      skip (']' ==)
      skipSpace
