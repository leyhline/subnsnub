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
import System.Exit

data SilenceDetectException = ParserException String
  | ProcessException String
  deriving (Show, Eq)

instance Exception SilenceDetectException

data SilenceInterval = SilenceInterval Double Double
 deriving (Eq, Show)

detectSilence :: FilePath -> IO [SilenceInterval]
detectSilence audioFile = let
  args = ["-i", audioFile, "-af", "silencedetect=d=1", "-f", "null", "-"]
  cmd = (proc "ffmpeg" args){ std_err = CreatePipe }
  cmdStr = "'ffmpeg " ++ unwords args ++ "'"
  f :: Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO [SilenceInterval]
  f _ _ (Just errHdl) p = do
    exitCode <- waitForProcess p
    case exitCode of
      ExitSuccess -> parseSilences . T.pack <$> hGetContents errHdl
      ExitFailure code -> throw $ ProcessException $ cmdStr ++ " quit with exit code " ++ show code
  f _ _ _ _ = throw $ ProcessException $ "Failed to create pipe for stderr of " ++ cmdStr
  in withCreateProcess cmd f

parseSilences :: Text -> [SilenceInterval]
parseSilences input = either (throw . ParserException) id (parseOnly silenceParser input)

silenceParser :: Parser [SilenceInterval]
silenceParser = many' intervalParser
  where
    intervalParser :: Parser SilenceInterval
    intervalParser = do
      skipHeader
      string "silence_start: "
      startTime <- double
      skipHeader
      string "silence_end: "
      SilenceInterval startTime <$> double
    skipHeader :: Parser ()
    skipHeader = do
      manyTill (skipWhile ('[' /=) >> anyChar) (string "silencedetect")
      skipWhile (']' /=) >> anyChar
      skipSpace
