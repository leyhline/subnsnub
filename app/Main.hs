module Main where

import Options.Applicative
import SilenceDetect

data SilenceDetectOpts = SilenceDetectOpts
  { path :: String }

silenceDetectOpts :: Parser SilenceDetectOpts
silenceDetectOpts = SilenceDetectOpts
  <$> strOption
    (  long "file"
    <> metavar "PATH"
    <> help "path of audio file for silence detection" )

main :: IO ()
main = runDetectSilence =<< execParser opts
  where
    opts = info (silenceDetectOpts <**> helper)
      (  fullDesc
      <> progDesc "Stuff with audio books and subtitles"
      <> header "subssnub" )

runDetectSilence :: SilenceDetectOpts -> IO ()
runDetectSilence opts = do
  intervals <- detectSilence (path opts)
  let test = map print intervals
  sequence_ test
