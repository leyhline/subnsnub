module Main where

import Options.Applicative
import System.IO
import SilenceDetect
import XmlExtract
import qualified Data.Text.IO as T

data SilenceDetectOpts = SilenceDetectOpts
  { audiopath :: String
  , xmlpath :: String }

silenceDetectOpts :: Parser SilenceDetectOpts
silenceDetectOpts = SilenceDetectOpts
  <$> strOption
    (  long "audiopath"
    <> metavar "PATH"
    <> help "path of audio file for silence detection" )
  <*> strOption
    (  long "xmlpath"
    <> metavar "PATH"
    <> help "path of XML file of ebook" )

main :: IO ()
main = runDetectSilence =<< execParser opts
  where
    opts = info (silenceDetectOpts <**> helper)
      (  fullDesc
      <> progDesc "Stuff with audio books and subtitles"
      <> header "subssnub" )

runDetectSilence :: SilenceDetectOpts -> IO ()
runDetectSilence (SilenceDetectOpts audiop xmlp) = do
  intervals <- detectSilence audiop
  let test = map print intervals
  sequence_ test
  let f hdl = mapM_ T.putStrLn . extractParagraphs =<< T.hGetContents hdl
  withFile xmlp ReadMode f
