module SilenceDetectSpec
  ( spec
  ) where

import SilenceDetect
import Data.Text(Text)
import Test.Hspec

ffmpegOutput :: Text
ffmpegOutput = "\
\ffmpeg version n4.4.1 Copyright (c) 2000-2021 the FFmpeg developers                \n\
\Press [q] to stop, [?] for help                                                    \n\
\[silencedetect @ 0x55aea22aae40] silence_start: 8.32113trate=N/A speed=0.000695x   \n\
\[silencedetect @ 0x55aea22aae40] silence_end: 9.51601 | silence_duration: 1.19488  \n\
\[silencedetect @ 0x55aea22aae40] silence_start: 11.4657                            \n\
\[silencedetect @ 0x55aea22aae40] silence_end: 12.529 | silence_duration: 1.06331   \n\
\[silencedetect @ 0x55aea22aae40] silence_start: 15.2015                            \n\
\[silencedetect @ 0x55aea22aae40] silence_end: 18.4216 | silence_duration: 3.22009  \n\
\[silencedetect @ 0x55aea22aae40] silence_start: 19.0617                            \n\
\video:1kB audio:31013kB subtitle:0kB other streams:0kB global headers:0kB muxing overhead: unknown"

spec :: Spec
spec = do
  describe "detectSilence" $ do
    it "calls FFmpeg and parses abridged test string for silence intervals" $ do
      let
        intervals = parseSilences ffmpegOutput
        si1 = SilenceInterval 8.32113 9.51601
        si2 = SilenceInterval 11.4657 12.529
        si3 = SilenceInterval 15.2015 18.4216
        expectedIntervals = [si1, si2, si3]
      intervals `shouldBe` expectedIntervals
