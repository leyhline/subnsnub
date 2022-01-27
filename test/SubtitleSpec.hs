module SubtitleSpec
  ( spec
  ) where

import Test.Hspec
import Data.Text (Text)
import Subtitle

expectedSrt :: Text
expectedSrt = "1\n\
\00:00:00,000 --> 00:00:01,000\n\
\「佐さ藤とう和かず真まさん、\n\n\
\2\n\
\00:00:02,230 --> 00:00:04,568\n\
\ようこそ死後の世界へ。\n\n\
\3\n\
\00:00:05,000 --> 00:00:06,679\n\
\あなたはつい先ほど、不幸にも亡なくなりました。"

spec :: Spec
spec = do
  describe "secondsToTime" $ do
    it "converts seconds < 60" $ secondsToTime 11.4657 `shouldBe` Time 0 0 11 466
    it "converts seconds > 60 but < 3600" $ secondsToTime 357.447 `shouldBe` Time 0 5 57 447
    it "converts seconds > 3600" $ secondsToTime 3657.459555 `shouldBe` Time 1 0 57 460
  describe "srtSubtitles" $ do
    it "converts captions to valid SRT format" $ do
      let
        c1 = Caption 1 (secondsToTime 0.0) (secondsToTime 1.0) "「佐さ藤とう和かず真まさん、"
        c2 = Caption 2 (secondsToTime 2.23) (secondsToTime 4.5678) "ようこそ死後の世界へ。"
        c3 = Caption 3 (secondsToTime 5.0) (secondsToTime 6.6789) "あなたはつい先ほど、不幸にも亡なくなりました。"
        cs = [c1, c2, c3]
      srtSubtitles cs `shouldBe` expectedSrt
