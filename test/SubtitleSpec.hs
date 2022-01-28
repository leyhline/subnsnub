module SubtitleSpec
  ( spec
  ) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import Subtitle

caption1 :: Caption
caption1 = Caption 1 (secondsToTime 0.0) (secondsToTime 1.0) "「佐さ藤とう和かず真まさん、"
caption2 :: Caption
caption2 = Caption 2 (secondsToTime 2.23) (secondsToTime 4.5678) "ようこそ死後の世界へ。"
caption3 :: Caption
caption3 = Caption 3 (secondsToTime 5.0) (secondsToTime 6.6789) "あなたはつい先ほど、不幸にも亡なくなりました。"

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
    it "converts captions to valid SRT format" $
      let cs = [caption1, caption2, caption3]
      in srtSubtitles cs `shouldBe` expectedSrt
  describe "readSrt" $ do
    it "converts SRT subtitle format to internal Caption datatype" $
      let captions = readSrt expectedSrt
          expected = [caption1, caption2, caption3]
      in captions `shouldBe` expected
    it "converts slightly malformed subtitle format to internal Caption datatype" $
      let captions = readSrt (('\n' `T.cons` expectedSrt) `T.snoc` '\n')
          expected = [caption1, caption2, caption3]
      in captions `shouldBe` expected
    it "converts multiline subtitle to internal Caption datatype" $
      let captions = readSrt "3\n00:00:05,000 --> 00:00:06,679\nあなたはつい先ほど、\n不幸にも亡なくなりました。"
          expected = [caption3]
      in captions `shouldBe` expected
  describe "captionsToXml" $ do
    it "from a list of Caption data, create a XML string, result is not empty" $
      let captions = [caption1, caption2, caption3]
          xml = captionsToXml captions
      in xml `shouldSatisfy` (('<' ==) . T.head)
