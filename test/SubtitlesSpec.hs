{-# OPTIONS_GHC -Wno-missing-signatures #-}

module SubtitlesSpec
  ( spec
  ) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import Subtitles
import SubtitleMarkup

subtitle1 = Subtitle 1 (secondsToTime 0.0) (secondsToTime 1.0)
  [ SubText "「"
  , SubRuby
    [ SubText "佐"
    , SubRt [SubText "さ"]
    , SubText "藤"
    , SubRt [SubText "とう"]
    , SubText "和"
    , SubRt [SubText "かず"]
    , SubText "真"
    , SubRt [SubText "ま"]
    ]
  , SubText "さん、"]
subtitle2 = Subtitle 2 (secondsToTime 2.23) (secondsToTime 4.5678)
  [SubText "ようこそ死後の世界へ。"]
subtitle3 = Subtitle 3 (secondsToTime 5.0) (secondsToTime 6.6789)
  [ SubText "あなたはつい先ほど、不幸にも"
  , SubRuby [SubText "亡", SubRt [SubText "な"]]
  , SubText "くなりました。"]

expectedSrt :: Text
expectedSrt = "1\n\
\00:00:00,000 --> 00:00:01,000\n\
\「<ruby>佐<rt>さ</rt>藤<rt>とう</rt>和<rt>かず</rt>真<rt>ま</rt></ruby>さん、\n\n\
\2\n\
\00:00:02,230 --> 00:00:04,568\n\
\ようこそ死後の世界へ。\n\n\
\3\n\
\00:00:05,000 --> 00:00:06,679\n\
\あなたはつい先ほど、不幸にも<ruby>亡<rt>な</rt></ruby>くなりました。"

spec :: Spec
spec = do
  describe "secondsToTime" $ do
    it "converts seconds < 60" $ secondsToTime 11.4657 `shouldBe` Time 0 0 11 466
    it "converts seconds > 60 but < 3600" $ secondsToTime 357.447 `shouldBe` Time 0 5 57 447
    it "converts seconds > 3600" $ secondsToTime 3657.459555 `shouldBe` Time 1 0 57 460
  describe "srtSubtitles" $ do
    it "converts subtitles to valid SRT format" $
      let cs = [subtitle1, subtitle2, subtitle3]
      in srtSubtitles cs `shouldBe` expectedSrt
  describe "readSrt" $ do
    it "converts SRT subtitle format to internal Subtitle datatype" $
      let subtitles = readSrt expectedSrt
          expected = [subtitle1, subtitle2, subtitle3]
      in subtitles `shouldBe` expected
    it "converts slightly malformed subtitle format to internal Subtitle datatype" $
      let subtitles = readSrt (('\n' `T.cons` expectedSrt) `T.snoc` '\n')
          expected = [subtitle1, subtitle2, subtitle3]
      in subtitles `shouldBe` expected
    it "converts multiline subtitle to internal Subtitle datatype" $
      let subtitles = readSrt "3\n00:00:05,000 --> 00:00:06,679\nあなたはつい先ほど、\n不幸にも<ruby>亡<rt>な</rt></ruby>くなりました。"
          expected = [subtitle3]
      in subtitles `shouldBe` expected
  describe "subtitlesToXml" $ do
    it "from a list of Subtitle data, create a XML string, result is not empty" $
      let subtitles = [subtitle1, subtitle2, subtitle3]
          xml = subtitlesToXml "test.ogg" subtitles
      in xml `shouldSatisfy` (('<' ==) . T.head)
