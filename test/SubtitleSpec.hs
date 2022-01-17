module SubtitleSpec
  ( spec
  ) where

import Subtitle
import qualified Data.Text.IO as T
import Test.Hspec

spec :: Spec
spec = do
  describe "xmlToSrt" $ do
    it "returns SRT subtitles from XML paragraphs" $ do
      let
        c1 = Caption 1 (Time 0 0 0 0) (Time 0 1 0 0) "<br />"
        c2 = Caption 2 (Time 0 1 0 0) (Time 0 2 0 0) "「佐藤和真さん、ようこそ死後の世界へ。あなたはつい先ほど、不幸にも亡くなりました。短い人生でしたが、あなたの生は終わってしまったのです」"
        c3 = Caption 3 (Time 0 2 0 0) (Time 0 3 0 0) "　真っ白な部屋の中、俺は唐突にそんな事を告げられた。"
        expectedCaptions = [c1, c2, c3]
      contents <- T.readFile "test/testinput.xhtml"
      let captions = xmlToSrt contents
      captions `shouldBe` expectedCaptions
