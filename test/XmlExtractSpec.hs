module XmlExtractSpec
  ( spec
  ) where

import XmlExtract
import qualified Data.Text.IO as T
import Test.Hspec

spec :: Spec
spec = do
  describe "extractParagraphs" $ do
    it "returns list of paragraphs from XML p elements" $ do
      let
        p1 = "<br />"
        p2 = "「佐藤和真さん、ようこそ死後の世界へ。あなたはつい先ほど、不幸にも亡くなりました。短い人生でしたが、あなたの生は終わってしまったのです」"
        p3 = "　真っ白な部屋の中、俺は唐突にそんな事を告げられた。"
        expected = [p1, p2, p3]
      contents <- T.readFile "test/testinput.xhtml"
      let given = extractParagraphs contents
      given `shouldBe` expected
