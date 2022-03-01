module SubtitleMarkupSpec
  ( spec
  ) where

import Test.Hspec
import SubtitleMarkup

spec :: Spec
spec = do
  describe "subtitleContentToAnki" $ do
    it "returns text without markup and ruby" $ do
      let sub = [SubBold [SubText "Bold ", SubItalic [SubText "ItalicBold "]], SubText "notBold ", SubRuby [SubText "日本", SubRt [SubText "にほん"]]]
      toAnki sub `shouldBe` "Bold ItalicBold notBold 日本"
