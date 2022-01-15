module SilenceDetectSpec
  ( spec
  ) where

import SilenceDetect
import Test.Hspec

spec :: Spec
spec = do
  describe "detectSilence" $ do
    it "calls FFmpeg and parses stderr for silence intervals" $ do
      True `shouldBe` True -- TODO
