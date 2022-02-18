 {-# OPTIONS_GHC -Wno-missing-signatures #-}

module AnkiConnectSpec
  ( spec
  ) where

import Data.Aeson
import AnkiConnect
import SubtitleMarkup
import Test.Hspec

subs :: [SubtitleMarkup]
subs =
  [ [SubText "好き"]
  , [SubText "嫌い"]
  ]

audioPaths :: [FilePath]
audioPaths =
  [ "/tmp/file1_01h01m01s100ms-01h01m01s200ms.ogg"
  , "/tmp/file2_01h01m01s100ms-01h01m01s200ms.ogg"
  ]

expectedJson = "{\"action\":\"addNotes\",\"version\":6,\"params\":{\"notes\":\
\[{\"deckName\":\"Learning\",\"modelName\":\"Japanese sentences\",\"fields\":{\"SentKanji\":\"\229\165\189\227\129\141\",\"Notes\":\"file1 (01h01m01s100ms)\"},\"tags\":[\"file1\",\"subnsnub\"],\"audio\":[{\"path\":\"/tmp/file1_01h01m01s100ms-01h01m01s200ms.ogg\",\"filename\":\"file1_01h01m01s100ms-01h01m01s200ms.ogg\",\"fields\":[\"SentAudio\"]}],\"video\":[],\"picture\":[]}\
\,{\"deckName\":\"Learning\",\"modelName\":\"Japanese sentences\",\"fields\":{\"SentKanji\":\"\229\171\140\227\129\132\",\"Notes\":\"file2 (01h01m01s100ms)\"},\"tags\":[\"file2\",\"subnsnub\"],\"audio\":[{\"path\":\"/tmp/file2_01h01m01s100ms-01h01m01s200ms.ogg\",\"filename\":\"file2_01h01m01s100ms-01h01m01s200ms.ogg\",\"fields\":[\"SentAudio\"]}],\"video\":[],\"picture\":[]}\
\]}}"

spec :: Spec
spec = do
  describe "mkAddNotesAction" $ do
    it "create a valid Action that can be encoded to JSON" $ do
      let addNotes = mkAddNotesAction subs audioPaths
          jsonAction = encode addNotes
      jsonAction `shouldBe` expectedJson
