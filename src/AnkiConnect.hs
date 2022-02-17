{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-
subnsnub - helps to create subtitles from and for audiobooks
Copyright (C) 2022  Thomas Leyh <thomas.leyh@mailbox.org>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

{-|
Module      : AnkiConnect
Description : Create new flash cards using the Anki-Connect interface.

See: <https://foosoft.net/projects/anki-connect/>
-}
module AnkiConnect
  ( subtitlesToAnki
  , mkAddNoteAction
  , mkAddNotesAction
  ) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Network.HTTP.Req
import System.Process
import System.IO.Temp
import System.FilePath
import Subtitles (Subtitle(..), Time, showTime, showTimeUnits)
import SubtitleMarkup

data Action = Action
  { action :: Text
  , version :: Integer
  , params :: Params
  } deriving (Generic, Show)

instance ToJSON Action where
  toEncoding = genericToEncoding defaultOptions

data Params
  = AddNote
    { note :: Note }
  | AddNotes
    { notes :: [Note] }
  deriving (Generic, Show)

instance ToJSON Params where
  toEncoding = genericToEncoding defaultOptions

data Note = Note
  { deckName :: Text
  , modelName :: Text
  , fields :: Fields
  , options :: Maybe NoteOptions
  , tags :: [Text]
  , audio :: [Media]
  , video :: [Media]
  , picture :: [Media]
  } deriving (Generic, Show)

instance ToJSON Note where
  toEncoding = genericToEncoding defaultOptions

data Fields = Fields
  { sentKanji :: Text
  , notes :: Text
  } deriving (Generic, Show)

instance ToJSON Fields where
  toJSON (Fields sentKanji' notes') = object ["SentKanji" .= sentKanji', "Notes" .= notes']

data NoteOptions = NoteOptions
  { allowDuplicate :: Bool
  , duplicateScope :: Text
  , duplicateScopeOptions :: DuplicateScopeOptions
  } deriving (Generic, Show)

instance ToJSON NoteOptions where
  toEncoding = genericToEncoding defaultOptions

data DuplicateScopeOptions = DuplicateScopeOptions
  { deckName :: Text
  , checkChildren :: Bool
  , checkAllModels :: Bool
  } deriving (Generic, Show)

instance ToJSON DuplicateScopeOptions where
  toEncoding = genericToEncoding defaultOptions

data Media = Media
  { path :: FilePath
  , filename :: FilePath
  , skipHash :: Maybe Text
  , fields :: [Text]
  } deriving (Generic, Show)

instance ToJSON Media where
  toEncoding = genericToEncoding defaultOptions

data AddNoteResult = AddNoteResult
  { result :: Integer
  , error :: Text
  } deriving (Generic, Show)

data AddNotesResult = AddNotesResult
  { result :: [Integer]
  , error  :: Text
  } deriving (Generic, Show)

subtitlesToAnki :: [Subtitle] -> FilePath -> IO ()
subtitlesToAnki subs audioSrc = do
  tuples <- withSystemTempDirectory "subnsnub" f
  let addNotes = uncurry mkAddNotesAction $ unzip tuples
  sendAction addNotes
  where
    f :: FilePath -> IO [(SubtitleMarkup, FilePath)]
    f tempdir = mapM
      (\(Subtitle _ start stop sub) -> do
        let audioDstName = srcToDstName start stop audioSrc
            audioDstDir  = tempdir </> audioDstName
        createAudio start stop audioSrc audioDstDir
        return (sub, audioDstDir)
      ) subs
    -- create storeMedia action
    -- create addNotes action

sendAction :: Action -> IO ()
sendAction ac = runReq defaultHttpConfig $ do
  r <- req POST (http "localhost") (ReqBodyJson ac) jsonResponse (port 8765)
  let _ = responseBody r :: Value
  return ()

srcToDstName :: Time -> Time -> FilePath -> FilePath
srcToDstName start stop src = concat [baseName, "_", T.unpack $ showTimeUnits start, "-", T.unpack $ showTimeUnits stop] <.> extension
  where
    baseName = takeBaseName src
    extension = takeExtension src

mkAddNoteAction :: SubtitleMarkup -> FilePath -> Action
mkAddNoteAction sub audioPath =
  Action "addNote" 6 (AddNote $ mkNote sub audioPath)

mkAddNotesAction :: [SubtitleMarkup] -> [FilePath] -> Action
mkAddNotesAction subs audioPaths =
  Action "addNotes" 6 (AddNotes $ zipWith mkNote subs audioPaths)

mkNote :: SubtitleMarkup -> FilePath -> Note
mkNote sub audioPath = Note
  "Learning"
  "Japanese sentences"
  (Fields
    (subtitleMarkupToAnki sub)
    "subnsnub")
  Nothing
  ["subnsnub"]
  [Media audioPath (takeFileName audioPath) Nothing ["SentAudio"]]
  []
  []

createAudio :: Time -> Time -> FilePath -> FilePath -> IO ()
createAudio start stop src dst = do
  let
    args = ffmpegArgs start stop src dst
    cmd = proc "ffmpeg" args
  --cmdStr = "'ffmpeg " ++ unwords args ++ "'"
  (_, _, _, p) <- createProcess cmd
  _ <- waitForProcess p
  return ()

ffmpegArgs :: Time -> Time -> FilePath -> FilePath -> [String]
ffmpegArgs start stop src dst =
  [ "-vn"
  , "-ss", T.unpack $ showTime '.' start
  , "-to", T.unpack $ showTime '.' stop
  , "-i", src
  , "-map_metadata", "-1", "-ac", "1"
  , "-c:a", "libopus", "-b:a", "24k", "-application", "voip"
  , dst]
