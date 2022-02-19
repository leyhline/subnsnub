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

import Control.Exception
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics
import Network.HTTP.Req
import System.Exit
import System.Process
import System.IO
import System.IO.Temp
import System.FilePath
import Subtitles (Subtitle(..), Time, showTime, showTimeUnits)
import SubtitleMarkup

data AnkiConnectException
  = ProcessException String
  | RequestException String
  deriving (Show, Eq)

instance Exception AnkiConnectException

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
  toEncoding = genericToEncoding defaultOptions { sumEncoding = UntaggedValue }

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
  toEncoding = genericToEncoding defaultOptions { omitNothingFields = True }

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
  toEncoding = genericToEncoding defaultOptions { omitNothingFields = True }

data AddNoteResult = AddNoteResult
  { result :: Maybe Integer
  , error  :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON AddNoteResult

data AddNotesResult = AddNotesResult
  { result :: Maybe [Integer]
  , error  :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON AddNotesResult

subtitlesToAnki :: [Subtitle] -> FilePath -> IO ()
subtitlesToAnki subs audioSrc = do
  withSystemTempDirectory "subnsnub" f
  where
    f :: FilePath -> IO ()
    f tempdir = sendAction . uncurry mkAddNotesAction . unzip =<< mapM
      (\(Subtitle _ start stop sub) -> do
        let audioDstName = srcToDstName start stop audioSrc
            audioDstDir  = tempdir </> audioDstName
        createAudio start stop audioSrc audioDstDir
        return (sub, audioDstDir)
      ) subs

sendAction :: Action -> IO ()
sendAction ac = runReq defaultHttpConfig $ do
  r <- req POST (http "localhost") (ReqBodyJson ac) jsonResponse (port 8765)
  rBody <- if responseStatusCode r == 200
    then return (responseBody r :: AddNotesResult)
    else throw $ RequestException $ B.unpack $ responseStatusMessage r
  liftIO $ print rBody

srcToDstName :: Time -> Time -> FilePath -> FilePath
srcToDstName start stop src = concat
  [ cleanBaseName $ takeBaseName src, "_"
  , T.unpack $ showTimeUnits start, "-"
  , T.unpack $ showTimeUnits stop
  ] <.> "ogg"

cleanBaseName :: String -> String
cleanBaseName = mapMaybe f
  where f c
          | isSpace c = Just '_'
          | isAlphaNum c = Just c
          | otherwise = Nothing

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
    (T.pack $ concat [title, " (", startTime, ")"]))
  Nothing
  [T.pack title, "subnsnub"]
  [Media audioPath (takeFileName audioPath) Nothing ["SentAudio"]]
  []
  []
  where
    (r1, r2) = break ('_' ==) $ reverse $ takeBaseName audioPath
    startTime = takeWhile ('-' /=) $ reverse r1
    title = reverse $ tail r2

createAudio :: Time -> Time -> FilePath -> FilePath -> IO ()
createAudio start stop src dst = do
  let
    args = ffmpegArgs start stop src dst
    cmd = (proc "ffmpeg" args){ std_out = CreatePipe, std_err = CreatePipe }
    cmdStr = "'ffmpeg " ++ unwords args ++ "'"
  (_, Just outHdl, Just errHdl, p) <- createProcess cmd
  exitCode <- waitForProcess p
  _ <- hGetContents outHdl -- drain pipe
  err <- hGetContents errHdl
  case exitCode of
    ExitSuccess -> TIO.putStrLn $ T.concat ["Processed: ", T.pack src, " ", showTimeUnits start, "-", showTimeUnits stop]
    ExitFailure code -> throw $ ProcessException $ cmdStr ++ " quit with exit code " ++ show code ++ "\n" ++ err
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
