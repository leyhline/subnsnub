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
  , mkAddNotesAction
  ) where

import Control.Exception
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.Either (lefts)
import Data.Maybe (mapMaybe, catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Printf
import GHC.Generics
import Network.HTTP.Req
import System.Exit
import System.Process
import System.IO
import System.IO.Temp
import System.FilePath
import Subtitles (Subtitle(..), Time(..))
import SubtitleMarkup (Markup, toAnki, showSub)

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
  = AddNotes
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

data AddNotesResult = AddNotesResult
  { result :: [Maybe Integer]
  , error  :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON AddNotesResult

subtitlesToAnki :: [Subtitle] -> FilePath -> IO ()
subtitlesToAnki subs audioSrc = do
  maybeIds <- withSystemTempDirectory "subnsnub" f
  let
    logs :: [Either Markup Integer]
    logs = zipWith (\Subtitle { markup = sub } -> maybe (Left sub) Right) subs maybeIds
  -- [Either FailureSub SuccessNoteId] -> just print failure to stdout
  mapM_ (TIO.putStrLn . T.append "Failure: " . showSub) (lefts logs)
  where
    f :: FilePath -> IO [Maybe Integer]
    f tempdir = sendAction . uncurry mkAddNotesAction . unzip =<< mapM
      (\(Subtitle _ start stop sub) -> do
        let audioDstName = srcToDstName start stop audioSrc
            audioDstDir  = tempdir </> audioDstName
        createAudio start stop audioSrc audioDstDir
        return (sub, audioDstDir)
      ) subs

sendAction :: Action -> IO [Maybe Integer]
sendAction ac = runReq defaultHttpConfig $ do
  r <- req POST (http "localhost") (ReqBodyJson ac) jsonResponse (port 8765)
  rBody <- if responseStatusCode r == 200
    then return (responseBody r :: AddNotesResult)
    else throw $ RequestException $ B.unpack $ responseStatusMessage r
  liftIO (putStrLn "" >> putStrLn (buildMsg rBody))
  return $ result rBody
  where
    buildMsg (AddNotesResult results errMsg) =
      let totalNotes = length results
          goodNotes  = length $ catMaybes results
          msg = fromMaybe "None" errMsg
      in concat ["Notes creates: ", show goodNotes, "/", show totalNotes, "; error: ", T.unpack msg]

srcToDstName :: Time -> Time -> FilePath -> FilePath
srcToDstName start stop src = concat
  [ cleanBaseName $ takeBaseName src, "_"
  , T.unpack $ showTimeUnits start, "-"
  , T.unpack $ showTimeUnits stop
  ] <.> "ogg"

showTimeUnits :: Time -> Text
showTimeUnits (Time h m s ms) = T.pack $ printf "%02dh%02dm%02ds%03dms" h m s ms

cleanBaseName :: String -> String
cleanBaseName = mapMaybe f
  where f c
          | isSpace c = Just '_'
          | isAlphaNum c = Just c
          | otherwise = Nothing

mkAddNotesAction :: [Markup] -> [FilePath] -> Action
mkAddNotesAction subs audioPaths =
  Action "addNotes" 6 (AddNotes $ zipWith mkNote subs audioPaths)

mkNote :: Markup -> FilePath -> Note
mkNote sub audioPath = Note
  "Learning"
  "Japanese sentences"
  (Fields
    (toAnki sub)
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
    ExitSuccess -> TIO.putStr (T.concat ["Processed: ", T.pack src, " ", showTimeUnits start, "-", showTimeUnits stop, "\r"]) >> hFlush stdout
    ExitFailure code -> throw $ ProcessException $ cmdStr ++ " quit with exit code " ++ show code ++ "\n" ++ err
  return ()

ffmpegArgs :: Time -> Time -> FilePath -> FilePath -> [String]
ffmpegArgs start stop src dst =
  [ "-vn"
  , "-ss", showTime start
  , "-to", showTime stop
  , "-i", src
  , "-map_metadata", "-1", "-ac", "1"
  , "-c:a", "libopus", "-b:a", "24k", "-application", "voip"
  , dst]
  where showTime (Time h m s ms) = printf "%02d:%02d:%02d.%03d" h m s ms
