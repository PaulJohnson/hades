{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}


{- |

Folders and file names for autosave files.
-}

module System.Hades.Autosave (
   AutosaveFile (..),
   autosaveExtension,
   AppName,
   autosaveFolder,
   autosaveFiles,
   autosaveNewPath,
   autosaveExpire,
   AutosaveContents (..),
   autosaveGetContents,
   autosaveProcess
) where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Function
import Data.IORef
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Time
import Data.UUID hiding (null)
import Data.UUID.V5
import System.Directory
import System.FilePath
import Text.Read


-- | The Application Name is used to determine the autosave folder.
type AppName = String


-- | Metadata for autosave files.
data AutosaveFile = AutosaveFile {
      autosavePath :: FilePath,   -- ^ Filename within "autosaveFolder".
      autosaveHash :: String,     -- ^ Hashcode component of the filename.
      autosaveSeq :: Int,         -- ^ Sequence number component of the filename.
      autosaveWhen :: UTCTime     -- ^ Timestamp of the file.
   } deriving Show

-- | The extension for autosave files. This is constant ".asv" regardless of the program.
autosaveExtension :: String
autosaveExtension = ".asv"


-- | Extract the hash and serial number components from an autosave pathname.
splitAutosave :: FilePath -> (String, Int)
splitAutosave path = (hash, n)
   where
      fn = dropExtension $ takeFileName path
      (hash, serial) = span (/= 'X') fn
      n = fromMaybe 0 $ readMaybe $ drop 1 serial


-- | Folder where autosave files will go. The argument is the application name.
-- The program will also attempt to create the folder if it
-- does not exist.
autosaveFolder :: AppName -> IO FilePath
autosaveFolder appName = do
   dataDir <- getAppUserDataDirectory appName
   createDirectoryIfMissing True dataDir
   return dataDir


-- | Returns a list of known autosave files in no particular order.
autosaveFiles :: AppName -> IO [AutosaveFile]
autosaveFiles appName = do
   folder <- autosaveFolder appName
   files <- filter ((== autosaveExtension) . takeExtension) <$> listDirectory folder
   forM files $ \f -> do
      let
         (hash, seqNum) = splitAutosave f
         path = folder </> f
      AutosaveFile path hash seqNum <$> getModificationTime path


-- | Autosave files are named with a hash of their pathname followed by a serial number. This
-- allows autosave versions of the same file to be tracked while avoiding the problem of
-- escaping the pathname. However it does mean that different paths that reach the same file
-- will have different autosave sequences.
autosaveNewPath :: AppName -> FilePath -> IO FilePath
autosaveNewPath appName path = do
      folder <- autosaveFolder appName
      let
         uuid = generateNamed namespaceURL $ BS.unpack $ encodeUtf8 $ T.pack $ "file://" ++ path
         hash = toString uuid
      existing <- filter ((== hash) . autosaveHash) <$> autosaveFiles appName
      if null existing
         then
            return $ newName folder hash (0 :: Int)
         else do
            let n = maximum $ map autosaveSeq existing  -- Safe because existing /= []
            return $ newName folder hash (n+1)
   where
      newName fldr h n = fldr </> h ++ "X" ++ show n <.> autosaveExtension


-- | Trim the autosave folder so that only the latest N files are kept for each hash code.
autosaveExpire :: AppName -> Int -> IO ()
autosaveExpire appName n = do
   files <- sortOn autosaveHash <$> autosaveFiles appName
   let
      groups = groupBy ((==) `on` autosaveHash) files
      expired = concatMap (drop n . sortOn (Down . autosaveWhen)) groups
   forM_ expired $ removeFile . autosavePath


-- The contents of an autosave file are the filename encoded in UTF8 and terminated by an
-- STX (ctrl-B) character, and a lazy bytestring containing the saved data.
data AutosaveContents = AutosaveContents {
      autosaveFor :: FilePath,
      autosaveData :: LBS.ByteString
   }


-- | If the original filepath can be decoded then return the contents. Otherwise return Nothing.
autosaveGetContents :: FilePath -> IO (Maybe AutosaveContents)
autosaveGetContents path =
   try (LBS.readFile path) >>= \case
      Left (_ :: IOException) -> return Nothing
      Right raw -> do
         let
            header = LBS.takeWhile (/= 2) $ LBS.take 4096 raw
               -- Protect from corrupt files which don't have the STX character.
               -- 4096 is the Linux MAX_PATH. Windows MAX_PATH is much smaller.
            saved = LBS.drop 1 $ LBS.dropWhile (/= 2) raw
         case decodeUtf8' $ LBS.toStrict header of
            Left _ -> return Nothing
            Right name -> return $ Just $ AutosaveContents (T.unpack name) saved


-- | Forks off a thread which will autosave values sent using an IO action. The rate of
-- saves is throttled. Values sent before the timeout are discarded. Each value is saved
-- in a separate file. Once the number of files exceeds a given value the old files are deleted.
--
-- The return value is an IO function which takes the pathname of the file and a value to save.
-- If the pathname is unknown then an empty string may be used.
autosaveProcess :: (ToJSON a) =>
   AppName
   -> NominalDiffTime  -- ^ Minimum interval between autosaves.
   -> Int              -- ^ Maximum number of files to keep.
   -> IO (FilePath -> a -> IO())
autosaveProcess appName interval copies = do
      values <- atomically newTQueue
      void $ forkFinally (saveThread values) (handleFailure values)
      return $ \path value -> atomically $ writeTQueue values (path, value)
   where
      saveThread :: (ToJSON a) => TQueue (String, a) -> IO ()
      saveThread values = do
         lastSaveTime <- newIORef zeroTime
         forever $ do
            (path, value) <- atomically $ readTQueue values
            now <- getCurrentTime
            prevTime <- readIORef lastSaveTime
            when (now `diffUTCTime` prevTime > interval) $ do
               writeIORef lastSaveTime now
               savePath <- autosaveNewPath appName path
               let
                  header = LBS.fromStrict $ encodeUtf8 (T.pack path) `BS.snoc` 2
                     -- The '2' marks the end of the header (ASCII STX).
                  body = encode value
               LBS.writeFile savePath $ header `LBS.append` body
               autosaveExpire appName copies
      handleFailure values _ = void $ forkIO $ do
         threadDelay $ 60 * 1000000 -- 1 minute in microseconds. Throttles restart.
         void $ forkFinally (saveThread values) (handleFailure values)
      zeroTime = UTCTime (ModifiedJulianDay 0) 0  -- A long time ago, in a galaxy far, far away.
