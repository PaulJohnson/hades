{-# LANGUAGE OverloadedLabels #-}

{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

{- |
-}
module Hades.GI.Saving (
   FileStatus (FileStatus),
   fileStatusLock,
   fileStatusPath,
   readOnlyStatus,
   openUserFile,
   forEditing,
   openJsonFile,
   openExcelFile,
   saveFile,
   saveAsFile,
   writeAsFile,
   tryDecodeXml,
   catchFileError,
   catchFileError_,
   checkExtension,
   ExportType (..)
) where

import Control.Lens
import Codec.Xlsx
import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.ICU.Convert as ICU
import GI.Gtk (AttrOp ((:=)))
import qualified GI.Gtk as Gtk
import Reactive.Banana.GI.ErrorBox
import System.FilePath
import System.Hades.EditLock
import System.IO.Error


-- | Metadata about the file where a model is saved.
data FileStatus = FileStatus {
      _fileStatusLock :: Maybe EditLock,
      _fileStatusPath :: FilePath
   }

-- | If the file is writeable then it will be locked for editing.
fileStatusLock :: Lens' FileStatus (Maybe EditLock)
fileStatusLock = lens _fileStatusLock $ \s l -> s {_fileStatusLock = l}

-- | The path of the file being edited.
fileStatusPath :: Lens' FileStatus FilePath
fileStatusPath = lens _fileStatusPath $ \s p -> s {_fileStatusPath = p}


-- | Is the status read-only? "Nothing" means the model is unsaved and so can be edited freely.
-- A value with a lock can be edited. A value with no lock cannot be edited.
readOnlyStatus :: Maybe FileStatus -> Bool
readOnlyStatus Nothing = False
readOnlyStatus (Just s) = s ^. fileStatusLock . to isNothing


-- | Open a file, prompting for the path with the GTK File Chooser. If successful then return
-- the chosen pathname and its contents. If not then return @Nothing@. Problems are reported to
-- the user with GTK nofication windows.
openUserFile :: (Gtk.IsWidget parent) =>
   Maybe parent             -- ^ Widget in parent window.
   -> [Gtk.FileFilter]      -- ^ Possible file types. Default is first item. Empty for no filter.
   -> Maybe FilePath        -- ^ Default file name, if known.
   -> IO (Maybe (FilePath, LB.ByteString))
openUserFile parent filters fname = do
   dialog <- Gtk.new Gtk.FileChooserNative [
         #action := Gtk.FileChooserActionOpen,
         #title := "Open Model File"]
   parentWin <- join <$> mapM (Gtk.castTo Gtk.Window <=< Gtk.widgetGetToplevel) parent
   forM_ parentWin $ \w -> Gtk.set dialog [#transientFor := w]
   mapM_ (Gtk.fileChooserAddFilter dialog) filters
   mapM_ (Gtk.fileChooserSetFilename dialog) fname
   Gtk.fileChooserSetDoOverwriteConfirmation dialog False
   result <- Gtk.nativeDialogRun dialog
   if result == fromIntegral (fromEnum Gtk.ResponseTypeAccept)
      then do
         mTarget <- Gtk.fileChooserGetFilename dialog
         Gtk.nativeDialogDestroy dialog
         case mTarget of
            Nothing -> return Nothing
            Just target -> fmap (target, ) <$> catchFileError parent (LB.readFile target)
      else do
         Gtk.nativeDialogDestroy dialog
         return Nothing


-- | If the argument action opens a file then try to get an edit lock on it. If successful return
-- the lock. Otherwise ask the user if they want a read-only copy.
forEditing :: (Gtk.IsWidget parent) =>
   parent               -- ^ Widget in parent window for read-only dialog.
   -> Maybe FileStatus     -- ^ Status of the old model, if any.
   -> IO (Maybe (FilePath, a))   -- ^ IO action that can open a file, returning the path and contents
                              -- if successful.
   -> IO (Maybe (FileStatus, a))
forEditing parent oldStatus openAct = openAct >>= \case
   Nothing -> return Nothing
   Just (p, v) -> do
      mapM_ dropEditLock $ oldStatus ^? _Just . fileStatusLock . _Just
      tryEditLock p >>= \case
         EditLockSuccessful l -> return $ Just (FileStatus (Just l) p, v)
         EditLockFailed msg -> do
            dialog <- Gtk.new Gtk.MessageDialog [
                  #messageType := Gtk.MessageTypeQuestion,
                  #buttons := Gtk.ButtonsTypeOkCancel,
                  #text :=
                     msg <> ".\n\
                     \You can currently open it read-only. Either save it in a new file \
                     \or try again later."
               ]
            showProblem dialog p v
         EditLockError err -> do
            dialog <- Gtk.new Gtk.MessageDialog [
                  #messageType := Gtk.MessageTypeWarning,
                  #buttons := Gtk.ButtonsTypeOkCancel,
                  #text :=
                     "Cannot create the editing lock file because: " <> T.pack (show err) <> ".\n\
                     \The file will be opened read-only."
               ]
            showProblem dialog p v
   where
      showProblem d p v = do
         parentWin <- Gtk.castTo Gtk.Window =<< Gtk.widgetGetToplevel parent
         forM_ parentWin $ \w -> Gtk.set d [#transientFor := w]
         result <- Gtk.dialogRun d
         Gtk.widgetDestroy d
         return $ if result == fromIntegral (fromEnum Gtk.ResponseTypeOk)
            then Just (FileStatus Nothing p, v) else Nothing


-- | As for "openUserFile", but also parses the file contents as a JSON object.
openJsonFile :: (FromJSON a, Gtk.IsWidget parent) =>
   parent                   -- ^ Widget in parent window.
   -> [Gtk.FileFilter]      -- ^ Possible file types. Default is first item. Empty for no filter.
   -> Maybe FilePath        -- ^ Default file name, if known.
   -> IO (Maybe (FilePath, a))
openJsonFile parent filters fname =
   openUserFile (Just parent) filters fname >>= \case
      Nothing -> return Nothing
      Just (target, text) -> case eitherDecode' text of
         Left str -> do
            errorBox (Just parent) $ T.pack $
               "File \"" <> target <> "\" was corrupt and could not be read.\n\n"
               <> "Technical details: " <> str
            return Nothing
         Right doc -> return $ Just (target, doc)


-- | As for "openUserFile", but also parses the file contents as an Excel file.
openExcelFile :: (Gtk.IsWidget parent) =>
   parent -> Maybe FilePath -> IO (Maybe (FilePath, Xlsx))
openExcelFile parent fname = do
   xlsFilter <- Gtk.fileFilterNew
   Gtk.fileFilterSetName xlsFilter $ Just "Excel files *.xlsx"
   Gtk.fileFilterAddPattern xlsFilter "*.xlsx"
   openUserFile (Just parent) [xlsFilter] fname >>= \case
      Nothing -> return Nothing
      Just (target, text) -> case toXlsxEither text of
         Left err -> do
            errorBox (Just parent) $ T.pack $
               "File \"" <> target <> "\" was corrupt and could not be read.\n\n"
               <> "Technical details: " <> show err
            return Nothing
         Right book -> return $ Just (target, book)


-- | If the target file name is known then overwrite that file. Otherwise invoke "saveAsFile"
saveFile :: (ToJSON a, Gtk.IsWidget parent) =>
   parent                   -- ^ Widget in parent window.
   -> [Gtk.FileFilter]      -- ^ Possible file types if "saveAsFile" is invoked.
                            --   Default is first item. Empty for no filter
   -> (FilePath -> FilePath) -- ^ Modify the chosen pathname, e.g. to have the right extension.
   -> Maybe FilePath        -- ^ Target file name, if known.
   -> a                     -- ^ Document to be saved.
   -> IO (Maybe (FilePath, a))
saveFile parent filters pathFunc fname document =
   case fname of
      Just target -> do
         success <- catchFileError_ (Just parent) $ LB.writeFile target $ encode document
         if success
            then return $ Just (target, document)
            else saveAsFile parent filters pathFunc fname document
      Nothing -> saveAsFile parent filters pathFunc fname document


-- | As for "writeAsFile", except that the dialog title is hard coded.
saveAsFile :: (ToJSON a, Gtk.IsWidget parent) =>
   parent                   -- ^ Widget in parent window.
   -> [Gtk.FileFilter]      -- ^ Possible file types. Default is first item. Empty for no filter.
   -> (String -> String)    -- ^ Modify the chosen pathname, e.g. to have the right extension.
   -> Maybe String          -- ^ The file name for this document, if known.
   -> a                     -- ^ Document to be saved.
   -> IO (Maybe (FilePath, a))
saveAsFile parent = writeAsFile parent "Save Model File" "Save"


-- | Save the document under a new file name. Returns the new name. If the
-- operation is cancelled or fails then it does nothing.
writeAsFile :: (ToJSON a, Gtk.IsWidget parent) =>
   parent                   -- ^ Widget in parent window.
   -> Text                  -- ^ Title for dialog.
   -> Text                  -- ^ Label for accept button.
   -> [Gtk.FileFilter]      -- ^ Possible file types. Default is first item. Empty for no filter.
   -> (String -> String)    -- ^ Modify the chosen pathname, e.g. to have the right extension.
   -> Maybe String          -- ^ The file name for this document, if known.
   -> a                     -- ^ Document to be saved.
   -> IO (Maybe (FilePath, a))
writeAsFile parent titleTxt buttonTxt filters pathFunc fname document = do
   dialog <- Gtk.new Gtk.FileChooserNative [
         #action := Gtk.FileChooserActionSave,
         #title := titleTxt,
         #acceptLabel := buttonTxt
      ]
   parentWin <- Gtk.castTo Gtk.Window =<< Gtk.widgetGetToplevel parent
   forM_ parentWin $ \w -> Gtk.set dialog [#transientFor := w]
   mapM_ (Gtk.fileChooserSetFilename dialog) fname
   mapM_ (Gtk.fileChooserAddFilter dialog) filters
   Gtk.fileChooserSetDoOverwriteConfirmation dialog True
   result <- Gtk.nativeDialogRun dialog
   if result == fromIntegral (fromEnum Gtk.ResponseTypeAccept)
      then do
         mTarget <- fmap pathFunc <$> Gtk.fileChooserGetFilename dialog
         Gtk.nativeDialogDestroy dialog
         case mTarget of
            Nothing -> return Nothing
            Just target -> do
               success <- catchFileError_ (Just parent) $ LB.writeFile target $ encode document
               if success
                  then return $ Just (target, document)
                  else saveAsFile parent filters pathFunc fname document
      else do
         Gtk.nativeDialogDestroy dialog
         return Nothing


-- | Try to find out what encoding this XML file is in, and return as a @Right@ text string.
-- @Left@ indicates an error.
tryDecodeXml :: B.ByteString -> IO (Either Text Text)
tryDecodeXml bytes =
      handle exceptionHandler $
         -- This is either in an 8-bit format or its in one of the UTF multi-byte formats.
         if B.take (T.length xmlTag) bytes == T.encodeUtf8 xmlTag
            then do  -- This is an 8-bit format. Read the encoding from the XML Declaration.
               let
                  -- Find the closing "?>". 100 chars > the longest possible declaration.
                  decl = fst $ T.breakOn "?>" $ T.take 100 $ T.decodeLatin1 bytes
                  -- Extract a quoted string following "encoding", or "" if not found.
                  encoding1 = snd $ T.breakOn "encoding" decl
                  encoding2 = T.drop 1 $ snd $ T.breakOn "\"" encoding1
                  encoding3 = fst $ T.breakOn "\"" encoding2
                  encoding = if encoding3 == "" then "utf-8" else encoding3
               -- Get the converter for the encoding.
               cnv <- ICU.open (T.unpack encoding) (Just True)
               return $ Right $ ICU.toUnicode cnv bytes
            else  -- Not an 8-bit format. Compare the initial bytes to find out which.
               case snd <$> find testHeader headers of
                  Just decoder -> return $ Right $ decoder bytes
                  Nothing -> return $ Left "Not an XML file."
   where
      testHeader (tag, _) = tag `B.isPrefixOf` bytes
      headers = [
            (T.encodeUtf16LE xmlTag, T.decodeUtf16LEWith T.lenientDecode),
            (T.encodeUtf16BE xmlTag, T.decodeUtf16BEWith T.lenientDecode),
            (T.encodeUtf32LE xmlTag, T.decodeUtf32LEWith T.lenientDecode),
            (T.encodeUtf32BE xmlTag, T.decodeUtf32BEWith T.lenientDecode)
         ]
      xmlTag = "<?xml"
      exceptionHandler :: SomeException -> IO (Either Text Text)
      exceptionHandler = return . Left . T.pack . show


-- | Like "catchFileError" but with return value ignored. Returns True for normal exit,
-- False for exception.
catchFileError_ :: (Gtk.IsWidget parent) => Maybe parent -> IO a -> IO Bool
catchFileError_ parent action = isJust <$> catchFileError parent action


-- | Put up a dialog box with a meaningful error message when a file access fails,
-- and return Nothing.
catchFileError :: (Gtk.IsWidget parent) => Maybe parent -> IO a -> IO (Maybe a)
catchFileError parent action = catchIOError (Just <$> action) $ \err -> do
      print err
      errorBox parent
         $ T.pack $ messageText err <> "\n\nTechnical details:\n" <> show err
      return Nothing
   where
      messageText err = case ioeGetFileName err of
         Just errorFile -> "File \"" <> errorFile <> "\" " <> errorType err <> "."
         Nothing -> "That file " <> errorType err <> "."
      errorType err
         | isAlreadyExistsError err  = "already exists"
         | isDoesNotExistError err   = "does not exist"
         | isAlreadyInUseError err   = "cannot be accessed because it is being used"
         | isFullError err           = "cannot be written because the device is full"
         | isEOFError err            = "is incomplete or corrupt"
         | isIllegalOperation err    = "cannot be accessed for some reason (illegal operation)"
         | isPermissionError err     = "cannot be accessed because you do not have permission"
         | isUserError err           = "cannot be accessed for some reason (programmer exception)"
         | otherwise                 = "cannot be accessed due to an unknown error"


-- | Ensure that a pathname has the right extension. If not, append it.
checkExtension :: String -- ^ Extension without the \".\" separator.
   -> FilePath -> FilePath
checkExtension ext path =
   if takeExtension path == extSeparator : ext then path else addExtension path ext

-- | Supported types of file export.
data ExportType = SVG | Postscript | PDF | PNG deriving (Eq, Ord, Bounded, Enum, Show, Read)
