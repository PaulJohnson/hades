{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

{- |

General file utilities for managing Docx Word documents.

The OpenOffice document standard uses Zip as a packaging mechanism so that different file types
(e.g. image files) can be included verbatim. There are also a number of metadata files. Some
of these can be copied verbatim from an existing file, and others
in modified versions.

To this end an empty Word file has been unzipped and its components stored in the application data.
-}

module Model.Report.Docx.File (
  writeDocxFile,
  insertDocxFile,
  renderXml,
  findNameDF,
  getSkeletonStyles
) where

import Codec.Archive.Zip
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding
import Data.Time
import Data.Time.Clock.POSIX
import Model.Report.Docx.Document
import Model.Report.Docx.Utils
import Paths_hades
import System.Directory
import System.Environment
import System.FilePath
import qualified Text.XML.Light as X
import qualified Text.XML.Light.Cursor as X


-- | The directory where the Word skeleton files are stored.
getSkeletonDir :: IO FilePath
getSkeletonDir = getDataFileName "docx"


-- | Read the named styles from the styles.xml file in the skeleton. Missing styles are silently
-- ignored.
getSkeletonStyles :: [Text] -> IO [X.Element]
getSkeletonStyles names = do
    stylePath <- (</> "word" </> "styles.xml") <$> getSkeletonDir
    styleFile <- X.parseXML <$> LB.readFile stylePath
    return $ map (getStyle styleFile) names
  where
    getStyle file nm = case getStyleById nm file of
      Nothing -> namedBlankStyle nm
      Just s -> s


-- | A blank style (no formatting changes) with the given name.
--
-- Word requires that every style used in the document must have an entry in the styles.xml file.
-- This fulfils that requirement.
namedBlankStyle :: Text -> X.Element
namedBlankStyle nm = X.node (wName "style") (
    [wAttr "type" "paragraph", wAttr "customStyle" "1", wAttr "styleId" nm],
    [
      X.node (wName "name") [wAttr "val" nm],
      X.node (wName "basedOn") [wAttr "val" "Normal"],
      X.node (wName "qFormat") (),
      pPr ()
    ])


-- | Package up a list of ByteStrings as a Word document.
--
-- The skeleton files are modified in two ways before being written:
--
-- 1. XML files can be parsed and edited using a cursor.
--
-- 2. Files can be added, overwriting any skeleton file of the same path.
--
-- The core document properties will have the date, user and title set automatically.
--
-- The @FilePath@s within the skeleton MUST use \"/\" as a pathname separator regardless of the
-- platform. DO NOT use the "(</>)" operator to create them, as it will do the wrong thing on
-- Windows.
writeDocxFile ::
  Text      -- ^ Title of the report.
  -> FilePath  -- ^ File to be created. Will have \".docx\" suffix appended if it is not present.
  -> [(FilePath, X.Cursor -> X.Cursor)]  -- ^ Edits to XML files within the skeleton.
  -> [(FilePath, LB.ByteString)]  -- ^ Pairs of paths within the zip file and their contents.
  -> IO ()
writeDocxFile title target1 edits additions = do
    docTime <- getCurrentTime
    let posixTime = floor $ utcTimeToPOSIXSeconds docTime
    userName <- do
      user1 <- lookupEnv "USERNAME"  -- Windows user name.
      user2 <- lookupEnv "USER"      -- Unix user name.
      return $ fromMaybe "unknown" $ user1 <|> user2
    skeleton <- do
      skelDir <- getSkeletonDir
      paths <- walkDirectory skelDir
      pairs <- forM paths $ \p -> do
        bytes <- LB.readFile p
        return (map fixSlash $ makeRelative skelDir p, bytes)
      return $ M.fromList pairs
    let
      target2 = if "docx" `isExtensionOf` target1 then target1 else target1 <.> "docx"
      core = setCreation title docTime userName
      skel2 = foldr (\(p, f) -> M.adjust (updateXml f) p) skeleton (core : edits)
      skel3 = foldr (uncurry M.insert) skel2 additions
      entries = map (\(p, bytes) -> toEntry p posixTime bytes) $ M.toList skel3
      arch = foldr addEntryToArchive emptyArchive entries
    LB.writeFile target2 $ fromArchive arch
  where
    fixSlash '\\' = '/'
    fixSlash c = c
    updateXml f bytes =
      maybe bytes (renderXml . X.toForest . f) $ X.fromForest (X.parseXML bytes)


-- | Insert material into an existing template Word document.
--
-- The @FilePath@s within the template MUST use \"/\" as a pathname separator regardless of the
-- platform. DO NOT use the "(</>)" operator to create them, as it will do the wrong thing on
-- Windows.
insertDocxFile ::
  FilePath          -- ^ Word file to use as template.
  -> FilePath       -- ^ Word file to write result into.
  -> [(FilePath, X.Cursor -> X.Cursor)]  -- ^ XML files within the template to modify.
  -> [(FilePath, LB.ByteString)]   -- ^ New media to add to the template.
  -> IO ()
insertDocxFile templatePath targetPath edits additions = do
    docTime <- getCurrentTime
    let posixTime = floor $ utcTimeToPOSIXSeconds docTime
    userName <- do
      user1 <- lookupEnv "USERNAME"  -- Windows user name.
      user2 <- lookupEnv "USER"      -- Unix user name.
      return $ fromMaybe "unknown" $ user1 <|> user2
    template <- M.fromList . map extract . zEntries . toArchive <$> LB.readFile templatePath
    let
      targetPath2 =
        if "docx" `isExtensionOf` targetPath then targetPath else targetPath <.> "docx"
      core = setModified docTime userName
      template2 = foldr (\(p, f) -> M.adjust (updateXml f) p) template (core : edits)
      template3 = foldr (uncurry M.insert) template2 additions
      entries = map (\(p, bytes) -> toEntry p posixTime bytes) $ M.toList template3
      arch = foldr addEntryToArchive emptyArchive entries
    LB.writeFile targetPath2 $ fromArchive arch
  where
    extract e = (eRelativePath e, fromEntry e)
    updateXml f bytes =
      maybe bytes (renderXml . X.toForest . f) $ X.fromForest (X.parseXML bytes)


-- | Update core attributes with creation details.
setCreation ::
  Text    -- ^ Document title.
  -> UTCTime   -- ^ Creation time.
  -> String    -- ^ Creator name.
  -> (FilePath, X.Cursor -> X.Cursor)
setCreation title utcTime n = ("docProps/core.xml", f)
  where
    f = applyAll [setTitle, setCreator, setModifiedBy, setCreated, setModified1]
    setTitle c = X.modifyContent (setElemText $ T.unpack title) <$> findNameDF titleQN (goStart c)
    setCreator c = X.modifyContent (setElemText n) <$> findNameDF creatorQN (goStart c)
    setModifiedBy c = X.modifyContent (setElemText n) <$> findNameDF modifiedByQN (goStart c)
    setCreated c = X.modifyContent (setElemText dateTime) <$> findNameDF createdQN (goStart c)
    setModified1 c = X.modifyContent (setElemText dateTime) <$> findNameDF modifiedQN (goStart c)
    titleQN = X.QName "title" Nothing (Just "dc")
    creatorQN = X.QName "creator" Nothing (Just "dc")
    modifiedByQN = X.QName "lastModifiedBy" Nothing (Just "cp")
    createdQN = X.QName "created" Nothing (Just "dcterms")
    modifiedQN = X.QName "modified" Nothing (Just "dcterms")
    dateTime = formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%SZ") utcTime


-- | Update core attributes with modification details.
setModified :: UTCTime -> String -> (FilePath, X.Cursor -> X.Cursor)
setModified utcTime n = ("docProps/core.xml", f)
  where
    f = applyAll [setModifiedBy, setModified1]
    setModifiedBy c = X.modifyContent (setElemText n) <$> findNameDF modifiedByQN (goStart c)
    setModified1 c = X.modifyContent (setElemText dateTime) <$> findNameDF modifiedQN (goStart c)
    modifiedByQN = X.QName "lastModifiedBy" Nothing (Just "cp")
    modifiedQN = X.QName "modified" Nothing (Just "dcterms")
    dateTime = formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%SZ") utcTime




-- | Returns a list of the plain files (not the directories) in the path. The pathnames returned
-- use "/" as a separator regardless of the platform so that they are compatible with zip files.
walkDirectory :: FilePath -> IO [FilePath]
walkDirectory path = do
  isFile <- doesFileExist path  -- File and directory should be mutually exclusive.
  isDirectory <- doesDirectoryExist path
  if isFile
    then return [path]
    else if isDirectory
      then do
        contents <- map ((path <> "/") <>) <$> listDirectory path
        concat <$> mapM walkDirectory contents
      else return []  -- Should never happen.


-- | Encode an XML document as a lazy bytestring.
renderXml :: [X.Content] -> LB.ByteString
renderXml = encodeUtf8 . mconcat . map (LT.pack . X.showContent)
