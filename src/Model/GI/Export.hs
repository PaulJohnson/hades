{-# LANGUAGE OverloadedLabels #-}

{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

-- |
module Model.GI.Export (
   previewFolder,
   previewReport,
   previewFolderClean,
   getImageResources,
   formatReportHtml,
   writeReportToFile,
   writeMatrixToFile
) where

import Codec.Xlsx
import Control.Monad.Except
import Control.Exception
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as B64
import Data.Either
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified GI.GLib as GLib
import GI.Gtk (AttrOp ((:=)))
import qualified GI.Gtk as GI
import GI.Cairo.Render
import Hades.GI.BasicShapes
import Hades.GI.Saving
import Model.Abstract.DiagramType
import Model.Abstract.PackageTree
import Model.GI.ModelDiagrams
import Model.Report.Base
import Model.Report.Dialogs
import Model.Report.Document
import Model.Report.Docx
import Model.Report.Docx.Document
import Model.Report.Html
import Reactive.Banana.GI.ErrorBox
import System.Directory
import System.FilePath
import System.Hades.DataFiles
import System.IO
import System.IO.Error
import System.IO.Temp
import qualified Text.Blaze.Html.Renderer.Utf8 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA


-- | Table of images. For each ImageRef there are 1 or 2 files. The first is the PNG, and the
-- second is an optional SVG.
type ImageMap = Map ImageRef (BL.ByteString, Maybe BL.ByteString)


-- | Preview files are stored in this folder.
previewFolder :: IO FilePath
previewFolder = do
   h <- getHomeDirectory
   return $ h </> ".dsmPreviews"


-- | Write the report to a temporary file and folder in HTML, then view it.
previewReport :: (HasDiagrams HadesRender v) =>
   Model v
   -> Text    -- ^ Report title
   -> [Block]
   -> [ImageRef]
   -> IO ()
previewReport model title blks imgs = flip catchIOError previewError $ do
         previewFolderClean
         fldr <- previewFolder
         imageMap <- getImageResources model imgs
         createDirectoryIfMissing True fldr
         baseName <- emptyTempFile fldr "preview.html"
         formatReportHtml title baseName blks imageMap
         uri <- GLib.filenameToUri baseName Nothing
         catchAny
            (GI.showUriOnWindow noWindow uri 0)  -- Zero is magic timestamp value for "now".
            (\err -> errorBox noWidget (T.pack $ show err))
   where
      previewError err = errorBox noWindow $ T.pack $ "File error during preview: " <> show err
      catchAny :: IO a -> (SomeException -> IO a) -> IO a
      catchAny = Control.Exception.catch
      noWindow :: Maybe GI.Window
      noWindow = Nothing
      noWidget :: Maybe GI.Widget
      noWidget = Nothing


{- | Clean the preview folder: attempt to delete any files older than 8 hours. Ignore any failures.

In theory "previewReport" should create a temporary folder and delete it once the preview is
closed. However it uses an external web browser as a viewer, so we don't know when that is.
The application also can't just delete the file once the program exits because the previewer
might have the file still open. This is the best compromise available.
-}
previewFolderClean :: IO ()
previewFolderClean = flip catchIOError cleanError $ do
   now <- getCurrentTime
   top <- previewFolder
   folderContents <- map (top </>) <$> listDirectory top
   forM_ folderContents $ \path -> do
      modTime <- getModificationTime path
      when (now `diffUTCTime` modTime > expiry) $ removePathForcibly path
   where
      expiry :: NominalDiffTime
      expiry = fromIntegral (8 * 60 * 60 :: Int)  -- 8 hours
      cleanError _ = return ()  -- Ignore any errors


-- | Write the report to the specified HTML file. Images are placed in a folder of the same name.
-- The first argument is the title of the resulting report.
formatReportHtml ::
   Text    -- ^ Report title
   -> FilePath  -- ^ Path for HTML file.
   -> [Block]
   -> ImageMap
   -> IO ()
formatReportHtml title path blks imageMap = do
      dataDir <- getProgramDataFolder
      stylesheet <- BL.readFile $ dataDir </> "report.css"
      let
         imageFunc =
            let m = M.mapWithKey imagePath imageMap
            in fromMaybe "brokenImage" . (`M.lookup` m)
               -- brokenImage can't happen because "m" includes every image in "blks".
         headStuff = H.head $ H.meta ! HA.charset (H.toValue ("UTF-8" :: Text))
               <> H.style (H.unsafeLazyByteString stylesheet)
               <> H.title (H.toHtml title)
         reportHtml = mkHtmlDocument imageFunc headStuff blks
         path2 = if hasExtension path then path else path <.> ".html"
         resultDir = dropExtension path2  -- hasExtension path2, therefore resultDir /= path2
         relativeDir = takeBaseName path2
         reportFile = "index.html"
      createDirectoryIfMissing False resultDir
      -- Write the HTML for the report.
      withFile (resultDir </> reportFile)  WriteMode $ \h ->
         H.renderHtmlToByteStringIO (B.hPut h) reportHtml
      -- Write a stub file which redirects to the actual report.
      withFile path2 WriteMode $ \h ->
         H.renderHtmlToByteStringIO (B.hPut h) $ H.docTypeHtml $
            H.head (
               H.preEscapedText $
                  "<meta http-equiv=\"refresh\" content=\"1; url="
                  <> T.pack (relativeDir </> reportFile) <> "\" />")
            <> H.body (
               H.p $
                  (H.a
                        ! HA.href (H.toValue $ relativeDir </> reportFile)
                        $ H.toHtml ("Click here" :: Text))
                  <> H.toHtml (" for the report." :: Text))
      forM_ (M.toList imageMap) $ \(ref, bytes) -> case ref of
         IconRef iconName -> writeImage (resultDir </> T.unpack iconName) bytes
         DiagramRef modelId -> writeImage (resultDir </> show modelId) bytes
   where
      imagePath (IconRef nm) (_, Nothing) = T.unpack nm <.> "png"  -- Some icons are only PNG.
      imagePath (IconRef nm) (_, Just _) = T.unpack nm <.> "svg"
      imagePath (DiagramRef mId) _ = show mId <.> "svg"  -- Diagrams always have SVG.
      writeImage base (png, Nothing) = void $ BL.writeFile (base <.> "png") png
      writeImage base (_, Just svg) = void $ BL.writeFile (base <.> "svg") svg


-- | Ask the user for a file to write to, and create a formatted report file.
writeReportToFile :: (HasDiagrams HadesRender v) =>
   ReportFormat
   -> Text   -- ^ Report title.
   -> Model v
   -> [Block]
   -> [ImageRef]
   -> IO ()
writeReportToFile format title model blks imgs = do
      imageMap <- getImageResources model imgs
      case format of
         ReportHtml target -> formatReportHtml title target blks imageMap
         ReportDocx fmt target -> makeDocx (Just fmt) title target blks imageMap
         ReportTemplate template target -> insertDocx template target blks imageMap

-- | The binary data associated with each ImageRef. Within the result elements the first item
-- is a PNG, the second is an optional SVG.
getImageResources :: (HasDiagrams HadesRender v) =>
   Model v -> [ImageRef] -> IO ImageMap
getImageResources model refs = do
      theme <- GI.iconThemeGetDefault
      results <- forM refs $ \ref -> do
         png <- fromMaybe nullPng <$> binaryResourceFunc theme ref PngTarget
         svg <- binaryResourceFunc theme ref SvgTarget
         return (ref, (png, svg))
      return $ M.fromList results
   where
      -- Minimal PNG file: one transparent pixel.
      nullPng = BL.fromStrict $ B64.decodeLenient
         "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABAQMAAAAl21bKAAAAA1\
         \BMVEUAAACnej3aAAAAAXRSTlMAQObYZgAAAApJREFUCNdjYAAAAAIAAeIhvDMAAAAASUVORK5CYII="
      binaryResourceFunc theme (IconRef iconName) PngTarget =
         -- 48x48 icon taken because that is what the Diametric theme has available.
         GI.iconThemeLookupIcon theme iconName 48 [GI.IconLookupFlagsNoSvg] >>= \case
            Nothing -> return Nothing
            Just info -> GI.iconInfoGetFilename info >>= \case
               Nothing -> return Nothing
               Just iconPath ->
                  if takeExtension iconPath == ".png"
                     then Just <$> BL.readFile iconPath
                     else return Nothing
      binaryResourceFunc theme (IconRef iconName) SvgTarget =
         -- Any resolution except 48x48. Otherwise we still get the PNG.
         GI.iconThemeLookupIcon theme iconName 128 [GI.IconLookupFlagsForceSvg] >>= \case
            Nothing -> return Nothing
            Just info -> GI.iconInfoGetFilename info >>= \case
               Nothing -> return Nothing
               Just iconPath ->
                  if takeExtension iconPath == ".svg"
                     then Just <$> BL.readFile iconPath
                     else return Nothing
      binaryResourceFunc _ (DiagramRef modelId) PngTarget =
         fromRight (return Nothing) $ evalModelEdit id model $ do  -- ModelEdit (IO BL.ByteString)
            goToEntity modelId
            current >>= \case
               Nothing -> return $ return Nothing  -- Should never happen.
               Just e ->
                  getDiagramWrapper e >>= \case
                     Just (DiagramWrapper _ dgram _ prsm _ ctrl _ _) -> do
                        let (draw, size) = renderForExport prsm model ctrl dgram
                        return $ Just <$> renderOnPng (72*2) size draw
                     _ -> return $ return Nothing  -- No diagram. Shouldn't happen.
      binaryResourceFunc _ (DiagramRef modelId) SvgTarget =
         fromRight (return Nothing) $ evalModelEdit id model $ do  -- ModelEdit (IO BL.ByteString)
            goToEntity modelId
            current >>= \case
               Nothing -> return $ return Nothing  -- Should never happen.
               Just e ->
                  getDiagramWrapper e >>= \case
                     Just (DiagramWrapper _ dgram _ prsm _ ctrl _ _) -> return $ do  -- IO
                        let (draw, (w, h)) = renderForExport prsm model ctrl dgram
                        svgPath <- emptySystemTempFile "dsm-.svg"
                        withSVGSurface svgPath w h $ \surface -> do
                           renderWith surface draw
                           surfaceFinish surface
                        -- Strict readFile because we are about to delete the file.  Linux plays
                        -- nicely when an open file is deleted, but Windows will throw an error.
                        bytes <- B.readFile svgPath
                        removeFile svgPath
                        return $ Just $ BL.fromStrict bytes
                     _ -> return $ return Nothing  -- No diagram. Shouldn't happen.
      binaryResourceFunc _ _ ExternalTarget = return Nothing


writeMatrixToFile :: Xlsx -> IO ()
writeMatrixToFile book = do
      dialog <- GI.new GI.FileChooserNative [
            #action := GI.FileChooserActionSave,
            #title := "Export matrix as",
            #acceptLabel := "Export"
         ]
      filters <- mkFilters
      mapM_ (GI.fileChooserAddFilter dialog) filters
      GI.fileChooserSetDoOverwriteConfirmation dialog True
      result <- GI.nativeDialogRun dialog
      when (result == fromIntegral (fromEnum GI.ResponseTypeAccept)) $ do
         mTarget <- GI.fileChooserGetFilename dialog
         GI.nativeDialogDestroy dialog
         case mTarget of
            Nothing -> return ()
            Just target -> do
               t <- getPOSIXTime
               BL.writeFile (checkExtension "xlsx" target) $ fromXlsx t book
   where
      mkFilters = do
         fileFilter <- GI.fileFilterNew
         GI.fileFilterSetName fileFilter $ Just "Excel *.xlsx"
         GI.fileFilterAddPattern fileFilter "*.xlsx"
         return [fileFilter]
