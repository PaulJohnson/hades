{-# LANGUAGE OverloadedStrings #-}
{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

{- |

Generate Word files from report documents.
-}
module Model.Report.Docx (
   insertDocx,
   makeDocx
) where

import Control.Arrow
import Control.Lens
import Control.Monad.State
import qualified Data.ByteString.Lazy as BL
import qualified Data.Char as C
import qualified Data.Colour.SRGB as C
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Model.Report.Base
import Model.Report.Document
import Model.Report.Docx.Document
import Model.Report.Docx.Drawing
import Model.Report.Docx.File
import Model.Report.Docx.Utils
import Model.Report.Walkable
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding
import Data.Tree
import Reactive.Banana.Common
import qualified Text.XML.Light as X
import qualified Text.XML.Light.Cursor as XC
import qualified Text.XML.Light.Lens as X


-- | Insert document content into an existing Word file.
insertDocx ::
   FilePath   -- ^ Existing Word file.
   -> FilePath   -- ^ File to store new document in.
   -> [Block]    -- ^ Content to be inserted.
   -> Map ImageRef (BL.ByteString, Maybe BL.ByteString)  -- ^ Files to include.
   -> IO ()
insertDocx template target blocks imageMap = do
      defaultStyles <- getSkeletonStyles styleNames
      let
         xmlUpdates = [
               ("[Content_Types].xml",
                  addMissingContentType "png" "image/png" .
                  addMissingContentType "svg" "image/svg+xml") ,
               ("word/_rels/document.xml.rels",
                  \c -> maybe c (addRels rels) $ findNameDF (X.unqual "Relationships") c),
               ("word/document.xml",
                  \c -> fromMaybe c $ modifyDocumentAttrs c >>= appendBodyElements docBody),
               ("word/styles.xml",
                  applyAll $ map addMissingStyle defaultStyles)
            ]
      insertDocxFile template target xmlUpdates internalFiles
   where
      ((docBody, internalFiles), result) = flip runState docStart $ do
            addImages imageMap
            b <- concat <$> mapM (blockToDocx 0 Nothing) blocks
            fs <- forM (M.toList imageMap) $ \(ref, (png, svg)) -> do
               f1 <- getMediaRef ref PngTarget >>= \case
                  Just rel -> return [("word/" <> relTarget rel, png)]
                     -- Zip file, so deliberately not using System.Filepath.</>
                  Nothing -> return []  -- Cannot happen.
               f2 <- case svg of
                  Nothing -> return []
                  Just bytes -> getMediaRef ref SvgTarget >>= \case
                     Just rel -> return [("word/" <> relTarget rel, bytes)]
                     Nothing -> return []
               return $ f1 ++ f2
            return (b, concat fs)
      xrefs = result ^. externalRelationships ++
            concat (M.elems (result ^. internalRelationships))
      rels = map (X.Elem . relationshipXml) xrefs
      addRels :: [X.Content] -> XC.Cursor -> XC.Cursor
      addRels r = X.currentL . X._Elem . X.elContentL %~ (r ++)
      styleNames = nub  $ dsmStyleNames ++ query (maybeToList . attrClass) blocks
      addMissingContentType :: String -> String -> XC.Cursor -> XC.Cursor
      addMissingContentType extension mime c1 =
         case XC.findRec
               ((Just "Types" ==) . preview (X.currentL . X._Elem . X.elNameL . X.qNameL))
               c1 of
            Nothing -> c1  -- Template document is seriously broken.
            Just c2 -> case XC.findChild (isContentType extension . XC.current) c2 of
               Just _ -> c1   -- Content type already listed. Do nothing.
               Nothing ->
                  X.currentL . X._Elem . X.elContentL %~ cons (X.Elem (X.unode "Default" [
                        X.Attr (X.unqual "Extension") extension,
                        X.Attr (X.unqual "ContentType") mime
                     ])) $ c2
      isContentType ext (X.Elem e) =
         X.qName (X.elName e) == "Default" &&
         (X.attrVal <$> find (("Extension" ==) . X.qName . X.attrKey) (X.elAttribs e)) == Just ext
      isContentType _ _ = False


-- | The XML namespaces that must appear in the @w.document@ node.
requiredNamespaces :: Map String String
requiredNamespaces = M.fromList [
      ("wpc", "http://schemas.microsoft.com/office/word/2010/wordprocessingCanvas"),
      ("cx", "http://schemas.microsoft.com/office/drawing/2014/chartex"),
      ("cx1", "http://schemas.microsoft.com/office/drawing/2015/9/8/chartex"),
      ("cx2", "http://schemas.microsoft.com/office/drawing/2015/10/21/chartex"),
      ("cx3", "http://schemas.microsoft.com/office/drawing/2016/5/9/chartex"),
      ("cx4", "http://schemas.microsoft.com/office/drawing/2016/5/10/chartex"),
      ("cx5", "http://schemas.microsoft.com/office/drawing/2016/5/11/chartex"),
      ("cx6", "http://schemas.microsoft.com/office/drawing/2016/5/12/chartex"),
      ("cx7", "http://schemas.microsoft.com/office/drawing/2016/5/13/chartex"),
      ("cx8", "http://schemas.microsoft.com/office/drawing/2016/5/14/chartex"),
      ("mc", "http://schemas.openxmlformats.org/markup-compatibility/2006"),
      ("aink", "http://schemas.microsoft.com/office/drawing/2016/ink"),
      ("am3d", "http://schemas.microsoft.com/office/drawing/2017/model3d"),
      ("o", "urn:schemas-microsoft-com:office:office"),
      ("r", "http://schemas.openxmlformats.org/officeDocument/2006/relationships" ),
      ("m", "http://schemas.openxmlformats.org/officeDocument/2006/math"),
      ("v", "urn:schemas-microsoft-com:vml"),
      ("wp14", "http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing"),
      ("wp", "http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"),
      ("w10", "urn:schemas-microsoft-com:office:word"),
      ("w", "http://schemas.openxmlformats.org/wordprocessingml/2006/main"),
      ("w14", "http://schemas.microsoft.com/office/word/2010/wordml"),
      ("w15", "http://schemas.microsoft.com/office/word/2012/wordml"),
      ("w16cid", "http://schemas.microsoft.com/office/word/2016/wordml/cid"),
      ("w16se", "http://schemas.microsoft.com/office/word/2015/wordml/symex"),
      ("wpg", "http://schemas.microsoft.com/office/word/2010/wordprocessingGroup"),
      ("wpi", "http://schemas.microsoft.com/office/word/2010/wordprocessingInk"),
      ("wne", "http://schemas.microsoft.com/office/word/2006/wordml"),
      ("wps", "http://schemas.microsoft.com/office/word/2010/wordprocessingShape")
   ]


-- | The set of XML namespaces that can be listed as ignorable in the @w:document@ node.
ignorableNamespaces :: Set String
ignorableNamespaces = S.fromList ["w14", "w15", "w16se", "w16cid", "wp14"]


-- | Insert any missing namespaces into the @w:document@ node.
modifyDocumentAttrs :: XC.Cursor -> Maybe XC.Cursor
modifyDocumentAttrs = XC.root >>> XC.findRec (isDocument . XC.current) >>> fmap addRequired
   where
      isDocument = isNamedElem "document"
      isMcIgnorable x = X.qName x == "Ignorable" && X.qPrefix x == Just "mc"
      addRequired = XC.modifyContent $ \case
         X.Elem e -> let
               (oldNs, rest1) =
                  partition ((Just "xmlns" ==) . X.qPrefix . X.attrKey) $ X.elAttribs e
               oldNsMap =
                  M.fromList $ map (\(X.Attr k v) -> (X.qName k, v)) oldNs
               (oldIgnore, rest2) =
                  partition (isMcIgnorable . X.attrKey) rest1
               oldIgnoreSet = mconcat $ map (S.fromList . words . X.attrVal) oldIgnore
               newNs = map (\(n, v) -> X.Attr (X.QName n Nothing $ Just "xmlns") v) $
                  M.toList $ oldNsMap <> requiredNamespaces
               newIgnore = X.Attr {
                     X.attrKey = X.QName "Ignorable" Nothing (Just "mc"),
                     X.attrVal = unwords $ S.toList $ S.union oldIgnoreSet ignorableNamespaces
                  }
            in X.Elem $ e {X.elAttribs = newNs ++ newIgnore : rest2}
         v -> v


-- | Append the elements to the word document. The new elements are added at the end of the @w:body@
-- node, except that if there is a final @w:sectPr@ node then that will be preserved as the final
-- node in the @w:body@.
appendBodyElements :: [X.Element] -> XC.Cursor -> Maybe XC.Cursor
appendBodyElements newElements =
      XC.root >>> XC.findRec (isBody . XC.current) >>> fmap (XC.modifyContent go)
   where
      isBody = isNamedElem "body"
      isSectPr = isNamedElem "sectPr"
      go (X.Elem body) =
            X.Elem $ body {X.elContent = mainContents <> map X.Elem newElements <> sect}
         where
            oldContents = X.elContent body
            (mainContents, sect) = case oldContents ^? _Snoc of
               Nothing -> (oldContents, [])
               Just (m, s) -> if isSectPr s then (m, [s]) else (oldContents, [])
      go v = v


-- | True if the content has the @qName@ given and the namespace prefix "w".
isNamedElem :: String -> X.Content -> Bool
isNamedElem nm (X.Elem e) = X.qName (X.elName e) == nm && X.qPrefix (X.elName e) == Just "w"
isNamedElem _ _ = False


-- | Create a new Word file using the built-in template.
makeDocx ::
   Maybe ReportPageLayout
   -> Text   -- ^ Document title.
   -> FilePath   -- ^ File to store document in.
   -> [Block]    -- ^ Document content.
   -> Map ImageRef (BL.ByteString, Maybe BL.ByteString)  -- ^ Files to include.
   -> IO ()
makeDocx fmt title target blocks imageMap = do
      let
         ((docBody, internalFiles), result) = flip runState docStart $ do
               addImages imageMap
               b <- concat <$> mapM (blockToDocx 0 Nothing) blocks
               fs <- forM (M.toList imageMap) $ \(ref, (png, svg)) -> do
                  f1 <- getMediaRef ref PngTarget >>= \case
                     Just rel -> return [("word/" <> relTarget rel, png)]
                        -- Zip file, so deliberately not using System.Filepath.</>
                     Nothing -> return []  -- Cannot happen.
                  f2 <- case svg of
                     Nothing -> return []
                     Just bytes -> getMediaRef ref SvgTarget >>= \case
                        Just rel -> return [("word/" <> relTarget rel, bytes)]
                        Nothing -> return []
                  return $ f1 ++ f2
               return (b, concat fs)
         document = wordDocType $ wBody $ docBody ++ pageProperty
         xrefs = result ^. externalRelationships ++
               concat (M.elems (result ^. internalRelationships))
         rels = map (X.Elem . relationshipXml) xrefs
         xmlUpdates = [("word/_rels/document.xml.rels",
               \c -> maybe c (addRels rels) $ findNameDF (X.unqual "Relationships") c)]
         binaryDoc = ("word/document.xml", encodeUtf8 $ LT.pack $ wordShowTopElement document)
      writeDocxFile title target xmlUpdates $ binaryDoc : internalFiles
   where
      addRels :: [X.Content] -> XC.Cursor -> XC.Cursor
      addRels xrefs = X.currentL . X._Elem . X.elContentL %~ (xrefs ++)
      -- Similar to X.showTopElement, but with the attributes that Word likes to see.
      wordShowTopElement :: X.Element -> String
      wordShowTopElement c = xml_header ++ X.showElement c
      xml_header = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
      pageProperty = case fmt of
         Nothing -> []
         Just p -> [
               sectPr $ case p of
                  ReportPageLayout Letter Landscape ->
                     pgSz (Just $ 11 * 72) (Just $ 8.5*72) (Just True)
                  ReportPageLayout Letter Portrait ->
                     pgSz (Just $ 8.5*72) (Just $ 11 * 72) (Just False)
                  ReportPageLayout A4 Landscape ->
                     pgSz (Just 841.89) (Just 595.276) (Just True)
                  ReportPageLayout A4 Portrait ->
                     pgSz (Just 595.276) (Just 841.89) (Just False)
                  ReportPageLayout A3 Landscape ->
                     pgSz (Just 1190.55) (Just 841.89) (Just True)
                  ReportPageLayout A3 Portrait ->
                     pgSz (Just 841.89) (Just 1190.55) (Just False)
            ]


{- Design note:

Zip files use "/" as a separator, so on Windows System.FilePath.</> does the wrong thing for paths
in zip files. The zip-archive library normalises paths with "\", but it is possible that in future
this program will extend existing template Word files rather than creating its own, which would
break any code that assumed the use of "\". Hence all zip file paths in Hades use the "/" at all
times.
-}


-- | Add these images to the document state.
addImages :: Map ImageRef (a, Maybe a) -> DocMonad ()
addImages map1 =
   forM_ (M.toList map1) $ \(ref, (_, svg)) -> do
      addMedia ref ecmaImage PngTarget
      case svg of
         Just _ -> addMedia ref ecmaImage SvgTarget
         Nothing -> return ()


-- | Generate a fragment of WordProcessingML for the block. The text argument is an optional
-- style name for the paragraphs in the block.
blockToDocx ::
   Int             -- ^ Indented list level of current block.
   -> Maybe Text   -- ^ Optional style name for paragraphs in this block.
   -> Block
   -> DocMonad [X.Element]
-- PLain
blockToDocx l _ (Plain inlines) =
   return . wPara . ((pPr $ pStyle (Just "Compact") ++ pIndent l False) :) <$>
      inlinesToDocx [] inlines
      -- Note: return :: a -> [a]
-- Para
blockToDocx 0 Nothing (Para inlines) =
   return . wPara <$> inlinesToDocx [] inlines
blockToDocx l style (Para inlines) =
   return . wPara . ((pPr $ pStyle style ++ pIndent l False) :) <$> inlinesToDocx [] inlines
-- Heading
blockToDocx _ _ (Heading level attr inlines) = do
   txt <- inlinesToDocx [] inlines >>= attrBookmark attr
   let style = Just $ "Heading" <> T.pack (show level)
   return [wPara $ pPr (pStyle style) : txt]
-- OrderedList
blockToDocx _ _ (OrderedList _ []) = return []
blockToDocx l style (OrderedList _ (blocks : blockss)) = do
   firstItem <- listItem NumList (l+1) style True blocks
   restItems <- mapM (listItem NumList (l+1) style False) blockss
   return $ firstItem ++ concat restItems
-- UnorderedList
blockToDocx l style (UnorderedList blockss) =
   concat <$> mapM (listItem BulletList (l+1) style False) blockss
-- DefinitionList
blockToDocx l style (DefinitionList items) = do
   rs <-forM items $ \(term, paras) -> do
      termPara <- wPara . ((pPr $ pStyle style ++ pIndent l False) :) <$>
            inlineToDocx [] (Strong term)
      defParas <- mapM (blockToDocx (l+1) style) paras
      return $ termPara : concat defParas
   return $ concat rs
-- Matrix
blockToDocx _ _ (Matrix attr caption headers cells) =
   matrixToDocx attr caption headers cells
-- Div
blockToDocx l _ (Div attr blks) = do
   paras <- concat <$> mapM (blockToDocx l $ attrClass attr) blks
   attrBookmark attr paras
-- Picture
blockToDocx _ _ (Picture attr caption altText size modelId) = do
   drawing <- inlinePicture size (DiagramRef modelId) altText
   c <- inlinesToDocx [] caption
   let
      picPara = wPara [pPr $ pStyle $ Just "DiagramBox", wRun drawing]
      captionPara = wPara $ [
            pPr $ pStyle $ Just "Caption",
            wRun $ wText "Figure ",
            wField " SEQ Figure \\* ARABIC ",
            wRun $ wText ": "] ++ c
   attrBookmark attr [picPara, captionPara]
-- Null
blockToDocx _ _ Null = return []


-- | Internal type for numbered or bulleted lists.
data ListType = NumList | BulletList


-- | Numbered item. First paragraph is outdented with the number, the rest are just
-- indented.
listItem ::
   ListType
   -> Int      -- ^ Indentation level of this item.
   -> Maybe Text  -- ^ Optional paragraph style.
   -> Bool  -- ^ True if this is the first item in the list, so restart numbering.
   -> [Block]
   -> DocMonad [X.Element]
listItem typ level style isFirst blocks =
   case blocks of
      [] ->
         return [wPara [pPr $ pStyle style ++ pIndent level True, wRun $ numField isFirst]]
      (firstBlock: restBlocks) -> do
         firstPara <- case firstBlock of
            (Plain inlines) -> numPara (Just "Compact") <$> inlinesToDocx [] inlines
            (Para inlines) -> numPara style <$> inlinesToDocx [] inlines
            _ -> do
               paraBody <- blockToDocx level style firstBlock
               return $ numPara style [] ++ paraBody  -- Separate paragraph for number.
         restParas <- mapM (blockToDocx level style) restBlocks
         return $ firstPara ++ concat restParas
   where
      numPara :: Maybe Text -> [X.Element] -> [X.Element]
      numPara style1 paraBody = [wPara $
            pPr (pStyle style1 ++ pIndent level True) :
            wRun [numField True, wTab] :
            paraBody
         ]
      numField restart = case typ of
         NumList -> wField $
            "LISTNUM NumberValue \\l " <> tShow level <> (if restart then " \\s 1" else "")
         BulletList ->
            wText $ T.singleton $ concat (repeat "•◦⁃‣▪") !! max 0 (level-1)


-- | Wrap the elements in a bookmark if the Attr has an identifier.
attrBookmark :: Attr -> [X.Element] -> DocMonad [X.Element]
attrBookmark attr = case attrIdentifier attr of
   Nothing -> return  -- Do nothing.
   Just (ident, _) -> bookmark (T.pack $ show ident)


-- | Convert a list of inlines into DocX XML markup.
inlinesToDocx ::
   [X.Element]   -- ^ Properties to apply to the text.
   -> [Inline]   -- ^ Text.
   -> DocMonad [X.Element]
inlinesToDocx _ [] = return [wRun $ wText ""]
inlinesToDocx props inlines = concat <$> mapM (inlineToDocx props) inlines


-- Convert an Inline to DocX XML markup. (Icons not implemented yet)
inlineToDocx ::
   [X.Element]   -- ^ Properties to apply to the text.
   -> Inline
   -> DocMonad [X.Element]
inlineToDocx [] (Str txt) = return [wRun $ wText txt]
inlineToDocx props (Str txt) = return [wRun [rPr props, wText txt]]
inlineToDocx props (Emph inlines) = inlinesToDocx (X.node (wName "i") () : props) inlines
inlineToDocx props (Strong inlines) = inlinesToDocx (X.node (wName "b") () : props) inlines
inlineToDocx [] Space = return [wRun $ wText " "]
inlineToDocx props Space = return [wRun [rPr props, wText " "]]
inlineToDocx props (Link linkId inlines) =
   return . linkInternal (T.pack $ show linkId) <$> inlinesToDocx props inlines
inlineToDocx props (LinkOut url inlines) = do
   txt <- inlinesToDocx props inlines
   r <- linkExternal url txt
   return [r]
inlineToDocx props (Icon nm) = do
   r <- inlinePicture iconSize (IconRef nm) "Icon"
   return $ if null props then [wRun r] else [wRun [rPr props, r]]
inlineToDocx props (Highlight clr inlines) = do
   let hex = map C.toUpper $ drop 1 $ C.sRGB24show $ getColour clr
   inlinesToDocx
      (X.node (wName "shd") [wAttr "fill" $ T.pack hex, wAttr "val" "clear"] : props)
      inlines
inlineToDocx _ (Span attr inlines) = do
   r <- attrRun attr inlines
   return [r]


-- | The first class in the attribute is the run style. Other classes are ignored.
-- The elements are also bracketed by a bookmark if there is an identifier.
attrRun :: Attr -> [Inline] -> DocMonad X.Element
attrRun attr inlines = do
   let   -- Take the first class as a style name. Ignore the rest.
      style = maybeToList $ rStyle <$> attrClass attr
   txt <- case attrIdentifier attr of
      Nothing -> inlinesToDocx [] inlines
      Just (ident, _) -> inlinesToDocx [] inlines >>= bookmark (T.pack $ show ident)
   return $ wRun $ style ++ txt


-- | Size of icons within the document, in points.
iconSize :: (Double, Double)
iconSize = (12, 12)


-- | Translate a matrix into a table in the Word document.
matrixToDocx :: Attr -> [Inline] -> [MatrixHeader] -> Forest [MatrixCell] -> DocMonad [X.Element]
matrixToDocx attr caption headers cells = do
      h <- headerRows
      d <- map wTr <$> dataRows cells
      c <- xmlCaption
      return [wTbl styleId $ h ++ d, c]
   where
      styleId :: Text
      styleId = fromMaybe "Matrix" $ attrClass attr
      xmlCaption :: DocMonad X.Element
      xmlCaption = do
         c <- inlinesToDocx [] caption
         return $ wPara $ [pPr $ pStyle $ Just "Caption",
               wRun $ wText "Table ",
               wField " SEQ Table \\* ARABIC ",
               wRun $ wText ": "] ++ c
      headerRows :: DocMonad [X.Element]
      headerRows = case headers of
         [] -> return []
         [grp] -> return . wTr . (wTrPr wTblHeader :) . snd <$> groupHeader grp
         _ -> do
             (r1, r2) <- complexHeader
             return [wTr $ wTrPr wTblHeader : r1, wTr $ wTrPr wTblHeader : r2]
      groupHeader :: MatrixHeader -> DocMonad ([X.Element], [X.Element])
      groupHeader (MatrixHeader nm cols) = do
         nameRuns <- inlinesToDocx headerStyle nm
         colRuns <- forM cols $ \c -> do
            r <- inlinesToDocx headerStyle c
            return $ wTc [wTcPr [autoWidth, wVAlign "center"], wPara $ pPr (pJc "center") : r]
         let
            r1 = [wTc [
                  wTcPr [autoWidth, wGridSpan $ length cols, wVAlign "center"],
                  wPara  $ pPr (pJc "center") : nameRuns
               ]]
         return (r1, colRuns)
      firstGroupHeader :: MatrixHeader -> DocMonad ([X.Element], [X.Element])
      firstGroupHeader (MatrixHeader _ cols) = do
         colRuns <- forM cols $ \c -> do
            rs <- inlinesToDocx headerStyle c
            return $ wTc [
                  wTcPr [autoWidth, wVMerge True, wVAlign "center"],
                  wPara $ pPr (pJc "center") : rs
               ]
         return (colRuns, blanks cols)
      complexHeader :: DocMonad ([X.Element], [X.Element])
      complexHeader = do -- Only used when headers is not empty, so "head" and ":" are safe.
         let h1 : hs = headers
         grp1 <- firstGroupHeader h1
         grps <- mapM groupHeader hs
         return $ grp1 <> mconcat grps
      headerStyle = [X.node (wName "b") ()]
      dataRows :: Forest [MatrixCell] -> DocMonad [[X.Element]]
      dataRows trees = concat <$> mapM dataRow trees
      dataRow :: Tree [MatrixCell] -> DocMonad [[X.Element]]
      dataRow (Node cells1 childs) =
         dataRows childs >>= \case
            [] -> do
               thisRow <- mapM (wordCell False) cells1
               return [thisRow]
            [row1] -> do
               thisRow <- mapM (wordCell False) cells1
               return [thisRow ++ row1]
            (row1 : rows) -> do
               thisRow <- mapM (wordCell True) cells1
               return $ (thisRow ++ row1) : map (blanks cells1 ++) rows
      wordCell :: Bool -> MatrixCell -> DocMonad X.Element
      wordCell mergeFlag (attr1, blks) = do   -- True for merge.
         let style = attrClass attr
         content <- mapM (blockToDocx 0 style) blks >>= attrBookmark attr1 . concat
         let
            content1 = if null content then [wPara ()] else content
            properties = if mergeFlag
               then [autoWidth, wVMerge True, wVAlign "center"]
               else [autoWidth, wVAlign "center"]
         return $ wTc $ wTcPr properties : content1
      blanks :: [a] -> [X.Element]  -- Blank cells to merge with argument list above.
      blanks = map $ const $ wTc [wTcPr [autoWidth, wVMerge False], wPara ()]
      autoWidth = X.node (wName "tcW") [wAttr "w" "0", wAttr "type" "auto"]
