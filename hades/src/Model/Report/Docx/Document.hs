{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

{- |

Word OOXML combinators
-}

module Model.Report.Docx.Document (
   -- * Document Monad
   RelTargetType (..),
   Relationship (..),
   relationshipXml,
   EcmaType,
   ecmaImage,
   DocState (DocState),
   idCount,
   internalRelationships,
   externalRelationships,
   docStart,
   DocMonad,
   newId,
   -- * Mung other XML files
   getStyleById,
   addMissingStyle,
   dsmStyleNames,
   addExternal,
   addMedia,
   getMediaRef,
   tShow,
   -- * Word Namespace
   qual,
   qNode,
   wAttr,
   wName,
   nameSpace,
   -- * Basic Combinators
   wordDocType,
   keepNext,
   wBody,
   sectPr,
   pgSz,
   wPara,
   pPr,
   pStyle,
   pIndent,
   pJc,
   wRun,
   rPr,
   rStyle,
   wText,
   wTab,
   -- * Links and References
   wField,
   bookmark,
   linkInternal,
   linkExternal,
   -- * Tables
   wTbl,
   wTr,
   wTrPr,
   wTblHeader,
   wTc,
   wTcPr,
   wGridSpan,
   wVAlign,
   wVMerge,
   -- * Drawing
   wDrawing
) where

import Control.Lens hiding ((<.>))
import Control.Monad.State
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Model.Report.Base
import Model.Report.Docx.Utils
import System.FilePath
import qualified Text.XML.Light as X
import qualified Text.XML.Light.Cursor as XC


-- | Types of target for a relationship.
data RelTargetType = PngTarget | SvgTarget | ExternalTarget
   deriving (Eq, Ord, Show)


-- | A relationship with some resource.
--
-- If "relExternal" is false then the "relTarget" must be within the Word document package.
data Relationship = Relationship {
      relId :: Text,
      relExternal :: Bool,
      relType :: Text,   -- ^ The URI of the type as defined by ECMA-376.
      relTargetType :: RelTargetType,
      relTarget :: String    -- ^ Either the internal filepath or the external URL.
   } deriving (Eq, Ord, Show)


-- | An XML version of the relationship suitable for placing in an OOXML Relationships file.
relationshipXml :: Relationship -> X.Element
relationshipXml r = X.unode "Relationship" attrs
   where
      attrs =
         if relExternal r
            then X.Attr (X.unqual "TargetMode") "External" : attrs1
            else attrs1
      attrs1 = [
            X.Attr (X.unqual "Id") $ T.unpack $ relId r,
            X.Attr (X.unqual "Type") $ T.unpack $ relType r,
            X.Attr (X.unqual "Target") $ relTarget r
         ]

-- | The document generator state.
data DocState = DocState {
   _idCount :: Int,
   _internalRelationships :: Map ImageRef [Relationship],
   _externalRelationships :: [Relationship]
}


idCount :: Lens' DocState Int
idCount = lens _idCount $ \s n -> s{_idCount = n}


-- | Map of image resources to document relationships. An image may have multiple resources,
-- such as an SVG file with a PNG fallback.
internalRelationships :: Lens' DocState (Map ImageRef [Relationship])
internalRelationships = lens _internalRelationships $ \s rs -> s{_internalRelationships = rs}


-- | A convenient lens for accessing the relationships, if any, associated with an ImageRef.
internalRelationshipsAt :: ImageRef -> Lens' DocState [Relationship]
internalRelationshipsAt r = internalRelationships . at r . non []


-- | List of external document relationships.
externalRelationships :: Lens' DocState [Relationship]
externalRelationships = lens _externalRelationships $ \s rs -> s{_externalRelationships = rs}


-- | The initial state for the document monad.
docStart :: DocState
docStart = DocState 0 mempty []


-- | Document generation requires the tracking of state.
type DocMonad = State DocState


-- | The type of a related file, as defined by ECMA-376 Part 1.
type EcmaType = Text


-- | Type for images.
ecmaImage :: EcmaType
ecmaImage = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/image"


-- | Type for external resources.
ecmaExternal :: EcmaType
ecmaExternal = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink"


-- | Get a new decimal identifier which is unique in this document.
newId :: DocMonad Text
newId = do
   n <- use idCount
   idCount += 1
   return $ T.pack $ show n


-- | Add a relationship to an internal media file.
addMedia :: ImageRef -> EcmaType -> RelTargetType -> DocMonad ()
addMedia imgRef typ targetType = do
   let suffix = case targetType of {PngTarget -> "png"; SvgTarget -> "svg"; _ -> ""}
   rId <- ("dsmId" <>) <$> newId
   let rel = Relationship rId False typ targetType ("media/image-" <> T.unpack rId <.> suffix)
   internalRelationshipsAt imgRef %= (rel :)


-- | Get a reference to an internal media file, if one exists.
getMediaRef :: ImageRef -> RelTargetType -> DocMonad (Maybe Relationship)
getMediaRef imgRef targetType = do
   existing <- use $ internalRelationshipsAt imgRef
   return $ find ((targetType ==) . relTargetType) existing


-- | Add an external relationship to a URL and return it.
addExternal :: FilePath -> DocMonad Relationship
addExternal url = do
   rId <- ("dsmId" <>) <$> newId
   let rel = Relationship rId True ecmaExternal ExternalTarget url
   externalRelationships %= (rel :)
   return rel


-- | Search the style file for a style with the given @styleId@.
getStyleById :: Text -> [X.Content] -> Maybe X.Element
getStyleById style contents = do
   c1 <- XC.fromForest contents
   c2 <- XC.findRec (atNamedStyle style) c1
   case XC.current c2 of
      X.Elem e -> return e
      _ -> Nothing  -- Should never happen. However previous actions could fail.


-- | Add a style to the \"styles.xml\" file, if it is not already present. Returns @Nothing@
-- if the preconditions are not met.
addMissingStyle ::
   X.Element  -- ^ The style to add. Must be a @w:style@ node.
   -> XC.Cursor  -- ^ The style file to edit. Must contain a @w:styles@ node.
   -> Maybe XC.Cursor
addMissingStyle newStyle c = case styleId newStyle of
      Nothing -> Nothing
      Just n -> do  -- Maybe monad
         c1 <- XC.findRec atStyles $ goStart c
         return $ case XC.findChild (atNamedStyle n) c1 of
            Just _ -> c
            Nothing -> XC.modifyContent addStyle c1
   where
      atStyles (XC.current -> X.Elem e) = nameEq (wName "styles") $ X.elName e
      atStyles _ = False
      addStyle (X.Elem e) = X.Elem e {X.elContent = X.elContent e ++ [X.Elem newStyle]}
      addStyle x = x


-- | True if the cursor is at a style with the given styleId.
atNamedStyle :: Text -> XC.Cursor -> Bool
atNamedStyle nm (XC.current -> X.Elem el) =
      nameEq (wName "style") (X.elName el) && styleId el == Just nm
atNamedStyle _ _ = False


-- | The styleId attribute of the element, if any.
styleId :: X.Element -> Maybe Text
styleId = ((T.pack . X.attrVal) <$>) . find (nameEq (wName "styleId") . X.attrKey) . X.elAttribs


-- | Style names hard-coded into the DSM report generator
dsmStyleNames :: [Text]
dsmStyleNames = ["Caption", "Compact", "Matrix", "Hyperlink"]
   -- The user may have other paragraph styles, but "Matrix" is the only table style.


-- | "show" for text.
tShow :: (Show a) => a -> Text
tShow = T.pack . show



-- | Create a node with the qualified name.
qNode :: (X.Node t) => String -> String -> t -> X.Element
qNode ns nm = X.node $ qual ns nm


-- | Attribute with a qualified name for Word XML.
wAttr :: Text -> Text -> X.Attr
wAttr nm = X.Attr (wName nm) . T.unpack


-- | Qualfiied name for a Word XML node.
wName :: Text -> X.QName
wName n = qual "w" $ T.unpack n


-- | The top level declaration for a Document part.
wordDocType :: X.Element -> X.Element
wordDocType t = X.node (wName "document") (attr, t)
   where
      attr = [
            -- nameSpace "xsi" "http://www.w3.org/2001/XMLSchema-instance",
            nameSpace "wpc" "http://schemas.microsoft.com/office/word/2010/wordprocessingCanvas",
            nameSpace "cx" "http://schemas.microsoft.com/office/drawing/2014/chartex",
            nameSpace "cx1" "http://schemas.microsoft.com/office/drawing/2015/9/8/chartex",
            nameSpace "cx2" "http://schemas.microsoft.com/office/drawing/2015/10/21/chartex",
            nameSpace "cx3" "http://schemas.microsoft.com/office/drawing/2016/5/9/chartex",
            nameSpace "cx4" "http://schemas.microsoft.com/office/drawing/2016/5/10/chartex",
            nameSpace "cx5" "http://schemas.microsoft.com/office/drawing/2016/5/11/chartex",
            nameSpace "cx6" "http://schemas.microsoft.com/office/drawing/2016/5/12/chartex",
            nameSpace "cx7" "http://schemas.microsoft.com/office/drawing/2016/5/13/chartex",
            nameSpace "cx8" "http://schemas.microsoft.com/office/drawing/2016/5/14/chartex",
            nameSpace "mc" "http://schemas.openxmlformats.org/markup-compatibility/2006",
            nameSpace "aink" "http://schemas.microsoft.com/office/drawing/2016/ink",
            nameSpace "am3d" "http://schemas.microsoft.com/office/drawing/2017/model3d",
            nameSpace "o" "urn:schemas-microsoft-com:office:office",
            nameSpace "r" "http://schemas.openxmlformats.org/officeDocument/2006/relationships",
            nameSpace "m" "http://schemas.openxmlformats.org/officeDocument/2006/math",
            nameSpace "v" "urn:schemas-microsoft-com:vml",
            nameSpace "wp14" "http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing",
            nameSpace "wp" "http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing",
            nameSpace "w10" "urn:schemas-microsoft-com:office:word",
            nameSpace "w" "http://schemas.openxmlformats.org/wordprocessingml/2006/main",
            nameSpace "w14" "http://schemas.microsoft.com/office/word/2010/wordml",
            nameSpace "w15" "http://schemas.microsoft.com/office/word/2012/wordml",
            nameSpace "w16cid" "http://schemas.microsoft.com/office/word/2016/wordml/cid",
            nameSpace "w16se" "http://schemas.microsoft.com/office/word/2015/wordml/symex",
            nameSpace "wpg" "http://schemas.microsoft.com/office/word/2010/wordprocessingGroup",
            nameSpace "wpi" "http://schemas.microsoft.com/office/word/2010/wordprocessingInk",
            nameSpace "wne" "http://schemas.microsoft.com/office/word/2006/wordml",
            nameSpace "wps" "http://schemas.microsoft.com/office/word/2010/wordprocessingShape",
            X.Attr (qual "mc" "Ignorable") "w14 w15 w16se w16cid wp14"
            {- , X.Attr
                  (qual "xsi" "schemaLocation")
                  "http://purl.oclc.org/ooxml/wordprocessingml/main \
                     \ /home/paul/packages/ooxml/wml.xsd\n\
                  \http://purl.oclc.org/ooxml/drawingml/main \
                     \ /home/paul/packages/ooxml/dml-main.xsd"  -}
         ]
      -- The schemaLocation attribute and xsi namesapce are for debugging purposes.

-- | XML namespace declaration attribute.
nameSpace :: String -> String -> X.Attr
nameSpace str = X.Attr $ qual "xmlns" str


-- | Property to keep this paragraph with the next one.
keepNext :: X.Element
keepNext = X.node (wName "keepNext") ()


-- | Document main body.
wBody :: (X.Node t) => t -> X.Element
wBody = X.node (wName "body")


-- | Section properties.
sectPr :: (X.Node t) => t -> X.Element
sectPr = X.node (wName "sectPr")

-- | Page size. Width, height, orientation. Measurements are in points.
-- Orientation is True for landscape, False for portrait.
pgSz :: Maybe Double -> Maybe Double -> Maybe Bool -> X.Element
pgSz w h o = X.node (wName "pgSz") $ catMaybes [w1, h1, o1]
   where
      w1 = wAttr "w" . T.pack . show . r . (* 20) <$> w
      h1 = wAttr "h" . T.pack . show . r . (* 20) <$> h
      o1 = wAttr "orient" . (\b -> if b then "landscape" else "portrait") <$> o
      r :: Double -> Integer
      r = round


-- | Document paragraph.
wPara :: (X.Node t) => t -> X.Element
wPara = X.node (wName "p")


-- | Paragraph properties.
pPr :: (X.Node t) => t -> X.Element
pPr = X.node (wName "pPr")


-- | Paragraph style property.
pStyle :: Maybe Text -> [X.Element]
pStyle (Just txt) = [X.node (wName "pStyle") $ wAttr "val" txt]
pStyle Nothing = []


-- | Indentation for this paragraph. @pIndent 0 False@ returns an empty list.
pIndent ::
   Int   -- ^ List nesting level. Text is indented 0.5 inches per level.
   -> Bool  -- ^ If true, outdent the first line by one level.
   -> [X.Element]
pIndent 0 False = []
pIndent level outdent = [X.node (wName "ind") $ wIn ++ wOut]
   where
      wIn = [wAttr "start" $ tShow $ level * indentIncrement]
      wOut = [wAttr "hanging" $ T.pack $ show indentIncrement | outdent]
      indentIncrement = 720  -- 0.5 inches in 20ths of a point.


-- | Paragraph justification. Legal values include "start", "center", "end" and "both".
pJc :: Text -> X.Element
pJc txt = X.node (wName "jc") $ wAttr "val" txt

-- | Run of data with uniform formatting.
wRun :: (X.Node t) => t -> X.Element
wRun = X.node (wName "r")

-- | Run properties
rPr :: (X.Node t) => t -> X.Element
rPr = X.node (wName "rPr")


-- | Run style property.
rStyle :: Text -> X.Element
rStyle txt = X.node (wName "rStyle") $ wAttr "val" txt


-- | Plain text within a run.
wText :: Text -> X.Element
wText txt =
   if T.take 1 txt == " " || T.takeEnd 1 txt == " "
      then X.node
            (wName "t")
            (X.Attr (qual "xml" "space") "preserve", T.unpack txt)
      else X.node (wName "t") $ T.unpack txt


-- | Tab character within a run.
wTab :: X.Element
wTab = X.node (wName "tab") ()


-- | Tag a section of text with a bookmark.
bookmark :: Text -> [X.Element] -> DocMonad [X.Element]
bookmark nm es = do
      ident <- newId
      return $ bookmarkStart ident : es ++ [bookmarkEnd ident]
   where
      bookmarkStart ident = X.node (wName "bookmarkStart")
            [wAttr "id" ident, wAttr "name" nm]
      bookmarkEnd ident = X.node (wName "bookmarkEnd") $ wAttr "id" ident


-- | Hyperlink to a bookmark elsewhere in the document. First argument is bookmark name.
linkInternal :: Text -> [X.Element] -> X.Element
linkInternal nm es = X.node (wName "hyperlink") ([wAttr "anchor" nm], es)


-- | Hyperlink to a remote URL. First argument is the target URL.
linkExternal :: Text -> [X.Element] -> DocMonad X.Element
linkExternal url es = do
      rId <- addExternal $ T.unpack url
      return $ X.node
            (wName "hyperlink")
            ([X.Attr (qual "r" "id") (T.unpack $ relId rId)], hyperlinkStyle : es)
   where
      hyperlinkStyle = rPr $ rStyle "Hyperlink"


-- | Insert a simple field code into the document. The field is flagged as \"dirty\" so that
-- it will be automatically recalculated when the document is loaded.
wField :: Text -> X.Element
wField field = X.node (wName "fldSimple") [wAttr "instr" field, wAttr "dirty" "true"]


-- | Docx Table
wTbl ::
   Text -- ^ Table style name.
   -> [X.Element]  -- ^ Table rows.
   -> X.Element
wTbl style rows =
      X.node (wName "tbl") $ props : grid : rows
   where
      props = X.node (wName "tblPr") [
            X.node (wName "tblStyle") $ wAttr "val" style,
            X.node (wName "tblLayout") $ wAttr "type" "autofit"
         ]
      grid = X.node (wName "tblGrid") ()  -- No grid data defined.

-- | Table row.
wTr :: [X.Element] -> X.Element
wTr = X.node (wName "tr")


-- | Table row properties.
wTrPr :: (X.Node t) => t -> X.Element
wTrPr = X.node (wName "trPr")


-- | Table header row property.
wTblHeader :: X.Element
wTblHeader = X.node (wName "tblHeader") ()


-- | Table cell.
wTc :: (X.Node t) => t -> X.Element
wTc = X.node (wName "tc")

-- | Table cell properties.
wTcPr :: (X.Node t) => t -> X.Element
wTcPr = X.node (wName "tcPr")


-- | Horizontal cell merge table cell property. Argument is number of columns to span.
wGridSpan :: Int -> X.Element
wGridSpan n = X.node (wName "gridSpan") $ wAttr "val" $ T.pack $ show n


-- | Table cell vertical alignment property.
-- Legal arguments are "top", "bottom", "centre" or "both".
wVAlign :: Text -> X.Element
wVAlign txt = X.node (wName "vAlign") $ wAttr "val" txt


-- | Table cell is merged with others. Argument is True if this is the first cell in the merge
-- set, False if this is a continuation of merged cells.
wVMerge :: Bool -> X.Element
wVMerge b = if b
   then X.node (wName "vMerge") $ wAttr "val" "restart"
   else X.node (wName "vMerge") ()


-- | The content is a drawing. This is treated as a Run on its own.
wDrawing :: (X.Node t) => t -> X.Element
wDrawing = wRun . X.node (wName "drawing")
