{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

-- |

module Model.Report.Base (
   HasReports (..),
   MatrixId,
   ReportPaper (..),
   ReportOrientation (..),
   ReportPageLayout (ReportPageLayout),
   reportPagePaper,
   reportPageOrientation,
   reportPageDefault,
   Report (..),
   reportName,
   reportDescription,
   reportDefaultInput,
   reportDefaultTemplate,
   reportSpec,
   blankReport,
   ReportFragment (..),
   _ReportLiteral,
   _ReportField,
   _ReportFieldList,
   _ReportHeading,
   _ReportDiagram,
   _ReportMatrix,
   _ReportLookup,
   _SubReport,
   ImageRef (..),
   fragmentDescription,
   runReport
) where

import Control.Arrow
import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Monad
import Data.Aeson
import Data.Function (on)
import Data.List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree
import Hades.Abstract.Geometry
import Hades.Abstract.Delta
import Hades.GI.BasicShapes (HadesRender)
import Model.Abstract.DiagramType
import Model.Abstract.PackageTree as P
import Model.Lookups.Base
import Model.Matrices.Base
import Model.Query.Base
import Model.Query.Diagram
import Model.Reflection.Parser
import Model.Reflection.Reflective
import Model.Reflection.Types
import Model.Reflection.Values
import Model.Report.Document as D
import Model.Report.Walkable
import Reactive.Banana.Common (Colour)
import Text.Megaparsec as MP


-- | Entity types that can include reports.
class (HasMatrices v) => HasReports v where
   _Report :: Prism' v (Report v)


-- | Paper size for Word exports.
data ReportPaper = Letter | A3 | A4 deriving (Eq)

instance Show ReportPaper where
   show Letter = "US Letter"
   show A3 = "A3"
   show A4 = "A4"


-- | Paper orientation for Word exports.
data ReportOrientation = Portrait | Landscape deriving (Eq, Show)


-- | Page layout for Word exports.
data ReportPageLayout = ReportPageLayout {
      _reportPagePaper :: ReportPaper,
      _reportPageOrientation :: ReportOrientation
   } deriving (Eq)

instance Show ReportPageLayout where show (ReportPageLayout p o) = show p <> " " <> show o

reportPagePaper :: Lens' ReportPageLayout ReportPaper
reportPagePaper = lens _reportPagePaper $ \s p -> s {_reportPagePaper = p}

reportPageOrientation :: Lens' ReportPageLayout ReportOrientation
reportPageOrientation = lens _reportPageOrientation $ \s o -> s {_reportPageOrientation = o}


-- | A4 Landscape
reportPageDefault :: ReportPageLayout
reportPageDefault = ReportPageLayout A4 Landscape

-- | A report is a list of report fragments. The results are concatenated.
data Report v = Report {
      _reportName :: Name,
      _reportDescription :: Text,
      _reportDefaultInput :: Maybe ModelId,
      _reportDefaultTemplate :: Maybe FilePath,
      _reportSpec :: [ReportFragment v]
   } deriving (Eq)

instance ToJSON (Report v) where
   toJSON (Report nm desc input template spec) = object [
         "type" .= ("report" :: Text),
         "name" .= nm,
         "description" .= desc,
         "input" .= input,
         "template" .= template,
         "spec" .= spec
      ]

instance FromJSON (Report v) where
   parseJSON = withObject "Report" $ \v -> do
      checkType v "report"
      Report <$>
         v .: "name" <*>
         v .:? "description" .!= "" <*>
         v .:? "input" .!= Nothing <*>
         v .:? "template" <*>
         v .: "spec"

instance Reflective (Report v) where
   reflectiveName _ = Variant "Report"
   reflectiveDefaults = [Report (Name "") "" Nothing Nothing []]
   reflectiveBuiltIn _ = [nameField, descriptionField]
   reflectiveGet rpt = M.fromList [
         (nameField, ExtText $ rpt ^. reportName . nameText),
         (descriptionField, ExtText $ rpt ^. reportDescription)
      ]
   reflectiveSet rpt = do
      nm <- Name . T.strip <$> extract (rpt ^. reportName . nameText) _ExtText nameField
      desc <- extract (rpt ^. reportDescription) _ExtText descriptionField
      return $ reportName .~ nm $ reportDescription .~ desc $ rpt
   reflectiveBuiltInRefs = mempty
   reflectiveArrows = mempty


reportName :: Lens' (Report v) Name
reportName = lens _reportName $ \r n -> r {_reportName = n}

reportDescription :: Lens' (Report v) Text
reportDescription = lens _reportDescription $ \r t -> r {_reportDescription = t}

reportDefaultInput :: Lens' (Report v) (Maybe ModelId)
reportDefaultInput = lens _reportDefaultInput $ \r x -> r {_reportDefaultInput = x}

reportDefaultTemplate :: Lens' (Report v) (Maybe FilePath)
reportDefaultTemplate = lens _reportDefaultTemplate $ \r p -> r {_reportDefaultTemplate = p}

reportSpec :: Lens' (Report v) [ReportFragment v]
reportSpec = lens _reportSpec $ \r s -> r {_reportSpec = s}


blankReport :: Report v
blankReport = Report (Name "") "" Nothing Nothing [ReportHeading 1 "" (Just (nameField, ""))]


-- | Part of a report. Where text is inserted a double newline marks a paragraph break.
data ReportFragment v =
      ReportLiteral Text  -- ^ Insert the text.
      | ReportField FieldId  -- ^ Insert the field value for the current entity.
      | ReportFieldList [FieldId] -- ^ Insert the fields as a definition list.
      | ReportHeading Int Text (Maybe (FieldId, Text)) -- ^ Insert a heading at the given level.
      | ReportDiagram  -- ^ If the current entity is a diagram then insert it with caption.
      | ReportMatrix (Maybe MatrixId)
            -- ^ Insert the given matrix with the current entity as input.
      | ReportLookup (Maybe ModelId)
            -- ^ Insert the given lookup table.
      | SubReport (Maybe QueryId) SortOrder [ReportFragment v]
            -- ^ Execute the given query with the current entity as input, and insert the report
            -- fragment once for each output of the query.
      deriving (Eq)

instance ToJSON (ReportFragment v) where
   toJSON (ReportLiteral txt) = object ["literal" .= txt]
   toJSON (ReportField fname) = object ["field" .= fname]
   toJSON (ReportFieldList fnames) = object ["field-list" .= fnames]
   toJSON (ReportHeading lvl t1 rest) = object ["level" .= lvl, "text1" .= t1, "rest" .= rest]
   toJSON ReportDiagram = object ["type" .= ("diagram" :: Text)]
   toJSON (ReportMatrix modelId) = object ["matrix" .= modelId]
   toJSON (ReportLookup modelId) = object ["lookup" .= modelId]
   toJSON (SubReport modelId sortOrder rprt) =
      object ["query" .= modelId, "sort" .= sortOrder, "subreport" .= rprt]

instance FromJSON (ReportFragment v) where
   parseJSON = withObject "Report fragment" $ \v ->
      (ReportLiteral <$> v .: "literal")
      <|> (ReportField . migrateField1 <$> v .: "field")
      <|> (ReportFieldList . map migrateField1 <$> v .: "field-list")
      <|> (ReportHeading <$>
            v .: "level" <*>
            v .: "text1" <*>
            ((_Just . _1 %~ migrateField1) <$> v .: "rest"))
      <|> (checkType v "diagram" >> return ReportDiagram)
      <|> (ReportMatrix <$> v .: "matrix")
      <|> (ReportLookup <$> v .: "lookup")
      <|> (SubReport <$>
            v .: "query" <*>
            v .:? "sort" .!= SortOrder [(nameField, Ascending)] <*>
            v .: "subreport")


_ReportLiteral :: Prism' (ReportFragment v) Text
_ReportLiteral = prism ReportLiteral $ \case {ReportLiteral txt -> Right txt; v -> Left v}

_ReportField :: Prism' (ReportFragment v) FieldId
_ReportField = prism ReportField $ \case {ReportField f -> Right f; v -> Left v}

_ReportFieldList :: Prism' (ReportFragment v) [FieldId]
_ReportFieldList = prism ReportFieldList $ \case {ReportFieldList fs -> Right fs; v -> Left v}

_ReportHeading :: Prism' (ReportFragment v) (Int, Text, Maybe (FieldId, Text))
_ReportHeading = prism (\(lvl, txt, rest) -> ReportHeading lvl txt rest) $ \case
      ReportHeading lvl txt rest -> Right (lvl, txt, rest)
      v -> Left v

_ReportDiagram :: Prism' (ReportFragment v) ()
_ReportDiagram = prism (const ReportDiagram) $ \case {ReportDiagram -> Right (); v -> Left v}

_ReportMatrix :: Prism' (ReportFragment v) (Maybe MatrixId)
_ReportMatrix = prism ReportMatrix $ \case {ReportMatrix mId -> Right mId; v -> Left v}

_ReportLookup :: Prism' (ReportFragment v) (Maybe ModelId)
_ReportLookup = prism ReportLookup $ \case {ReportLookup mId -> Right mId; v -> Left v}

_SubReport :: Prism' (ReportFragment v) (Maybe QueryId, SortOrder, [ReportFragment v])
_SubReport = prism
   (\(qId, o, rp) -> SubReport qId o rp)
   (\case {SubReport qId o rp -> Right (qId, o, rp); v -> Left v})


-- | One-line summary of a report fragment.
fragmentDescription :: (EntityClass v) => Int -> Model v -> ReportFragment v -> Text
fragmentDescription n model frag = ellipsize n $ case frag of
      ReportLiteral txt -> "\"" <> line1 txt <> "\""
      ReportField fid -> "<" <> fname fid <> ">"
      ReportFieldList fids -> "Fields: " <> T.intercalate ", " (map fname fids)
      ReportHeading lvl txt1 rest ->
         "Heading " <> T.pack (show lvl) <> ": " <> txt1 <> case rest of
            Nothing -> ""
            Just (fid, txt2) -> " <" <> fname fid <> "> " <> txt2
      ReportDiagram -> "Diagram"
      ReportMatrix matrixId -> "Matrix: " <> fragName matrixId
      ReportLookup tableId -> "Lookup Table: " <> fragName tableId
      SubReport queryId order sub ->
         "Section: " <> fragName queryId <> " ordered by " <> showSortOrder allFields order <>
         ".  " <> case sub of
            [] -> ""
            x:_ -> fragmentDescription n model x
   where
      line1 txt = case T.lines txt of {[] -> ""; (x:_) -> x}
      fragName Nothing =
         "<Not set>"
      fragName (Just e) =
         fromMaybe "<Deleted>" (modelContents model ^? ix e . entityName . nameText)
      ellipsize n1 txt = if T.length txt > n
         then T.take (n1-1) txt <> "…"  -- U+2026 ellipsis.
         else txt
      allFields = withStampFields $ withBuiltIn $ modelFields model
      fname fid = fromMaybe "-deleted-" $ allFields ^? ix fid . fieldName


-- | Reports can reference images. These images need to be tracked so that the right images
-- can be packaged up with the report.
data ImageRef =
   IconRef Text   -- ^ Reference to a named icon.
   | DiagramRef ModelId   -- ^ Reference to a diagram within the model.
   deriving (Eq, Ord, Show)



-- | Report results are assembled in Result Fragments. This consists of a list of PanDoc @Block@
-- elements followed by a list of @Inline@ elements. The latter may be prepended onto the first
-- paragraph of the next result, or they may be rendered as a separate paragraph. This allows a
-- report fragment to be created either as a separate paragraph or as the start of a paragraph.
data ResultFragment = ResultFragment {
      resultBlocks :: [Block],
      resultInlines :: [Inline],
      resultImages :: [ImageRef],
      resultErrors :: [(ModelId, Text)]
   }

instance Semigroup ResultFragment where
   frag1 <> frag2 =
      case resultBlocks frag2 of
         [] -> ResultFragment
            (resultBlocks frag1)
            (resultInlines frag1 ++ inlines2)
            (resultImages frag1 ++ resultImages frag2)
            (resultErrors frag1 ++ resultErrors frag2)
         blks2First : blks2Rest -> ResultFragment
            (resultBlocks frag1 ++ prefixBlock (resultInlines frag1) blks2First ++ blks2Rest)
            (resultInlines frag2)
            (resultImages frag1 ++ resultImages frag2)
            (resultErrors frag1 ++ resultErrors frag2)
      where
         inlines2 = let i = trimSpaces $ resultInlines frag2 in if null i then [] else Space : i

instance Monoid ResultFragment where
   mempty = ResultFragment [] [] [] []


-- | If there is an inline element at the end of the result, turn it into a separate paragraph
-- using the supplied function.
finaliseResult :: ResultFragment -> ResultFragment
finaliseResult v@(ResultFragment _ [] _ _) = v
finaliseResult (ResultFragment blocks inlines images errs) =
   ResultFragment (blocks ++ [Para $ trimSpaces inlines]) [] images errs


-- | Generate a document from a report. Returns a list of PanDoc blocks and a list of diagram
-- model IDs. These diagrams are referred to by their IDs in the PanDoc. Hence when the PanDoc
-- is converted into an export file each diagram must be written into the same folder with the
-- file name generated by appending \"@.svg@\" to the text version of the model ID.
--
-- Returns a triple of the PanDoc blocks, the images they reference, and error reports.
runReport :: (EntityClass v, HasDiagrams HadesRender v, HasLookups v, HasMatrices v) =>
   Report v -> ModelId -> ModelEdit v v ([Block], [ImageRef], [(ModelId, Text)])
runReport rprt modelId = do
   result <- mapM (runFragment modelId) (rprt ^. reportSpec)
   let (ResultFragment blks _ images errs) = finaliseResult $ mconcat result
   return (blks, images, errs)


-- | Generate a result fragment from a report fragment.
runFragment :: (EntityClass v, HasDiagrams HadesRender v, HasLookups v, HasMatrices v) =>
   ModelId -> ReportFragment v -> ModelEdit v v ResultFragment
runFragment modelId frag = do
      allFields <- withStampFields . withBuiltIn . modelFields <$> getModel
      let fname fid = fromMaybe "-deleted-" $ allFields ^? ix fid . fieldName
      goToEntity modelId
      vals <- getVariantData
      case frag of
         ReportLiteral txt -> return $ textToBlocks txt
         ReportField field -> snd <$> fieldFragment field (vals ^. extValue field)
         ReportFieldList fnames -> do
            items <- forM fnames $ \n -> do
               defItem <- case M.lookup n vals of
                     Nothing -> return mempty
                     Just v -> snd <$> fieldFragment n v
               return (n, finaliseResult defItem)
            return $ ResultFragment
                  [DefinitionList $ map (\(fid, defItem) ->
                        ([Str $ fname fid <> ": "], resultBlocks defItem)
                     ) items]
                  []
                  (concatMap (resultImages . snd) items)
                  []
         ReportHeading lvl txt Nothing ->
            return $ ResultFragment [Heading lvl (headAttrs lvl) [Str txt]] [] [] []
         ReportHeading lvl txt1 (Just (fid, txt2))->
            case M.lookup fid vals of
               Nothing -> return $ ResultFragment
                     [Heading lvl (headAttrs lvl) [Str $ txt1 <> " [?] " <> txt2]]
                     []
                     []
                     [(modelId, "Field " <> fname fid <> " not found in level " <>
                           T.pack (show lvl) <> " heading.")]
               Just v -> return $ ResultFragment
                     [Heading lvl (headAttrs lvl)
                           [Str $ T.strip $ txt1 <> " " <> displayValue v <> " " <> txt2]]
                     []
                     []
                     []
         ReportDiagram -> blockDiagram
         ReportMatrix Nothing -> return mempty
         ReportMatrix (Just matrixId) -> blockMatrix matrixId modelId
         ReportLookup Nothing -> return mempty
         ReportLookup (Just tableId) -> blockLookup tableId
         SubReport Nothing _ subReport -> mconcat <$> forM subReport (runFragment modelId)
         SubReport (Just queryId) order subReport -> do
            model <- getModel
            goToEntity1 "Subreport uses a deleted query." queryId
            current >>= \case
               (preview (_Just . entityContents . _QueryDiagram . queryDiagram) -> Just qd) ->
                  case compileQueryDiagram model qd of
                     Left msgs -> return $ ResultFragment [] [] [] $ map (queryId,) msgs
                     Right qry -> do
                        -- Get list of model IDs from query in alphabetical order.
                        let (argIds, warnings) = runQuery qry [] model $ S.singleton modelId
                        idList <- map snd . sortBy (compareValues order `on` fst) <$>
                              mapM getEntValues (S.toList argIds)
                        -- Run the subreport and collate the results.
                        result <- mconcat <$> forM idList (\mId ->
                              mconcat <$> forM subReport (runFragment mId))
                        return $ result <> ResultFragment [] [] [] (map (queryId, ) warnings)
               _ ->    -- Should never happen.
                  return $ ResultFragment [] [] [] [(queryId, "Subreport query is not a query.")]
   where
      headAttrs lvl = Attr (Just (modelId, lvl)) Nothing
      getEntValues mId = do
         goToEntity mId
         vs <- getData
         return (vs, mId)


-- | Render the extension value as a Document element with formatting taken from the field type.
-- Also returns the type of the field as a text string.
fieldFragment :: (EntityClass v) => FieldId -> ExtValue -> ModelEdit v v (Attr, ResultFragment)
fieldFragment fname v = do
   fields <- withBuiltIn . modelFields <$> getModel
   let
      displayText = ResultFragment [] [Str $ displayValue v] [] []
      mkAttr txt = Attr Nothing $ Just txt
   case fields ^? ix fname . fieldType of
      Just (BuiltInDef ModelNote) ->
         return (mkAttr "Note", finaliseResult $ textToBlocks $ displayValue v)
            -- Separate paragraph regardless.
      Just (BuiltInDef typ) -> return (mkAttr $ builtInName typ, displayText)
      Just (DecoBoolDef (DecoBool boolIcons boolColours)) ->
         case v of
            ExtBool b -> do
               let r = mconcat [
                        case boolIcons of
                           Just (iFalse, iTrue) ->
                              inlineIcon $ if b then iTrue else iFalse
                           Nothing -> mempty,
                        case boolColours of
                           Just (cFalse, cTrue) ->
                              inColour (if b then cFalse else cTrue) displayText
                           Nothing -> displayText
                     ]
               return (mkAttr "Boolean", r)
            _ -> return (mkAttr "Text", displayText)  -- Type does not match data.
      Just (DecoIntDef (DecoRange _ m)) ->
         case v of
            ExtInt i -> case rangeMap m i of
               Just (mIcon, mColour) -> do
                  let r = mconcat [
                           maybe mempty inlineIcon mIcon,
                           case mColour of
                              Just c -> inColour c displayText
                              Nothing -> displayText
                        ]
                  return (mkAttr "Integer", r)
               Nothing -> return (mkAttr "Integer", displayText)
            _ -> return (mkAttr "Text", displayText)  -- Type does not match data.
      Just (DecoRealDef (DecoRange _ m)) ->
         case v of
            ExtReal r -> case rangeMap m r of
               Just (mIcon, mColour) -> do
                  let r1 = mconcat [
                           maybe mempty inlineIcon mIcon,
                           case mColour of
                              Just c -> inColour c displayText
                              Nothing -> displayText
                        ]
                  return (mkAttr "Real", r1)
               Nothing -> return (mkAttr "Real", displayText)
            _ -> return (mkAttr "Text", displayText)  -- Type does not match data.
      Just (EnumDef items) ->
         case v of
            ExtText txt -> case find ((txt ==) . view enumItemName) items of
               Just (EnumItem _ mIcon mColour) -> do
                  let r = mconcat [
                           maybe mempty inlineIcon mIcon,
                           case mColour of
                              Just c -> inColour c displayText
                              Nothing -> displayText
                        ]
                  return (mkAttr "Enum", r)
               _ -> return (mkAttr "Enum", displayText)  -- Enumeration value not found in type.
            _ -> return (mkAttr "Text", displayText)  -- Type does not match data.
      _ -> return (mkAttr "Text", displayText)  -- Type data missing or bogus.


inColour :: Colour -> ResultFragment -> ResultFragment
inColour colour (ResultFragment blks inlines pics errs) =
      ResultFragment (walk fixInlines blks) (fixInlines inlines) pics errs
   where
      fixInlines :: [Inline] -> [Inline]
      fixInlines [] = []
      fixInlines l = [Highlight colour l]


-- | Convert some literal text into PanDoc blocks. The first paragraph will be appended to the
-- "Inline" argument. The last paragraph will become an "Inline" unless it ends with a newline.
textToBlocks :: Text -> ResultFragment
textToBlocks txt = mconcat $ makeBlocks paras
   where
      paras = T.splitOn "\n\n" txt
      makeBlocks [] = []
      makeBlocks [t]
         | T.null txt          = []
         | T.last txt == '\n'  = [makePara t]
         | otherwise           = [makeInline t]
      makeBlocks (t: rest) = makePara t : makeBlocks rest
      makePara t = ResultFragment [Para [Str t]] [] [] []
      makeInline t = ResultFragment [] [Str t] [] []


-- | If the block contains "Inline" items then prefix the argument on to it. Otherwise convert
-- the "Inline" items into a separate block.
prefixBlock :: [Inline] -> Block -> [Block]
prefixBlock ins1 (Plain ins2) = [Plain $ trimSpaces ins1 ++ Space : trimSpaces ins2]
prefixBlock ins1 (Para ins2) = [Para $ trimSpaces ins1 ++ Space : trimSpaces ins2]
prefixBlock ins1 blk = [Para $ trimSpaces ins1, blk]


-- | Add the named icon to the output.
inlineIcon :: Text -> ResultFragment
inlineIcon txt = ResultFragment
      []
      [Icon txt]
      [IconRef txt]
      []


-- | Insert the diagram (if any) from the current entity.
blockDiagram :: (EntityClass v, HasDiagrams HadesRender v) => ModelEdit v v ResultFragment
blockDiagram = do
      modelId <- currentId
      nm <- currentName
      current >>= \case
         Nothing -> noDiagram modelId $ "Entity " <> nm <> " is missing."
         Just e -> getDiagramWrapper e >>= \case
            Just (DiagramWrapper _ diagram _ _ _ _ _ _) -> do
               let   -- Used pattern match due to escaped type variables in field "diagramToEdit".
                  size1 = scaleBox pointFactor $ diagramBounds diagram
                  shrinkFactor = size1 `fitIntoBox` maxDiagramBox
                  size2 = scaleBox shrinkFactor size1
               return $ ResultFragment [block size2 nm modelId] [] [DiagramRef modelId] []
            _ -> noDiagram modelId $ "Entity " <> nm <> " is not a diagram."
   where
      block size nm modelId = Picture
            (Attr Nothing $ Just "DiagramBox")
            [Str nm]
            nm
            (boxWidth size, boxHeight size)
            modelId
      noDiagram modelId msg = return $ ResultFragment [Para [Str msg]] [] [] [(modelId, msg)]
      scaleBox _ NoBox = NoBox
      scaleBox f (BoundBox p1 p2) = BoundBox (scalePt f p1) (scalePt f p2)
      scalePt f (Point x y) = Point (x * f) (y * f)
      pointFactor = 72 / 96 :: Double  -- Points per pixel.
      maxDiagramBox = mkBoundBox (Point 0 0) (Point diagramWidthMax diagramHeightMax)
      diagramWidthMax = 72 * 7  -- 7 inches in points.
      diagramHeightMax = 72 * 9   -- 9 inches in points.

{- Design Note:

The scaling here is a bit clunky. The SVG diagram is automatically scaled from pixels (96 dpi) down
to points (72 per inch). Hence the size for the diagram also needs to be scaled by the same factor.
If the result is bigger than 7x9 inches in either dimension it is scaled to fit. The maximum size
was chosen to give reasonable results on both UK A4 and US Letter sizes of paper.
-}


-- | Insert a lookup table as a table.
blockLookup :: (EntityClass v, HasLookups v) =>
   ModelId  -- ^ The model ID of the lookup table.
   -> ModelEdit v v ResultFragment
blockLookup tableId = do
      goToEntity1 "The report uses a deleted lookup table." tableId
      model <- getModel
      tableNm <- currentName
      current >>= \case
         (preview (_Just . entityContents . _LookupTable) -> Just table) ->
            case (
                  modelFields model ^? ix (table ^. horizontalAxis) . to goodAxis . _Just,
                  modelFields model ^? ix (table ^. verticalAxis) . to goodAxis . _Just) of
               (Just (hType, hName, hs), Just (vType, vName, vs)) -> do
                  headerRow <- mapM (fmap extractInlines . fieldFragment hType) hs
                  rows <- forM vs $ \row -> do
                     rowHeader <- fieldFragment vType row
                     cells <- forM hs $ \col ->
                        fieldFragment
                              (table ^. tableOutput) $
                              table ^. tableValues . extValue (col, row)
                     let
                        (headerItem, headerImages, headerRefs) = extractBlocks rowHeader
                        (cellRow, cellImages, cellRefs) = unzip3 $ map extractBlocks cells
                     return (
                           Node [headerItem] [Node cellRow []],
                           headerImages ++ concat cellImages,
                           headerRefs ++ concat cellRefs
                        )
                  let
                     (headerInlines, headerImages, headerErrs) = unzip3 headerRow
                     cells :: Forest [MatrixCell]
                     (cells, cellImages, cellErrs) = unzip3 rows
                     headers = [
                           MatrixHeader [Str ""] [[Str vName]],
                           MatrixHeader [Str hName] headerInlines
                        ]
                  return $ ResultFragment
                        [D.Matrix noAttr [Str tableNm] headers cells]
                        []
                        (concat headerImages ++ concat cellImages)
                        (concat headerErrs ++ concat cellErrs)
               _ -> return $ ResultFragment [] [] [] [(tableId, "Invalid lookup table.")]
         _ -> return $ ResultFragment [] [] []
               [(tableId, "Lookup table does not exist (deleted?).")]
   where
      extractInlines :: (Attr, ResultFragment) -> ([Inline], [ImageRef], [(ModelId, Text)])
      extractInlines (_, ResultFragment blks inlines imgs errs) =
            (concatMap topInlines blks ++ inlines, imgs, errs)  -- O(n^2), but we know n is small.
      -- Smash all the text in a block into a single list of Inlines.
      topInlines :: Block -> [Inline]
      topInlines (Plain inlines) = inlines
      topInlines (Para inlines) = inlines
      topInlines (OrderedList _ blocks) = concatMapInlines (concatMapInlines topInlines) blocks
      topInlines (UnorderedList blocks) = concatMapInlines (concatMapInlines topInlines) blocks
      topInlines (DefinitionList pairs1) = concatMapInlines
            (\(inlines, blocks) ->
               trimSpaces $ inlines <> [Space] <> concatMapInlines topInlines blocks)
            pairs1
      topInlines (Heading _ _ inlines) = inlines
      topInlines D.Matrix {} = mempty  -- Complicated and will never happen.
      topInlines (Div _ blocks) = concatMapInlines topInlines blocks
      topInlines (Picture _ caption _ _ _) = caption
      topInlines D.Null = mempty
      concatMapInlines f = intercalate [Space] . map (trimSpaces . f)
      extractBlocks :: (Attr, ResultFragment) -> (MatrixCell, [ImageRef], [(ModelId, Text)])
      extractBlocks (attr, frag) =
         let ResultFragment blks _ imgs errs = finaliseResult frag
         in ((attr, blks), imgs, errs)


-- | Insert a matrix as a table.
blockMatrix :: (EntityClass v, HasLookups v, HasMatrices v) =>
   ModelId   -- ^ The model ID of the matrix.
   -> ModelId  -- ^ The entity to pass as an input to the matrix.
   -> ModelEdit v v ResultFragment
blockMatrix matrixId modelId = do
      model <- getModel
      goToEntity1 "Report uses a deleted matrix." matrixId
      current >>= \case
         (preview (_Just . entityContents . _Matrix) -> Just matrix) ->
            case compileMatrix matrix model of
               Left errs -> return $ ResultFragment [] [] [] $ map (matrixId,) errs
               Right queries -> do
                  let
                     (results, warns) = runMatrix queries model $ S.singleton modelId
                     warnFrag = ResultFragment [] [] [] $ map (matrixId, ) warns
                  if null results
                     then return warnFrag
                     else do
                        blk <- tabulate
                              (matrix ^. matrixName . nameText)
                              (matrix ^. matrixGroups)
                              results
                        return $ blk <> warnFrag
         _ -> return $ ResultFragment [] [] [] [(matrixId, "Matrix does not exist (deleted?).")]
            -- Should never happen.


-- | Tabulate a matrix.
tabulate :: (EntityClass v, HasLookups v, Queryable v) =>
   Text -> [ColumnGroup] -> Forest ModelId -> ModelEdit v v ResultFragment
tabulate _ [] _ = return mempty
tabulate caption groups nodes = do
      model <- getModel
      let
         sorts = map (view groupSort) groups
         entForest =
            sortForest sorts $
            mapMaybeForest (\u -> M.lookup u $ modelContents model) nodes
         forest1 = fmap (fmap (\e -> (entityId e, entityPropertiesWithStamps e))) entForest
      cells <- walkForestM (map tabulateRow groups) forest1
      headers <- mapM mkHeader groups
      let
         cellBlocks = fmap (map $ second $ resultBlocks . finaliseResult) <$> cells
         images = concatMap (resultImages . snd) $ concat $ concatMap flatten cells
         errs = concatMap (resultErrors . snd) $ concat $ concatMap flatten cells
      return $ ResultFragment [D.Matrix noAttr captionInlines headers cellBlocks] [] images errs
   where
      captionInlines = [Str caption]
      tabulateRow _ [] =
            return []
      tabulateRow grp values@((mId, _) : _) =
            mapM (tabulateCell mId (map snd values)) $ grp ^. groupColumns
      mkHeader grp = do
         let
            heads = map (return . Str . view matrixColumnName) $ grp ^. groupColumns
         goToEntity1 ("Matrix " <> caption <> " uses a deleted query.") $ grp ^. groupQuery
         queryName <- currentName
         return $ MatrixHeader [Str queryName] heads
      mapMaybeTree f (Node i cs) = case f i of
         Just x -> Just $ Node x $ mapMaybeForest f cs
         Nothing -> Nothing
      mapMaybeForest f = mapMaybe $ mapMaybeTree f


-- | Return the fragment for a single cell.
tabulateCell :: (EntityClass v, HasLookups v, Queryable v) =>
   ModelId     -- ^ The ID of the entity being tabulated.
   -> [ExtensionValues]
         -- ^ Extension values for this entity and all its predecessors in the matrix.
   -> MatrixColumn  -- ^ The column definition for this cell.
   -> ModelEdit v v (Attr, ResultFragment)
tabulateCell _ [] _ = return (Attr Nothing $ Just "Text", ResultFragment [] [] [] [])
tabulateCell modelId valueses@(values : _) c = do
      let allValues = foldr M.union mempty valueses
      knownFields <- withStampFields . withBuiltIn . modelFields <$> getModel
      case c ^. matrixColumnData of
         FieldColumn f -> do
            let
               fname = f ^. fieldColumnText
               addLink [] = []
               addLink txts = fromMaybe txts $ do
                  link <- f ^. fieldColumnLink
                  values ^? ix link >>= \case
                     ExtText url -> return [LinkOut url txts]
                     _ -> Nothing
            (attr, ResultFragment blks inlines images errs) <-
               fieldFragment fname $ values ^. extValue fname
            return (attr, ResultFragment (map (walk addLink) blks) (addLink inlines) images errs)
         ExprColumn _ expr -> do
            model <- getModel
            case MP.parse (topExpr entityFunctions knownFields) "" expr of
               Left bundle -> return
                  (mkAttr "Text",
                   ResultFragment [] [] [] $
                     map ((modelId,) . T.pack . MP.parseErrorTextPretty) $
                     NE.take 3 $
                     bundleErrors bundle)
               Right parsed -> do
                  ent <- goToEntity modelId >> current
                  let stamps = maybe M.empty entityStampsAsFields ent
                  case evaluate parsed (M.union stamps allValues) model of
                     Left msg -> return (mkAttr "Text", ResultFragment [] [] [] [(modelId, msg)])
                     Right v -> return (mkAttr $ valueTypeName v, textToBlocks $ displayValue v)
         QueryColumn queryId -> do
            model <- getModel
            let
               resultNames = T.intercalate ", " . sort . mapMaybe getName . S.toList
               getName mId = view (entityName . nameText) <$> M.lookup mId (modelContents model)
               result = do  -- Either monad
                  queryEnt <- maybe
                        (Left ["No such query"])
                        Right $
                        M.lookup queryId (modelContents model)
                  qd <- maybe
                        (Left [queryEnt ^. entityName . nameText <> " is not a query"])
                        Right $
                        queryEnt ^? entityContents . _QueryDiagram . queryDiagram
                  qry <- compileQueryDiagram model qd
                  return $ resultNames $ fst $ runQuery qry [] model $ S.singleton modelId
            case result of
               Left msgs -> return (mkAttr "Text", ResultFragment [] [] [] $ map (queryId, ) msgs)
               Right txt -> return (mkAttr "Text", textToBlocks txt)
         LookupColumn tableId -> do
            model <- getModel
            let
               result = do   -- Either monad
                  tblEnt <- maybe
                        (Left ["No such lookup table"])
                        Right $
                        M.lookup tableId (modelContents model)
                  maybe
                        (Left [tblEnt ^. entityName . nameText <> " is not a lookup table."])
                        Right $
                        tblEnt ^? entityContents . _LookupTable
            case result of
               Left msgs -> return (mkAttr "Text", ResultFragment [] [] [] $ map (modelId, ) msgs)
               Right tbl -> fieldFragment (tbl ^. tableOutput) $ runLookup tbl allValues
   where
      mkAttr txt = Attr Nothing $ Just txt
