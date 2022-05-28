{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

{- |

Export of matrices to Excel spreadsheets.
-}

module Model.Matrices.Excel (
  matrixExportDialog,
  matrixErrorsDialog,
  matrixWarningsDialog,
  matrixBook,
  matrixCells
) where

import Codec.Xlsx
import Codec.Xlsx.Formatted
import Control.Lens
import Control.Monad
import Data.Colour.SRGB
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time.Calendar
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree
import Model.Abstract.PackageTree
import Model.Abstract.Properties
import Model.Lookups.Base
import Model.Matrices.Base
import Model.Reflection.Parser
import Model.Reflection.Reflective
import Model.Reflection.Types
import Model.Reflection.Values
import Model.Query.Base
import Model.Query.Diagram
import Reactive.Banana.ArrowDialog
import Reactive.Banana.Common
import Text.Megaparsec as MP

{- Design note:

The code here is irritatingly similar to Model.Report.Base.tabulate, but there are too many
points of difference to make it worth while factoring them out.
-}


-- | Dialog to select an input to the matrix.
matrixExportDialog :: (EntityClass v, Queryable v) =>
  Model v -> Matrix -> Dialog' (Model v) () (Set ModelId)
matrixExportDialog model matrix = refPicker True inputFilter
  where
    inputFilter e = S.member (e ^. entityContents . to reflectiveName) $ inputSet model
    inputSet = matrixInputSet matrix


matrixErrorsDialog :: [Text] -> Dialog' e () a
matrixErrorsDialog msgs = Dialog "Matrix Errors" (CloseButton "_Close") $ message1 $
  T.intercalate "\n" msgs <> "\n\nCannot proceed with export."

matrixWarningsDialog :: [Text] -> Dialog' e () a
matrixWarningsDialog msgs = Dialog "Matrix Warnings" OkButton $ message1 $
  T.intercalate "\n" msgs <> "\n\nClick \"OK\" to proceed with export, \"Cancel\" to stop."


-- | Render a set of formatted cells as a single worksheet in an Excel workbook.
matrixBook :: (EntityClass v) =>
  Model v
  -> Matrix
  -> Text   -- ^ Title for the matrix. Illegal characters are filtered.
  -> Map (Int, Int) FormattedCell
  -> Xlsx
matrixBook model matrix txt cells =
    (xlSheets .~ [(sheetNameFilter txt, sheet)]) .
    (xlStyles .~ renderStyleSheet (formattedStyleSheet s)) $ def
  where
    sheet =
      (wsCells .~ formattedCellMap s) .
      (wsColumnsProperties .~ cols) .
      (wsRowPropertiesMap .~ rows) .
      (wsMerges .~ formattedMerges s) $ def
    s = formatted cells minimalStyleSheet
    cols = zipWith (matrixColumn model)
          [1..] $
          concatMap (view groupColumns) $ matrix ^. matrixGroups
    rowCount = maximum $ map fst $ M.keys cells
    rows = M.fromList $ zip [1..rowCount] $ repeat $
        RowProps (Just $ AutomaticHeight 10) Nothing False
    -- Filter the sheet name to conform to undocumented rules.
    sheetNameFilter = T.take 31 . apostrophes . T.filter sheetNameBanned
    sheetNameBanned = not . (`elem` ("[]*?:/\\" :: String))
    apostrophes t = let
      t1 = if T.take 1 t == "'" then T.drop 1 t else t
      in if T.takeEnd 1 t1 == "'" then T.dropEnd 1 t1 else t1


-- | Matrix as a set of formatted cells. The output can be passed to the "formatted" function in
-- "Codec.Xlsx.Formatted". The @Left@ value contains error messages while the @Right@ value
-- contains the returned matrix and any warning messages.
matrixCells :: (EntityClass v, HasLookups v, Queryable v) =>
  Model v   -- ^ Model containing data to export.
  -> ModelId   -- ^ Matrix input entity.
  -> Matrix  -- ^ Matrix defining data to export.
  -> Either [Text] (Map (Int, Int) FormattedCell, [Text])
matrixCells model input matrix = do -- Either monad.
  queries <- compileMatrix matrix model
  let (idForest, warnings) = runMatrix queries model $ S.singleton input
  evalModel $ do
    let
      groups = matrix ^. matrixGroups
      sorts = map (view groupSort) groups
    headers <- case groups of
      [] -> return []
      [g] ->
        let hr = map (makeHeaderCell . view matrixColumnName) $ g ^. groupColumns
        in return $ zip (zip (repeat 2) [1..]) hr
      _ -> headerAssign (1, 1) <$> mapM mkHeader groups
    forest1 <- sortForest sorts . forestCatMaybe <$>
        mapForestM (\i -> goToEntity i >> current) idForest
    forest2 <- walkForestM (map tabulateRow groups) $ fmap (fmap (\ent -> (
          entityId ent,
          M.union (entityStampsAsFields ent) (ent ^. entityVariantProperties model))))
        forest1
    return (M.union (M.fromList headers) $ fst $ forestAssign (3, 1) forest2, warnings)
  where
    treeAssign (row, col) (Node cells childs) =
      let
        (childCells, height) = forestAssign (row, col + length cells) childs
        cells1 = M.fromList $
            zip (zip (repeat row) [col ..]) $ map (formattedRowSpan .~ height) cells
      in (M.union cells1 childCells, height)
    forestAssign _ [] = (mempty, 1)
    forestAssign (row, col) nodes =
      let
        (cells, heights) = unzip $
          zipWith treeAssign (zip (scanl (+) row heights) $ repeat col) nodes
      in (mconcat cells, sum heights)
    evalModel act = case evalModelEdit id model act of
      Left err -> Left ["Internal error: " <> T.pack (show err)]
      Right r -> Right r
    tabulateRow grp vs =
      let
        (mIds, values) = unzip vs
      in mapM (tabulateCell mIds values) $ grp ^. groupColumns
    mkHeader grp = do
      let heads = map (view matrixColumnName) $ grp ^. groupColumns
      when (null heads) $ throwUser "Query group has no columns."
      goToEntity1 "The matrix uses a deleted query." $ grp ^. groupQuery
      queryName <- currentName
      let
        h1 = (formattedColSpan .~ length heads) $ makeHeaderCell queryName
        h2 = map makeHeaderCell heads
      return (h1, h2)
    -- Assign coordintates to headers starting at (x,y) in the top left.
    headerAssign _ [] = []
    headerAssign (row, col) ((h1, h2) : pairs) =
        ((row, col), h1) :
        zipWith (\n h -> ((row + 1, n), h)) [col ..] h2 ++
        headerAssign (row, col + length h2) pairs


-- | Display a cell. The extension values should match the model IDs; they are passed down
-- separately to reduce computation.
tabulateCell :: (EntityClass v, HasLookups v, Queryable v) =>
  [ModelId] -> [ExtensionValues] -> MatrixColumn -> ModelEdit v v FormattedCell
tabulateCell [] _ _ = return def
tabulateCell outerIds@(modelId:_) valueses c = do
    let
      values = fromMaybe mempty $ listToMaybe valueses
        -- This entity
      allValues = mconcat valueses
        -- This entity and all its parents in the results tree.
    goToEntity modelId
    ent <- current
    nm <- currentName
    model <- getModel
    result <- case c ^. matrixColumnData of
      FieldColumn (FieldColumnData fname lname) -> do
        let
          link = do
            l <- lname
            values ^? extValue l . _ExtText
        makeFieldCell fname (values ^. extValue fname) link
      ExprColumn _ expr -> do
        let allFields = withStampFields $ withBuiltIn $ modelFields model
        case MP.parse (topExpr entityFunctions allFields) "" expr of
          Left bundle -> return $ warningCell $
              nm <> ": " <> T.pack (MP.parseErrorTextPretty $
              NE.head $
              bundleErrors bundle)
          Right parsed -> do
            let stamps = maybe M.empty entityStampsAsFields ent
            case evaluate parsed (M.union stamps allValues) model of
              Left msg -> return $ warningCell $ nm <> ": " <> msg
              Right v -> return $ extValueToExcel v Nothing
      QueryColumn queryId -> do
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
            return $ resultNames $ fst $ runQuery qry outerIds model $ S.singleton modelId
        case result of
          Left msg -> return $ warningCell $ fromMaybe "" $ listToMaybe msg
          Right txt -> return $ extValueToExcel (ExtText txt) Nothing
      LookupColumn tableId -> do
        let
          result = do  -- Either monad
            tableEnt <- maybe
              (Left "No such lookup table")
              Right $
              M.lookup tableId (modelContents model)
            tbl <- maybe
              (Left $ tableEnt ^. entityName . nameText <> " is not a lookup table")
              Right $
              tableEnt ^? entityContents . _LookupTable
            return (runLookup tbl allValues, tbl ^. tableOutput)
        case result of
          Left msg -> return $ warningCell msg
          Right (v, fname) -> makeFieldCell fname v Nothing
    return $
      (formattedFormat . formatBorder .~ matrixBorders) .
      (formattedFormat . formatAlignment .~ matrixAlignment)
      $ result
  where
    warningCell txt =
      (formattedCell . cellValue ?~ CellText txt) .
      (formattedFormat . formatFont .~ warningFont) $ def
    warningFont = Just $ fontColor ?~ (colorARGB ?~ "FFA00000" $ def) $ def




-- | Generate a cell formatted as a column header.
makeHeaderCell :: Text -> FormattedCell
makeHeaderCell txt =
    (formattedCell . cellValue ?~ CellText txt) .
    (formattedFormat .~
      ((formatFont . _Just . fontBold ?~ True) .
      (formatAlignment . _Just . alignmentHorizontal ?~ CellHorizontalAlignmentCenter)
      $ matrixFormat)) $ def


-- | Convert an Extension Value to an Excel cell, along with the default format for the type. The
-- text argument is an optional hyperlink.
extValueToExcel :: ExtValue -> Maybe Text -> FormattedCell
extValueToExcel v link = case link of
    Nothing -> (formattedCell . cellValue .~ cv) . (formattedFormat .~ cf) $ def
    Just txt ->
      (formattedCell . cellFormula ?~ CellFormula (hyperlink txt) False True) .
      (formattedFormat .~ cf) $ def
  where
    (cv, cf) = case v of
      ExtNone -> (Nothing, def)
      ExtBool b -> (Just $ CellBool b, def)
      ExtInt n -> (Just $ CellDouble $ fromIntegral n, def)
      ExtReal x -> (Just $ CellDouble x, def)
      ExtText t -> (Just $ CellText t,
          formatAlignment ?~ (alignmentWrapText ?~ True $ def) $ def)
      ExtDate d -> (Just $ CellDouble $ fromIntegral $ diffDays d excelEpoch,
          formatNumberFormat ?~ StdNumberFormat NfDMmmYy $ def)
    excelEpoch = fromGregorian 1899 12 30    -- Day zero in Excel. Yes, its the 30th not 31st.
    hyperlink txt = NormalFormula $ Formula $
        "HYPERLINK(\"" <> excelEscape txt <> "\",\"" <> excelEscape (displayValue v) <> "\")"
    excelEscape = T.replace "\"" "\"\""

-- | Format an extension value according to the field type.
makeFieldCell :: (EntityClass v) =>
  FieldId   -- ^ Field name to display.
  -> ExtValue  -- ^ Value of the field.
  -> Maybe Text  -- ^ Optional link URL.
  -> ModelEdit v v FormattedCell
makeFieldCell fid v link = do
    fields <- withBuiltIn . modelFields <$> getModel
    let baseCell = extValueToExcel v link
    case fields ^? ix fid . fieldType of
      Just (BuiltInDef _) -> return baseCell
      Just (DecoBoolDef (DecoBool _ Nothing)) -> return baseCell
      Just (DecoBoolDef (DecoBool _ (Just (cf, ct)))) -> return $ case v of
        ExtBool False -> addColour cf baseCell
        ExtBool True -> addColour ct baseCell
        _ -> baseCell
      Just (DecoBoolDef (DecoRange _ _)) -> return baseCell  -- Cannot happen.
      Just (DecoIntDef (DecoRange _ rm)) -> return $ case v of
        ExtInt n -> case rangeMap rm n of
          Just (_, Just c) -> addColour c baseCell
          _ -> baseCell
        _ -> baseCell
      Just (DecoRealDef (DecoRange _ rm)) -> return $ case v of
        ExtReal x -> case rangeMap rm x of
          Just (_, Just c) -> addColour c baseCell
          _ -> baseCell
        _ -> baseCell
      Just (EnumDef enums) -> return $ case v of
        ExtText t ->
          let e = find ((t ==) . view enumItemName) enums
          in case e ^? _Just . enumItemColour . _Just of
            Just c -> addColour c baseCell
            Nothing -> baseCell
        _ -> baseCell
      Nothing -> return baseCell
  where
    addColour c = formattedFormat . formatFill ?~ fill
      where
        fill = Fill $ Just $
            (fillPatternType ?~ PatternTypeSolid) .
            (fillPatternBgColor ?~ argb) .  -- Not sure we need this, but never mind.
            (fillPatternFgColor ?~ argb) $ def
        argb = colorARGB ?~ ("FF" <> T.toUpper hex) $ def
        hex = T.pack $ drop 1 $ sRGB24show $ getColour c  -- sRGB24show adds a leading '#'.


-- | Basic format. Specifies Alignment, Border and Font.
matrixFormat :: Format
matrixFormat =
  (formatAlignment .~ matrixAlignment) .
  (formatBorder .~ matrixBorders) .
  (formatFont .~ matrixFont) $ def


-- | Roboto font.
matrixFont :: Maybe Font
matrixFont = Just $
  (fontName ?~ "Roboto") .
  (fontFamily ?~ FontFamilySwiss) $ def


-- | Black border around each cell.
matrixBorders :: Maybe Border
matrixBorders = Just $
    (borderLeft .~ s) . (borderTop .~ s) . (borderRight .~ s) . (borderBottom .~ s) $ def
  where
    s = Just $ BorderStyle (Just $ colorAutomatic ?~ True $ def) (Just LineStyleThin)


-- | Vertically centered, text wrapped. Horizontal alignment left at default.
matrixAlignment :: Maybe Alignment
matrixAlignment = Just $
  (alignmentVertical ?~ CellVerticalAlignmentCenter) .
  (alignmentWrapText ?~ True) $ def


-- | Matrix column data converted to Excel column data. First argument is the Excel column number.
matrixColumn :: (EntityClass v) => Model v -> Int -> MatrixColumn -> ColumnsProperties
matrixColumn model n col = ColumnsProperties {
      cpMin = n,
      cpMax = n,
      cpWidth = Just colWidth,
      cpStyle = Nothing,
      cpHidden = False,
      cpCollapsed = False,
      cpBestFit = True
    }
  where
    colWidth = min widthLimit $ max headingWidth typeWidth
    headingWidth = fromIntegral (T.length $ col ^. matrixColumnName) / 2
    typeWidth = case col ^. matrixColumnData of
      FieldColumn (FieldColumnData fname _) ->
        case withBuiltIn (modelFields model) ^? ix fname . fieldType of
          Just (BuiltInDef ModelBool) -> 4
          Just (BuiltInDef ModelInt) -> 4
          Just (BuiltInDef ModelReal) -> 6
          Just (BuiltInDef ModelText) -> 20
          Just (BuiltInDef ModelURL) -> 20
          Just (BuiltInDef ModelNote) -> 40
          Just (BuiltInDef ModelDate) -> 15
          Just (DecoBoolDef _) -> 4
          Just (DecoIntDef _) -> 4
          Just (DecoRealDef _) -> 6
          Just (EnumDef _) -> 10
          Nothing -> 4
      ExprColumn False _ -> 10
      ExprColumn True _ -> 20
      QueryColumn _ -> 15
      LookupColumn _ -> 10
    widthLimit = 30  -- Maximum column width in characters.
