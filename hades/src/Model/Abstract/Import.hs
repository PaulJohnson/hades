{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

-- |

module Model.Abstract.Import (
   NameClashAction (..),
   importExcel
) where

import Codec.Xlsx
import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.Either
import Data.Function
import Data.List hiding (delete)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Model.Abstract.PackageTree
import Model.Reflection.Reflective
import Model.Reflection.Types
import Model.Reflection.Values
import Reactive.Banana.Common


-- | Action to take when an imported entity shares the same name as an existing entity.
data NameClashAction =
   CancelClash             -- ^ Cancel the entire import.
   | MergeClash            -- ^ Merge with the existing entity.
   | DropClash             -- ^ Drop the imported entity.
   deriving (Eq, Enum, Bounded)

instance Show NameClashAction where
   show CancelClash = "Cancel import"
   show MergeClash = "Merge with existing entities"
   show DropClash = "Drop imported entities"


-- | Import an Excel spreadsheet into the model at the current location.
-- Fails if the type cannot be imported here.
importExcel :: (EntityClass v, Reflective v) =>
   NameClashAction
   -> Xlsx
   -> v            -- ^ Template value for imports. Generally from "reflectiveDefaults".
   -> ModelEdit v v [Text]
importExcel clash book template = do
      -- Get a sensible error now rather than a wierd one later on.
      ent <- current
      nm <- currentName
      case ent of
         Just parent ->
            unless (entityCanHaveChild (parent ^. entityContents) template) $ typeError nm
         Nothing ->
            unless (entityCanHaveChild dummyRoot template) $ typeError nm
      fields <- modelFields <$> getModel
      parentId <- currentId
      case xlsFindData fields book of
         Left msg -> throwUser msg
         Right d -> importEntities clash parentId template d
   where
      dummyRoot = Package (Name "dummy root") mempty ^. re _Package
         -- Dummy value used to test if the template can be imported into root.
      typeError nm = throwUser $
         "Cannot import " <> variantUserLabel (reflectiveName template) <>
         " entities into " <> nm


-- | Create a new entity from the extension values. Fails if the import has no name.
--
-- This allows the import of fields which are not currently defined for the entity.
importEntity :: (EntityClass v, Reflective v) =>
   NameClashAction  -- ^ If "CancelClash" then fail, otherwise just a warning message.
   -> ModelId   -- ^ Parent for the new entity.
   -> v         -- ^ Template value. Generally from "reflectiveDefaults"
   -> ExtensionValues   -- ^ Values to import.
   -> ModelEdit v w (Maybe Text)
importEntity clash parentId base exts = do
   let
      (v, exts2) = flip runState exts $ do
         mapM_ (extract 0 _ExtInt) $ M.keys entityStampFields  -- Ignore stamp field values.
         reflectiveSet base
      nm = v ^. name . nameText
   when (nm == "") $ throwUser "Entity with no name"
   goToEntity parentId
   childs <- currentChildren
   if M.member (Name nm) childs
      then case clash of
         CancelClash ->
            throwUser $ "Duplicate name: " <> nm
         MergeClash -> do
            followPath [Name nm]
            void $ setData exts2
            return $ Just $ "Merged with existing: " <> nm
         DropClash ->
            return $ Just $ "Dropped duplicate: " <> nm
      else do
         ent <- mkEntity v
         goToEntity $ entityId ent
         void $ setData exts2
         return Nothing


-- | Create new entities from a list of extension values. Returns a list of any warnings.
importEntities :: (EntityClass v, Reflective v) =>
   NameClashAction  -- ^ If "CancelClash" then fail, otherwise just a warning message.
   -> ModelId  -- ^ Parent for the new entity
   -> v        -- ^ Template value.
   -> [ExtensionValues]
   -> ModelEdit v w [Text]
importEntities clash parentId base exts =
   catMaybes <$> mapM (importEntity clash parentId base) exts


-- | Excel spreadsheet row.
type XlsRow = Map Int CellValue


-- | Find a block of rows all the same length starting with a row of text cells
-- that includes \"Name\".
-- The field table is used to ensure that the resulting extension values have the
-- correct type.
xlsFindData :: FieldTable -> Xlsx -> Either Text [ExtensionValues]
xlsFindData fields book = do
      sheets <- case book ^. xlSheets of
         [] -> Left "No worksheets found in Excel file."
         pairs -> return pairs
      case partitionEithers $ map sheetExtract sheets of
         (fails, []) -> Left $ "No data could be imported:" <> mconcat (map ("\n   " <>) fails)
         (_, rs) -> return $ concat rs
   where
      xlsEpoch = case book ^. xlDateBase of
         DateBase1900 -> fromGregorian 1899 12 30    -- Yes, its the 30th not 31st.
         DateBase1904 -> fromGregorian 1904 1 1
      findHeader [] = Nothing
      findHeader (row:rows) = case goodHeaderRow fields row of
         Just h -> Just (h, rows)
         Nothing -> findHeader rows
      dataRows _ [] = []
      dataRows hdr (row:rows) =
         case goodDataRow xlsEpoch hdr row of
            Just exts -> exts : dataRows hdr rows
            Nothing -> dataRows hdr rows
      sheetExtract :: (Text, Worksheet) -> Either Text [ExtensionValues]
      sheetExtract (nm, sheet) = case findHeader $ concatMap snd $ contiguous $ xlsRows sheet of
            Nothing -> Left $ "Sheet " <> nm <> " has no header rows."
            Just (_, []) -> Left $ "Sheet " <> nm <> " has a header row but no data."
            Just (hdrs, ds) -> case dataRows hdrs ds of
                  [] -> Left $ "Sheet " <> nm <> " data rows were all invalid."
                  xs -> return xs


-- | Extract the rows from a WorkSheet.
xlsRows :: Worksheet -> Map Int XlsRow
xlsRows sheet = M.fromList $ map (_2 %~ M.fromList) contents
   where
      contents :: [(Int, [(Int, CellValue)])]
      contents =
         map (\xss -> (fst $ fst $ head xss, map (_1 %~ snd) xss)) $
            -- Factor out row num. "head" is safe because groupBy never produces empty sublists.
         groupBy ((==) `on` (fst . fst)) $
         sortOn (fst . fst) $
         mapMaybe (f . (_2 %~ view cellValue)) $
         M.toList $
         sheet ^. wsCells
      f (x, Just y) = Just (x, y)
      f (_, Nothing) = Nothing


-- | A row containing exactly one contiguous block of 2 or more text cells including one that says
-- \"Name\". Returns the starting column and the field IDs. Unrecognised fields are silently
-- ignored.
goodHeaderRow :: FieldTable -> XlsRow -> Maybe (Int, [Field])
goodHeaderRow ftable row =
      case contiguous row of
            [(c, cells)] -> do
               guard $ length cells > 1
               headNames <- mapM (preview _CellText) cells
               let heads = mapMaybe (`M.lookup` nameTable) headNames
               guard $ elem nameField $ map fieldId heads
               return (c, heads)
            _ -> Nothing
   where
      nameTable = fieldsByName $ withBuiltIn ftable
      _CellText = prism CellText $ \case {CellText txt -> Right txt; v -> Left v}
         -- This is likely to be added to the xlsx library at some point.


-- | The row is a valid data row if it contains no cells outside the header and at least one cell
-- within it. The field table is used to ensure that the resulting extension values have the
-- correct type.
goodDataRow ::
   Day                   -- ^ Excel zero day for this spreadsheet.
   -> (Int, [Field])  -- ^ As returned by "goodHeaderRow"
   -> XlsRow
   -> Maybe ExtensionValues
goodDataRow excelEpoch (s, hdrs) row = do
         minKey <- fst <$> M.lookupMin row   -- Fails if row is empty, which is what we want.
         guard $ s <= minKey
         return $ M.fromList $ flip mapMaybe (zip [s .. ] hdrs) $ \(i, f) ->
            (fieldId f, ) <$> (row ^? ix i . to (
                  typeConversion excelEpoch $ f ^. fieldType) . _Just)


-- | Convert an Excel value into an extension value if possible. First argument is the Excel
-- epoch date for converting numbers into dates.
typeConversion :: Day -> TypeDef -> CellValue -> Maybe ExtValue
typeConversion _ (BuiltInDef ModelBool) (CellBool b) = Just $ ExtBool b
typeConversion _ (BuiltInDef ModelInt) (CellDouble d) = Just $ ExtInt $ round d
typeConversion _ (BuiltInDef ModelInt) c = ExtInt <$> extractText c ^? textPrism
typeConversion _ (BuiltInDef ModelReal) (CellDouble d) = Just $ ExtReal d
typeConversion _ (BuiltInDef ModelReal) c = ExtReal <$> extractText c ^? textPrism
typeConversion _ (BuiltInDef ModelText) c = Just $ ExtText $ extractText c
typeConversion _ (BuiltInDef ModelURL) c = Just $ ExtText $ extractText c
typeConversion _ (BuiltInDef ModelNote) c = Just $ ExtText $ extractText c
typeConversion excelEpoch (BuiltInDef ModelDate) (CellDouble d) =
      Just $ ExtDate $ addDays (floor d) excelEpoch
typeConversion _ (BuiltInDef ModelDate) c =
   let t = extractText c
   in ExtDate <$> (t ^? datePrism longDate <|> t ^? datePrism shortDate)
typeConversion _ DecoBoolDef {} (CellBool b) = Just $ ExtBool b
typeConversion _ DecoIntDef {} (CellDouble d) = Just $ ExtInt $ round d
typeConversion _ DecoIntDef {} c = ExtInt <$> extractText c ^? textPrism
typeConversion _ DecoRealDef {} (CellDouble d) = Just $ ExtReal d
typeConversion _ DecoRealDef {} c = ExtReal <$> extractText c ^? textPrism
typeConversion _ EnumDef {} c = Just $ ExtText $ extractText c
typeConversion _ _ _ = Nothing


-- | The text representation of an Excel value.
extractText :: CellValue -> Text
extractText (CellText txt)       = txt
extractText (CellDouble d)       = T.pack $ show d
extractText (CellBool b)         = T.pack $ show b
extractText (CellRich runs) = T.concat $ map _richTextRunText runs
extractText (CellError _)        = ""

-- | Find contiguous blocks of keys.
contiguous :: Map Int v -> [(Int, [v])]
contiguous m = map block $ runs $ M.keys m
   where
      -- block :: (Int, Int) -> (Int, [v])
      block (s, n) = (s, mapMaybe (\i -> m ^? ix i) [s .. s+n-1])
      runs :: [Int] -> [(Int, Int)]  -- Start value and length of contiguous indexes.
      runs [] = []
      runs [x] = [(x, 1)]
      runs (x1:xs) = go x1 1 xs
         where
            go s n [] = [(s, n)]
            go s n (x2:xs2)
               | x2 == x1 + n  = go s (n+1) xs2
               | otherwise     = (x1, n) : runs (x2:xs2)
