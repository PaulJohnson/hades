{-# LANGUAGE Arrows #-}  -- HLint cant get this from cabal file.

{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

{- |
-}

module Model.Lookups.Base (
  maxAxisRange,
  HasLookups (..),
  LookupTable (LookupTable),
  tableName,
  horizontalAxis,
  verticalAxis,
  tableOutput,
  tableValues,
  tableFunctions,
  lookupEditGadget,
  goodAxis,
  lookupForest,
  runLookup
) where

import Control.Arrow
import Control.Lens hiding ((.=))
import Control.Monad
import Data.Aeson
import Data.Bifunctor as BF
import Data.Colour.Names
import Data.Default
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree
import qualified Data.UUID as U
import Model.Abstract.PackageTree
import Model.Abstract.Properties
import Model.Reflection.Dialogs
import Model.Reflection.Parser
import Model.Reflection.Reflective
import Model.Reflection.Types
import Model.Reflection.Values
import Reactive.Banana.ArrowDialog
import Reactive.Banana.Common


-- | The maximum size of an integer range allowed in an axis. Value = 10.
maxAxisRange :: Integer
maxAxisRange = 10


-- | Prism for the lookup table in a model entity.
class HasLookups v where
  _LookupTable :: Prism' v LookupTable


-- | Lookup tables provide a 2D matrix mapping a pair of inputs to an output.
data LookupTable = LookupTable {
    _tableName :: Name,
    _horizontalAxis :: FieldId,
    _verticalAxis :: FieldId,
    _tableOutput :: FieldId,
    _tableValues :: Map (ExtValue, ExtValue) ExtValue
  } deriving (Eq, Show)

instance Default LookupTable where
  def = LookupTable (Name "") U.nil U.nil U.nil mempty

instance ToJSON LookupTable where
  toJSON tbl = object [
      "type" .= ("lookup-table" :: Text),
      "name" .= (tbl ^. tableName),
      "horizontal" .= (tbl ^. horizontalAxis),
      "vertical" .= (tbl ^. verticalAxis),
      "output" .= (tbl ^. tableOutput),
      "values" .= (tbl ^. tableValues)
    ]

instance FromJSON LookupTable where
  parseJSON = withObject "Lookup Table" $ \v -> do
    t <- v .: "type"
    when (t /= "lookup-table") $ fail $ t <> " is not a lookup table."
    LookupTable <$>
      v .: "name" <*>
      (migrateField1 <$> v .: "horizontal") <*>
      (migrateField1 <$> v .: "vertical") <*>
      (migrateField1 <$> v .: "output") <*>
      v .: "values"

instance Reflective LookupTable where
  reflectiveName _ = Variant "Lookup Table"
  reflectiveDefaults = [LookupTable (Name "") U.nil U.nil U.nil mempty]
  reflectiveBuiltIn _ = [nameField]
  reflectiveGet t = M.singleton nameField $ ExtText $ t ^. tableName . nameText
  reflectiveSet t = do
    nm <- Name . T.strip <$> extract (t ^. tableName . nameText) _ExtText nameField
    return $ tableName .~ nm $ t
  reflectiveBuiltInRefs = mempty
  reflectiveArrows = mempty


tableName :: Lens' LookupTable Name
tableName = lens _tableName $ \s n -> s {_tableName = n}

horizontalAxis :: Lens' LookupTable FieldId
horizontalAxis = lens _horizontalAxis $ \s h -> s {_horizontalAxis = h}

verticalAxis :: Lens' LookupTable FieldId
verticalAxis = lens _verticalAxis $ \s v -> s {_verticalAxis = v}

tableOutput :: Lens' LookupTable FieldId
tableOutput = lens _tableOutput $ \s t -> s {_tableOutput = t}

-- | The values stored by the table. The key is the (x, y) position.
tableValues :: Lens' LookupTable (Map (ExtValue, ExtValue) ExtValue)
tableValues = lens _tableValues $ \s v -> s {_tableValues = v}


-- | Functions for lookup tables. Currently just \"Lookup\".
tableFunctions :: (EntityClass v, HasLookups v) => FunctionTable (Model v)
tableFunctions = M.fromList [
    ("Lookup", \argVals extVals model -> do
      arity "Lookup" argVals 1
      path0 <- displayValue <$> getArg "Lookup" argVals 0
      let path = map Name $ filter (not . T.null) $ T.splitOn "/" path0
      tbl <- BF.first (showError path0) $ evalModelEdit id model $ getTable path
      return $ runLookup tbl extVals
    )]
  where
    showError path (UserError txt) = txt <> ": " <> path
    showError _ (InternalError txt) = "Internal error: " <> txt
    getTable path = do
      goToRoot
      followPath path
      current >>= \case
        Nothing -> throwUser "Not a lookup table"
        Just e -> do
          case e ^? entityContents . _LookupTable of
            Nothing -> throwUser "Not a lookup table"
            Just tbl -> return tbl


-- | Editor for lookup tables.
lookupEditGadget :: (EntityClass v) => GadgetF' (Model v) w LookupTable
lookupEditGadget = getInitialEnv $ \model -> proc input -> do
      hdrF <- headingForm -< input
      let
        hdr = hdrF input
        tblDef = gridExtract
            (modelFields model)
            (hdr ^. horizontalAxis)
            (hdr ^. verticalAxis)
            (hdr ^. tableOutput)
      bodyF <- exec bodyForm -< (tblDef, hdr)
      returnA -< const $ bodyF hdr
      -- Or possibly "bodyF . hdrF", but it aint broke so lets not fix it.
  where
    headingForm = form Vertical [
        ("Table name:", focusing (tableName . nameText) simpleTextBox),
        ("Horizontal axis:", focusing horizontalAxis axisCombo),
        ("Vertical axis:", focusing verticalAxis axisCombo),
        ("Output:", focusing tableOutput outputCombo)]
    bodyForm ::
        Maybe ([ExtValue], [ExtValue], Field)
        -> GadgetF' e w LookupTable
    bodyForm Nothing =
      focusing id $ message1 "Select the extension fields for the table above."
    bodyForm (Just (hAxis, vAxis, out)) =
        let fld = fieldElement False out
        in grid
          (map displayValue hAxis)
          (map displayValue vAxis)
          [[cell col row (focusing id fld) | col <- hAxis] | row <- vAxis]
      where
        cell col row = focusingOver (tableValues . extValue (col, row))
    gridExtract fields hAxis vAxis output =
      case (
          fields ^? ix hAxis . to goodAxis . _Just,
          fields ^? ix vAxis . to goodAxis . _Just,
          fields ^? ix output) of
        (Just (_, _, hs), Just (_, _, vs), Just out) -> Just (hs, vs, out)
        _ -> Nothing
    emptyItem = ComboItem "<Select type>" Nothing (Just $ Colour darkred) U.nil
    axisCombo = comboBox $
      (emptyItem :) .
      map (\(fid, fname, _) -> ComboItem fname Nothing Nothing fid) .
      mapMaybe goodAxis . sortOn (view fieldName) . M.elems . modelFields
    outputCombo = comboBox $
      (emptyItem :) .
      map (\f -> ComboItem (f ^. fieldName) Nothing Nothing (fieldId f)) .
      M.elems . modelFields


-- | If the field is suitable for use as an axis then return its ID, name and possible values.
goodAxis :: Field -> Maybe (FieldId, FieldName, [ExtValue])
goodAxis f = do
    vs <- f ^. fieldType . to goodAxis1
    return (fieldId f, f ^. fieldName, if f ^. fieldMulti == One then vs else ExtNone : vs)
  where
    goodAxis1 :: TypeDef -> Maybe [ExtValue]
    goodAxis1 (BuiltInDef ModelBool) = Just [ExtBool False, ExtBool True]
    goodAxis1 (DecoBoolDef _) = Just [ExtBool False, ExtBool True]
    goodAxis1 (DecoIntDef (DecoRange (Just x1, Just x2) _)) =
      if x2 > x1 && x2 - x1 < maxAxisRange
        then Just $ map ExtInt [x1 .. x2]
        else Nothing
    goodAxis1 (EnumDef vs) = Just $ map (ExtText . view enumItemName) vs
    goodAxis1 _ = Nothing


-- | Extract the forest from the model and select all the members of the argument set.
lookupForest :: (EntityClass v, HasLookups v) =>
  Model v -> Forest (Text, Maybe Text, Maybe ModelId)
lookupForest = modelFilter $ isJust . preview (entityContents . _LookupTable)


-- | Find the inputs in the list of entity values, and hence return the output or "ExtNone".
runLookup :: LookupTable -> ExtensionValues -> ExtValue
runLookup tbl values = fromMaybe ExtNone $ do
    col <- findValue $ tbl ^. horizontalAxis
    row <- findValue $ tbl ^. verticalAxis
    return $ tbl ^. tableValues . extValue (col, row)
  where
    findValue field = M.lookup field values
