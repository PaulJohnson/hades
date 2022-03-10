{-# LANGUAGE Arrows #-}

{-
Copyright © Paul Johnson 2020. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.
-}

module Model.Matrices.Design (
   DesignItem (..),
   _DesignQuery,
   _DesignColumn,
   designItemText,
   Design,
   matrixDesignGadget,
   matrixDesignIso,
   sortOrderGadget
) where

import Control.Arrow
import Control.Lens
import Control.Lens.Hades
import Control.Monad
import Data.Default
import Data.List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree
import qualified Data.UUID as U
import Model.Abstract.PackageTree
import Model.Abstract.Properties
import Model.Lookups.Base
import Model.Matrices.Base
import Model.Query.Diagram
import Model.Reflection.Dialogs
import Model.Reflection.Parser
import Model.Reflection.Types
import Reactive.Banana.ArrowDialog
import Reactive.Banana.Menu
import Reactive.Banana.Table
import Text.Megaparsec


-- | An element in a matrix design. See "Design" for details.
data DesignItem =
   DesignQuery ModelId SortOrder
   | DesignColumn MatrixColumn
   deriving Eq

_DesignQuery :: Prism' DesignItem (ModelId, SortOrder)
_DesignQuery = prism
      (uncurry DesignQuery) $
      \case {DesignQuery mId sorts -> Right (mId, sorts); v -> Left v}

_DesignColumn :: Prism' DesignItem MatrixColumn
_DesignColumn = prism DesignColumn $ \case {DesignColumn c -> Right c; v -> Left v}


-- | Descriptive text for the item.
designItemText :: (EntityClass v) => Model v -> DesignItem -> Text
designItemText model (DesignQuery qId qSort) =
   let fields = modelFields model `M.union` entityStampFields `M.union` builtInFields
   in fromMaybe "- ? -" (modelContents model ^? ix qId . entityName . nameText) <> (
         case qSort of
            (SortOrder []) -> ""
            fs -> " sorted by " <> showSortOrder fields fs
      )
designItemText model (DesignColumn c) =
   c ^. matrixColumnName <> ": " <>
      case c ^. matrixColumnData of
         FieldColumn (FieldColumnData fid Nothing) -> "Field <" <> fname fid <> ">"
         FieldColumn (FieldColumnData fid (Just lfid)) ->
            "Field <" <> fname fid <> "> with link <" <> fname lfid <> ">"
         ExprColumn False expr -> "Formula: " <> expr
         ExprColumn True expr -> "Memo formula: " <> expr
         QueryColumn qId -> "Query <" <>
            fromMaybe "- ? -" (modelContents model ^? ix qId . entityName . nameText) <> ">"
         LookupColumn tId -> "Lookup table <" <>
            fromMaybe "- ? -" (modelContents model ^? ix tId . entityName . nameText) <> ">"
   where
      allFields = withStampFields $ withBuiltIn $ modelFields model
      fname = maybe "<deleted>" (view fieldName) . (`M.lookup` allFields)


-- | A matrix design is represented as a "Forest" for the benefit of the GUI. However it must
-- conform to the following structure laws:
--
-- * The first level nodes must be "DesignQuery" values.
--
-- * The second level nodes must be "DesignColumn" values with no children.
type Design = Forest DesignItem


-- | Report on the validity of the design. A null result means the design:
--
-- * Conforms to the laws for "Design".
--
-- * Only refers to field names that are valid in the model.
--
-- * Only refers to queries that exist in the model.
validDesign :: (EntityClass v, HasLookups v, Queryable v) =>
   Model v -> Design -> Design -> Maybe Text
validDesign model _ = listToMaybe . concat . zipWith validGroup [1..]
   where
      entityFunctions1 :: (EntityClass v) => Model v -> FunctionTable (Model v)
      entityFunctions1 _ = entityFunctions  -- Tie things together for type checker.
      validGroup :: Int -> Tree DesignItem -> [Text]
      validGroup n (Node (DesignQuery mId _) childs) =
         map ((T.pack $ "Group " <> show n <> ": ") <>) $
            verifyQuery mId ++
            concatMap verifyColumn childs
      validGroup n _ = [T.pack $ "Item " <> show n <> ": Not a column group."]
      verifyQuery mId = case modelContents model ^? ix mId . entityContents . _QueryDiagram of
         Just _ -> []
         Nothing -> ["Query missing or invalid."]
      verifyLookupTable tId = case modelContents model ^? ix tId . entityContents . _LookupTable of
         Just _ -> []
         Nothing -> ["Lookup table missing or invalid."]
      verifyColumn (Node (DesignColumn c) []) =
         checkName c ++ map
            ((c ^. matrixColumnName <> ": ") <>)
            (case c ^. matrixColumnData of
               FieldColumn (FieldColumnData fid Nothing) -> checkField fid
               FieldColumn (FieldColumnData fid (Just lfid)) ->
                  checkField fid ++ checkField lfid
               ExprColumn _ expr -> checkExpr expr
               QueryColumn qId -> verifyQuery qId
               LookupColumn tId -> verifyLookupTable tId)
      verifyColumn (Node (DesignColumn c) _) = [c ^. matrixColumnName <> " has children."]
      verifyColumn _ = ["Contains another group"]
      checkName c = ["Column with no name." | T.null $ c ^. matrixColumnName]
      checkField fid = ["Unknown or deleted field" | not $ M.member fid allFields]
      checkExpr expr = case parse (topExpr (entityFunctions1 model) allFields) "" expr of
         Left bundle -> map (T.pack . parseErrorTextPretty) $ NE.take 3 $ bundleErrors bundle
         Right _ -> []
      allFields = withStampFields $ withBuiltIn $ modelFields model


-- | Isomoprphism between "Matrix" and "Design". Round-trip equality is guaranteed if the @Design@
-- complies with its laws.
matrixDesignIso :: Iso' Matrix (Name, Text, Set ModelId, Design)
matrixDesignIso = iso
      (\(Matrix nm desc inputs grps) -> (nm, desc, inputs, grps ^. columnDesignIso))
      (\(nm, desc, inputs, design) -> Matrix
            (nameText %~ T.strip $ nm)
            desc
            inputs
            (design ^. from columnDesignIso))


columnDesignIso :: Iso' [ColumnGroup] Design
columnDesignIso = iso (map groupToItem) (mapMaybe itemToGroup)
   where
      groupToItem cg = Node
            (DesignQuery (cg ^. groupQuery) (cg ^. groupSort))
            (map columnToItem $ cg ^. groupColumns)
      columnToItem c = Node (DesignColumn c) []
      itemToGroup (Node (DesignQuery mId sortFields) columns) =
         Just $ ColumnGroup mId (mapMaybe itemToColumn columns) sortFields
      itemToGroup _ = Nothing
      itemToColumn (Node (DesignColumn c) _) = Just c
      itemToColumn _ = Nothing


-- | Present the matrix design as a tree.
matrixDesignGadget :: (EntityClass v, HasLookups v, HasMatrices v, Queryable v) =>
   ModelId -> Gadget' (Model v) (ModelEdit v Matrix (Maybe Text)) ((), Matrix)
matrixDesignGadget matrixId =
   getInitial $ \((), matrix1) ->
      let
         nm1 = matrix1 ^. matrixName . nameText
      in proc ((), matrix) -> do
         let d = matrix ^. matrixDesignIso
         d' <- accum $ box Vertical [[
               form Vertical [
                     ("Name:", focusing (_1 . nameText) simpleTextBox),
                     ("Description:", focusing _2 (memoBox MemoSmall False)),
                     ("Default Inputs:", focusing _3
                        (modelSubsetSpecifier (const $ const True) (const Nothing)))
                  ],
               simpleFrame "Matrix contents" $ focusing _4 $ validateText1 validDesign $
                  forestEditor designItemText designMenu canBeParent $ Just $ const itemDialog
            ]] -< d
         model <- getEnv -< ()
         _ <- send (matrixDelta nm1) -< (model, d, d')
         returnA -< ((), d' ^. from matrixDesignIso)
   where
      -- | Compare two matrices. If they are the same then return Nothing. Otherwise return
      -- an update for the second with an appropriate message. Assumes only a single edit.
      matrixDelta :: (EntityClass v) =>
         Text -> (Model v, (Name, Text, Set ModelId, Design), (Name, Text, Set ModelId, Design))
         -> Maybe (ModelEdit v Matrix (Maybe Text))
      matrixDelta oldName (model, (n1, d1, s1, t1), v2@(n2, d2, s2, t2))
         | n1 /= n2  = Just $ do
            goToEntity matrixId
            void $ modifyValue $ matrixName .~ n2
            return $ Just $ "Renamed " <> oldName
         | d1 /= d2  = Just $ do
            goToEntity matrixId
            void $ modifyValue $ matrixDescription .~ d2
            return $ Just $ "Edited " <> oldName <> " description"
         | s1 /= s2  = Just $ do
            goToEntity matrixId
            void $ modifyValue $ matrixDefaultInput .~ s2
            return $ Just $ "Edited " <> oldName <> " default inputs"
         | t1 /= t2  = Just $ do
            let
               ft1 = concatMap flatten t1
               ft2 = concatMap flatten t2
               z = zip ft1 ft2
               diff = find (uncurry (/=)) z
               l1 = length ft1
               l2 = length ft2
               changed
                  | l1 > l2  =  -- Something deleted from ft1.
                     "Deleted " <> designItemText model (maybe (last ft1) fst diff)
                        -- "last" is safe because length ft1 > 0
                  | l1 < l2  =  -- Something inserted in ft2.
                     "Inserted " <> designItemText model (maybe (last ft2) snd diff)
                  | otherwise = case diff of
                     Just (d, _) -> "Changed " <> designItemText model d
                     Nothing -> "Changed something"  -- Should never happen.
            goToEntity matrixId
            void $ setValue $ v2 ^. from matrixDesignIso
            return $ Just $ "Edit " <> oldName <> ": " <> changed
         | otherwise  = Nothing
      designMenu Nothing =
         Menu [[menuItem "Add group" $ TreeAddAfter $
               DesignQuery U.nil $ SortOrder [(nameField, Ascending)]]]
      designMenu (Just DesignQuery {}) = Menu [[
            menuItem "Add field column" $ TreeAddIn $ DesignColumn $
                  MatrixColumn "-title-" $ FieldColumn $ FieldColumnData nameField Nothing,
            menuItem "Add formula column" $ TreeAddIn $ DesignColumn $
                  MatrixColumn "-title-" $ ExprColumn True "Name ++ \": \" ++ Description",
            menuItem "Add query column" $ TreeAddIn $ DesignColumn $
                  MatrixColumn "-title-" $ QueryColumn U.nil,
            menuItem "Add lookup table column" $ TreeAddIn $ DesignColumn $
                  MatrixColumn "-title-" $ LookupColumn U.nil
         ], [
            menuItem "New group before" $ TreeAddBefore $
                  DesignQuery U.nil $ SortOrder [(nameField, Ascending)],
            menuItem "New group after" $ TreeAddAfter $
                  DesignQuery U.nil $ SortOrder [(nameField, Ascending)]
         ], [
            menuItem "Delete group" TreeDelete
         ]]
      designMenu (Just DesignColumn {}) = Menu [[
            menuItem "Insert field column before" $ TreeAddBefore $ DesignColumn $
                  MatrixColumn "-title-" $ FieldColumn $ FieldColumnData nameField Nothing,
            menuItem "Insert formula column before" $ TreeAddBefore $ DesignColumn $
                  MatrixColumn "-title-" $ ExprColumn True "Name ++ \": \" ++ Description",
            menuItem "Insert query column before" $ TreeAddBefore $ DesignColumn $
                  MatrixColumn "-title-" $ QueryColumn U.nil,
            menuItem "Insert lookup table column before" $ TreeAddBefore $ DesignColumn $
                  MatrixColumn "-title-" $ LookupColumn U.nil
         ], [
            menuItem "Insert field column after" $ TreeAddAfter $ DesignColumn $
                  MatrixColumn "-title-" $ FieldColumn $ FieldColumnData nameField Nothing,
            menuItem "Insert formula column after" $ TreeAddAfter $ DesignColumn $
                  MatrixColumn "-title-" $ ExprColumn True "Name ++ \": \" ++ Description",
            menuItem "Insert query column after" $ TreeAddAfter $ DesignColumn $
                  MatrixColumn "-title-" $ QueryColumn U.nil,
            menuItem "Insert lookup table column after" $ TreeAddAfter $ DesignColumn $
                  MatrixColumn "-title-" $ LookupColumn U.nil
         ], [
            menuItem "Delete column" TreeDelete
         ]]
      canBeParent DesignQuery {} = True
      canBeParent _ = False
      -- itemDialog :: (EntityClass v, Queryable v, HasLookups v) =>
      --    DesignItem -> Maybe (Dialog' (Model v) w DesignItem)
      itemDialog DesignQuery {} = Just groupDialog
      itemDialog (DesignColumn c) = Just $ case c ^. matrixColumnData of
         FieldColumn {} -> fieldDialog1
         ExprColumn {} -> exprDialog
         QueryColumn {} -> queryDialog
         LookupColumn {} -> lookupDialog
      -- Given a prism for the column data, return a prism for the column as a pair.
      columnPrism :: Prism' MatrixColumnData a -> Prism' MatrixColumn (Text, a)
      columnPrism p = prism'
               (\(txt1, dat1) -> MatrixColumn txt1 $ dat1 ^. re (clonePrism p)) $
               \(MatrixColumn txt dat) -> (txt, ) <$> (dat ^? clonePrism p)
      -- fieldDialog1 :: (EntityClass v) => Dialog' (Model v) w DesignItem
      fieldDialog1 = Dialog "Matrix Column" OkApplyButton $
         accum $ prismaticOver def (_DesignColumn . columnPrism _FieldColumn) $ form Vertical [
               ("Heading:", focusing _1 simpleTextBox),
               ("Column Text:", focusing (_2 . fieldColumnText) $ comboBox fieldNameMenu),
               ("Column Link:", focusing (_2 . fieldColumnLink) $ comboBox linkFieldMenu)
            ]
      exprDialog :: (EntityClass v) => Dialog' (Model v) w DesignItem
      exprDialog = Dialog "Field Column" OkApplyButton $ getInitialEnv $ \model ->
         let knownFields = withStampFields $ withBuiltIn $ modelFields model
         in  accum $ prismaticOver def (_DesignColumn . columnPrism _ExprColumn) $ form Vertical [
               ("Heading:", focusing _1 simpleTextBox),
               ("Multi-line display: ", focusing (_2 . _1) tickBox),
               ("Formula: ", focusing (_2 . _2) $ expressionEntry entityFunctions knownFields)
            ]
      queryDialog :: (EntityClass v, Queryable v) => Dialog' (Model v) w DesignItem
      queryDialog = Dialog "Query Column" OkApplyButton $ accum $
         prismaticOver ("", U.nil) (_DesignColumn . columnPrism _QueryColumn) $ form Vertical [
               ("Heading:", focusing _1 simpleTextBox),
               ("Query:", focusing (_2 . from (setMaybeIso . defaultIso U.nil)) queryPick)
            ]
      lookupDialog :: (EntityClass v, HasLookups v) => Dialog' (Model v) w DesignItem
      lookupDialog = Dialog "Lookup Column" OkApplyButton $ accum $
         prismaticOver ("", U.nil) (_DesignColumn . columnPrism _LookupColumn) $ form Vertical [
               ("Heading:", focusing _1 simpleTextBox),
               ("Lookup table:", focusing (_2 . from (setMaybeIso . defaultIso U.nil)) $
                  treeSelector lookupForest)
            ]
      groupDialog :: (EntityClass v, Queryable v) => Dialog' (Model v) w DesignItem
      groupDialog = Dialog "Matrix Column Group" OkApplyButton $ accum $
         prismaticOver (U.nil, SortOrder []) _DesignQuery $ form Vertical [
               ("Group Query:", focusing (_1 . from (setMaybeIso . defaultIso U.nil)) queryPick),
               ("Sort by:", focusingOver _2 sortOrderGadget)
            ]
      -- List of field names
      fieldNameMenu =
         map (\x -> ComboItem (x ^. fieldName) Nothing Nothing (fieldId x)) .
               sortOn (view fieldName) . M.elems . withStampFields . withBuiltIn . modelFields
      linkFieldMenu model = ComboItem " - " Nothing Nothing Nothing : cs
         where
            cs = map (\x -> ComboItem (x ^. fieldName) Nothing Nothing (Just $ fieldId x)) $
               M.elems $
               M.filter ((BuiltInDef ModelURL ==) . view fieldType) $
               modelFields model
      -- Model tree showing only queries.
      queryPick :: (EntityClass v, Queryable v) => Gadget' (Model v) w (Set ModelId)
      queryPick = treeSelector queryForest


-- | Gadget for setting the sort order on a list of entities.
sortOrderGadget :: (EntityClass v) => GadgetF' (Model v) w SortOrder
sortOrderGadget = getInitialEnv $ \model ->
      let
         fidMap = entityStampFields `M.union` withBuiltIn (modelFields model)
         nameMap = fieldsByName $ withBuiltIn $ modelFields model
         sortOrderIso :: Iso' SortOrder [(FieldName, SortDir)]
         sortOrderIso = iso
            (mapMaybe (\(k, o) -> (, o) . view fieldName <$> M.lookup k fidMap) . getSortOrder)
            (SortOrder . mapMaybe (\(k, o) -> (, o) . fieldId <$> M.lookup k nameMap))
            -- Not a true Iso because unknown fields are dropped. But that is the Right Thing here.
         fieldList = sort $ map (view fieldName) $ M.elems $
               withStampFields $ withBuiltIn $ modelFields model
      in focusing (sortOrderIso . extraIso) $ table
         [TableDelete, TableShuffle]
         [mkField "Field Name" _1 Nothing (EditCombo fieldList) Nothing,
         mkField "Order" _2 Nothing (EditEnum (T.pack . show) [Ascending, Descending]) Nothing]
         Nothing
   where
      extraIso :: Iso' [(FieldName, SortDir)] [(FieldName, SortDir)]
      extraIso = iso
         (++ [(blankText, Ascending)])
         (filter ((/= T.strip blankText) . T.strip . fst))
      blankText = " -?- "
