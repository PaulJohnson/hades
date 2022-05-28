{-# LANGUAGE Arrows #-}

{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

{- |

The basic types that are required for any kind of model:

* Packages

* Queries

* Matrices

* Reports

* Lookup Tables

Although this is in the @Model.Abstract@ module hierarchy it isn't perfectly abstract; there are
references to the "HadesRender" monad dragged in by query diagrams and some GTK stuff for report
writing.
-}

module Model.Abstract.BasicTypes (
  HasBasics (..),
  ModelBasics (..),
  _BasicPackage,
  _BasicQuery,
  _BasicMatrix,
  _BasicReport,
  _BasicLookup,
  basicsName,
  basicsIsDiagram,
  basicsDiagramWrapper,
  basicsDiagramEntityTypes,
  basicsCanHaveChild,
  rootMenu,
  packageMenu,
  basicsMenu,
  basicsActivation,
  basicsIcon
) where

import Control.Arrow
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Trans
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.UUID as UUID
import Hades.GI
import Model.Abstract.DiagramType
import Model.Abstract.PackageTree
import Model.GI.Export
import Model.Lookups.Base
import Model.Matrices.Base
import Model.Matrices.Design
import Model.Matrices.Excel
import Model.Matrices.Screen
import Model.Query.Diagram
import Model.Reflection.NamedRelation (Relation)
import Model.Reflection.References
import Model.Reflection.Reflective
import Model.Reflection.Values
import Model.Report.Base
import Model.Report.Dialogs
import Reactive.Banana.ArrowDialog
import Reactive.Banana.Menu


-- | Class for types that include all the basic types managed here.
--
-- This exists primarily to abbreviate the class context lists.
class (EntityClass v, HasReports v, HasLookups v) => HasBasics v where
  _Basics :: Prism' v (ModelBasics v)


-- | All models need variants for Packages, Queries, Matrices and Reports.
-- This is a convenience type which implements the necessary instances for these variants.
--
-- All model entity types should have "ToJSON" instances that include a @\"type\"@ field and
-- "FromJSON" instances that check the @\"type\"@ field.
data ModelBasics v =
  BasicPackage (Package v)
  | BasicQuery (QueryDiagram v)
  | BasicMatrix Matrix
  | BasicReport (Report v)
  | BasicLookup LookupTable
  deriving (Eq)

instance (EntityClass v, Queryable v) => ToJSON (ModelBasics v) where
  toJSON (BasicPackage p) = toJSON p
  toJSON (BasicQuery qd) = toJSON qd
  toJSON (BasicMatrix m) = toJSON m
  toJSON (BasicReport rpt) = toJSON rpt
  toJSON (BasicLookup tbl) = toJSON tbl

instance (EntityClass v, Queryable v) => FromJSON (ModelBasics v) where
  parseJSON v = do
    t <- withObject "Model base types" (.:? "type") v
        -- V0.5 did not have "type" fields in every model entity.
    case t of          -- Of the four types here, V0.5 only had queries.
      Nothing -> BasicQuery <$> parseJSON v
      Just "package" -> BasicPackage <$> parseJSON v
      Just "Query Diagram" -> BasicQuery <$> parseJSON v
      Just "matrix" -> BasicMatrix <$> parseJSON v
      Just "report" -> BasicReport <$> parseJSON v
      Just "lookup-table" -> BasicLookup <$> parseJSON v
      Just str -> fail $ "Unknown entity type: " <> str

instance (EntityClass v, Queryable v) => Reflective (ModelBasics v) where
  reflectiveName (BasicPackage p) = variantCoerce $ reflectiveName p
  reflectiveName (BasicQuery q) = variantCoerce $ reflectiveName q
  reflectiveName (BasicMatrix m) = variantCoerce $ reflectiveName m
  reflectiveName (BasicReport r) = variantCoerce $ reflectiveName r
  reflectiveName (BasicLookup t) = variantCoerce $ reflectiveName t
  reflectiveDefaults =
    map BasicPackage reflectiveDefaults ++
    map BasicQuery reflectiveDefaults ++
    map BasicMatrix reflectiveDefaults ++
    map BasicReport reflectiveDefaults ++
    map BasicLookup reflectiveDefaults
  reflectiveBuiltIn (BasicPackage p) = reflectiveBuiltIn p
  reflectiveBuiltIn (BasicQuery q) = reflectiveBuiltIn q
  reflectiveBuiltIn (BasicMatrix m) = reflectiveBuiltIn m
  reflectiveBuiltIn (BasicReport rpt) = reflectiveBuiltIn rpt
  reflectiveBuiltIn (BasicLookup tbl) = reflectiveBuiltIn tbl
  reflectiveGet (BasicPackage p) = reflectiveGet p
  reflectiveGet (BasicQuery q) = reflectiveGet q
  reflectiveGet (BasicMatrix m) = reflectiveGet m
  reflectiveGet (BasicReport rpt) = reflectiveGet rpt
  reflectiveGet (BasicLookup tbl) = reflectiveGet tbl
  reflectiveSet (BasicPackage p) = BasicPackage <$> reflectiveSet p
  reflectiveSet (BasicQuery q) = BasicQuery <$> reflectiveSet q
  reflectiveSet (BasicMatrix m) = BasicMatrix <$> reflectiveSet m
  reflectiveSet (BasicReport rpt) = BasicReport <$> reflectiveSet rpt
  reflectiveSet (BasicLookup tbl) = BasicLookup <$> reflectiveSet tbl
  reflectiveBuiltInRefs = mconcat [
      refTypeTableCoerce (reflectiveBuiltInRefs :: RefTypeTable (Package v)),
      refTypeTableCoerce (reflectiveBuiltInRefs :: RefTypeTable (QueryDiagram v)),
      refTypeTableCoerce (reflectiveBuiltInRefs :: RefTypeTable Matrix),
      refTypeTableCoerce (reflectiveBuiltInRefs :: RefTypeTable (Report v)),
      refTypeTableCoerce (reflectiveBuiltInRefs :: RefTypeTable LookupTable)
    ]
  reflectiveArrows = mconcat [
      M.mapKeys variantCoerce
        (reflectiveArrows :: Map (Variant (Package v)) (Relation, Relation)),
      M.mapKeys variantCoerce
        (reflectiveArrows :: Map (Variant (QueryDiagram v)) (Relation, Relation)),
      M.mapKeys variantCoerce
        (reflectiveArrows :: Map (Variant Matrix) (Relation, Relation)),
      M.mapKeys variantCoerce
        (reflectiveArrows :: Map (Variant (Report v)) (Relation, Relation)),
      M.mapKeys variantCoerce
        (reflectiveArrows :: Map (Variant LookupTable) (Relation, Relation))
    ]


basicsName :: (EntityClass v) => Lens' (ModelBasics v) Name
basicsName = singular $  -- Safe because this covers all five possibilities.
    (_BasicPackage . packageName) `failing`
    (_BasicQuery . queryDiagram . diagramEntityName) `failing`
    (_BasicMatrix . matrixName) `failing`
    (_BasicReport . reportName) `failing`
    (_BasicLookup . tableName)


basicsCanHaveChild :: (EntityClass v) => ModelBasics v -> Bool
basicsCanHaveChild (BasicPackage _) = True
basicsCanHaveChild _ = False


_BasicPackage :: (EntityClass v) => Prism' (ModelBasics v) (Package v)
_BasicPackage = prism BasicPackage $ \case {BasicPackage p -> Right p; v -> Left v}


_BasicQuery :: (EntityClass v) => Prism' (ModelBasics v) (QueryDiagram v)
_BasicQuery = prism BasicQuery $ \case {BasicQuery q -> Right q; v -> Left v}


_BasicMatrix :: (EntityClass v) => Prism' (ModelBasics v) Matrix
_BasicMatrix = prism BasicMatrix $ \case {BasicMatrix m -> Right m; v -> Left v}


_BasicReport :: (EntityClass v) => Prism' (ModelBasics v) (Report v)
_BasicReport = prism BasicReport $ \case {BasicReport rpt -> Right rpt; v -> Left v}


_BasicLookup :: Prism' (ModelBasics v) LookupTable
_BasicLookup = prism BasicLookup $ \case {BasicLookup tbl -> Right tbl; v -> Left v}


-- | True if this is a query diagram. False otherwise.
basicsIsDiagram :: (EntityClass v) => ModelBasics v -> Bool
basicsIsDiagram BasicQuery {} = True
basicsIsDiagram _ = False


-- | Wrapper for query digrams embedded in a "ModelBasics". All other values return @Nothing@.
basicsDiagramWrapper :: (EntityClass v, Queryable v) =>
  Entity v -> ModelEdit v (ModelBasics v) (Maybe (EntityWrapper HadesRender v))
basicsDiagramWrapper = withSubtype _BasicQuery . queryDiagramWrapper


-- | Diagram entities for basic types. Empty because query diagrams do not display entities.
basicsDiagramEntityTypes :: DiagramEntityTypes v
basicsDiagramEntityTypes = mempty


rootMenu :: (HasBasics v) =>
  Prism' v (ModelBasics v)
  -> Menu (ModelScript p v v (Maybe Text))
    -- ^ Application actions, such as adding new entities and diagrams.
  -> Menu (ModelScript p v v (Maybe Text))
rootMenu prsm appMenu = packageMenu1 prsm appMenu UUID.nil


-- | Context menu for packages in the model tree.
packageMenu :: (HasBasics v) =>
  Traversal' v (ModelBasics v)
  -> Menu (ModelScript p v v (Maybe Text))
    -- ^ Application actions, such as adding new entities and diagrams.
  -> ModelId
  -> Menu (ModelScript p v v (Maybe Text))
packageMenu prsm appMenu uuid =
  Menu [[menuItem "Properties" $ editEntityProperties uuid]] <>
  packageMenu1 prsm appMenu uuid <>
  Menu [[
      cloneEntityMenuItem uuid,
      menuItem "Delete" $ deleteEntity uuid
    ]]


-- | Menu for one of the built-in basic items in the application model tree.
basicsMenu :: (Editable HadesRender v, HasBasics v, HasDiagrams HadesRender v) =>
    Prism' v (ModelBasics v)
    -> Menu (ModelScript HadesRender v v (Maybe Text))
      -- ^ Application menu.
    -> Entity v
    -> Menu (ModelScript HadesRender v v (Maybe Text))
basicsMenu prsm appMenu ent =
  case ent ^? entityContents . prsm of
    Nothing -> Menu [[]]  -- Only occurs if the application calls this on the wrong variant.
    Just BasicPackage {} -> packageMenu prsm appMenu $ entityId ent
    Just BasicQuery {} -> promoteScript _QueryDiagram <$> diagramEntityMenu (entityId ent)
    Just BasicMatrix {} ->
      Menu [[
          menuItem "Run Matrix" $ matrixRun $ entityId ent,
          menuItem "Edit Matrix" $ matrixEdit $ entityId ent,
          menuItem "Export to Excel" $ matrixExport $ entityId ent
        ]] <>
      entityMenu ent
    Just BasicReport {} ->
      Menu [[
          menuItem "Preview Report" $ reportPreview $ entityId ent,
          menuItem "Write to File" $ reportRun $ entityId ent,
          menuItem "Edit Report" $ reportEdit prsm $ entityId ent
        ]] <>
      entityMenu ent
    Just BasicLookup {} ->
      Menu [[
          menuItem "Edit Lookup Table" $ lookupEdit $ entityId ent
        ]] <>
      entityMenu ent



-- | Activation for one of the built-in basic items in the application model tree.
basicsActivation :: (Editable HadesRender v, HasBasics v, HasDiagrams HadesRender v) =>
  Entity v
  -> ModelScript HadesRender v v (Maybe Text)
basicsActivation ent = case ent ^? entityContents . _Basics of
    Nothing -> return Nothing  -- Only occurs if the application calls this on the wrong variant.
    Just BasicPackage {} -> editEntityProperties $ entityId ent
    Just BasicQuery {} -> promoteScript _QueryDiagram $ queryActivation ent
    Just BasicMatrix {} -> matrixRun $ entityId ent
    Just BasicReport {} -> reportPreview $ entityId ent
    Just BasicLookup {} -> lookupEdit $ entityId ent


-- | Icon name for each variant.
basicsIcon :: (EntityClass v, Reflective v) => ModelBasics v -> Text
basicsIcon BasicPackage {} = "model-package"
basicsIcon BasicQuery {} = "model-query"
basicsIcon BasicMatrix {} = "model-matrix"
basicsIcon BasicReport {} = "model-report"
basicsIcon BasicLookup {} = "model-lookup"



-- | Internal function. Context menu for packages in the model tree, but without the "Delete"
-- option. Can also be used as the menu for the root.
packageMenu1 :: (HasBasics v) =>
  Traversal' v (ModelBasics v)
  -> Menu (ModelScript p v v (Maybe Text))
    -- ^ Application actions, such as adding new entities and diagrams.
  -> ModelId
  -> Menu (ModelScript p v v (Maybe Text))
packageMenu1 prsm appMenu uuid =
    (promoteScript prsm <$> Menu [[
      menuItem "Add package" $ do
        parentName <- lift $ goToEntity uuid >> currentName
        insertEntity basicPackage uuid >>= \case
          Nothing -> return Nothing
          Just (newName, _) -> return $ Just $
            "Added package " <> newName ^. nameText <> " to " <> parentName
      ]])
    <> appMenu
    <> (promoteScript prsm <$> Menu [[
        menuItem "Add query" $ do
          parentName <- lift $ goToEntity uuid >> currentName
          insertEntity basicQuery uuid
            >>= \case
              Nothing -> return Nothing
              Just (newName, _) -> return $ Just $
                  "Added query " <> newName ^. nameText <> " to " <> parentName,
        menuItem "Add matrix" $ do
          parentName <- lift $ goToEntity uuid >> currentName
          insertEntity basicMatrix uuid >>= \case
              Nothing -> return Nothing
              Just (newName, _) -> return $ Just $
                  "Added matrix " <> newName ^. nameText <> " to " <> parentName,
        menuItem "Add report" $ do
          parentName <- lift $ goToEntity uuid >> currentName
          insertEntity basicReport uuid >>= \case
              Nothing -> return Nothing
              Just (newName, _) -> return $ Just $
                  "Added report " <> newName ^. nameText <> " to " <> parentName,
        menuItem "Add lookup table" $ do
          parentName <- lift $ goToEntity uuid >> currentName
          insertEntity basicLookup uuid >>= \case
              Nothing -> return Nothing
              Just (newName, _) -> return $ Just $
                  "Added lookup table " <> newName ^. nameText <> " to " <> parentName
      ]])
  where
    basicPackage = Package (Name "Package ") mempty ^. re _Package
    basicQuery =
      QueryDiagram
          (DiagramEntity (Name "Query ") mempty ())
          everything
          mempty
      ^. re _QueryDiagram
    basicMatrix = Matrix (Name "Matrix ") "" S.empty [] ^. re _Matrix
    basicReport = Report (Name "Report ") "" Nothing Nothing [] ^. re _Report
    basicLookup =
      LookupTable (Name "Lookup ") UUID.nil UUID.nil UUID.nil mempty ^. re _LookupTable
    everything = S.fromList $ map reflectiveName reflectiveDefaults


-- | Open a matrix for editing in a diagram window. Returns "Nothing" to indicate that merely
-- opening the matrix does not change anything.
matrixEdit :: (EntityClass v, HasLookups v, HasMatrices v, Queryable v) =>
  ModelId -> ModelScript p v v (Maybe Text)
matrixEdit mId = do
  openEditor $ ViewWrapper {
      wrappedEntityId = mId,
      viewTag = "edit",
      viewInitial = (),
      viewInput = Right $ preview $ to modelContents . ix mId . entityContents . _Matrix,
      viewGadget = sendMap (Just . lift . withSubtype _Matrix) $ matrixDesignGadget mId
    }
  return Nothing


matrixRun :: (Editable p v, EntityClass v, Eq v, HasMatrices v, HasLookups v) =>
  ModelId -> ModelScript p v v (Maybe Text)
matrixRun mId = do
  lift $ goToEntity mId
  lift current >>= \case
    Nothing -> lift $ throwUser "Matrix has been deleted."
    Just ent ->
      lift (getMatrixWrapper ent) >>= \case
        Just wrapper -> openEditor wrapper
        Nothing -> lift $ throwInternal $ (ent ^. entityName . nameText) <> ": Not a matrix"
  return Nothing  -- Opening a matrix in a window has no effect in itself.


-- | Export the output of a matrix to an Excel spreadsheet.
matrixExport :: (EntityClass v, Eq v, HasLookups v, HasMatrices v) =>
  ModelId -> ModelScript p v v (Maybe Text)
matrixExport mId = do
  model <- lift getModel
  lift $ goToEntity mId
  nm <- lift currentName
  lift current >>= \case
    (preview (_Just . entityContents . _Matrix) -> Just matrix) -> do
      r <- openDialog
          (constantDialog $ matrixExportDialog model matrix)
          (matrix ^. matrixDefaultInput)
      case r of
        Nothing -> return Nothing
        Just targetSet -> do
          let
            targetId = fromMaybe UUID.nil $ S.lookupMin targetSet
          lift $ goToEntity targetId
          targetName <- lift currentName
          let title = nm <> " - " <> targetName
          case matrixCells model targetId matrix of
            Left msgs -> do
              void $ openDialog (constantDialog $ matrixErrorsDialog msgs) ()
              return Nothing
            Right (cells, warns) -> do
              goAhead <- if null warns
                then return True
                else isJust <$> openDialog
                  (constantDialog $ matrixWarningsDialog warns)
                  ()
              when goAhead $ performIO $
                writeMatrixToFile $ matrixBook model matrix title cells
              return Nothing
    _ -> lift $ throwInternal $ nm <> " is not a matrix: cannot export it."


reportEdit :: (EntityClass v, HasLookups v, HasMatrices v, HasDiagrams HadesRender v) =>
  Prism' v (ModelBasics v)
  -> ModelId
  -> ModelScript p v v (Maybe Text)
reportEdit prsm mId = do
  openEditor ViewWrapper {
      wrappedEntityId = mId,
      viewTag = "edit",
      viewInitial = (),
      viewInput = Right $ preview $
          to modelContents . ix mId . entityContents . prsm . _BasicReport,
      viewGadget =
          sendMap (Just . lift . withSubtype (prsm . _BasicReport)) $
          reportDesignGadget mId
    }
  return Nothing


reportPreview :: (EntityClass v, HasLookups v, HasMatrices v, HasReports v,
    HasDiagrams HadesRender v) =>
  ModelId
  -> ModelScript p v v (Maybe Text)
reportPreview mId = do
    lift $ goToEntity mId
    lift current >>= \case
      (preview (_Just . entityContents . _Report) -> Just rpt) -> do
        model <- lift getModel
        openDialog (constantDialog $ reportRunDialog True rpt) (reportRunDefault rpt) >>= \case
          Nothing -> return Nothing
          Just params -> do
            let tgtId = fromMaybe UUID.nil $ listToMaybe $ S.toList $ params ^. reportTarget
            lift $ goToEntity tgtId
            targetName <- lift currentName
            let title = rpt ^. reportName . nameText <> ": " <> targetName
            (blks, imgs, warns) <- lift $ runReport rpt tgtId
            goAhead <- if null warns
              then return True
              else isJust <$> openDialog (constantDialog $ reportWarningsDialog warns) ()
            when goAhead $ performIO $ previewReport model title blks imgs
            return Nothing
      _ -> lift $ throwInternal "Report has disappeared: cannot preview it."


reportRun :: (EntityClass v, HasLookups v, HasMatrices v, HasReports v,
    HasDiagrams HadesRender v) =>
  ModelId
  -> ModelScript p v v (Maybe Text)
reportRun mId = do
  lift $ goToEntity mId
  lift current >>= \case
    (preview (_Just . entityContents . _Report) -> Just rpt) -> do
      model <- lift getModel
      openDialog (constantDialog $ reportRunDialog False rpt) (reportRunDefault rpt) >>= \case
        Nothing -> return Nothing
        Just params -> do
          let
            targetId = fromMaybe UUID.nil $ listToMaybe $ S.toList $ params ^. reportTarget
          lift $ goToEntity targetId
          targetName <- lift currentName
          let title = rpt ^. reportName . nameText <> ": " <> targetName
          (blks, imgs, warns) <- lift $ runReport rpt targetId
          goAhead <- if null warns
            then return True
            else isJust <$> openDialog (constantDialog $ reportWarningsDialog warns) ()
          when goAhead $ performIO $
            writeReportToFile (params ^. reportFormat) title model blks imgs
          return Nothing
    _ -> lift $ throwInternal "Report has disappeared: cannot run it."


lookupEdit :: (EntityClass v, HasLookups v, HasDiagrams HadesRender v) =>
  ModelId
  -> ModelScript p v v (Maybe Text)
lookupEdit mId = do
    openEditor ViewWrapper {
        wrappedEntityId = mId,
        viewTag = "edit",
        viewInitial = (),
        viewInput = Right $ preview $ to modelContents . ix mId . entityContents . _LookupTable,
        viewGadget = proc ((), tbl1) -> do
          tbl2 <- send changeModel <<< accum lookupEditGadget -< tbl1
          returnA -< ((), tbl2)
      }
    return Nothing
  where
    changeModel update = Just $ lift $ do
      goToEntity mId
      nm <- currentName
      b <- modifyValue $ _LookupTable .~ update
      return $ if b then Just $ "Modified " <> nm else Nothing
