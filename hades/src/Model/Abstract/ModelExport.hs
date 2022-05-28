{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

{- |

Functions for exporting and importing parts of models.

There are two kinds of thing that can be exported:

* Metadata for data extensions and references.

* Subsets of the entities in a model, along with their relationships.

-}

module Model.Abstract.ModelExport (
  ExportList (..),
  ModelExport (..),
  makeExport,
  exportTree,
  exportEntities,
  importEntities,
  importMetadata
) where

import Control.Lens hiding ((.=))
import Control.Monad
import Data.Aeson
import Data.List hiding (delete)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Tree
import Model.Abstract.PackageTree
import qualified Data.UUID as U
import Model.Reflection.NamedRelation (NamedRelation)
import qualified Model.Reflection.NamedRelation as NR
import Model.Reflection.References
import Model.Reflection.Reflective
import Model.Reflection.Types
import Model.Reflection.Values


-- | A list of entities which has been flattened so that all parents are either in the
-- list or set to "U.nil"
newtype ExportList v = ExportList [Entity v] deriving (Semigroup, Monoid)

instance (ToJSON v) => ToJSON (ExportList v) where
  toJSON (ExportList vs) = toJSON vs

instance (FromJSON v, EntityClass v) => FromJSON (ExportList v) where
  parseJSON v = ExportList <$> parseJSON v


-- | Exported data on its way to or from serialization.
data ModelExport v = ModelExport {
    exportContents :: ExportList v,
    exportRelations :: NamedRelation ModelId,
    exportFields :: FieldTable,
    exportRefTypes :: RefTypeTable v,
    exportExtensions :: Map (Variant v) [FieldId]
  }

instance (ToJSON v) => ToJSON (ModelExport v) where
  toJSON v = object [
      "exportContents" .= exportContents v,
      "exportRelations" .= exportRelations v,
      "exportFields" .= exportFields v,
      "exportRefTypes" .= exportRefTypes v,
      "exportExtensions" .= exportExtensions v
    ]

instance (FromJSON v, EntityClass v) => FromJSON (ModelExport v) where
  parseJSON = withObject "Model Export" $ \v ->
    ModelExport <$>
        v .: "exportContents" <*>
        v .: "exportRelations" <*>
        v .: "exportFields" <*>
        v .: "exportRefTypes" <*>
        v .: "exportExtensions"


makeExport :: (EntityClass v) =>
  Set ModelId  -- ^ Entities in the model to be exported. "U.nil" means everything.
  -> Bool      -- ^ True if metadata is to be exported as well.
  -> ModelEdit v w (ModelExport v)
makeExport items metadata = do
  model <- getModel
  trees <- forM (S.toList items) $ \mId -> do
    goToEntity mId
    modelPackageTree >>= \case
      Nothing -> modelPackageForest
      Just t -> return [t]
  let
    (rels, exports) =
      exportEntities (modelRelations model) $ mconcat $ map exportTree $ concat trees
  return $ if metadata
    then ModelExport {
          exportContents = exports,
          exportRelations = rels,
          exportFields = modelFields model,
          exportRefTypes = modelRefTypes model,
          exportExtensions = modelExtensions model
        }
    else ModelExport {
          exportContents = exports,
          exportRelations = rels,
          exportFields = mempty,
          exportRefTypes = mempty,
          exportExtensions = mempty
        }


-- | Marshall a tree of entities into a list. The root entity is given a parent of "U.nil" because
-- it's parent isn't being exported.
exportTree :: Tree (Entity v) -> ExportList v
exportTree (Node root childs) =
  ExportList $ fixedRoot : concatMap flatten childs
  where
    fixedRoot = entityParent .~ U.nil $ root


-- | Locates all relations between items in the export list. Also removes duplicates in the
-- export list. If an item occurs twice, once with a parent and once without, the version with a
-- parent will be preferred.
exportEntities ::
  NamedRelation ModelId
  -> ExportList v
  -> (NamedRelation ModelId, ExportList v)
exportEntities nr (ExportList vs) =
    (filterRelations nr, ExportList $ M.elems entityTable)
  where
    mkPair ent = (entityId ent, ent)
    preferParented ent1 ent2 = if U.null $ ent1 ^. entityParent then ent2 else ent1
    entityTable = M.fromListWith preferParented $ map mkPair vs
    filterRelations :: NamedRelation ModelId -> NamedRelation ModelId
    filterRelations = NR.fromList . map (_2 %~ filterPair) . filterPair . NR.toList
    filterPair :: [(ModelId, a)] -> [(ModelId, a)]
    filterPair = filter ((`M.member` entityTable) . fst)


-- | The converse of "exportEntities". This will overwrite any entity in the model with the same
-- ID.
--
-- Relationships after the import are a union of the old and the new, except that where
-- the relation type has a \"1\" end the new relationship will replace the old.
importEntities :: (EntityClass v, Reflective v) =>
  ModelId   -- ^ New parent for the imported entities.
  -> NamedRelation ModelId
  -> ExportList v
  -> ModelEdit v w ()
importEntities location nr (ExportList vs) = withBaseType $ do
    checkModel
    goToEntity location
    try go >>= \case
      Left err -> throwUser $
        "Error found during import. Rolling back.\n\n" <> T.pack (show err)
      Right () -> return ()
  where
    -- Main routine for this function.
    go = do
      case evalModelEdit id (emptyModel "") (mkRawModel >> importFromModel) of
        Left err -> throwUser $ T.pack $ show err
        Right importAction -> importAction
      forM_ (NR.toList nr) $ \(id1, rs) ->
        forM_ rs $ \(id2, relName) -> do
          ent1 <- goToEntity id1 >> current
          ent2 <- goToEntity id2 >> current
          fromMaybe (return ()) $ addCheckedRelation relName <$> ent1 <*> ent2
      checkModel
    -- Traverse the package tree in this model and return an action to copy that tree into
    -- some other model in a top-down order.
    importFromModel = do
      goToRoot
      mapM_ insertTree <$> modelPackageForest
    insertTree (Node ent1 subTrees) = fromHere $ do
      -- Children will be automatically re-added by "addEntity" so insert without them.
      let ent2 = entityChildren .~ mempty $ ent1
      -- Delete any existing version of the imported entity.
      fromHere $ try (goToEntity (entityId ent2)) >>= \case
        Right () -> delete
        Left _ -> return ()
      -- Add the entity and any children.
      void $ addEntity $ \p -> entityParent .~ p $ ent2
      goToEntity $ entityId ent2
      mapM_ insertTree subTrees
    -- Assemble the imported entities into a temporary model.
    -- This is necessary so that the "tellLog" calls occur in a top-down order and hence can
    -- be applied to the GUI tree.
    mkRawModel = do
      forM_ vs $ \ent ->
        if U.null $ ent ^. entityParent
          then do
            goToRoot
            void $ addEntity $ const ent
          else do
            m <- getModel
            setModel $ m {modelContents = M.insert (entityId ent) ent $ modelContents m}
      getModel



importMetadata :: (EntityClass v) => ModelExport v -> ModelEdit v w ()
importMetadata import1 = do
    m <- getModel
    let
      newFields = M.union (exportFields import1) (modelFields m)
      RefTypeTable oldRefs = modelRefTypes m
      RefTypeTable importRefs = exportRefTypes import1
      newRefs = RefTypeTable $ M.union importRefs oldRefs
      newExts = M.unionWith combineExts (modelExtensions m) (exportExtensions import1)
    setModel $ m {modelFields = newFields, modelRefTypes = newRefs, modelExtensions = newExts}
  where
    combineExts es1 es2 = nub $ es1 ++ es2
