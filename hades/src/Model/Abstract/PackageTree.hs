{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.
-}



{- |

The abstract @Model@ uses "ModelId"s as references. Each entity in the model has a @ModelId@ by
which it can be accessed. Entities are organised into a tree. A built-in entity type called
@Package@ is normally used at the nodes, but other entities can have children too.

This structure provides a hybrid of functional and OO approaches. On the one hand the model is
persistent, while on the other hand a @ModelId@ acts like a pointer to an entity within the model.
Of course this means that the model is victim to all the usual vices of OO programming; stale
references and memory leaks are both possible. The package tree is stored purely in terms of
references, although it is possible to extract it as a "Tree" for easier traversal. Hence the
model structure can become inconsistent in several ways. In theory the interface provided here
should be \"safe\" in that it can't be used to create an inconsistent model. The "checkModel"
function can be used
to verify that the model is still consistent. If an inconsistency is detected at any point
then an "InternalError" will be returned.
-}


module Model.Abstract.PackageTree (
  -- ** Model Structure
  ModelId,
  Stamp,
  thisChange,
  EntityClass (..),
  Name (..),
  Package (..),
  packageName,
  packageMeta,
  nameText,
  nameField,
  descriptionField,
  Model (modelName, modelRoot, modelContents, modelMeta, modelRelations, modelFields,
      modelRefTypes, modelExtensions),
  trialModelSizeLimit,
  studentModelSizeLimit,
  emptyModel,
  Entity (entityId, entityCreated),
  entityName,
  entityParent,
  entityChildren,
  entityContents,
  entityExtensions,
  entityProperties,
  entityVariantProperties,
  entityModified,
  entityStampsAsFields,
  entityPropertiesWithStamps,
  entityStampFields,
  withStampFields,
  -- ** Model Editing Monad
  ModelEditState,
  modelStartState,
  stateModel,
  stateLog,
  ModelError (..),
  ModelEdit,
  ModelEditLog (..),
  ModelUpdate (..),
  runModelEdit,
  execModelEdit,
  evalModelEdit,
  sequenceEvents,
  queryModel,
  monadic,
  throwUser,
  throwInternal,
  try,
  -- ** Editing Actions
  getModel,
  setModel,
  tellLog,
  getModelName,
  setModelName,
  withSubtype,
  withBaseType,
  usingTraversal,
  searchEntities,
  queryRelations,
  modifyRelations,
  addCheckedRelation,
  goToRoot,
  goToEntity,
  goToEntity1,
  depth,
  current,
  currentId,
  recordModified,
  currentIsModified,
  currentName,
  currentPath,
  getData,
  setData,
  getMeta,
  setMeta,
  getVariantData,
  setVariantData,
  currentChildren,
  moveUp,
  moveDown,
  followPath,
  mkModelId,
  mkPackage,
  mkEntity,
  addEntity,
  cloneEntity,
  unsafeEntityClone,
  createPath,
  modelPackageTree,
  modelPackageForest,
  fromHere,
  delete,
  modelNewName,
  renameEntity,
  modifyValue,
  setValue,
  moveEntity,
  checkModel,
  -- * JSON Utility
  checkType
) where

import Control.Arrow
import Control.Lens hiding ((.=))
import Control.Monad.Except
import Control.Monad.State
import Data.Aeson
import Data.Aeson.Types (Parser, toJSONKeyText)
import qualified Data.HashMap.Strict as HM
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Tree
import Data.UUID
import qualified Data.UUID as U
import Data.UUID.Generate
import Model.Reflection.Reflective
import Model.Reflection.NamedRelation (NamedRelation)
import qualified Model.Reflection.NamedRelation as NR
import Model.Reflection.Parser
import Model.Reflection.References
import Model.Reflection.Types
import Model.Reflection.Values
import Reactive.Banana.Combinators hiding ((<>))
import System.Hades.DataFiles
import System.IO.Unsafe (unsafePerformIO)
import Text.NaturalOrder

-- | An entity within the model.
type ModelId = UUID


-- | Wrapper for text fields to be compared using natural order sort.
-- Note that @Name \"Foo\" == Name \"foo\"@
newtype Name = Name Text deriving (Read, Show)

-- | The underlying text of a Name.
nameText :: Iso' Name Text
nameText = iso (\(Name txt) -> txt) Name

instance Eq Name where
  Name x == Name y  = naturalOrder x y == EQ

instance Ord Name where
  compare (Name x) (Name y) = naturalOrder x y

instance ToJSON Name where toJSON (Name txt) = toJSON txt

instance FromJSON Name where parseJSON = withText "name" $ return . Name . T.strip

instance ToJSONKey Name where
  toJSONKey = toJSONKeyText $ view nameText

instance FromJSONKey Name where
  fromJSONKey = FromJSONKeyText $ Name . T.strip

-- | The user name and date.
type Stamp = (Text, Day)

-- | Every entity in a model has a mutable name, is reflective, and may have children.
--
-- Law: "reflectiveBuiltIn" must contain "nameField".
class (Eq a, Eq (PackageMeta a), ToJSON (PackageMeta a), FromJSON (PackageMeta a),
    Monoid (PackageMeta a), Reflective a) => EntityClass a where
  -- | The metadata to be stored in Packages.
  type PackageMeta a :: *
  -- | The name of this entity.
  name :: Lens' a Name
  -- | Cast this entity to and from package, if it is one.
  _Package :: Prism' a (Package a)
  -- | True if the first argument can have the second argument as a child.
  -- This should not depend on variable attributes of entities in a model.
  entityCanHaveChild :: a -> a -> Bool
  -- | True if this entity can be moved somewhere else in the tree.
  entityCanMove :: a -> Bool
  -- | Entities exported by this entity.
  entityExports :: Entity a -> ModelEdit a a [Entity a]
  -- | Clone of the entity, giving it the new name, if possible. The clone should be inserted as a
  -- child of the current location with a new name, and the new entity returned.
  entityClone :: Entity a -> Name -> Maybe (ModelEdit a a (Entity a))
  -- | Formula functions for entities in models.
  entityFunctions :: FunctionTable (Model a)


-- | Constant text for the reflective \"Name\" field. Every entity should have this.
nameField :: FieldId
nameField = fieldsByName builtInFields ^?! ix "Name" . to fieldId  -- Safe because its a constant.


-- | Constant text for the reflective \"Description\" field. If an entity has a descriptive text
-- field then this should be the name.
descriptionField :: FieldId
descriptionField = fieldsByName builtInFields ^?! ix "Description" . to fieldId


-- | The stamp to be applied to model changes. The date is
-- current date in the current timezone treated as a pure value.
--
-- Under the hood this uses the infamous @unsafePerformIO@. It is also agressively inlined so
-- it *should* stay up to date even if the program is run past midnight, rather than being
-- considered a global constant that only gets computed once.
thisChange :: Stamp
thisChange = unsafePerformIO $ do
  u <- getUserName
  d <- localDay . zonedTimeToLocalTime <$> getZonedTime
  return (T.pack u, d)
{-# INLINE thisChange #-}  -- Insist on this even though hlint disagrees.


-- | The simplest type that can be put in a model. It carries a name and some metadata,
-- and can have anything as a child.
data Package a = Package {
    _packageName :: Name,
    _packageMeta :: PackageMeta a
  }

instance (EntityClass a) => Eq (Package a) where
  Package n1 m1 == Package n2 m2 = n1 == n2 && m1 == m2

instance (EntityClass a) => ToJSON (Package a) where
  toJSON (Package nm meta) = object [
      "type" .= ("package" :: Text),
      "name" .= nm,
      "meta" .= meta
    ]

instance (EntityClass a) => FromJSON (Package a) where
  parseJSON = withObject "Package" $ \v -> do
    t <- v .: "type"
    when (t /= "package") $ fail $ t <> " is not a package."
    Package <$> v .: "name" <*> v .:? "meta" .!= mempty

instance (EntityClass a) => Reflective (Package a) where
  reflectiveName _ = Variant "Package"
  reflectiveDefaults = [Package (Name "") mempty]
  reflectiveBuiltIn _ = [nameField]
  reflectiveGet p = M.singleton nameField $ ExtText $ p ^. packageName . nameText
  reflectiveSet p = do
    nm <- Name <$> extract (p ^. packageName . nameText) _ExtText nameField
    return $ packageName .~ nm $ p
  reflectiveBuiltInRefs = mempty
  reflectiveArrows = mempty


packageName :: (EntityClass a) => Lens' (Package a) Name
packageName = lens _packageName $ \s nm -> s {_packageName = nm}

packageMeta :: (EntityClass a, EntityClass b) =>
    Lens (Package a) (Package b) (PackageMeta a) (PackageMeta b)
packageMeta = lens _packageMeta $ \s m -> s {_packageMeta = m}


-- | Entity stored in the model. The Id and Created fields are functions because they are not
-- changed during the life of the entity. Everything else is a lens.
data Entity v = Entity {
    entityId :: ModelId,
    _entityParent :: ModelId, -- ^ Nil if the parent is the root.
    _entityChildren :: Map Name ModelId,  -- ^ Models consist of nested entities indexed by name.
    _entityContents :: v,
    _entityExtensions :: ExtensionValues, -- ^ Excluding values built in to @v@.
    entityCreated :: Stamp,  -- ^ Date the entity was created and who did it.
    _entityModified :: Stamp
  } deriving (Eq, Show)

instance Functor Entity where
  fmap = over entityContents

instance (ToJSON v) => ToJSON (Entity v) where
  toJSON (Entity ident parent children1 contents exts created modified) =
    object [
        "identity" .= ident,
        "parent" .= parent,
        "children" .= children1,
        "contents" .= contents,
        "extensions" .= exts,
        "created" .= created,
        "modified" .= modified
      ]
{-
Old V0.5 code kept for reference. Packages were not first-class and extensions were the
the responsibility of each item.

  toJSON (Entity ident parent contents) =
    object $ ["identity" .= ident, "parent" .= parent] ++ case contents of
      EntityPackage n es -> ["name" .= n, "package" .= M.mapKeys (^. nameText) es ]
      EntityItem v -> ["item" .= v]
-}

instance (EntityClass v, FromJSON v) => FromJSON (Entity v) where
  parseJSON = withObject "Entity" $ \v ->
      if HM.member "contents" v
        then parseV06 v
        else parseV05 v  -- Also parses old-style version 0.5 model entities.
    where
      parseV06 v =
        Entity <$>
          v .: "identity" <*>
          v .: "parent" <*>
          v .: "children" <*>
          v .: "contents" <*>
          v `parseExtVals` "extensions" <*>
          v .:? "created" .!= thisChange <*>
          v .:? "modified" .!= thisChange  -- For legacy files, timestamp is now.
      parseExtVals :: Object -> Text -> Parser ExtensionValues
      parseExtVals v nm = v .: nm <|> M.mapKeys migrateField <$> v .: nm
      parseV05 v =
          (v .:? "package") >>= \case
            Nothing -> do  -- Non-package item
              item <- v .: "item"  -- JSON object which may contain an "extensions" field.
              Entity <$>
                v .: "identity" <*>
                v .: "parent" <*>
                return mempty <*>  -- No children in V0.5 except in parents.
                v .: "item" <*>
                (M.mapKeys migrateField <$> (item .:? "extensions" .!= mempty)) <*>
                return thisChange <*>   -- No creation or modified stamps in V0.5.
                return thisChange
            Just children1 ->  -- Package item
              Entity <$>
                v .: "identity" <*>
                v .: "parent" <*>
                return (M.mapKeys Name children1) <*>
                (review _Package . flip Package mempty . Name <$> v .: "name") <*>
                return mempty <*>
                return thisChange <*>
                return thisChange

        {- Old V 0.5 code kept for reference.

          Entity <$> v .: "identity" <*> v .: "parent" <*> (
            (EntityPackage <$> v .: "name" <*> (M.mapKeys Name <$> v .: "package"))
            <|> (EntityItem <$> v .: "item")
          )
        -}


-- | Each Entity has a name. This is the last component in its pathname.
entityName :: (EntityClass v) => Lens' (Entity v) Name
entityName = entityContents . name

-- | Each Entity has a parent. This is the "null" ModelID if the parent is the root.
entityParent :: Lens' (Entity v) ModelId
entityParent = lens _entityParent $ \s p -> s {_entityParent = p}

-- | Each entity has zero or more children. The model semantics may impose limits on which
-- entities can have children.
entityChildren :: Lens' (Entity v) (Map Name ModelId)
entityChildren = lens _entityChildren $ \s c -> s {_entityChildren = c}

-- | The model value stored in this entity.
entityContents :: Lens (Entity v1) (Entity v2) v1 v2
entityContents = lens _entityContents $ \s c -> s {_entityContents = c}

-- | Extension values in addition to the built-in fields.
entityExtensions :: Lens' (Entity v) ExtensionValues
entityExtensions = lens _entityExtensions $ \s e -> s {_entityExtensions = e}

-- | Entity modifed stamp (user and date).
entityModified :: Lens' (Entity v) Stamp
entityModified = lens _entityModified $ \s d -> s {_entityModified = d}


-- | Extension values including the built-in fields. Doesn't quite follow the lens laws because
-- an attempt to set a built-in field with the wrong type may be ignored.
entityProperties :: (EntityClass v) => Lens' (Entity v) ExtensionValues
entityProperties = lens getProperties setProperties
  where
    getProperties e = M.union (reflectiveGet $ e ^. entityContents) (e ^. entityExtensions)
    setProperties e1 newExts1 = let
        v1 = e1 ^. entityContents
        (v2, newExts2) = runState (reflectiveSet v1) newExts1
      in entityContents .~ v2 $ entityExtensions .~ newExts2 $ e1


-- | As for "entityProperties", but limits both reading and writing to fields defined in the
-- entity variant.
entityVariantProperties :: (EntityClass v) => Model v -> Lens' (Entity v) ExtensionValues
entityVariantProperties model = lens getProperties setProperties
  where
    fields e = S.fromList $
      fromMaybe [] (modelExtensions model ^? ix (e ^. entityContents . to reflectiveName))
    getProperties e =
      M.union
        (reflectiveGet $ e ^. entityContents)
        (M.restrictKeys (e ^. entityExtensions) $ fields e)
    setProperties e1 newExts1 = let
        v1 = e1 ^. entityContents
        (v2, newExts2) = runState (reflectiveSet v1) newExts1
        newExts3 = M.restrictKeys newExts2 (fields e1)
      in entityContents .~ v2 $ entityExtensions %~ M.union newExts3 $ e1


-- | Converts the entity creation and modification stamps into extension fields called
-- \"Created\", \"CreatedBy\", \"Modified\" and \"ModifiedBy\".
entityStampsAsFields :: (EntityClass v) => Entity v -> ExtensionValues
entityStampsAsFields e =
  M.union
    (stampAsFields created createdBy $ entityCreated e)
    (stampAsFields modified modifiedBy $ e ^. entityModified)
  where
    stampAsFields dateField userField (user, date) =
      M.fromList [(userField, ExtText user), (dateField, ExtDate date)]
    entityStampsByName = fieldsByName entityStampFields
    created = fieldId $ fromJust $ M.lookup "Created" entityStampsByName
    createdBy = fieldId $ fromJust $ M.lookup  "CreatedBy" entityStampsByName
    modified = fieldId $ fromJust $ M.lookup  "Modified" entityStampsByName
    modifiedBy = fieldId $ fromJust $ M.lookup  "ModifiedBy" entityStampsByName


-- | Union of the entity properties with the creation and modification stamps.
--
-- This is a bit of a mess. The stamps are treated like extension values for expressions but not
-- for anything else. Hence it is up to the caller to decide whether to include the stamps or not.
entityPropertiesWithStamps :: (EntityClass v) => Entity v -> ExtensionValues
entityPropertiesWithStamps e = M.union (e ^. entityProperties) (entityStampsAsFields e)


-- | The list of stamp fields definitions. These are read-only.
entityStampFields :: FieldTable
entityStampFields = M.fromList $ map mkPair [
    ("Created", One, BuiltInDef ModelDate),
    ("CreatedBy", One, BuiltInDef ModelText),
    ("Modified", One, BuiltInDef ModelDate),
    ("ModifiedBy", One, BuiltInDef ModelText)
  ]
  where
    mkPair (nm, multi, typ) = let fid = migrateField nm in (fid, Field True fid nm multi typ)



-- | The abstract concept of a model.
data Model v = Model {
    modelName :: Text,
    modelRoot :: Map Name ModelId,    -- ^ Top level of model name tree.
    modelContents :: Map ModelId (Entity v),  -- ^ Entities keyed by ModelID
    modelMeta :: PackageMeta v,   -- ^ Metadata for the notional root package.
    modelRelations :: NamedRelation ModelId,
      -- ^ Each side of a relationship can be with an entity on its own, or an export of
      -- the entity. The first ID is the base entity. The second ID is either the ID
      -- of an export or "U.nil" if the relationship is with the base entity.
    modelFields :: FieldTable,
    modelRefTypes :: RefTypeTable v,
    modelExtensions :: Map (Variant v) [FieldId]
  }


-- Haskell "deriving" can't deduce (Eq (PackageMeta v)), so we have an explicit instance instead.
instance (Eq v, EntityClass v) => Eq (Model v) where
  m1 == m2  = and [
        by modelName,
        by modelRoot,
        by modelContents,
        by modelMeta,
        by modelRelations,
        by modelFields,
        by modelRefTypes,
        by modelExtensions
      ]
    where
      by :: (Eq a) => (Model v -> a) -> Bool
      by f = f m1 == f m2


instance (EntityClass v, ToJSON v) => ToJSON (Model v) where
  toJSON (Model mName root contents meta relations fields refs exts) =
    object [
      "modelName" .= mName,
      "modelRoot" .= M.mapKeys (^. nameText) root,
      "modelContents" .= M.elems contents,
      "modelMeta" .= meta,
      "modelRelations" .= relations,
      "modelFields" .= M.elems fields,
      "modelRefTypes" .= refs,
      "modelExtensions" .= M.mapKeys (view variantName) exts]

instance (EntityClass v, FromJSON v) => FromJSON (Model v) where
  parseJSON = withObject "model" $ \v ->
      if HM.member "modelFields" v then parseModel06 v else parseModel05 v
    where
      parseModel06 v = Model <$>
        (T.strip <$> (v .: "modelName")) <*>
        v .: "modelRoot" <*>
        (M.fromList . map (entityId &&& id) <$> v .: "modelContents") <*>
        v .:? "modelMeta" .!= mempty <*>
        v .: "modelRelations" <*>
        v `parseFieldTable` "modelFields" <*>
        v .: "modelRefTypes" <*>
        v `parseExtensions` "modelExtensions"
      parseModel05 v = do
        mName <- T.strip <$> v .: "modelName"
        root <- M.mapKeys Name <$> v .: "modelRoot"
        contents <- M.fromList . map (entityId &&& id) <$> v .: "modelContents"
        relations <- v .: "modelRelations"
        typeTable <- v .:? "modelTypes" .!= M.empty
        refs <- v .:? "modelRefTypes" .!= mempty
        exts1 :: Map Text [Object] <- v .:? "modelExtensions" .!= M.empty
        exts2 :: Map Text [Field] <- mapM (mapM $ parseField05 typeTable) exts1
        let
          allFields = M.fromList $ map (\f -> (fieldId f, f)) $ concat $ M.elems exts2
          exts3 = M.mapKeys Variant $ fmap (map fieldId) exts2
        return $ Model mName root contents mempty relations allFields refs exts3
      parseField05 :: Map Text TypeDef -> Object -> Parser Field
      parseField05 typeTable v = do
        nm <- v .: "name"
        mult <- v .: "multi"
        typ <- v .: "type"
        case M.lookup typ typeTable of
          Just t -> return $ Field False (migrateField nm) nm mult t
          Nothing -> return $ Field False (migrateField nm) nm mult $ BuiltInDef ModelText
      -- Migrate from fields identified by name to identifying by UUID.
      parseFieldTable :: Object -> Text -> Parser FieldTable
      parseFieldTable v nm =
        M.fromList . map (\f -> (fieldId f, f)) <$> (
            v .: nm <|>
            M.elems <$> (v .: nm :: Parser (Map Text Field))
          )
      parseExtensions :: Object -> Text -> Parser (Map (Variant v) [FieldId])
      parseExtensions v nm =
        v .: nm <|> (fmap (map migrateField) <$> v .: nm)

instance (EntityClass v) => Show (Model v) where
  show model = case evalModelEdit id model getForest of
      Right forest ->
        let forestNames = map (fmap (show . (\v -> (v ^. entityName, entityId v)))) forest
        in "Model " ++ T.unpack (modelName model) ++ "\n" ++ drawForest forestNames
      Left err -> "showModelTree: " ++ show err
    where
      getForest = do
        checkModel
        modelPackageForest


-- | Create a new model with no contents. The argument is the model name.
emptyModel :: (EntityClass v) => Text -> Model v
emptyModel mName = Model mName M.empty M.empty mempty NR.empty M.empty mempty M.empty


-- | Maximum number of entities permitted for trial licenses.
trialModelSizeLimit :: Int
trialModelSizeLimit = 300


-- | Maximum number of entities permitted for student licenses.
studentModelSizeLimit :: Int
studentModelSizeLimit = 500


-- | Adds the stamp fields to the field table. The results should only be used in a read-only
-- context.
withStampFields :: FieldTable -> FieldTable
withStampFields = M.union entityStampFields

-- ---------------------------------------------------------------------
-- Editing Models
-- ---------------------------------------------------------------------

-- | In order to pass information about model changes out to a GUI, the ModelEdit monad keeps
-- a log of changes made by its primitives. In all cases the nil ModelID means the whole model.
data ModelEditLog v =
  ModelAdd ModelId (Model v)  -- ^ The entity has been added to the model.
  | ModelDelete ModelId (Model v)  -- ^ The entity to be deleted, and the model beforehand.
  | ModelMove ModelId (Model v) (Model v) -- ^ The entity moved, and the model before and after.
  | ModelRename ModelId (Model v) (Model v)  -- ^ The entity, and the model before and after.
  | ModelOther (Model v) (Model v)  -- ^ A change that does not affect the tree view.

instance (EntityClass v) => Show (ModelEditLog v) where
  show (ModelAdd uuid _) =
    "ModelAdd " <> show uuid <> "\n"
  show (ModelDelete uuid _) =
    "ModelDelete " <> show uuid <> "\n"
  show (ModelMove uuid _ _) =
    "ModelMove " <> show uuid <> "\n"
  show (ModelRename uuid _ _) =
    "ModelRename " <> show uuid <> "\n"
  show (ModelOther _ _) =
    "ModelOther " <> "/n"


-- | Errors within the "ModelEdit" monad are either user errors (e.g. no such path) or
-- internal errors where the model has become inconsistent. "checkModel" performs a systematic
-- check, but other functions can also fail due to internal inconsistencies.
--
-- A @UserError@ does not necessarily mean that the end user has made an error; it is more that
-- the problem is not an internal inconsistency in the model state.
data ModelError = UserError Text | InternalError Text

instance Show ModelError where
  show (UserError txt) = "Model library client error: " ++ T.unpack txt
  show (InternalError txt) = "Model library internal error: " ++ T.unpack txt


-- | Internal state record for "ModelEdit".
data ModelEditState v = ModelEditState {
  stateGenerator :: Generator,
  stateModel :: Model v,
  statePosition :: ModelId,
  stateLogR :: [ModelEditLog v]  -- ^ Reverse order.
}


-- | The initial state for running ModelEdits.
modelStartState ::
  Generator
    -- ^ Random generator for ModelIDs. Cryptographic because StdGen doesn't have enought bits.
  -> Model v
    -- ^ The model being accessed.
  -> ModelEditState v
modelStartState gen model = ModelEditState gen model nil []


-- | The log of changes made so far.
stateLog :: ModelEditState v -> [ModelEditLog v]
stateLog = reverse . stateLogR


-- | Internal function to access the current entity, or @Nothing@ if at root.
lookupCurrent :: (EntityClass v) => ModelEditState v -> Either ModelError (Maybe (Entity v))
lookupCurrent ms = do
  let
    u = statePosition ms
    m = stateModel ms
  if U.null u
    then return Nothing
    else case M.lookup u $ modelContents m of
      Nothing -> Left $ InternalError $ "ModelID " <> toText u <> " has gone missing."
      Just v -> Right $ Just v


-- | The @ModelEdit@ monad provides a view on the underlying data through
-- a @Prism' v w@. This allows ModelEdit actions to be written which target the subset of the
-- data stored in the model of type @w@ without having to know about the underlying type of
-- @v@. So for instance given:
--
-- > type FooBar = Either Foo Bar
-- > start :: ModelState FooBar
--
-- we can have an edit action that only deals with objects of type @Foo@:
--
-- > action :: ModelEdit v Foo ()
--
-- which can be executed with
--
-- > runModelEdit _Left start action
--
-- The monadic state includes a cursor which can be moved up and down
-- the model tree, and hence used to access different entities in the model.
-- ModelIDs provide something like object identity. The ModelIDs
-- are visible and can be used in unsafe ways, for instance to construct a cycle in the tree.
newtype ModelEdit v w a =
  ModelEdit (Traversal' v w -> ModelEditState v -> Either ModelError (a, ModelEditState v))

instance Functor (ModelEdit v w) where
  fmap f (ModelEdit act) = ModelEdit $ \prsm ms1 -> case act prsm ms1 of
    Left e -> Left e
    Right (v, ms2) -> Right (f v, ms2)

instance Applicative (ModelEdit v w) where
  pure v = ModelEdit $ \_ ms -> Right (v, ms)
  (ModelEdit apF) <*> (ModelEdit vF) = ModelEdit $ \prsm ms1 -> do -- Either monad
    (f, ms2) <- apF prsm ms1
    (v, ms3) <- vF prsm ms2
    return (f v, ms3)

instance Monad (ModelEdit v w) where
  (ModelEdit vF) >>= f = ModelEdit $ \prsm ms1 ->
    case vF prsm ms1 of
      Left err -> Left err
      Right (v, ms2) -> let ModelEdit f2 = f v in f2 prsm ms2

instance MonadError ModelError (ModelEdit v w) where
  throwError e = ModelEdit $ \_ _ -> Left e
  catchError (ModelEdit fv) ef = ModelEdit $ \prsm ms ->
    case fv prsm ms of
      Left e -> let ModelEdit f = ef e in f prsm ms
      v -> v


-- | Updates to the model carry a return type, a model update function, and the log of edits made.
data ModelUpdate v a = ModelUpdate {
    updateValue :: a,
    updateNewModel :: Model v,
    updateEditLog :: [ModelEditLog v]
  }

instance Functor (ModelUpdate v) where
  fmap f (ModelUpdate v m l) = ModelUpdate (f v) m l


-- | Run an editing action, starting at the root. If the action is successful then it returns
-- the monadic value, the updated model, and a set of changes.
--
-- The set of changed ModelIDs will list the entities that changed value, and the parents of those
-- inserted, deleted or renamed. In the case of a move both parents will be listed.
--
-- The "ModelUpdate" contains a duplicate of the edit logs in the "ModelEditState", so use
-- one or the other but not both.
runModelEdit ::
  Traversal' v w
  -> ModelEditState v
  -> ModelEdit v w a
  -> Either ModelError (ModelUpdate v a, ModelEditState v)
runModelEdit prsm st (ModelEdit f) =
  case f prsm st of
    Left e -> Left e
    Right (v, ms) -> Right (ModelUpdate v (stateModel ms) (stateLog ms), ms)


-- | As for "runModelEdit", except it ignores the result.
execModelEdit ::
  Traversal' v w
  -> ModelEditState v
  -> ModelEdit v w a
  -> Either ModelError (Model v, [ModelEditLog v])
execModelEdit prsm st act =
  case runModelEdit prsm st act of
    Left err -> Left err
    Right (ModelUpdate _ model changes, _) -> Right (model, changes)


-- | As for "runModelEdit", except it ignores any change to the model.
evalModelEdit :: Traversal' v w -> Model v -> ModelEdit v w a -> Either ModelError a
evalModelEdit prsm m act = case runModelEdit prsm (modelStartState g m) act of
    Left err -> Left err
    Right (ModelUpdate v _ _, _) -> Right v
  where g = unsafePerformIO newGenerator
    -- This is safe because 'g' is only used to create new ModelIDs for
    -- additions to the model, and these are all thrown away.


-- | Evaluate a model action in the context of a model behavior and an event. Any changes made
-- to the model by the edit action are ignored.
--
-- This is actually a Reactive Banana wrapper around "evalModelEdit"
queryModel ::
  Traversal' v w
  -> Behavior (Model v)
  -> Event a
  -> (a -> ModelEdit v w b)
  -> (Event ModelError, Event b)
queryModel prsm modelB e f = split $ evalModelEdit prsm <$> modelB <@> (f <$> e)


-- | Combine monadic events by sequencing. This is intended for events carrying
-- @ModelEdit v w Text@, but could be used for any other monad.
sequenceEvents :: (Monad m) => [Event (m (Maybe Text))] -> Event (m (Maybe Text))
sequenceEvents = foldr (unionWith seqResults) never
  where
    -- Simultaneous events probably can't happen. If so then this is dead code.
    seqResults act1 act2 = do
      str1 <- act1
      str2 <- act2
      return $ case (str1, str2) of
        (Nothing, Nothing) -> Nothing
        (Just s, Nothing) -> Just s
        (Nothing, Just s) -> Just s
        (Just s1, Just s2) -> Just $ s1 <> ", " <> s2


-- | Internal function for logging changes.
tellLog :: ModelEditLog v -> ModelEdit v w ()
tellLog x = ModelEdit $ \_ ms1 -> Right ((), ms1{stateLogR = x : stateLogR ms1})


-- | Convenience function for running ModelEdit actions in other monads. If the model edit
-- succeeds then return its value, otherwise return the first argument.
monadic :: (Show e, Monad m) => a -> Either e a -> m a
monadic v (Left _) = return v
monadic _ (Right v) = return v


-- | Throw an error at the user level (e.g. named element does not exist)
throwUser :: Text -> ModelEdit v w a
throwUser = throwError . UserError

-- | Throw an internal error due to failure of the Model to maintain consistency.
throwInternal :: Text -> ModelEdit v w a
throwInternal = throwError . InternalError


-- | Try an action that may cause a user error. If it succeeds then return the result, otherwise
-- return the error message with no changes made. Internal errors are not caught.
try :: ModelEdit v w a -> ModelEdit v w (Either Text a)
try (ModelEdit act) = ModelEdit $ \prsm ms1 ->
  case act prsm ms1 of
    Left (UserError msg) -> Right (Left msg, ms1)
    Left err -> Left err
    Right (v, ms2) -> Right (Right v, ms2)


-- -----------------------------------------------------------
-- ModelEdit actions
-- -----------------------------------------------------------

getModel :: ModelEdit v w (Model v)
getModel = ModelEdit $ \_ ms -> Right (stateModel ms, ms)

-- Replaces the model.
--
-- Beware: this function is unsafe.
-- It is up to the application to ensure that the replacement
-- model is consistent and that "tellLog" has been called to update the UI widget.
setModel :: Model v -> ModelEdit v w ()
setModel newModel = ModelEdit $ \_ ms -> Right ((), ms {stateModel = newModel})


getModelName :: (EntityClass v) => ModelEdit v w Text
getModelName = ModelEdit $ \_ ms -> Right (modelName $ stateModel ms, ms)


-- | Convert a computation over an inner type @w2@ to an outer type @w1@.
withSubtype :: Traversal' w1 w2 -> ModelEdit v w2 a -> ModelEdit v w1 a
withSubtype p2 (ModelEdit f) = ModelEdit $ \p1 ms -> f (p1 . p2) ms


-- | Get access to the current prism.
usingTraversal :: (Traversal' v w -> ModelEdit v w a) -> ModelEdit v w a
usingTraversal f =
  ModelEdit $ \prsm ms ->
    let (ModelEdit act) = f prsm in act prsm ms


-- | Search the model for a string. Returns a list of entity IDs where the string is found,
-- paired with lists of the field IDs and string indexes for each match within the entity.
searchEntities :: (EntityClass v) =>
  Bool   -- ^ True for case insensitive search.
  -> Text   -- ^ Text to search for. All fields are searched using their text representation.
  -> ModelEdit v w [(ModelId, [(FieldId, Int)])]
searchEntities caseBlind needle =
    mapMaybe search . M.toList . modelContents <$> getModel
  where
    search :: (EntityClass v) => (ModelId, Entity v) -> Maybe (ModelId, [(FieldId, Int)])
    search (k, ent) =
      case valueSearch caseBlind needle $ ent ^. entityProperties of
        [] -> Nothing
        rs -> Just (k, rs)


-- | Lift a computation on the base type up to an inner type @w@.
withBaseType :: ModelEdit v v a -> ModelEdit v w a
withBaseType (ModelEdit f) = ModelEdit $ \_ ms -> f id ms


setModelName :: (EntityClass v) => Text -> ModelEdit v w ()
setModelName newName = ModelEdit $ \_ ms ->
  let newModel = (stateModel ms) {modelName = newName}
  in Right ((), ms {stateModel = newModel})


-- | Get data about relationships between entities.
queryRelations :: (EntityClass v) => (NamedRelation ModelId -> a) -> ModelEdit v w a
queryRelations f = ModelEdit $ \_ ms ->
  Right (f $ modelRelations $ stateModel ms, ms)


-- | Modify relationships between entities.
modifyRelations :: (EntityClass v) =>
  (NamedRelation ModelId -> NamedRelation ModelId) -> ModelEdit v w ()
modifyRelations f = ModelEdit $ \_ ms ->
  let
    relations = modelRelations $ stateModel ms
    newModel = (stateModel ms) {modelRelations = f relations}
  in Right ((), ms {stateModel = newModel})


-- | Add a new relation. If either side of the relation is \"1\" as opposed to \"many\" then
-- any previous conflicting relations will be deleted.
--
-- This only enforces cardinality. Relationships that are not described in the model reference
-- types are still permitted. Unit cardinality matters to the model semantics; for instance it
-- makes no sense for an arrow end to be connected to more than one box. Other relationships
-- can be preserved without damaging the model semantics, so they are permitted.
addCheckedRelation :: (EntityClass v) => NR.Relation -> Entity v -> Entity v -> ModelEdit v w ()
addCheckedRelation relName entityA entityB = do
  oldState <- queryRelations $ NR.member (entityId entityA) (entityId entityB) relName
  if oldState
    then return ()   -- Relation already exists. No-op.
    else fromHere $ do
      model <- getModel
      let (RefTypeTable refTypes) = modelRefTypes model <> reflectiveBuiltInRefs
      case M.lookup relName refTypes of
        Nothing -> return ()
        Just refType -> do
          let
            variantA = entityA ^. entityContents . to reflectiveName
            variantB = entityB ^. entityContents . to reflectiveName
            singleA = fst $ refTypeOtherSide variantA refType
            singleB = fst $ refTypeOtherSide variantB refType
          when singleA $ do
            otherEnds <- queryRelations $ NR.relation (entityId entityA) relName
            forM_ otherEnds $ \e -> do
              modifyRelations $ NR.delete (entityId entityA) e relName
              recordModified e
          when singleB $ do
            otherEnds <- queryRelations $ NR.relation (entityId entityB) relName
            forM_ otherEnds $ \e -> fromHere $ do
              modifyRelations $ NR.delete (entityId entityB) e relName
              recordModified e
      recordModified $ entityId entityA
      recordModified $ entityId entityB
      modifyRelations $ NR.insert (entityId entityA) (entityId entityB) relName


-- | Move the cursor to the root.
goToRoot :: ModelEdit v w ()
goToRoot = ModelEdit $ \_ ms -> Right ((), ms{statePosition = nil})


-- | Move the cursor to the Entity with the ModelID. Nil argument means go to root.
goToEntity :: (EntityClass v) => ModelId -> ModelEdit v w ()
goToEntity uuid = ModelEdit $ \_ ms ->
    let result = Right ((), ms{statePosition = uuid})
    in if U.null uuid
      then result   -- Go to root.
      else case M.lookup uuid $ modelContents $ stateModel ms of
        Nothing -> Left $ UserError $ "Entity " <> toText uuid <> " does not exist."
        Just _ -> result


-- | Similar to "goToEntity" but with a friendlier error message if it fails.
goToEntity1 :: (EntityClass v) => Text -> ModelId -> ModelEdit v w ()
goToEntity1 msg uuid = try (goToEntity uuid) >>= \case
  Left _ -> throwUser msg
  Right () -> return ()


-- | How many levels from the root are we? Zero means at the root.
depth :: (EntityClass v) => ModelEdit v w Int
depth = length <$> currentPath


-- | The entity at the current location in the tree. @Nothing@ if we are at the root.
current :: (EntityClass v) => ModelEdit v w (Maybe (Entity v))
current = ModelEdit $ \_ ms -> do
  e <- lookupCurrent ms
  return (e, ms)


-- | The ID of the current location in the tree.
currentId :: ModelEdit v w ModelId
currentId = ModelEdit $ \_ ms -> Right (statePosition ms, ms)


-- | Update the "enitityModified" field of the entity with the current "Stamp". Does nothing
-- if the argument is the root.
recordModified :: (EntityClass v) => ModelId -> ModelEdit v w ()
recordModified modelId = ModelEdit $ \_ ms ->
  if U.null modelId
    then Right ((), ms)   -- No-op for root.
    else case M.lookup modelId $ modelContents $ stateModel ms of
      Nothing -> Left $ InternalError $ "ModelID " <> toText modelId <> " has gone missing."
      Just e1 ->
        let
          e2 = entityModified .~ thisChange $ e1
          model = stateModel ms
          newContents = M.insert modelId e2 $ modelContents model
        in Right ((), ms {stateModel = model {modelContents = newContents}})


-- | Update the "entityModified" stamp for the current entity. Does nothing if the current
-- entity is root.
currentIsModified :: (EntityClass v) => ModelEdit v w ()
currentIsModified = currentId >>= recordModified


-- | Get the reflective data for the current location. Root returns its name.
getData :: (EntityClass v) => ModelEdit v w ExtensionValues
getData = ModelEdit $ \_ ms ->
  lookupCurrent ms >>= \case
    Nothing ->
      Right (M.singleton nameField $ ExtText $ modelName $ stateModel ms, ms)
    Just e ->
      let
        e1 = reflectiveGet $ e ^. entityContents
        e2 = e ^. entityExtensions
        e3 = entityStampsAsFields e
      in Right (e1 `M.union` e2 `M.union` e3, ms)


-- | Modify the reflective data for the current location. Root changes its name but ignores any
-- other values. Returns @True@ if the new data is not equal to the old (i.e. something has
-- actually changed).
setData :: (EntityClass v) => ExtensionValues -> ModelEdit v w Bool
setData newExts1 =
  current >>= \case
    Nothing -> do
      oldName <- getModelName
      let newName = T.strip $ evalState (extract oldName _ExtText nameField) newExts1
      setModelName newName
      return $ newName /= oldName
    Just e1 -> do
      oldExts <- getData
      if oldExts == newExts1
        then return False
        else do
          let
            v1 = e1 ^. entityContents
            (v2, newExts2) = runState (reflectiveSet v1) newExts1
            v3 = name . nameText %~ T.strip $ v2
            e3 =  entityContents .~ v3 $
                entityExtensions .~ newExts2 $ e1
            nameChange = v1 ^. name /= v3 ^. name
          -- Important thzt any renaming is done first
          -- so if it fails the rest will be cancelled.
          when nameChange $ void $ renameEntity $ v3 ^. name
          if e1 == e3
            then return False
            else do
              modelModify $ entityModified .~ thisChange $ e3
              return True


-- | If "current" is a "Package" or root then get the metadata for it.
getMeta :: (EntityClass v) => ModelEdit v w (Maybe (PackageMeta v))
getMeta = current >>= \case
  Nothing -> Just . modelMeta <$> getModel
  Just ent -> return $ ent ^? entityContents . _Package . packageMeta


-- | Set the metadata for the current value. If the value is not a package or root then this is
-- ignored.
setMeta :: (EntityClass v) => PackageMeta v -> ModelEdit v w ()
setMeta pm = current >>= \case
  Nothing -> do
    m <- getModel
    setModel $ m {modelMeta = pm}
  Just ent ->
    modelModify $ entityContents . _Package . packageMeta .~ pm $ ent


-- | The set of field IDs defined for the current entity in the model (including built-in fields).
currentVariantFields :: (EntityClass v) => ModelEdit v w (Set FieldId)
currentVariantFields =
  current >>= \case
    Nothing -> return $ S.singleton nameField
    Just e -> do
      typs <- modelExtensions <$> getModel
      return $ S.fromList $
          reflectiveBuiltIn (e ^. entityContents) ++
          fromMaybe [] (typs ^? ix (e ^. entityContents . to reflectiveName))


-- | Like "getData", but restricts the results to fields defined for the current variant.
getVariantData :: (EntityClass v) => ModelEdit v w ExtensionValues
getVariantData = do
  fields <- currentVariantFields
  d <- getData
  return $ M.restrictKeys d fields


-- | Like "setData", but only modifies fields defined for the current variant.
setVariantData :: (EntityClass v) => ExtensionValues -> ModelEdit v w Bool
setVariantData newExts = do
  fields <- currentVariantFields
  setData $ M.restrictKeys newExts fields


-- | The name of the current location.
currentName :: (EntityClass v) => ModelEdit v w Text
currentName = ModelEdit $ \_ ms ->
  lookupCurrent ms >>= \case
    Just e -> return (e ^. entityName . nameText, ms)
    Nothing -> return (modelName $ stateModel ms, ms)


-- | The entities from the current one up to the root.
currentPath :: (EntityClass v) => ModelEdit v w [Entity v]
currentPath = ModelEdit $ \_ ms -> do
    entities <- ancestry ms
    return (entities, ms)
  where
    ancestry ms = lookupCurrent ms >>= \case
        Nothing -> return []
        Just e -> (e :) <$> ancestry ms{statePosition = e ^. entityParent}


-- | The children we can move down to from here. Always succeeds, even at a root or leaf.
currentChildren :: (EntityClass v) => ModelEdit v w (Map Name ModelId)
currentChildren = ModelEdit $ \_ ms ->
  lookupCurrent ms >>= \case
    Nothing -> Right (modelRoot $ stateModel ms, ms)
    Just e -> Right (e ^. entityChildren, ms)


-- | Move up towards the root. If already there then this is a no-op.
moveUp :: (EntityClass v) => ModelEdit v w ()
moveUp = ModelEdit $ \_ ms ->
  lookupCurrent ms >>= \case
    Nothing -> Right ((), ms)
    Just e -> Right ((), ms{statePosition = e ^. entityParent})


-- | Move down one level. The argument function is used to select the child.
moveDown :: (EntityClass v) => (Map Name ModelId -> ModelEdit v w ModelId) -> ModelEdit v w ()
moveDown child = currentChildren >>= child >>= goToEntity


-- | Move down by following a path of names starting from the top. Note that this in reverse
-- order compared to the result of "currentPath"
followPath :: (EntityClass v) => [Name] -> ModelEdit v w ()
followPath (next:path) = do
    moveDown goNext
    followPath path
  where
    goNext table = case M.lookup next table of
      Nothing -> throwUser $ next ^.nameText <> " not found."
      Just u -> return u
followPath [] = return ()


-- | Generate a new ModelId.
mkModelId :: ModelEdit v w ModelId
mkModelId = ModelEdit $ \_ ms -> do  -- Either monad
  let g1 = stateGenerator ms
  (newId, g2) <- either (Left . InternalError) Right $ generateUUID g1
  return (newId, ms{stateGenerator = g2})


-- | Insert the entity as a child of the current location. The new entity must
-- not exist already, either by name or ModelId. The latter is not checked.
-- The modified date is set to the current date.
--
-- The new entity is passed as a function which takes the new parent ModelId.
addEntity :: (EntityClass v) => (ModelId -> Entity v) -> ModelEdit v w (Entity v)
addEntity entF = do
    ent <- ModelEdit $ \_ ms -> do  -- Either monad
      let
        model@(Model mName root contents meta rel typs refs exts) = stateModel ms
        newEntity = entF $ statePosition ms
        entContents = newEntity ^. entityContents
        newId = entityId newEntity
        eName = newEntity ^. entityName
      lookupCurrent ms >>= \case
        Nothing -> do
          newRoot <- add eName newId root
          let
            newContents = M.insert newId newEntity contents
            newModel = Model mName newRoot newContents meta rel typs refs exts
          unless (entityCanHaveChild dummyPackage entContents) $
            Left $ UserError $
                "Model root cannot have " <> eName ^. nameText <> " as a child."
          return (newEntity, ms {stateModel = newModel})
        Just parent -> do
          let
            parentContents = parent ^. entityContents
          unless (entityCanHaveChild parentContents entContents) $
            Left $ UserError $ parent ^. entityName . nameText <>
                " cannot have " <> eName ^. nameText <> " as a child."
          newSiblings <- add eName newId $ parent ^. entityChildren
          let
            newParent = entityChildren .~ newSiblings $ parent
            newContents =
                M.insert (entityId parent) newParent $
                M.insert newId newEntity contents
            newModel = model {modelContents = newContents}
          return (newEntity, ms {stateModel = newModel})
    m <- getModel
    currentIsModified
    tellLog $ ModelAdd (entityId ent) m
    return ent
  where
    -- Stand-in for root when testing if new entity is legal at the top level.
    dummyPackage = Package (Name "") mempty ^. re _Package
    add k val tbl =
      if M.member k tbl
        then Left $ UserError $ k ^. nameText <> " already exists."
        else Right $ M.insert k val tbl


-- | Create a clone of the current entity with a suitably modified name.
--
-- When a diagram is cloned the elements in the clone will have the same "DiagramId" values as
-- the original, breaking the principle that UUIDs should be unique. Diagram IDs are
-- local to each diagram so this should not be an issue. However if some future feature has
-- diagrams merging then this will become an issue.
cloneEntity :: (EntityClass v) => ModelEdit v w (Maybe (Entity v))
cloneEntity = withBaseType $ current >>= \case
    Nothing -> throwUser "Cannot clone root."
    Just e -> do
      moveUp
      let
        oldName = e ^. entityName . nameText
      newName <- modelNewName $ Name $ oldName <> " clone-"
      case entityClone e newName of
        Just act -> Just <$> act
        Nothing -> return Nothing


-- | An implementation for "entityClone" that will work for entities with no children. This
-- MUST NOT be used for entities that have children; doing so will leave the model in an
-- inconsistent state.
--
-- The new entity is given a fresh creation stamp.
unsafeEntityClone :: (EntityClass v) => Entity v -> Name -> ModelEdit v v (Entity v)
unsafeEntityClone ent newName = do
  newId <- mkModelId
  let stamp = thisChange
  addEntity $ \parentId ->
    (entityParent .~ parentId) .
    (entityModified .~ stamp) .
    (entityName .~ newName) $
    ent {entityId = newId, entityCreated = stamp}
      -- The only time when entity ID and creation stamp are modified.


-- | Create an empty package entity as a child of the current location. The new package must
-- not exist already.
mkPackage :: (EntityClass v) => Name -> ModelEdit v w ModelId
mkPackage newName = withBaseType $ do
  newId <- mkModelId
  e <- addEntity $ \parent -> Entity {
        entityId = newId,
        _entityParent = parent,
        _entityChildren = mempty,
        _entityContents = review _Package $ Package newName mempty,
        _entityExtensions = mempty,
        entityCreated = thisChange,
        _entityModified = thisChange
      }
  return $ entityId e


-- | Follow a package name path, creating new packages along the way if necessary.
--
-- If one of the entities already exists and cannot have this child
-- then it will use "modelNewName" to generate alternatives until it finds a name which is
-- either an existing package or is not taken.
--
-- Returns the ID of the entity (whether it existed already or not) and a flag to indicate if any
-- new entities have been created.
createPath :: (EntityClass v) => [Name] -> ModelEdit v w (ModelId, Bool)
createPath [] = (, False) <$> currentId
createPath names = do
    results <- forM names $ \nm -> do
      ch <- currentChildren
      let
        Name txt = nm
        candidates = nm : [Name $ txt <> T.pack (show x) | x <- [1..] :: [Integer]]
      goDown ch candidates
    let (uuids, flags) = unzip results
    return (last uuids, or flags)
    -- "last" is safe because uuids cannot be empty because names cannot be empty.
  where
    -- Try names from the argument until we either find an existing package or can create a
    -- new one.
    goDown _ [] = throwInternal "Run out of numbers: cannot happen."
    goDown ch (nm : rest) =
      if nm `M.member` ch
        then
          fromHere (followPath [nm] >> current) >>= \case
            Nothing -> throwInternal $
              nm ^. nameText <> " is in a package but does not exist."
            Just existing ->
              case existing ^? entityContents . _Package of
                Just _ -> do
                  followPath [nm]
                  i <- currentId
                  return (i, False)
                Nothing -> goDown ch rest
        else do
          pkg <- mkPackage nm
          goToEntity pkg
          return (pkg, True)



-- | Create a new entity
mkEntity :: (EntityClass v) => v -> ModelEdit v w (Entity v)
mkEntity value = do
  newId <- mkModelId
  addEntity $ \parent -> Entity {
        entityId = newId,
        _entityParent = parent,
        _entityChildren = mempty,
        _entityContents = value,
        _entityExtensions = mempty,
        entityCreated = thisChange,
        _entityModified = thisChange
      }


-- | Returns the current Entity and any children as a Tree of Entities which
-- can be traversed. Fails at the root.
modelPackageTree :: (EntityClass v) => ModelEdit v w (Maybe (Tree (Entity v)))
modelPackageTree =
    current >>= \case
      Nothing -> return Nothing
      Just here -> Just . Node here <$> modelPackageForest


-- | Returns the children of the current entity as a forest. Does not return the current
-- entity, but this means it succeeds at the root.
modelPackageForest :: (EntityClass v) => ModelEdit v w (Forest (Entity v))
modelPackageForest = do
  cs <- M.elems <$> currentChildren  -- Get the ModelIds of the children.
  forM cs $ \child -> fromHere $ do
      moveDown $ const $ return child  -- We already have the child ModelId: don't get it again.
      modelPackageTree >>= \case
        Just t -> return t
        Nothing -> throwInternal "modelPackageForest: child not found."


-- | Executes the argument, and then returns to the original location if that still exists. If
-- not then it moves up to the first ancestor that does still exist. If the subtree has been moved
-- then the location will follow it.
fromHere :: (EntityClass v) =>  ModelEdit v w a -> ModelEdit v w a
fromHere action = do
    uuids <- map entityId <$> currentPath
    v <- action
    land uuids
    return v
  where
    land [] = goToRoot
    land (uuid : uuids) = do
      r <- try $ goToEntity uuid
      case r of
        Left _ -> land uuids
        Right () -> return ()


-- | Deletes the current Entity and any children, and moves up to the current parent. At
-- the root it deletes the whole model.
delete :: (EntityClass v) => ModelEdit v w ()
delete = do
    mName <- getModelName
    m <- getModel
    p <- currentPath
    oldId <- currentId
    oldRelations <- S.toList . S.map fst <$> queryRelations (NR.relations oldId)
    modifyRelations $ NR.deleteAll oldId
    mapM_ recordModified oldRelations
    case p of
      [] -> do  -- Delete whole model.
        tellLog $ ModelDelete nil m
        ModelEdit $ \_ ms -> Right ((), ms{stateModel = emptyModel mName})
      [ent] -> do   -- Delete one entry in root.
        tellLog $ ModelDelete (entityId ent) m
        clearTree ent
        modifyRoot $ M.delete $ ent ^. entityName
        goToRoot
      (ent:parent:_) -> do  -- Delete 'ent' from 'parent'
        tellLog $ ModelDelete (entityId ent) m
        clearTree ent
        clearName (ent ^. entityName) parent
        goToEntity $ entityId parent
  where
    -- Remove the current entity and all its children from the model contents and relations.
    clearTree e = do
      targets <- concatMap flatten <$> modelPackageForest
      mapM_ clearEntity targets
      clearEntity e
    -- Remove entity from model contents and relations
    clearEntity ent = do
      let oldId = entityId ent
      oldRelations <- S.toList . S.map fst <$> queryRelations (NR.relations oldId)
      mapM_ recordModified oldRelations
      modifyRelations $ NR.deleteAll oldId
      ModelEdit $ \_ ms ->
        let
          m1 = stateModel ms
          m2 = m1 {modelContents = M.delete oldId $ modelContents m1}
        in Right ((), ms{stateModel = m2})
    -- Remove a name from the specified package.
    clearName nm ent1 = ModelEdit $ \_ ms ->
      let
        ent2 = entityChildren . at nm .~ Nothing $ ent1
        m1 = stateModel ms
        m2 = m1 {modelContents = M.insert (entityId ent2) ent2 (modelContents m1)}
      in Right ((), ms{stateModel = m2})
    -- Modify the model root with 'f'.
    modifyRoot f = ModelEdit $ \_ ms ->
      let m1 = stateModel ms
      in Right ((), ms {stateModel = m1 {modelRoot = f $ modelRoot m1}})


-- | Find a possible new name for a child of entity by appending digits to the base name.
modelNewName :: (EntityClass v) => Name -> ModelEdit v w Name
modelNewName base = do
  let
    baseTxt = base ^. nameText
    candidates = map ((baseTxt <>) . T.pack . show) ([1..] :: [Integer])
  taken <- currentChildren
  return $ head $ filter (not . (`M.member` taken)) $ map Name candidates
    -- head is safe because candidates is an infinite list and they can't all be taken.


-- | Private function to replace an old version of the entity in the model with a new one.
-- Note that this changes the argument entity (following its ModelId), not the current one. It
-- also does not check for a change of name, so this is unsafe.
modelModify :: (EntityClass v) => Entity v -> ModelEdit v w ()
modelModify e = ModelEdit $ \_ ms ->
  let
    Model mName root contents meta rels typs refs exts = stateModel ms
    uuid = entityId e
  in if M.member uuid contents
    then Right ((), ms {stateModel =
      Model mName root (M.insert uuid e contents) meta rels typs refs exts})
    else Left $ UserError $ (e ^. entityName . nameText) <> " not in model."


-- | Rename the current entity. It is an error if a sibling of the same name already exists.
-- Renaming the root renames the model. The modification stamp is set unless the old name is the
-- same as the new name. Returns @True@ if the new name is different.
renameEntity :: (EntityClass v) => Name -> ModelEdit v w Bool
renameEntity newName = do
  let newName1 = nameText %~ T.strip $ newName
  oldName <- Name <$> currentName
  if oldName == newName1
    then return False
    else do
      path <- currentPath
      m1 <- getModel
      case path of
        [] -> let Name tn = newName1 in do
          setModelName tn
          m2 <- getModel
          tellLog $ ModelRename nil m1 m2
          return True
        (e : parents) -> do
          sibs <- fromHere $ moveUp >> currentChildren
          let
            newEntity = entityName .~ newName1 $ e
            uuid = entityId e
            newSibs = M.insert newName1 uuid $ M.delete oldName sibs
          when (M.member newName1 sibs && newName1 /= oldName) $
            throwUser $ newName1 ^. nameText <> " already exists."
          modelModify newEntity
          case parents of
            [] -> ModelEdit $ \_ ms ->   -- Parent is root.
              let model = stateModel ms
              in Right ((), ms{stateModel = model {modelRoot = newSibs}})
            (parent : _) ->              -- Parent is not root
              modelModify $ entityChildren .~ newSibs $ parent
          currentIsModified
          m2 <- getModel
          tellLog $ ModelRename (entityId e) m1 m2
          return True


-- | Modify the value of the current entity, if it has a value of type @w@.
-- Returns @True@ if the new value is different from the old.
modifyValue :: (EntityClass v) => (w -> w) -> ModelEdit v w Bool
modifyValue f = current >>= \case
    Nothing -> return False
    Just (e :: Entity v) -> usingTraversal $ \prsm ->
      case e ^? entityContents . prsm of
        Nothing -> return False
        Just oldVal -> do
          let
            newVal = f oldVal
            newEnt =
              (entityName . nameText %~ T.strip) .
              (entityContents . prsm .~ newVal) $ e
          if newEnt ^. entityContents == e ^. entityContents
            then return False
            else do
              when (newEnt ^. entityName /= e ^. entityName) $
                void $ renameEntity $ newEnt ^. entityName
              modelModify newEnt
              currentIsModified
              return True


-- | Set the value of the current entity, if it currently has a value of type @w@.
-- If there is no value of type @w@ then nothing happens.
--
-- Returns @True@ if the new value is different.
setValue :: (EntityClass v) => w -> ModelEdit v w Bool
setValue = modifyValue . const


-- | Move the current entity to be a child of the argument.
-- It is an error if an entity of the same name already exists at the target. After the move
-- the current entity is the same as before, but at its new location. Timestamps are not modified.
moveEntity :: (EntityClass v) => ModelId -> ModelEdit v w ()
moveEntity dest = do
    newPath <- map entityId <$> fromHere (goToEntity dest >> currentPath)
    current >>= \case
      Nothing -> throwUser "Cannot move the model root."
      Just ent -> do
        when (entityId ent `elem` newPath) $ throwUser "Cannot move an entity inside itself."
        before <- getModel
        unlink ent
        goToEntity dest
        relink ent
        goToEntity $ entityId ent
        after <- getModel
        tellLog $ ModelMove (entityId ent) before after
  where
    unlink :: (EntityClass v) => Entity v -> ModelEdit v w ()
    unlink ent = do
      path <- currentPath
      case path of
        [] -> throwInternal "moveEntity: empty path cannot happen."
          -- currentEntity not Nothing
        [_] ->  -- Entity is at root: remove it from there.
          ModelEdit $ \_ ms ->
            let model = stateModel ms
            in Right ((), ms {stateModel = model {
                modelRoot = M.delete (ent ^. entityName) $ modelRoot model
              }})
        (_ : parent : _) ->  -- Remove entity from parent.
          modelModify $
              entityChildren . at (ent ^. entityName) .~ Nothing $ parent
    relink :: (EntityClass v) => Entity v -> ModelEdit v w ()
    relink ent = do
      path <- currentPath
      let entContents = ent ^. entityContents
      case path of
        [] ->   -- New parent is root.
          ModelEdit $ \_ ms -> do  -- Either monad
            let
              model = stateModel ms
              eName = ent ^. entityName
              newRoot = M.insert eName (entityId ent) $ modelRoot model
              newContents = ix (entityId ent) . entityParent .~ dest $ modelContents model
            unless
              (entityCanHaveChild (Package (Name "") mempty ^. re _Package) entContents) $
              Left $ UserError $
                "Model root cannot have " <> eName ^. nameText <> " as a child."
            when (M.member eName $ modelRoot model) $
              Left $ UserError $ eName ^. nameText <> " already exists at the destination."
            Right ((), ms {
                stateModel = model {
                    modelRoot = newRoot,
                    modelContents = newContents},
                statePosition = entityId ent
              })
        (newParent : _) ->
          ModelEdit $ \_ ms -> do  -- Either monad.
            let
              eName = ent ^. entityName
              nm = eName ^. nameText
              model = stateModel ms
              newContents =
                ix (entityId ent) . entityParent .~ dest $
                ix (entityId newParent) . entityChildren . at eName ?~ entityId ent $
                modelContents model
            when (M.member eName $ newParent ^. entityChildren) $
              Left $ UserError $ nm <> " already exists at the destination."
            unless (entityCanHaveChild (newParent ^. entityContents) entContents) $
              Left $ UserError $ "Destination cannot have " <> nm <> " as a child."
            Right ((), ms {
                stateModel = model {modelContents = newContents},
                statePosition = entityId ent
              })



-- | Verify that the model is consistent:
--
-- * Package tree and contents contain the same set of ModelIds.
--
-- * Every node is the child of its parent.
--
-- * Content keys match value identities.
--
-- * Package tree ModelIds contain no duplicates.
--
-- * The directories in each package refer to entities with the same names and ModelIds.
--
-- * Relations are internally consistent.
--
-- * Relations contain a subset of content ModelIds.
--
-- If a problem is found then a helpful internal error message will be produced,
-- but this should never happen. If there is no error then this is a no-op.
checkModel :: (EntityClass v) => ModelEdit v w ()
checkModel = fromHere $ do
    goToRoot
    forest <- modelPackageForest
        -- Safe because we are on base type.
    model <- ModelEdit $ \_ ms -> Right (stateModel ms, ms)
    let
      rootList = map entityId $ concatMap flatten forest
      rootSet = S.fromList rootList
      contentList = M.toList $ modelContents model
      contentSet = S.fromList $ map fst contentList
      relationSet = S.fromList $
          concatMap (uncurry (:) . (_2 %~ map fst)) $
          NR.toList $
          modelRelations model
    -- Package tree and contents contain the same set of ModelIds.
    unless (rootSet == contentSet) $ throwInternal "package tree /= model contents"
    -- Every node is the child of its parent.
    mapM_ (checkParent $ rootAsEntity model) forest
    -- Package content table keys are consistent with corresponding entities.
    mapM_ contentConsistent contentList

    -- If rootList contains duplicates then these will be eliminated in the set and hence
    -- show up as a difference in the sizes.
    unless (S.size rootSet == length rootList) $
      throwInternal "Package tree contains duplicate entries."

    -- Directories in each package refer to entities with the same names and ModelIds.
    -- Strictly speaking the ModelId check is redundant because contentConsistent has
    -- already checked that, but it is kept here due to an abundance of caution.
    checkPackages (modelContents model) $ modelRoot model

    case runExcept $ NR.check $ modelRelations model of
      Left msg -> throwInternal msg
      Right () -> return ()

    -- Relations contain a subset of content ModelIds.
    unless (relationSet `S.isSubsetOf` contentSet) $
      throwInternal "Relations are not a subset of contents."
  where
    -- Check parent ModelIds match.
    checkParent p (Node e cs) = do
      unless (entityId p == e ^. entityParent) $ throwInternal $
        "Element not a child of its parent: \"" <> e ^. entityName . nameText <> "\""
      unless (entityCanHaveChild (p ^. entityContents) (e ^. entityContents)) $ throwInternal $
        "Element \"" <> (p ^. entityName . nameText)
        <> "\" has illegitimate child " <> (e ^. entityName . nameText) <> "\""
      mapM_ (checkParent e) cs
    -- Check key ModelIds match package ModelIds
    contentConsistent (u,v) = unless (u == entityId v) $ throwInternal $
        "Content ModelIds inconsistent: " <> toText u <> " -> " <> toText (entityId v)
    -- Recursively descend the tree, checking children for consistent name and ModelId.
    checkPackages contents table =
      forM_ (M.toList table) $ \(key, uuid) ->
        case M.lookup uuid contents of
          Nothing -> throwInternal $
            "\"" <> key ^. nameText <> "\" refers to a nonexistent entity."
          Just ent -> do
            when (key /= ent ^. entityName) $ throwInternal $
              "\"" <> key ^. nameText <> "\" refers to \"" <>
              ent ^. entityName . nameText <> "\""
            when (uuid /= entityId ent) $ throwInternal $
              "\"" <> key ^. nameText <> "\" refers to entity with wrong ModelId."
              -- This is a redundant check, but keep anyway.
            checkPackages contents $ ent ^. entityChildren
    rootAsEntity m = Entity {
          entityId = nil,
          _entityParent = nil,
          _entityChildren = modelRoot m,
          _entityContents = Package (Name $ modelName m) mempty ^. re _Package,
          _entityExtensions = mempty,
          entityCreated = thisChange,
          _entityModified = thisChange
        }


-- | Check that the @\"type\"@ field in the JSON is correct.
checkType :: Object -> Text -> Parser ()
checkType ob txt = do
  typ <- ob .: "type"
  when (typ /= txt) $ fail $ "Type = " <> show typ <> ", expected " <> show txt
