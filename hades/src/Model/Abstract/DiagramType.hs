{-# LANGUAGE UndecidableInstances #-}

{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

{- |

Models contain diagrams, and diagrams show the graphical representations of model entities.
When a diagram is edited the changes may have to be reflected in the model, and of course the
diagram itself needs to be updated in the model. Conversely when entities in the model change
the associated diagram elements must be updated accordingly.

This circular relationship between models and diagrams is encoded in this module. "DiagramEntity"
is used to store a diagram. When a diagram is being edited the "Delta" monad transformer used
for diagram editing is applied to the "ModelScript" monad, so edit commands in the diagram have
access to the both the model state and the various properties dialogs.

When a diagram element is used to represent a model entity, the element is called the \"avatar\"
of the entity. A model entity may have multiple avatars of different types in different diagrams.
This relationship is described by instances of the "Avatar" class. An instance of @Avatar v d@
says that diagram elmenets of type @d@ can optionally represent model entities of type @v@.
-}

module Model.Abstract.DiagramType (
   -- ** Diagram Editing Within Models
   DiagramEntity (..),
   diagramEntityName,
   diagramEntityContents,
   diagramEntityData,
   EntityWrapper (..),
   sameEntityView,
   DiagramEntityTypes,
   HasDiagrams (..),
   -- ** Avatars of Model Entities in Diagrams
   Avatar (..),
   ascend,
   avatarProperties,
   updateDiagram,
   diagramAvatars,
   nearestAvatar,
   findOrCreateAvatar,
   keyCommands,
   avatarConnector,
   avatarShapeMenu,
   -- ** Model Editing Scripts
   UserScriptFunctor (..),
   UserScript (..),
   ScriptMonad (..),
   ModelScript,
   ActivationScript,
   EntityMenu,
   Editable (..),
   promoteScript,
   withBaseScript,
   withHint,
   clearHint,
   openProperties,
   openDialog,
   openDiagram,
   openEditor,
   getModelSelection,
   setModelSelection,
   performIO,
   -- ** Common Operations
   goBackToEntity,
   insertEntity,
   clonePackage,
   logInsertion,
   editEntityProperties,
   deleteEntity,
   updateManyToOne,
   newBoxAvatarTool,
   newArrowAvatarTool,
   newDiagramBoxTool,
   newDiagramArrowTool,
   viewControlTool,
   findTextTool,
   -- ** Stock Menus
   cloneEntityMenuItem,
   entityMenu,
   diagramEntityMenu,
   diagramBackgroundMenu,
   -- ** Stock Dialogs
   packageNameDialog,
   diagramNameDialog
) where

import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Free
import Control.Monad.State.Class
import Data.Aeson
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree
import qualified Data.UUID as U
import qualified Data.Vector as V
import Hades.Abstract
import Model.Abstract.PackageTree as P
import Model.Abstract.Properties
-- import Model.Matrices.Base
import qualified Model.Reflection.NamedRelation as NR
import Model.Reflection.Reflective
import Model.Reflection.Values
import Reactive.Banana.ArrowDialog
import Reactive.Banana.Common
import Reactive.Banana.GI.ErrorBox (cannotHappen)
import Reactive.Banana.Menu as Menu


-- ToDo: consider introducing strongly typed UUIDs


-- | A diagram stored within a model entity. Typically the type argument to the containing "Model"
-- will be a sum type including one or more values of type @DiagramEntity d@ for appropriate
-- types of @d@.
--
-- This is an instance of "Reflective" as a matter of convenience.
data DiagramEntity d = DiagramEntity {
      _diagramEntityName :: Name,
      _diagramEntityContents :: Diagram d,
      _diagramEntityData :: DiagramMetadata d
   }

instance (Eq d, Viewable d, Eq (DiagramMetadata d)) =>
   Eq (DiagramEntity d) where
      DiagramEntity n1 c1 a1 == DiagramEntity n2 c2 a2 =
         n1 == n2 && c1 == c2 && a1 == a2

instance (Viewable d, Monoid (DiagramMetadata d)) => Reflective (DiagramEntity d) where
   reflectiveName _ = Variant $ itemType dummy  <> " Diagram"
      where
         dummy :: d
         dummy = error "Dummy value used for type name only."
   reflectiveDefaults = [DiagramEntity (Name "") mempty mempty]
   reflectiveBuiltIn _ = [nameField]
   reflectiveGet de = M.singleton nameField $ ExtText $ de ^. diagramEntityName . nameText
   reflectiveSet de = do
      nm <- extract (de ^. diagramEntityName . nameText) _ExtText nameField
      return $ diagramEntityName . nameText .~ nm $ de
   reflectiveBuiltInRefs = mempty
   reflectiveArrows = mempty

instance (Monoid (DiagramMetadata d), ToJSON d, ToJSON (DiagramMetadata d), Viewable d) =>
   ToJSON (DiagramEntity d) where
      toJSON de@(DiagramEntity dName dContents dAppearance) = object [
            "name" .= dName,
            "type" .= reflectiveName de,
            "contents" .= dContents,
            "appearance" .= dAppearance
         ]

instance (
      Monoid (DiagramMetadata d),
      FromJSON d,
      FromJSON (DiagramMetadata d),
      HasId d,
      Viewable d) =>
   FromJSON (DiagramEntity d) where
      parseJSON = withObject "diagram" $ \v -> do
            typ <- v .:? "type" .!= reflectiveName dummy  -- Version 0.5 diagrams had no type field.
            unless (typ == reflectiveName dummy) $
               fail $ "Wrong type of diagram: expected " <>
                     show (reflectiveName dummy) <> " but got " <> show typ
            DiagramEntity <$>
                  v .: "name" <*>
                  v .: "contents" <*>
                  v .:? "appearance" .!= mempty
         where
            dummy :: DiagramEntity d
            dummy = error "Dummy value used for type name only."


diagramEntityName :: Lens' (DiagramEntity d) Name
diagramEntityName = lens _diagramEntityName $ \e n -> e {_diagramEntityName = n}

diagramEntityContents :: Lens' (DiagramEntity d) (Diagram d)
diagramEntityContents = lens _diagramEntityContents $ \e d -> e {_diagramEntityContents = d}

diagramEntityData :: Lens' (DiagramEntity d) (DiagramMetadata d)
diagramEntityData = lens _diagramEntityData $ \e d -> e {_diagramEntityData = d}


{- | When an entity is to be edited in a diagram window this structure specifies the editor
to use.

The @p@ type parameter is the monad for drawing the actual shapes, such as the Cairo \"Render\"
-- monad.
-}
data EntityWrapper p v =
   -- | Typically the top level type @v@ is a union of several types @(w1, w2 ...)@,
   -- each of which covers a specific domain (e.g. GSN arguments). Each @w@ type will have
   -- constructors for the different types of domain-specific
   -- entity, one or more of which will be diagrams. Hence there will be a family of prisms
   -- of type @Prism' v w@, and for each type of diagram @d@ in @w@ there will also be a prism
   -- of type @Prism' w (DiagramEntity d)@.
   forall w d . (Avatar p v w d, p ~ Paint d, EntityClass v, Eq d) =>
   DiagramWrapper {
         wrappedEntityId :: ModelId,
            -- ^ The "ModelId" of the diagram in the model.
         diagramToEdit :: Diagram d,
         diagramEntityTraversal :: Traversal' w (DiagramEntity d),
            -- ^ Convert the edited diagram back to an entity in the model.
         diagramTraversal :: Traversal' v w,
            -- ^ Link between diagram update scripts and the underlying model.
         diagramDialog :: DialogSelector' (Model v) () d,
            -- ^ The property dialogues for diagram elements.
         diagramContext :: ViewContext d,
            -- ^ The initial control values for the diagram element rendering.
         diagramMenu :: Maybe (Point -> Menu (Action d)),
            -- ^ Context menu for the diagram background. @Nothing@ for no menu.
         diagramToolbar :: Changes (Model v) -> DeltaToolbar d
      }
   -- | Generate a view of the model based on the entity.
   --
   -- @s@ is a state value which is
   -- not derived from or saved in the model but which persists across refresh events.
   --
   -- The output from the gadget is ignored. Instead the gadget should generate "ModelEdit" events
   -- using "send". This allows for finer control and better undo messages.
   | forall a s . ViewWrapper {
         wrappedEntityId :: ModelId,  -- ^ The entity controlling this view.
         viewTag :: Text, -- ^ Identifier to differentiate views of the same entity.
         viewInitial :: s,   -- ^ Initial state.
         viewInput :: Either a (Model v -> Maybe a),
            -- ^ Left: input is independent of model.
            -- Right: input is extracted from the model. The initial value must not be @Nothing@.
         viewGadget :: Gadget' (Model v) (ModelScript p v v (Maybe Text)) (s, a)
            -- ^ The view of the model. As entities are modified the "Gadget" should send
            -- "ModelEdit" scripts. The text is the change description for the
            -- undo menu. If @Nothing@ is returned then any changes are ignored.
            --
            -- The Gadget is re-rendered whenever the "Refresh" button is pressed.
      }


-- | True if the two wrappers describe the same editor.
sameEntityView :: EntityWrapper p1 v1 -> EntityWrapper p2 v2 -> Bool
sameEntityView w1@DiagramWrapper {} w2@DiagramWrapper {} =
   wrappedEntityId w1 == wrappedEntityId w2
sameEntityView w1@ViewWrapper {} w2@ViewWrapper {} =
   wrappedEntityId w1 == wrappedEntityId w2 && viewTag w1 == viewTag w2
sameEntityView _ _ = False

{-
Design Note

The EntityWrapper does not take a "w" type parameter because the UserScriptFunctor needs to
have a hoist operation that can change "w1" into "w2" using a prism. However this cannot be
applied to the diagram menu items.
-}


-- | A map from diagram variant types to the variants that they can hold avatars for.
type DiagramEntityTypes v = Map (Variant v) [Variant v]


{- | Reflective data for types that can include diagrams.

Laws:

* @getDiagramWrapper@ returns a result if and only if @isDiagram@ returns True for the value
referenced.

* @reflectiveName v `member` diagramEntityTypes ==> isDiagram v@
-}
class (EntityClass v) => HasDiagrams p v | v -> p where
   -- | @True@ if the argument is an instance of @w@ and is a diagram.
   isDiagram :: v -> Bool
   -- | Returns a wrapper if the referenced entity is a diagram in an instance of @w@.
   getDiagramWrapper :: Entity v -> ModelEdit v v (Maybe (EntityWrapper p v))
   -- | Entities which can appear in diagrams.
   diagramEntityTypes :: DiagramEntityTypes v
   -- | If this @v@ is a diagram with avatars then replace the referenced entities according to the
   -- supplied function.
   diagramMapAvatars :: (ModelId -> Maybe ModelId) -> Entity v -> Entity v


-- | An instance of @Avatar@ defines a relationship between diagram element type @d@ and model
-- entity type @v@. When you drag a model entity into a diagram, the thing that appears in the
-- diagram is actually an avatar of the entity. The avatar can access its entity through the
-- "ModelScript" monad using the "liftBase" function.
--
-- There is a circular dependency between model and diagram, such that when a model is updated all
-- the diagrams are updated, and a diagram update is then used to update the model. The
-- "updateAvatar" function breaks this endless loop by returning "False" if the updated model
-- has left the diagram element unaffected.
--
-- If the diagram element type @d@ presents model entities as "itemChildren" then the "ModelId"s
-- should be used as the child "DiagramId"s.
class (Base d ~ ModelScript p v w, Connectable d, EntityClass v, Eq d, Monad p, Paint d ~ p) =>
   Avatar p v w d | d -> w where
      -- | Application information that is recorded with the picture in the model.
      type DiagramMetadata d :: *
      -- | Write the metadata in to the view context.
      updateViewContext :: DiagramMetadata d -> ViewContext d -> ModelEdit v w (ViewContext d)
      -- | Model ID for the diagram being edited. Usually hidden in the "diagramViewContext".
      getDiagramModelId :: Delta d DiagramId
      {- | If the argument is allowed on a @Diagram d@ then this adds a new avatar
         to the diagram, centred at the specified point. If it is not allowed then
         the result is @Nothing@.

         Some entities (e.g. connecting arrows) may need avatars for related entities as
         well. @findOrCreateAvatar@ is useful in this case. -}
      entityAvatar :: Entity v -> Point -> Delta d (Maybe d)
      {- | If the diagram element is an Avatar of an entity in the model then this is the ID of
         that entity. Some diagram elements may be free standing (e.g. comment boxes) and these
         will return @Nothing@. -}
      ascendedId :: d -> Maybe ModelId
      {- | Diagram elements have a list of \"child\" elements. This function links the model
         entity to the displayed list. The results must be entities within the model, but need
         not be the actual children of the ascended entity. -}
      avatarChildren :: d -> Delta d [ModelId]
      {- | Update the diagram avatar @d@ with data from the corresponding model entity.
         If the model no longer contains the entity then the avatar should also be deleted.
         If the diagram remains unchanged then return False, as this breaks the loop between
         diagram changes and model updates. Note that merely updating the view of a diagram
         element (e.g. with new values retrieved from the model) does not count as a change to
         the diagram. Only return "True" if a new version of the Diagram data structure needs
         to be pushed back into the model, for instance if the diagram element has been resized
         to accomodate model changes. -}
      updateAvatar :: d -> Delta d Bool


-- | If an diagram element is an Avatar of an entity in the model then this is it. Also leaves
-- the cursor on the entity in question.
--
-- @Nothing@ is returned if the target does not exist.
ascend :: (Avatar p v w d) => d -> Delta d (Maybe (Entity v))
ascend d = case ascendedId d of
      Nothing -> return Nothing
      Just target -> liftBase $ lift $ try (goToEntity target >> current) >>= \case
         Left _ -> return Nothing
         Right v -> return v


-- | When a diagram element is double-clicked then for avatars display the dialog box of the
-- ascended entity, and for non-avatars display the dialog box of the diagram element.
avatarProperties :: (Avatar p v w d, EntityClass v) =>
   d -> Delta d (Action d)
avatarProperties item =
   ascend item >>= \case
      Nothing -> yieldProperties item
      Just e -> do
         changed <- liftBase $ do  -- ModelScript monad.
            nm <- lift currentName
            mf <- openProperties $ entityId e
            {- This is a bit clunky. The property dialog has to take an entity in order to
            discriminate between variants and hence show the right dialog. However all the
            actual editing is done using the property values. -}
            forM mf $ \(newEnt, newRefs) -> do
               goBackToEntity (entityId e) nm
               oldRefs <- lift $ queryRelations $ getReferenceValues $ entityId e
               let
                  changedRefs = compareReferenceValues oldRefs newRefs
               changedVal <- lift $ setVariantData $ newEnt ^. entityProperties
               unless (null changedRefs) $ lift $ do
                  currentIsModified
                  mapM_ recordModified changedRefs
                  putReferenceValues e newRefs
               return $ changedVal || not (null changedRefs)
         desc <- itemDescription item
         when (or changed) $ tellCheckpoint $ "Edit " <> desc <> " properties"
         yieldViews


-- | Find all the avatars of the argument in the diagram, in no particular order.
diagramAvatars :: (Avatar p v w d) => ModelId -> Delta d [d]
diagramAvatars modelId = do
      es <- toList <$> use (deltaDiagram . diagramContents)
      return $ filter ((Just modelId ==) . ascendedId) es


-- | Find the nearest avatar of the argument to the given point, or Nothing if there isn't
-- one on the diagram.
nearestAvatar :: (Avatar p v w d) =>
   ModelId -> Point -> Delta d (Maybe d)
nearestAvatar uuid pt =
   diagramAvatars uuid >>= \case
      [] -> return Nothing
      [d] -> return $ Just d
      ds -> do  -- Find the nearest.
         results <- forM ds $ \d -> do
            pt2 <- ($ pt) <$> itemConnectionPoint d Nothing  -- Ignore the issue of which child
            return (distance $ pointDiff pt pt2, d)
         return $ Just $ snd $ minimumBy (comparing fst) results  -- Find the nearest one.


-- | Find or create an avatar of the entity in the diagram. If the entity already has one or more
-- avatars then return the nearest one. Otherwise create one at the given point and return that.
--
-- Returns @Nothing@ if the entity is not allowed on this diagram.
findOrCreateAvatar :: (Avatar p v w d) =>
   Entity v    -- ^ The entity that needs an avatar.
   -> Maybe ModelId  -- ^ An entity exported by the avatar.
   -> Point    -- ^ The point on the diagram where a new avatar should be created.
   -> Delta d (Maybe (d, Maybe DiagramId))
findOrCreateAvatar ent exportId pt =
   nearestAvatar (entityId ent) pt >>= \case
      Nothing ->
         entityAvatar ent pt >>= \case
            Just v ->
               return $ Just (v, exportId)
            Nothing ->
               return Nothing
      Just v -> return $ Just (v, exportId)


-- | Mapping from key presses to diagram actions.
keyCommands :: (Avatar (Paint d) v w d) => KeyCommands d
keyCommands = M.fromList [
      ((noMod, '\DEL'), applyToSelection (fmap ("Delete " <>) <$> itemsDescription) deleteItem),
      ((shiftMod, '\DEL'), applyToSelectionAvatars
            (fmap ("Delete " <>) <$> itemsDescription)
            (\ent -> liftBase $ lift $ do {goToEntity $ entityId ent; delete})
            deleteItem)
   ]


-- | Apply the command to all the avatars in the current diagram selection.
applyToSelectionAvatars :: (Avatar (Paint d) v w d) =>
   ([d] -> Delta d Text)
      -- ^ Message for the undo log, based on the items the action is applied to.
   -> (Entity v -> Delta d ())
      -- ^ Action to apply to avatars.
   -> (DiagramId -> Delta d ())
      -- ^ Action to apply to any elements that do not have avatars.
   -> Delta d ()
applyToSelectionAvatars strF avatarAction elementAction = do
   sel <- use deltaSelection
   unless (S.null sel) $ do
      contents <- use $ deltaDiagram . diagramContents
      msg <- strF $ mapMaybe (`M.lookup` contents) $ S.toList sel
      forM_ sel $ \i ->
         case M.lookup i contents of
            Nothing -> cannotHappen "Selected item not found in diagram" $ return ()
            Just e ->
               ascend e >>= \case
                  Nothing -> elementAction i
                  Just ent -> avatarAction ent
      tellCheckpoint msg


-- | Update all the elements in the diagram from the model.
updateDiagram :: (Avatar p v w d, EntityClass v, Eq d) =>
   Traversal' w (DiagramEntity d) -> Delta d ()
updateDiagram dTrav = do
   uuid <- getDiagramModelId
   mVal <- liftBase $ lift $ try $ do
      prsm <- usingTraversal return
      goToEntity uuid
      e <- current
      return $ e ^? _Just . entityContents . prsm . dTrav . diagramEntityContents
   case mVal of
      Left _ -> return ()   -- Diagram deleted.
      Right Nothing -> return ()   -- Should never happen.
      Right (Just newDiagram) -> do
         assign deltaDiagram newDiagram
         tellAll
         contents <- M.elems <$> use (deltaDiagram . diagramContents)
         r <- or <$> mapM updateAvatar contents
         when r $ do
            d <- use deltaDiagram
            liftBase $ lift $ do
               goToEntity uuid
               void $ modifyValue $ dTrav . diagramEntityContents .~ d
            tellCheckpoint ""  -- Flag value for update but no checkpoint.


-- | Find or create an avatar for the model entity, and hence create a "ConnectorEnd" that
-- matches it. If the "ModelId" is nil or does not exist then an "Unconnected" result is returned.
--
-- Once the caller has both connection ends it may need to adjust them with "optimiseEnds".
avatarConnector :: (Avatar p v w d) =>
   Maybe (ModelId, Maybe ModelId)
      -- ^ The identity of the model entity that needs to be connected to, and
      -- the exported entity (if any) that needs to be connected within the model entity
   -> Point  -- ^ The notional location of the entity that will own the connector. Used for
             -- computing the connection point.
   -> Point  -- ^ The point where the avatar of the model entity should be created.
   -> Delta d ConnectorEnd
avatarConnector Nothing _ pt = return $ Unconnected pt
avatarConnector (Just (modelId, exportId)) basePoint pt =
   liftBase (lift $ try (goToEntity modelId >> current)) >>= \case
      -- Nil modelId means not connected rather than root, but the result is the same.
      Left _ -> return $ Unconnected pt
      Right Nothing -> return $ Unconnected pt
      Right (Just entity) ->
         findOrCreateAvatar entity exportId pt >>= \case
            Nothing -> liftBase $ lift $ throwInternal
               "Arrow is connected to something that is not legal on this diagram."
            Just (avatar, child) -> do
               pt2 <- ($ basePoint) <$> itemConnectionPoint avatar child
               return $ Connected pt2 (identifier avatar) child


-- | Standard menu items for an avatar. Similar to "Hades.Abstract.Menus.basicShapeMenu" except
-- that it distinguishes between deleting the just the shape on the diagram and deleting the
-- model entity as well.
avatarShapeMenu :: (Avatar (Paint d) v w d, EntityClass v, HasDiagrams (Paint d) v) =>
   DiagramId -> Point -> Menu (Action d)
avatarShapeMenu dId = const $ Menu [
         [
            Menu.MenuItem "Properties" $ Right $ mkScriptAction $
               use (deltaDiagram . diagramContents . at dId) >>= \case
                  Nothing -> yieldViews
                  Just item -> avatarProperties item,
            Menu.MenuItem "Select in model" $ Right $ mkScriptAction selectInModel,
            Menu.MenuItem "Find in other diagrams" $ Right $
                  mkScriptAction $ findInDiagrams dId
         ], [
            Menu.MenuItem "To front" $ Right $ selectionOrTarget
                  (fmap (("Move " <>) . (<> " to front")) . itemsDescription)
                  moveTop
                  dId,
            Menu.MenuItem "To back" $ Right $ selectionOrTarget
                  (fmap (("Move " <>) . (<> " to back")) . itemsDescription)
                  moveBottom
                  dId
         ], [
            Menu.MenuItem "Delete from diagram" $ Right $
               selectionOrTarget
                  (fmap ("Delete from diagram " <>) . itemsDescription)
                  deleteItem
                  dId,
            Menu.MenuItem "Delete from model" $ Right $
               selectionOrTarget (fmap ("Delete " <>) . itemsDescription) deleteModelItem dId
         ]
      ]
   where
      deleteModelItem dId1 =
         withItem1 dId1 $ \d -> do
            deleteItem dId1
            case ascendedId d of
               Nothing -> return ()
               Just mId -> liftBase $ lift $ do
                  goToEntity mId
                  delete
      selectInModel = do
         sel <- use deltaSelection
         contents <- use $ deltaDiagram . diagramContents
         let
            targets = if dId `S.member` sel then S.toList sel else [dId]
            newSet = S.fromList $ mapMaybe (ascendedId <=< (`M.lookup` contents))  targets
         liftBase $ setModelSelection newSet
         yieldViews


findInDiagrams :: (p ~ Paint d, Avatar p v w d, EntityClass v, HasDiagrams p v) =>
   DiagramId -> Delta d (Action d)
findInDiagrams dId =
      use (deltaDiagram . diagramContents . at dId . to (>>= ascendedId)) >>= \case
         Nothing -> yieldViews  -- Not an avatar. Should not happen.
         Just (target :: ModelId) -> do
            ents <- liftBase $ lift $ fromHere $ goToRoot >> modelPackageForest
            wrappers <- liftBase $ lift $
               filter (hasAvatar target . snd) . catMaybes <$>
               mapM getWrapper (concatMap flatten ents)
            let
               prs = map diagramPair wrappers
               firstItem = case prs of {[] -> U.nil; x:_ -> menuItemValue x}
            liftBase (openDialog (linkDialog $ const prs) firstItem) >>= \case
               Nothing -> yieldViews
               Just diagramId -> do
                  liftBase $ openDiagram diagramId
                  yieldViews
   where
      diagramPair (nm, dw) = ComboItem (nm ^. nameText) Nothing Nothing (wrappedEntityId dw)
      getWrapper :: (EntityClass v, HasDiagrams p v) =>
         Entity v -> ModelEdit v w (Maybe (Name, EntityWrapper p v))
      getWrapper e = withBaseType $ fmap (e ^. entityName,) <$> getDiagramWrapper e
      hasAvatar :: ModelId -> EntityWrapper p v -> Bool
      hasAvatar mId (DiagramWrapper _ diag _ _ _ _ _ _) =
         let elems = M.elems $ diag ^. diagramContents
         in any ((Just mId ==) . ascendedId) elems
      hasAvatar _ _ = False
      linkDialog prs = constantDialog $ Dialog "Diagrams" OkApplyButton $
         accum $ form Vertical [("Found in:", focusing id $ validate (not . U.null) $ comboBox prs)]


-- | Background menu for diagrams. Allows the diagram to be selected in the model.
-- Some diagrams may add other items using the monoid instance.
diagramBackgroundMenu :: (Avatar (Paint d) v w d) =>
   Point -> Menu (Action d)
diagramBackgroundMenu _ = Menu [[
         menuItem "Select diagram in model" $ mkScriptAction $ do
            diagId <- getDiagramModelId
            liftBase $ setModelSelection $ S.singleton diagId
            yieldViews
      ]]


-- | The core language for "ModelScript".
data UserScriptFunctor p v next =
   ScriptProperties ModelId (Maybe (Entity v, ReferenceValues) -> next)
      -- ^ Pop up the standard dialog to edit an item of type @v@ along with its relationships.
   | forall s . ScriptDialog (DialogSelector' (Model v) () s) s (Maybe s -> next)
      -- ^ Pop up a dialog to edit an item of type @s@.
   | ScriptOpenDiagram (EntityWrapper p v) next
      -- ^ Open an entity described by the wrapper in a diagram window.
      -- Note that the subtype prism used in
      -- the diagram is independent of the prism used in the script that opened it.
   | ScriptGetSelection (Set ModelId -> next)
      -- ^ Get the set of currently selected model items.
   | ScriptSetSelection (Set ModelId) next
      -- ^ Change the set of currently selected model items.
   | ScriptGetHint (Maybe Text -> next)
      -- ^ Get the current hint text.
   | ScriptSetHint (Maybe Text) next
      -- ^ Set the hint text on the bottom bar.
   | ScriptPerformIO (IO next)
      -- ^ The IO will be performed and the result returned.

instance Functor (UserScriptFunctor p v) where
   -- Can't use "deriving Functor" because of existential in ScriptDialog.
   fmap f (ScriptProperties v nxt) = ScriptProperties v $ f . nxt
   fmap f (ScriptDialog dialog v nxt) = ScriptDialog dialog v $ f . nxt
   fmap f (ScriptOpenDiagram d nxt) = ScriptOpenDiagram d $ f nxt
   fmap f (ScriptGetSelection nxt) = ScriptGetSelection $ f . nxt
   fmap f (ScriptSetSelection newSelection nxt) = ScriptSetSelection newSelection $ f nxt
   fmap f (ScriptGetHint nxt) = ScriptGetHint $ f . nxt
   fmap f (ScriptSetHint h nxt) = ScriptSetHint h $ f nxt
   fmap f (ScriptPerformIO act) = ScriptPerformIO $ f <$> act


-- | Monad for writing interactive scripts which can directly interact with the user by
-- popping up dialogs and opening diagrams.
--
-- [@p@] The rendering monad for diagrams.
--
-- [@v@] The underlying type for properties.
--
-- [@m@] The base monad for the interactive scripts.
newtype UserScript p v m a = UserScript {stepScript :: FreeT (UserScriptFunctor p v) m a}
   deriving (Functor, Applicative, Monad, MonadError e, MonadTrans)


-- | Class of monads derived from "UserScript".
class (Monad m, Monad script) =>
   ScriptMonad p v m script | script -> p, script -> v, script -> m where
      liftScript :: UserScript p v m a -> script a

instance (Monad m) => ScriptMonad p v m (UserScript p v m) where
   liftScript = id

instance (Monad m, Viewable d, p ~ Paint d, Base d ~ UserScript (Paint d) v m) =>
   ScriptMonad p v m (Delta d) where
      liftScript = liftBase


-- | Interactive scripts based on the ModelEdit monad.
type ModelScript p v w = UserScript p v (ModelEdit v w)


-- | Underlying model types are often unions of different types, and we want to make these
-- composable. Hence given a type @Foo@ there will be a family of scripts in the
-- @ModelScript p v Foo@ monad, and likewise for models of type @Bar@. We can put them together
-- by creating a union type @FooBar@ with prisms @_Foo :: Prism FooBar Foo@ and similarly @_Bar@,
-- and then use @promoteScript _Foo@ to hoist a @ModelScript p FooBar Foo@ up to
-- @ModelScript p FooBar FooBar@.
promoteScript :: Traversal' w1 w2 -> ModelScript p v w2 a -> ModelScript p v w1 a
promoteScript prsm (UserScript act) = UserScript $ hoistFreeT (withSubtype prsm) act


withBaseScript :: ModelScript p v v a -> ModelScript p v w a
withBaseScript (UserScript act) = UserScript $ hoistFreeT withBaseType act

-- | Executed on activation (usually by double-clicking) of a model element.
-- @Nothing@ means root was activated.
type ActivationScript p v w = Maybe (Entity v) -> ModelScript p v w (Maybe Text)


-- | The context menu for an entity. An argument of @Nothing@ means root. If the entity has
-- no context menu then return @Nothing@.
--
-- Menu items are scripts which can manipulate the package tree and open dialog boxes
-- and diagrams. Scripts should return a description of what they have changed for the
-- benefit of the Undo list. Scripts that change nothing should return @Nothing@.
type EntityMenu p v w = Maybe (Entity v) -> Maybe (Menu (ModelScript p v w (Maybe Text)))



-- | Class of entities that can be edited through ModelScripts
class Editable p v where
   -- | Action when an entity is double-clicked.
   editableActivation :: ActivationScript p v v
   -- | Context menu for an entity when right clicked in the model tree.
   editableTreeMenu :: EntityMenu p v v
   -- | Context menu for an entity when right clicked in a dialog.
   editableDisplayMenu :: EntityMenu p v v



{- | Open the properties dialog for the argument, and return a function that will update
values with the edits that were made.

If \"Cancel\" is clicked then this will return @Nothing@.

If the dialog has an \"Apply\" button then the rest of the script will be run every time
the button is clicked. Scripts should ensure that they handle this correctly.
-}
openProperties :: (Monad m, ScriptMonad p v m script) =>
   ModelId  -- ^ The ID of the entity to be edited.
   -> script (Maybe (Entity v, ReferenceValues))
openProperties uuid = liftScript $ UserScript $ liftF $ ScriptProperties uuid id


-- | Open a specified "Dialog" for an arbitrary type. The caveats for "openProperties" apply
-- to this function as well.
openDialog :: (Monad m, ScriptMonad p v m script) =>
   DialogSelector' (Model v) () a -> a -> script (Maybe a)
openDialog dialog v = liftScript $ UserScript $ liftF $ ScriptDialog dialog v id


-- | Given a model ID for a diagram of type @w@, open the diagram.
openDiagram :: (EntityClass v, HasDiagrams p v, ScriptMonad p v (ModelEdit v w) script) =>
   ModelId -> script ()
openDiagram modelId =
   liftScript $ lift (try $ goToEntity modelId >> current) >>= \case
      Left _ -> lift $ throwUser "The diagram has been deleted."
      Right Nothing -> lift $ throwInternal "Model root is not a diagram."
      Right (Just ent) -> do
         let entName = ent ^. entityName . nameText
         lift (withBaseType $ getDiagramWrapper ent) >>= \case
            Nothing -> lift $ throwInternal $ "\"" <> entName <> "\" is not a diagram."
            Just wrapper -> UserScript $ liftF $ ScriptOpenDiagram wrapper ()


-- | Open an entity in a diagram window.
openEditor :: (Monad m, ScriptMonad p v m script) => EntityWrapper p v -> script ()
openEditor wrapper = liftScript $ UserScript $ liftF $ ScriptOpenDiagram wrapper ()


-- | The IO action will be performed. When it completes the result will be returned.
performIO :: (Monad m, ScriptMonad p v m script) => IO a -> script a
performIO act = liftScript $ UserScript $ liftF $ ScriptPerformIO act


-- | Get the set of currently selected entities in the model.
getModelSelection :: (Monad m, ScriptMonad p v m script) => script (Set ModelId)
getModelSelection = liftScript $ UserScript $ liftF $ ScriptGetSelection id


-- | Change the set of currently selected entities in the model.
setModelSelection :: (Monad m, ScriptMonad p v m script) => Set ModelId -> script ()
setModelSelection mIds = liftScript $ UserScript $ liftF $ ScriptSetSelection mIds ()


-- | Display the hint in the bottom bar during the action, then reset it to the previous value
-- once the action exits.
withHint :: (Monad m, ScriptMonad p v m script) =>
   Text -> script a -> script a
withHint txt act = do
   oldHint <- liftScript $ UserScript $ liftF $ ScriptGetHint id
   liftScript $ UserScript $ liftF $ ScriptSetHint (Just txt) ()
   r <- act
   liftScript $ UserScript $ liftF $ ScriptSetHint oldHint ()
   return r


-- | Clear the hint text.
clearHint :: (Monad m, ScriptMonad p v m script) => script ()
clearHint = liftScript $ UserScript $ liftF $ ScriptSetHint Nothing ()


-- | Go to a "ModelId" with a known name. If it is not there then report its disappearance.
--
-- This is commonly used after "openProperties" or "openDialog".
goBackToEntity :: (EntityClass v, ScriptMonad p v (ModelEdit v w) script) =>
   ModelId -> Text -> script ()
goBackToEntity uuid nm =
   liftScript $ lift $ try (goToEntity uuid) >>= \case
      Left _ -> throwUser $ nm <> " has disappeared."
      Right () -> return ()


-- | Ask the user for a value and insert it as an entity. Returns the name and "ModelId" of the new
-- entity if successful. The name of the template is used as the base for a new name which is
-- unique in the parent.
insertEntity :: (EntityClass v, ScriptMonad p v (ModelEdit v w) script) =>
   v         -- ^ Template entity.
   -> ModelId   -- ^ Parent of new entity.
   -> script (Maybe (Name, ModelId))
insertEntity value parent = liftScript $ do
   lift $ goToEntity parent
   nm1 <- lift $ modelNewName $ value ^. name
   initEntity <- lift $ mkEntity $ name .~ nm1 $ value
   let uuid = entityId initEntity
   openProperties uuid >>= \case
      Nothing -> lift $ do
         goToEntity uuid
         delete
         return Nothing
      Just (newEntity, newRelations) -> do
         goBackToEntity uuid (initEntity ^. entityName . nameText)
         let
            changed = S.toList $ M.foldr S.union S.empty newRelations
         lift $ mapM_ recordModified changed
         void $ lift $ setData $ newEntity ^. entityProperties
         lift $ putReferenceValues initEntity newRelations
         nm2 <- lift currentName
         return $ Just (Name nm2, uuid)


-- | Create a clone of a package or similar construct with children as a new child of the
-- current location. All the children are cloned recursively.
--
-- Relations to enities within the cloned set are also duplicated. Relations to entities outside
-- the cloned set are updated according to their cardinality. Diagrams are cloned, with avatars
-- for entities within the cloned set being updated to refer to the clones.
clonePackage :: (HasDiagrams p v) =>
   ((ModelId -> Maybe ModelId) -> PackageMeta v -> PackageMeta v)
      -- ^ Function for updating the package metadata with new model IDs.
   -> Entity v   -- ^ Entity to be cloned.
   -> Name       -- ^ New name for entity.
   -> ModelEdit v v (Entity v)
clonePackage metaUpdate ent newName = do
      --  Get tree ready for cloning. IDs are original but the root is renamed.
      entityTree <- fromHere $ do
         goToEntity $ entityId ent
         Node (entityName .~ newName $ ent) <$> modelPackageForest
      let oldIds = map entityId $ flatten entityTree
      cloneMap <- M.fromList <$> forM oldIds (\i -> (i, ) <$> mkModelId)
      let cloneFunc = flip M.lookup cloneMap
      result <- cloneTree cloneFunc entityTree
      -- Create new relations for the newly created entities. This is done after the tree clone
      -- to ensure that all referenced entities exist.
      forM_ (M.toList cloneMap) $ \(oldId, newId) -> do
         oldRefs <- S.toList <$> queryRelations (NR.relations oldId)
         newEnt <- getEnt "Cloned package includes root." newId
         forM_ oldRefs $ \(oldTarget, refName) -> fromHere $ do
            -- If the old target has been cloned then ref the clone. Otherwise ref the old target.
            let newTarget = fromMaybe oldTarget $ M.lookup oldTarget cloneMap
            newTargetEnt <- getEnt "Cloned entity references root." newTarget
            addCheckedRelation refName newEnt newTargetEnt
      return result
   where
      getEnt msg modelId = fromHere $ do
         goToEntity modelId
         current >>= \case
            Nothing -> throwInternal msg
            Just ent1 -> return ent1
      -- Clone a single node with its children.
      cloneTree f (Node e cs) = fromHere $
         case f $ entityId e of
            Nothing -> throwInternal "Cloned entity not in ID table."
            Just newId -> do
               newEnt <- addEntity $ \parentId ->
                     (entityParent .~ parentId) $
                     (entityContents . _Package . packageMeta %~ metaUpdate f) $
                     (entityChildren .~ mempty) $  -- Clear children so they can be added below.
                     diagramMapAvatars f $
                     e {entityId = newId, entityCreated = thisChange}
               goToEntity $ entityId newEnt
               mapM_ (cloneTree f) cs
               return newEnt






-- | Converts the output of "insertEntity" to a log format when the new ModelId is not required.
logInsertion :: (Monad m) => Maybe (Name, a) -> m (Maybe Text)
logInsertion Nothing = return Nothing
logInsertion (Just (nm, _)) = return $ Just $ "Insert " <> nm ^. nameText


-- | Pop up a properties dialog for the entity.
editEntityProperties :: (EntityClass v, ScriptMonad p v (ModelEdit v w) script) =>
   ModelId -> script (Maybe Text)
editEntityProperties uuid = liftScript $ do
   lift $ goToEntity uuid
   eName <- lift currentName
   lift current >>= \case
      Nothing -> lift $ throwUser "Root has no editable properties."
      Just _ -> do
         oldRefs <- lift $ queryRelations $ getReferenceValues uuid
         openProperties uuid >>= \case
            Nothing -> return Nothing
            Just (newEnt, newRefs) -> do
               goBackToEntity uuid eName
               let
                  changedRefs = compareReferenceValues oldRefs newRefs
               forM_ changedRefs $ \u -> lift $ fromHere $ do
                  goToEntity u
                  currentIsModified
               changedVal <- lift $ setData $ newEnt ^. entityProperties
               lift $ putReferenceValues newEnt newRefs
               if changedVal || not (null changedRefs)
                  then return $ Just $ "Edit " <> eName <> " properties"
                  else return Nothing


deleteEntity :: (EntityClass v, ScriptMonad p v (ModelEdit v w) script) =>
   ModelId -> script (Maybe Text)
deleteEntity uuid = liftScript $ lift $ do  -- ModelEdit monad
   goToEntity uuid
   target <- currentName
   delete
   return $ Just $ "Delete " <> target


-- | Add a new element to a diagram as an avatar for a new entity in the model.
--
-- During the rubber-band phase the diagram avatar will be given null "ModelId" and "DiagramId".
-- The drawing function should be able to cope with this. Once the points have been selected the
-- avatar will be recreated and inserted in the diagram with the "ModelId" of the entity it
-- represents.
newBoxAvatarTool :: (Avatar p v w d) =>
   ModelId  -- ^ ID of the package which is going to have the new entity added.
   -> (ModelId -> DiagramId -> BoundBox -> d)   -- ^ Diagram avatar for the new item.
   -> (BoundBox -> p ())  -- ^ Draw the new element given the two points.
   -> v   -- ^ The item to add.
   -> Delta d (Action d)
newBoxAvatarTool parent avatarFunc drawFunc newItem = do
   c1 <- withHint "Click and drag to define the new shape." yieldViews
   getCorners c1
      where
         getCorners act =
            case actionCommand act of
               Select -> do
                  addShape
                        (actionLocation act `movePoint` (-100, -50))
                        (actionLocation act `movePoint` (100, 50))
                  yieldViews
               Drag _ -> do
                  (p1, p2, act2) <- rubberBand drawShape (actionLocation act)
                  addShape p1 p2
                  return act2
               _ -> processSingleAction drawShape mempty act >>= getCorners
         drawShape p1 p2 = drawFunc $ mkBoundBox p1 p2
         addShape p1 p2 = do
            newEnt <- liftBase $ insertEntity newItem parent
            case newEnt of
               Nothing -> return ()  -- Cancel pressed in properties dialog.
               Just (newName, modelId) -> withHint "Enter details." $ do
                  void $ addItem $ \dgrmId -> avatarFunc modelId dgrmId $ mkBoundBox p1 p2
                  tellCheckpoint $ "Add " <> newName ^. nameText <> " to diagram"


newArrowAvatarTool :: (Avatar p v w d, Connectable d) =>
   ModelId  -- ^ ID of the package which is going to have the new entity added.
   -> (ModelId -> DiagramId -> LineShape -> d)   -- ^ Diagram avatar for the new item.
   -> v     -- ^ The element to add.
   -> (d -> Delta d ())
      -- ^ Update the model to match the new arrow. The entity already exists when this is called,
      -- but any other actions (such as relationships with endpoints) should be handled here.
   -> Delta d (Action d)
newArrowAvatarTool
         parent
         avatarFunc
         newModelItem
         modelUpdateFunc = do
      liftBase $ lift $ goToEntity parent
      (mShape, action) <- getArrowPoints $ avatarFunc U.nil
      case mShape of
         Just shape -> do
            ent <- liftBase $ lift $ do
               goToEntity parent
               mkEntity newModelItem
            newArrow <- addItem $ \u -> avatarFunc (entityId ent) u shape
            modelUpdateFunc newArrow
            tellCheckpoint $ "Add " <> ent ^. entityName . nameText
            return action
         Nothing -> return action


-- | Read a sequence of points from the mouse and turn them into a line shape.
getArrowPoints :: (Connectable d, Monad m, Base d ~ UserScript (Paint d) v m) =>
   (DiagramId -> LineShape -> d)   -- ^ Transient diagram element for drag animation.
   -> Delta d (Maybe LineShape, Action d)
getArrowPoints arrowFunc =
      withHint "Click and drag to start the arrow." $ do
         action1 <- yieldViews
         execAction action1
   where
      execAction act = do
         case actionCommand act of
            Drag _ -> do
               let
                  pt1 = actionLocation act
                  arr = arrowFunc transId $ LineShape (Unconnected pt1, Unconnected pt1) mempty
               end1 <- mkEndPoint ConnectionFrom arr pt1
               withHint progressHint $ do
                  (shape, action2) <- dragEnd (LineShape (end1, Unconnected pt1) mempty) act
                  case shape ^. lineEnds . _2 of
                     Unconnected {} -> do
                        transientView <- itemView $ arrowFunc transId shape
                        tellTransients $ ViewSet $ M.singleton transId transientView
                        (_1 %~ Just) <$> getMorePoints shape action2
                     Connected {} -> do
                        tellTransients $ ViewSet $ M.singleton transId mempty
                        return (Just shape, action2)
            _ -> processSingleAction drawShape mempty act >>= execAction
      drawShape _ _ = return ()  -- Only used when dragging anyway, so should never happen.
      dragEnd shape1 action1 = do
         let transient = arrowFunc transId shape1
         action2 <- processDrag1 (return (transient, shape1)) dragShapeTo action1
         let dragPoint = case actionCommand action2 of
               Drag d -> actionLocation action2 `movePoint` d  -- Should never happen.
               EndDrag d -> actionLocation action2 `movePoint` d
               _ -> actionLocation action2
         end2 <- mkEndPoint ConnectionTo transient dragPoint
         shape2 <- lineUpdate $ lineEnds . _2 .~ end2 $ shape1
         return (shape2, action2)
      getMorePoints shape1 action1 = do
         let transient = arrowFunc transId shape1
         case actionCommand action1 of
            EndDrag _ -> yieldViews >>= getMorePoints shape1
            Drag _ -> do
               newEnd <- mkEndPoint ConnectionTo transient $ actionLocation action1
               (shape2, action2) <- dragEnd (pointAppend shape1 newEnd) action1
               case shape2 ^. lineEnds . _2 of
                  Unconnected {} -> do
                     transientView <- itemView $ arrowFunc transId shape2
                     tellTransients $ ViewSet $ M.singleton transId transientView
                     getMorePoints shape2 action2
                  Connected {} -> do
                     tellTransients $ ViewSet $ M.singleton transId mempty
                     return (shape2, action2)
            _ -> do
               tellTransients $ ViewSet $ M.singleton transId mempty
               return (shape1, action1)
      dragShapeTo (transient, shape) act dPos = do
         let pt = actionLocation act `movePoint` dPos
         newEnd <- mkEndPoint ConnectionTo transient pt
         finalShape <- lineUpdate $ lineEnds . _2 .~ newEnd $ shape
         case actionCommand act of
            Drag _ -> do
               let trans = arrowFunc transId finalShape
               vw <- itemView trans
               tellTransients $ ViewSet $ M.singleton transId vw
            EndDrag _ ->
               tellTransients $ ViewSet $ M.singleton transId mempty
            _ -> return ()
      pointAppend (LineShape (e1, e2) middle) newEnd =
         LineShape (e1, newEnd) $ V.snoc middle $ connectorPoint e2
      -- A fixed but arbitrary UUID for the transient line.
      transId = U.fromWords 0x585f9fe9 0xa511a484 0xf93f09bb 0x274a2a10
      progressHint = "Click and drag to the next point. Finish with drag to target or single click."


-- | Add a new box element which is not associated with a new model entity.
newDiagramBoxTool :: (Viewable d, Base d ~ ModelScript p v w, Monad p, Paint d ~ p) =>
   a    -- ^ Default data to go inside the new element.
   -> DialogSelector' (Model v) () a   -- ^ Dialog to open (if any) when item is created.
   -> (DiagramId -> BoundBox -> a -> d)   -- ^ Diagram element.
   -> (d -> p ())  -- ^ Draw the new element.
   -> Text   -- ^ Message for undo log.
   -> Delta d (Action d)
newDiagramBoxTool v1 dSel newItemF drawF msg =
   withHint "Click and drag to define the new shape." $ do
      act <- yieldViews
      (mShape, act2) <- case actionCommand act of
         Select -> do
            let result = BoundBox
                  (actionLocation act `movePoint` (-100, -50))
                  (actionLocation act `movePoint` (100, 50))
            return (Just result, act)
         Drag _ -> do
            (p1, p2, act2) <- rubberBand
                  (\pa pb -> drawF $ newItemF U.nil (mkBoundBox pa pb) v1)
                  (actionLocation act)
            return (Just $ mkBoundBox p1 p2, act2)
         _ ->
            return (Nothing, act)
      case mShape of
         Just shape -> withHint "Enter details." $
            liftBase (openDialog dSel v1) >>= \case
               Just v2 -> do
                  void $ addItem $ \u -> newItemF u shape v2
                  tellCheckpoint msg
               Nothing ->
                  return ()
         Nothing ->
            return ()
      return act2


-- | Add a new arrow which is not associated with a new model entity.
newDiagramArrowTool ::
   (Connectable d, Viewable d, Base d ~ ModelScript p v w, Monad p, Paint d ~ p) =>
   a  -- ^ Default data to go inside the new arrow.
   -> Maybe (Dialog' (Model v) () a)   -- ^ Dialog to open (if any) when item is created.
   -> (DiagramId -> LineShape -> a -> d)   -- ^ Diagram avatar for the new item.
   -> Text   -- ^ Message for undo log.
   -> Delta d (Action d)
newDiagramArrowTool v1 dialogM newItemF msg = do
      (mShape, action) <- getArrowPoints $ \dId s -> newItemF dId s v1
      case mShape of
         Just shape -> do
            case dialogM of
               Just dialog ->
                  let dialog1 = dialog {dialogButtons = OkButton}
                  in liftBase (openDialog (const $ return $ Just dialog1) v1) >>= mapM_ (\v2 -> do
                           void $ addItem $ \u -> newItemF u shape v2
                           tellCheckpoint msg
                        )
               Nothing -> do
                  void $ addItem $ \u -> newItemF u shape v1
                  tellCheckpoint msg
            return action
         Nothing -> return action


-- | Tool for editing the view control (i.e. diagram appearance) dialog.
viewControlTool :: (EntityClass v, Viewable d, Base d ~ ModelScript p v w) =>
   ModelId  -- ^ The identity of this diagram.
   -> Traversal' w a  -- ^ How to find the view data in the entity.
   -> (Model v -> a -> ViewContext d -> ViewContext d)
         -- ^ Write updated view data back into the context.
   -> DialogSelector' (Model v) () a     -- ^ User dialog.
   -> DeltaTool d
viewControlTool modelId trav storeView dialog =
   DeltaTool "view-control" "Edit the diagram style" $ DeltaToolAction $ mkScriptAction $ do
      liftBase $ lift $ goToEntity modelId
      prsm <- liftBase $ lift $ usingTraversal return
      liftBase (lift current) >>= \case
         (preview (_Just . entityContents . prsm . trav) -> Just vc) -> do
            model <- liftBase $ lift getModel
            liftBase (openDialog dialog vc) >>= \case
               Nothing -> yieldViews
               Just v2 -> do
                  modify $ deltaContext %~ storeView model v2
                        -- (.=) is imported from Aeson not Lens, so using "modify" instead.
                  void $ liftBase $ lift $ modifyValue $ trav .~ v2
                  tellAll
                  tellCheckpoint "Diagram style edited."
                  yieldViews
         _ -> yieldViews  -- Should never happen.


-- | Search for text in entities and select matches. The type matches other edit functions
-- for convenience, but it always returns @Nothing@.
findTextTool :: (EntityClass v) => ModelScript p v v (Maybe Text)
findTextTool =
      openDialog (constantDialog findDialog) findDefault >>= \case
         Nothing -> return Nothing
         Just v -> do
            let (caseBlind, needle) = v
            if T.null needle
               then do
                  setModelSelection S.empty
                  return Nothing
               else do
                  result <- lift $ searchEntities caseBlind needle
                  setModelSelection $ S.fromList $ map fst result
                  return Nothing
   where
      findDialog = Dialog "Find in Model" OkButton $ accum $ form Vertical [
            ("Case insensitive: ", focusing _1 tickBox),
            ("Search for: ", focusing _2 $ styled1 "find-string" simpleTextBox)
         ]
      findDefault = (True, "")


-- | Where a many-to-one relationship exists between entities an update must
-- also delete any old relationship, otherwise the relationship will become many-to-many.
--
-- Unlike "addCheckedRelation" this routine does not check the type of the relation, and so
-- can operate with only the UUIDs.
updateManyToOne :: (EntityClass v) =>
   NR.Relation   -- ^ The relationship to be changed.
   -> ModelId    -- ^ The ID of the item on the \"Many\" side of the relationship.
   -> ModelId    -- ^ The new entity on the \"One\" side that the item will be connected to,
                 -- or "U.nil" for none.
   -> ModelEdit v w ()
updateManyToOne direction uuid newTarget = fromHere $ do
   oldTargets <- queryRelations $ NR.relation uuid direction
   when (S.toList oldTargets /= [newTarget]) $ do  -- No change means no-op.
      recordModified uuid
      forM_ oldTargets $ \i -> do
         recordModified i
         modifyRelations $ NR.delete uuid i direction
      unless (U.null newTarget) $ do
         recordModified newTarget
         modifyRelations $ NR.insert uuid newTarget direction


cloneEntityMenuItem :: (EntityClass v) => ModelId -> MenuItem (ModelScript p v w (Maybe Text))
cloneEntityMenuItem modelId = menuItem "Clone" $ do
   lift $ goToEntity modelId
   nm <- lift currentName
   lift cloneEntity >>= \case
      Just newEnt -> do
         setModelSelection $ S.singleton $ entityId newEnt
         return $ Just $ "Cloned " <> nm
      Nothing -> lift $ throwUser $ "Could not clone " <> nm


-- | Context menu for entities in the model tree.
entityMenu :: (EntityClass v) => Entity v -> Menu (ModelScript p v w (Maybe Text))
entityMenu ent =
   if isJust $ entityClone ent $ Name "dummy"  -- Result function is never used.
      then Menu.Menu [[
            menuItem "Properties" $ editEntityProperties $ entityId ent,
            cloneEntityMenuItem $ entityId ent,
            menuItem "Delete" $ deleteEntity $ entityId ent
         ]]
      else Menu.Menu [[
            menuItem "Properties" $ editEntityProperties $ entityId ent,
            menuItem "Delete" $ deleteEntity $ entityId ent
         ]]


-- | Context menu for diagrams in the model tree.
diagramEntityMenu :: (EntityClass v, HasDiagrams p v) =>
   ModelId -> Menu (ModelScript p v w (Maybe Text))
diagramEntityMenu ent = Menu.Menu [[
      menuItem "Open diagram" $ do
         openDiagram ent
         return Nothing,
      menuItem "Properties" $ editEntityProperties ent,
      cloneEntityMenuItem ent,
      menuItem "Delete" $ deleteEntity ent
   ]]


-- | Dialog for package names. Just a simple text field.
packageNameDialog :: Dialog' e w Name
packageNameDialog = Dialog "Package name" OkButton $
   accum $ focusing nameText $ validate (not . T.null) simpleTextBox


-- | Dialog for diagram names. Just a simple text field.
diagramNameDialog :: Dialog' e w (DiagramEntity d)
diagramNameDialog = Dialog "Diagram name" OkButton $
   accum $
   focusing (diagramEntityName . nameText) $
   validate (not . T.null) simpleTextBox
