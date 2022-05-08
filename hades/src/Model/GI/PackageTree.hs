{-# LANGUAGE OverloadedLabels #-}

{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

{- |

Maps the abstract PackageTree hierarchy to a GTK3 Tree model.

The "modelWidget" function is used to create the GTK3 tree widget. The architecture allows
the application to have a number of model editing widgets (typically a "modelWidget" and some
diagrams). Each such editor takes a @Behavior (Model v)@ value and returns an
@Event (ModelUpdate v Text)@. The top level application should feed all these updates into
"processModelUpdates".
-}
module Model.GI.PackageTree (
   -- * Data displayed by the GTK tree widget
   StockFunc,
   GtkTreeInfo (..),
   GtkEntity (..),
   ModelPath,
   goToTreePath,
   currentTreePath,
   entityToGtk,
   entityColumnTypes,
   entityToTreeRow,
   treeRowToEntity,
   clearTree,
   storeEntity,
   storeEntityTree,
   storeEntityForest,
   getEntity,
   getEntityTree,
   getEntityForest,
   getSelectedRows,
   treeSortF,
   -- * Model editing
   HistoryItem (..),
   ModelUpdate,
   updateGtkTree,
   replaceGtkTree,
   modelWidget,
   processModelUpdates,
   -- * Drag & Drop
   dragTargets,
   -- * Error Messages
   modelErrorWarning,
   modelErrorWarningOn,
) where

import Control.Lens
import Control.Monad
import Control.Monad.Trans
-- import Control.Monad.Writer  -- Required for checkGtkTree. See note at end of file.
import qualified Data.GI.Base.GType as Gtk
import Data.Int
import Data.IORef
import Data.List hiding (delete)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree
import Data.UUID hiding (null)
import qualified Data.UUID as UUID
import qualified GI.Gdk as Gdk
import qualified GI.GObject as G
import GI.Gtk (AttrOp ((:=)))
import qualified GI.Gtk as Gtk
import Model.Abstract.DiagramType
import Model.Abstract.PackageTree
import Reactive.Banana hiding ((<>))
import Reactive.Banana.Common
import Reactive.Banana.Frameworks
import Reactive.Banana.GI.Connect
import Reactive.Banana.GI.ErrorBox
import Reactive.Banana.GI.Menu
import Text.NaturalOrder
import Text.Read (readMaybe)


-- | Path to a model entity expressed as the indices of each child.
type ModelPath = [Int32]


-- | Convert a ModelPath into a GTK Tree Iterator that points to the unsorted store.
treeIterFromPath :: (MonadIO m) => GtkTreeInfo v -> ModelPath -> m (Maybe Gtk.TreeIter)
treeIterFromPath (GtkTreeInfo _ sorted _ _) path1 = do
   path2 <- Gtk.treePathNewFromIndices path1
   (b, iter) <- Gtk.treeModelGetIter sorted path2
   if b
      then Just <$> Gtk.treeModelSortConvertIterToChildIter sorted iter
      else return Nothing


-- | Follow a "TreePath" from the root to its target entity.
goToTreePath :: (EntityClass v) => ModelPath -> ModelEdit v w ()
goToTreePath [] = throwUser "Cannot go to the node above root."
goToTreePath (_:path) = do
      goToRoot
      forM_ path $ \x ->
         moveDown $ \tbl -> do
            when (x < 0 || x >= fromIntegral (M.size tbl)) $
                  throwUser "Path does not exist in model"
            return $ snd $ M.elemAt (fromIntegral x) tbl


-- | The entities from the root to the current one, expressed as numerical
-- indices within each package starting from zero. Note that the root has path of [0], so
-- the result is never nil.
currentTreePath :: (EntityClass v) => ModelEdit v w ModelPath
currentTreePath = map fromIntegral . reverse <$> fromHere go
   where
      go = current >>= \case
         Just e -> do
            moveUp
            tbl <- currentChildren
            case M.lookupIndex (e ^. entityName) tbl of
               Just n -> (n :) <$> go
               Nothing -> throwInternal $
                  "Path item " <> e ^. entityName . nameText <> " has gone missing."
         Nothing -> return [0]


-- | A node in the GTK version of the model.
data GtkEntity = GtkEntity {
   gtkName :: Name,
   gtkIcon :: Text,  -- ^ GTK stock ID for the icon to display.
   gtkId :: ModelId
} deriving (Eq, Ord, Show)


-- | The column types to store a GtkEntity structure in a row.
entityColumnTypes :: [Gtk.GType]
entityColumnTypes = [
      Gtk.gtypeString,  -- Name
      Gtk.gtypeString,  -- Icon
      Gtk.gtypeUInt,    -- UUID as 4 word32 values
      Gtk.gtypeUInt,
      Gtk.gtypeUInt,
      Gtk.gtypeUInt
   ]


-- | Convert a GtkEntity to a row.
entityToTreeRow :: (MonadIO m) => GtkEntity -> m [Gtk.GValue]
entityToTreeRow ent =
      sequence [
            liftIO $ Gtk.toGValue $ Just $ ent ^. to gtkName . nameText,
            liftIO $ Gtk.toGValue $ Just $ ent ^. to gtkIcon,
            liftIO $ Gtk.toGValue s1,
            liftIO $ Gtk.toGValue s2,
            liftIO $ Gtk.toGValue s3,
            liftIO $ Gtk.toGValue s4
         ]
   where
      (s1, s2, s3, s4) = toWords $ gtkId ent


-- | Convert a row back to a GtkEntity.
treeRowToEntity :: (MonadIO m) => [Gtk.GValue] -> m GtkEntity
treeRowToEntity (nm : ic : s1 : s2 : s3 : s4 : _) = liftIO $
   GtkEntity <$>
      (Name . fromMaybe "*error*" <$> Gtk.fromGValue nm) <*>
      (fromMaybe "" <$> Gtk.fromGValue ic) <*>
      (fromWords <$>
         Gtk.fromGValue s1 <*>
         Gtk.fromGValue s2 <*>
         Gtk.fromGValue s3 <*>
         Gtk.fromGValue s4)
treeRowToEntity _ = error "treeRowToEntity: insufficient elements provided."


-- | Clear the GTK tree and create a new root node. Returns an iterator on the root.
clearTree :: (MonadIO m, Gtk.IsTreeStore a) => a -> Name -> m Gtk.TreeIter
clearTree tree nm = do
   Gtk.treeStoreClear tree
   newRow <- Gtk.treeStoreInsert tree Nothing (-1)
   row <- entityToTreeRow $ GtkEntity nm "model-home" UUID.nil
   Gtk.treeStoreSet tree newRow [0 .. genericLength entityColumnTypes - 1] row
   return newRow


-- | Store a GtkEntity as a child of the iterator position and return an iterator that
-- points to it. It is not possible to create a new root with this function.
storeEntity :: (MonadIO m, Gtk.IsTreeStore a) =>
   a -> Gtk.TreeIter -> GtkEntity -> m Gtk.TreeIter
storeEntity tree iter ent = do
   newRow <- Gtk.treeStoreInsert tree (Just iter) (-1)
   row <- entityToTreeRow ent
   Gtk.treeStoreSet tree newRow [0 .. genericLength entityColumnTypes - 1] row
   return newRow


-- | Store a tree as a child of the iterator position.
storeEntityTree :: (MonadIO m, Gtk.IsTreeStore a) =>
   a -> Gtk.TreeIter -> Tree GtkEntity -> m ()
storeEntityTree tree iter (Node ent entlings) = do
   newIter <- storeEntity tree iter ent
   storeEntityForest tree newIter entlings


-- | Store a list of trees as children of the iterator position.
storeEntityForest :: (MonadIO m, Gtk.IsTreeStore a) =>
   a -> Gtk.TreeIter -> Forest GtkEntity -> m ()
storeEntityForest tree iter forest = forM_ forest $ storeEntityTree tree iter


-- | Retrieve a GtkEntity from the iterator position.
getEntity :: (MonadIO m, Gtk.IsTreeModel a) =>
   a -> Gtk.TreeIter -> m GtkEntity
getEntity tree iter = do
   row <- forM [0 .. genericLength entityColumnTypes - 1] $ Gtk.treeModelGetValue tree iter
   treeRowToEntity row


-- | Retrieve the current node and any descendants as a Tree.
getEntityTree :: (MonadIO m, Gtk.IsTreeModel a) =>
   a -> Gtk.TreeIter -> m (Tree GtkEntity)
getEntityTree tree iter = do
   ent <- getEntity tree iter
   (b, child) <- Gtk.treeModelIterNthChild tree (Just iter) 0
   entlings <- if b then getEntityForest tree child else return []
   return $ Node ent entlings


-- | Starting at the current location, get the current node and all the siblings that follow it,
-- and all their descendants.
getEntityForest :: (MonadIO m, Gtk.IsTreeModel a) =>
   a -> Gtk.TreeIter -> m (Forest GtkEntity)
getEntityForest tree iter = do
   x <- getEntityTree tree iter
   b <- Gtk.treeModelIterNext tree iter
   xs <- if b then getEntityForest tree iter else return []
   return $ x : xs


-- | Get the selected rows as a set of ModelPaths
getSelectedRows :: Gtk.TreeSelection -> IO (Set ModelPath)
getSelectedRows sel = do
   (paths1, _) <- Gtk.treeSelectionGetSelectedRows sel
   -- paths2 <- catMaybes <$> mapM Gtk.treePathGetIndices paths1
   paths2 <- catMaybes <$> mapM Gtk.treePathGetIndices paths1
   return $ S.fromList paths2


-- | Maps entity values onto stock icon IDs for the TreeView.
type StockFunc v = Entity v -> Text


-- | Extract the GtkEntity information from the Entity.
entityToGtk :: (EntityClass v) =>
   StockFunc v
   -> Entity v
   -> GtkEntity
entityToGtk picF ent =
      GtkEntity {
         gtkName = ent ^. entityName,
         gtkIcon = picF ent,
         gtkId = entityId ent
      }


-- | The GTK tree model information required for model updates to be reflected in the widget.
data GtkTreeInfo v = GtkTreeInfo
   Gtk.TreeStore
   Gtk.TreeModelSort
   Gtk.TreeView
   (StockFunc v)


{-
Design Note:

The GTK TreeModel lives in the IO monad, while the internal Model lives in the ModelEdit monad.
Hence the only ways to keep the internal Model in synch with the visible GTK TreeModel are:

1: Rebuild the GTK TreeModel every time the internal Model changes (which is O(n) and potentially
very slow on large models).

2: Record changes to the internal Model and then apply those to the GTK TreeModel.

The latter is fragile, but the fragility is manageable. It turns out that only four operations on
the internal Model are visible in the GTK TreeModel, and these are encoded in the ModelEditLog
type, which is recorded by the primitive operations in the ModelEdit monad. Hence the
updateGtkTree function can take this log and apply matching operations to the GTK TreeModel in the
IO monad.

See the note about checkGtkTree at the end of this file for details on test and debug for this
function.
-}


-- | Interpret a log of model changes as instructions to update the GTK TreeModel.
updateGtkTree :: (Gtk.IsWidget parent, EntityClass v) =>
   parent             -- ^ Parent widget for error pop-ups.
   -> GtkTreeInfo v   -- ^ GTK objects to update.
   -> [ModelEditLog v]  -- ^ Changes to make.
   -> IO ()
updateGtkTree parent info@(GtkTreeInfo store _ vw picF) = mapM_ doUpdate
   where
      doUpdate (ModelAdd uuid model) =
         case evalModelEdit id model $ entityPath uuid of
            Left err -> modelErrorWarning parent err
            Right (_, Nothing) -> modelErrorWarning parent $ InternalError
               "updateGtkTree Add: New entity has gone missing. Cannot update tree widget."
            Right (path, Just ent) -> do
               treeIterFromPath info path >>= \case   -- Check if it already exists. See note below.
                  Nothing -> return ()  -- Normal case.
                  Just it -> do
                     gtkEnt <- getEntity store it
                     when (gtkId gtkEnt == entityId ent) $ void $ Gtk.treeStoreRemove store it
               treeIterFromPath info (init path) >>= \case
                  Nothing -> pathNotFound ("Add " <> ent ^.entityName . nameText) (init path)
                  Just it -> do
                     newIter <- storeEntity store it $ entityToGtk picF ent
                     newPath <- Gtk.treeModelGetPath store newIter
                     Gtk.treeViewExpandToPath vw newPath
                  -- init is safe because currentTreePath never returns [].
      doUpdate (ModelDelete uuid model) = do
         case evalModelEdit id model $ goToEntity uuid >> currentTreePath of
            Left err -> modelErrorWarning parent err
            Right path ->
               treeIterFromPath info path >>= \case
                  Nothing -> pathNotFound "Delete" path
                  Just it -> void $ Gtk.treeStoreRemove store it
         when (UUID.null uuid) $  -- We have just deleted the root node, so now restore it.
            void $ clearTree store $ Name $ modelName model
      doUpdate (ModelMove uuid before after) =
         case evalModelEdit id before $ goToEntity uuid >> currentTreePath of
            Left err -> modelErrorWarning parent err
            Right oldPath -> treeIterFromPath info oldPath >>= \case
               Nothing -> pathNotFound "Move (old)" oldPath
               Just oldIter -> do
                  target <- getEntityTree store oldIter
                  void $ Gtk.treeStoreRemove store oldIter
                  case evalModelEdit id after $ goToEntity uuid >> moveUp >> currentTreePath of
                     Left err -> modelErrorWarning parent err
                     Right newPath -> treeIterFromPath info newPath >>= \case
                        Nothing -> pathNotFound "Move (new)" newPath
                        Just newIter -> storeEntityTree store newIter target
      doUpdate (ModelRename uuid before after) =
         if UUID.null uuid
         then
            treeIterFromPath info [0] >>= \case
               Nothing -> pathNotFound "Rename root" [0]
               Just it -> do
                  replacement <- entityToTreeRow $ GtkEntity
                     (Name $ modelName after)
                     "model-home"
                     UUID.nil
                  void $ Gtk.treeStoreSet store it columns replacement
         else
            case evalModelEdit id before $ goToEntity uuid >> currentTreePath of
               Left err -> modelErrorWarning parent err
               Right path ->
                  treeIterFromPath info path >>= \case
                     Nothing -> pathNotFound "Rename" path
                     Just it ->
                        case evalModelEdit id after $ goToEntity uuid >> current of
                           Left err -> modelErrorWarning parent err
                           Right Nothing -> modelErrorWarning parent $ InternalError
                              "updateGtkTree: Renamed entity is missing. Cannot update widget."
                           Right (Just ent) -> do
                              replacement <- entityToTreeRow $ entityToGtk picF ent
                              void $ Gtk.treeStoreSet store it columns replacement
      doUpdate (ModelOther _ _) = return ()
      columns = [0 .. genericLength entityColumnTypes - 1]
      entityPath uuid = do
         goToEntity uuid
         p <- currentTreePath
         e <- current
         return (p, e)
      pathNotFound txt path = modelErrorWarning parent $ InternalError $
            "updateGtkTree " <> txt <> ": " <> T.pack (show path) <>
            " not found in tree widget: cannot update."

{- Design note: if "Apply" is clicked repeatedly when creating a new entity the script will return
a log containing a ModelAdd for each click. This must be checked for in the ModelAdd.
 -}


-- | The entire model has just changed. Overwrite the current model in the GTK tree with a new one.
replaceGtkTree :: (EntityClass v) =>
   GtkTreeInfo v   -- ^ GTK objects to update.
   -> Model v         -- ^ Replacement model value.
   -> IO ()
replaceGtkTree (GtkTreeInfo store sorted vw picF) newModel =
      case evalModelEdit id newModel $ goToRoot >> modelPackageForest of
         Left err -> modelErrorWarning vw err
         Right forest -> do
            -- Get the selected set.
            treeSel <- Gtk.treeViewGetSelection vw
            (paths, _) <- Gtk.treeSelectionGetSelectedRows treeSel
            Gtk.treeSelectionUnselectAll treeSel  -- Probably happens anyway, but make sure.
            selected <- catMaybes <$> mapM getModelId paths
            -- treeViewMapExpandedRows cannot return anything, so stash results in IORef.
            expandedRef <- newIORef []
            Gtk.treeViewMapExpandedRows vw $ logExpanded expandedRef
            i <- clearTree store $ Name $ modelName newModel
            let gtkForest = fmap (entityToGtk picF) <$> forest
            storeEntityForest store i gtkForest
            expanded <- readIORef expandedRef
            -- Restore tree expansion state.
            forM_ expanded $ \modelId ->
               case evalModelEdit id newModel $ goToEntity modelId >> currentTreePath of
                  Left err -> modelErrorWarning vw err
                  Right path -> do
                     path2 <- Gtk.treePathNewFromIndices path
                     Gtk.treeViewExpandToPath vw path2
            -- Restore tree selection state.
            forM_ selected $ \modelId ->
               case evalModelEdit id newModel $ goToEntity modelId >> currentTreePath of
                  Left err -> modelErrorWarning vw err
                  Right path -> do
                     path2 <- Gtk.treePathNewFromIndices path
                     Gtk.treeSelectionSelectPath treeSel path2
   where
      getModelId :: Gtk.TreePath -> IO (Maybe ModelId)
      getModelId path = do
         (b, i) <- Gtk.treeModelGetIter sorted path
         if b
            then do
               n <- Gtk.treeModelGetNColumns sorted
               vals <- mapM (Gtk.treeModelGetValue sorted i) [0..(n-1)]
               Just . gtkId <$> treeRowToEntity vals
            else return Nothing
      logExpanded ref _ path =
         getModelId path >>= (modifyIORef ref . maybe id (:))


-- | A point in the model history is represented as the model state and the description of
-- the change that *followed* it. Hence for \"undo\" it is only necessary to revert to the model
-- state corresponding to the change description being undone. For "redo" the state from the
-- following entry must be used.
data HistoryItem v = HistoryItem {
      undoValue :: Model v, -- ^ Model state after the unde or redo.
      undoText :: Text      -- ^ Description of the change.
   }


{-
Design note.

The model is a forest, but this creates problems with dragging and dropping in a TreeView; you
can't move anything to the root because there is no representation to drop it on. Hence modelWidget
creates a fake "home" node as the parent of everything.

In the future there will be more than one model open at a time, and hence multiple "home" nodes.

This means that TreePath values from GTK have an extra leading 0 item prefixed, which has to be
ignored by goToTreePath.
-}

-- | Tree widget for a model. Visualised in a scrolled window in a viewport. The return values
-- are the GTK widget and tree data, update events, and menu events.
--
-- There is no way to keep the widget up to date with changes to the model behavior. Hence one
-- return value is the @GtkTreeInfo@, which provides access to the data being displayed by GTK.
-- It is up to the application to update this when the model changes.
modelWidget :: (Editable p v, EntityClass v) =>
   StockFunc v
   -> Model v                 -- ^ The initial value of the model.
   -> Changes (Model v)      -- ^ The ongoing model to display.
   -> Behavior (Set ModelId)  -- ^ The currently selected paths in the tree.
   -> MomentIO (GtkTreeInfo v,
        Event (Set ModelId),
        Event (ModelScript p v v (Maybe Text)))
modelWidget picF modelInit modelC selectB = do
      -- Get the data into a TreeStore.
      forest <- monadic [] $ evalModelEdit id modelInit gtkEntities
      (vw, store, sorted, renameEvent, activateEvent, menuPath, selectE) <-
         createTreeview (Name $ modelName modelInit) forest
      -- Set up event for activation dialogs.
      let
         info = GtkTreeInfo store sorted vw picF
         (activationError, activatedItem) =
            split $ evalModelEdit id <$> changesB modelC <@> (activateEvent <&> (\uuid -> do
                  goToEntity uuid
                  current
               ))
         activationUpdates = editableActivation <$> activatedItem
      stop1 <- reactimate1 $ modelErrorWarning vw <$> activationError
      -- Set up event for menus.
      let
         (menuError, menu) =
            split $ evalModelEdit id <$> changesB modelC <@> (menuPath <&> (\case
                  [] -> return Nothing
                  path -> do
                     goToTreePath path
                     editableTreeMenu <$> current
               ))
      menuEvent <- execute $ mkGtkMenu <$> filterJust menu
      popupMenuOn $ fst <$> menuEvent
      menuEventOut <- switchE $ snd <$> menuEvent
      stop2 <- reactimate1 $ modelErrorWarning vw <$> menuError
      -- Get current model into an IORef. This is needed by D&D during callbacks.
      modelRef <- liftIO $ newIORef modelInit
      stop3 <- reactimate1 $ writeIORef modelRef <$> changesE modelC
      -- Configure the TreeView for drag and drop, synchronised with the model.
      moveEvent <- modelDragAndDrop info vw modelRef
      -- Intercept key commands.
      keyEvent <- filterJust <$> registerIOSignal1 vw Gtk.onWidgetKeyPressEvent keyPressHandler
      let keyCommand = flip ($) <$> selectB <@> keyEvent  -- keyEvent applied to selectB
      -- Define the model behaviour and apply these events to it.
      let
         renameEvent1 =
            renameEvent <&> (\(path, newName) -> lift $ do  -- ModelEdit monad
               goToTreePath path
               oldName <- currentName
               changed <- renameEntity newName
               if changed
                  then return $ Just $ "Rename " <> oldName <> " as " <> newName ^. nameText
                  else return Nothing
            )
         modelScripts = sequenceEvents
               [renameEvent1, moveEvent, activationUpdates, menuEventOut, keyCommand]
      void $ Gtk.onWidgetDestroy vw $ stop1 >> stop2 >> stop3
      return (info, selectE, modelScripts)
   where
      gtkEntities = map (fmap $ entityToGtk picF) <$> modelPackageForest
      -- Set up the TreeView widget with a column for the name and icon.
      -- This code is sensitive to the order of the side effects within GTK.
      -- And people wonder why Haskellers think declarative interfaces are a good idea!
      createTreeview nm forest = do
         -- Set up the initial treeStore and create a treeView of it showing a single column.
         store <- Gtk.treeStoreNew entityColumnTypes
         sorted <- Gtk.new Gtk.TreeModelSort [#model := store]
         Gtk.treeSortableSetSortFunc sorted 0 treeSortF
         Gtk.treeSortableSetSortColumnId sorted 0 Gtk.SortTypeAscending
         iter <- clearTree store nm
         storeEntityForest store iter forest
         vw <- Gtk.treeViewNewWithModel sorted
         selection <- Gtk.treeViewGetSelection vw
         Gtk.treeSelectionSetMode selection Gtk.SelectionModeMultiple
         Gtk.treeViewSetHeadersVisible vw False
         Gtk.treeViewSetReorderable vw True
         -- styleContext <- Gtk.widgetGetStyleContext vw
         -- Gtk.styleContextAddClass styleContext "hades"
         -- Add a column to the view.
         column <- Gtk.treeViewColumnNew
         void $ Gtk.treeViewAppendColumn vw column
         -- Put two renderers for icon and name in the column.
         icon <- Gtk.cellRendererPixbufNew
         Gtk.treeViewColumnPackStart column icon False
         txt <- Gtk.cellRendererTextNew
         Gtk.setCellRendererTextEditable txt True
         -- Set up rename event.
         renameEvent <- filterJust <$> registerIOSignal2 txt Gtk.onCellRendererTextEdited
            (\path1 newName ->   -- path1 is the text version of the ModelPath.
               case mapM (readMaybe . T.unpack) $ T.splitOn ":" path1 of
                  Just path2 -> return ((), Just (path2, Name newName))
                  Nothing -> return ((), Nothing)
            )
         -- Set up row activation event carrying the UUID of the activated entity.
         activateEvent <- registerIOSignal2 vw Gtk.onTreeViewRowActivated $
            \path _ -> do
               (_, i) <- Gtk.treeModelGetIter sorted path
               item <- getEntity sorted i -- Trust TreeView for valid path
               return ((), gtkId item)
         -- Set up the menu handling events. There is no direct support for context menus in
         -- GTK3 TreeView, so we have to build it out of low level mouse events.
         menuEvent <- filterJust <$> registerIOSignal1
               vw
               Gtk.onWidgetButtonPressEvent
               (buttonHandler vw)
         -- Track the selected tree paths. Do not emit an event when updating from selectB.
         selectBlock <- liftIO $ newIORef False
         selectEvent <- registerIOSignal selection Gtk.onTreeSelectionChanged $ do
            blk <- liftIO $ readIORef selectBlock
            if blk
               then return ((), Nothing)
               else do
                  (rows, tree) <- Gtk.treeSelectionGetSelectedRows selection
                  result <- forM rows $ \row -> do
                     (_, i) <- Gtk.treeModelGetIter tree row
                     gtkId <$> getEntity tree i
                  return ((), Just $ S.fromList result)
         selectChange <- changes $
            updateSelection selectBlock selection <$> changesB modelC <*> selectB
         stop <- reactimate1' selectChange
         void $ Gtk.onWidgetDestroy vw stop
         -- Finish assembling the widgets ready for display.
         Gtk.treeViewColumnPackEnd column txt True
         Gtk.cellLayoutAddAttribute column txt "text" 0  -- GTK cells are stringly typed!
         Gtk.cellLayoutAddAttribute column icon "icon-name" 1
         Gtk.treeViewSetEnableTreeLines vw True
         path <- Gtk.treePathNewFromIndices [0]
         void $ Gtk.treeViewExpandRow vw path False
         return (vw, store, sorted, renameEvent, activateEvent, menuEvent, filterJust selectEvent)
      -- Handle mouse clicks to put up and remove menus. Right click activates a menu, next click
      -- removes it. Other clicks are rejected.
      buttonHandler vw event = do
         button <- Gdk.getEventButtonButton event
         x1 <- round <$> Gdk.getEventButtonX event
         y1 <- round <$> Gdk.getEventButtonY event
         (x2, y2) <- Gtk.treeViewConvertWidgetToBinWindowCoords vw x1 y1
         if button == 3  -- Right button
            then Gtk.treeViewGetPathAtPos vw x2 y2 >>= \case
                  (_, Just path1, _, _, _) -> do
                     -- path2 <- fromMaybe [] <$> Gtk.treePathGetIndices path1
                     path2 <- Gtk.treePathGetIndices path1
                     return (True, path2)
                  _ -> return (False, Just [])
            else return (False, Nothing)
      -- Handle key presses. For now only <Delete> is processed.
      keyPressHandler k = do
         v <- Gdk.getEventKeyKeyval k
         let r = M.lookup v recognisedKeys
         return (isJust r, r)
      recognisedKeys = M.fromList [
            (Gdk.KEY_Delete, \sel -> lift $ do
                  nms <- forM (S.toList sel) $ \i -> do
                     goToEntity i
                     nm <- currentName
                     delete
                     return nm
                  case nms of
                     [] -> return Nothing
                     [nm] -> return $ Just $ "Deleted " <> nm
                     _ -> return $ Just $ "Deleleted " <> T.pack (show $ length nms) <> " entities"
            )
         ]
      -- Track the tree selection. The set of selected rows is not an attribute so we can't
      -- use behaviorLink here. Also we have to be careful not to touch the selection when
      -- nothing has changed.
      updateSelection :: (EntityClass v) =>
         IORef Bool -> Gtk.TreeSelection -> Model v -> Set ModelId -> IO ()
      updateSelection ref selection model newSet1 = do
         writeIORef ref True  -- Block creation of selection events from these changes.
         let getPaths = forM (S.toList newSet1) $ \i -> goToEntity i >> currentTreePath
         case evalModelEdit id model getPaths of
            Left _ -> return ()  -- Should never happen.
            Right newPaths -> do
               let newSet2 = S.fromList newPaths
               treeView <- Gtk.treeSelectionGetTreeView selection
               oldSet <- getSelectedRows selection
               addSet <- mapM Gtk.treePathNewFromIndices $ S.toList $ newSet2 `S.difference` oldSet
               unSet <- mapM Gtk.treePathNewFromIndices $ S.toList $ oldSet `S.difference` newSet2
               forM_ addSet $ \path -> do
                  Gtk.treeViewExpandToPath treeView path
                  Gtk.treeSelectionSelectPath selection path
               forM_ unSet $ Gtk.treeSelectionUnselectPath selection
         writeIORef ref False  -- Re-enble selection events.


-- | Set up drag and drop for the GTK Treeview displaying the model.
modelDragAndDrop :: (EntityClass v) =>
   GtkTreeInfo v
   -> Gtk.TreeView
   -> IORef (Model v)
   -> MomentIO (Event (ModelScript p v v (Maybe Text)))
modelDragAndDrop info@(GtkTreeInfo store _ _ _) vw modelRef = do
      -- Start by doing the basic enabling for DnD on the model.
      targets <- dragTargets
      Gtk.treeViewEnableModelDragSource vw
            [Gdk.ModifierTypeButton1Mask]
            targets
            [Gdk.DragActionCopy]
      Gtk.treeViewEnableModelDragDest vw targets [Gdk.DragActionCopy]
      -- Notification of drop events.
      void $ registerIOSignal6 vw Gtk.onWidgetDragDataReceived $ \_ _ _ _ _ _ -> do
         G.signalStopEmissionByName vw "drag_data_received"
            -- Identifier as string: yuck! Have to do this or GTK throws a tantrum.
         return ((), Nothing)
      void $ registerIOSignal4 vw Gtk.onWidgetDragMotion $ \context x1 y1 t -> do
         -- Not sure whether Gtk.dragStatus needs to be called in here or if it could be
         -- done by reactimate, so I'll play safe and keep it in here.
         (x2, y2) <- Gtk.treeViewConvertWidgetToBinWindowCoords vw x1 y1
         (_, mPath, _, _, _) <- Gtk.treeViewGetPathAtPos vw x2 y2
         case mPath of
            Nothing -> return (False, Nothing)
            Just path -> do
               sel <- Gtk.treeViewGetSelection vw
               target <- fromMaybe [] <$> Gtk.treePathGetIndices path
               rows <- S.toList <$> getSelectedRows sel
               case rows of
                  [source] -> do
                     ok <- moveCheck source target
                     Gtk.treeViewSetDragDestRow
                           vw
                           (if ok then Just path else Nothing)
                           Gtk.TreeViewDropPositionIntoOrAfter
                     Gdk.dragStatus context [Gdk.DragActionCopy | ok] t
                     return (ok, Nothing)
                  _ -> return (False, Nothing)
      filterJust <$> registerIOSignal4 vw Gtk.onWidgetDragDrop (\context x1 y1 t -> do
            G.signalStopEmissionByName vw "drag_drop"  -- Identifier as string: yuck.
                  -- Need to do this to stop the default GTK tree drop processing.
            (x2, y2) <- Gtk.treeViewConvertWidgetToBinWindowCoords vw x1 y1
            (_, mPath, _, _, _) <- Gtk.treeViewGetPathAtPos vw x2 y2
            case mPath of
               Nothing -> do
                  Gtk.dragFinish context False False t
                  return (False, Nothing)
               Just path -> do
                  -- target <- fromMaybe [] <$> Gtk.treePathGetIndices path
                  target <- fromMaybe [] <$> Gtk.treePathGetIndices path
                  sel <- Gtk.treeViewGetSelection vw
                  rows <- S.toList <$> getSelectedRows sel
                  case rows of
                     [source] -> do
                        ok <- moveCheck source target
                        if ok
                           then do
                              Gtk.dragFinish context True False t
                              return (True, Just $ makeMove source target)
                           else do
                              Gtk.dragFinish context False False t
                              return (False, Nothing)
                     _ -> do
                        Gtk.dragFinish context False False t
                        return (False, Nothing)
         )
   where
      -- True if you can drop the source on to the target.
      moveCheck source target = do
         mSource <- getModelContents source
         mTarget <- getModelContents target
         case (mSource, mTarget) of
            (Just src, Just tgt) ->
               return $ entityCanMove src && entityCanHaveChild tgt src
            _ -> return False  -- Something has gone wrong, so disallow everything.
      -- Get the entity contents for the path. If root then return a fictional Package.
      -- This is only used for checking D&D destinations, so a Package is the Right Thing.
      getModelContents path =
         treeIterFromPath info path >>= \case
            Nothing -> return Nothing
            Just iter -> do
               model <- readIORef modelRef
               gEnt <- getEntity store iter
               if UUID.null $ gtkId gEnt
                  then  -- Root entity.
                     return $ Just $ Package (Name $ modelName model) mempty ^. re _Package
                  else
                     case M.lookup (gtkId gEnt) (modelContents model) of
                        Nothing ->  -- GTK tree entity no longer in model. Got out of sych somehow.
                           return Nothing
                        Just ent ->
                           return $ Just $ ent ^. entityContents
      makeMove source target = lift $ do   -- :: ModelEdit v (Maybe Text)
         goToTreePath target
         targetId <- currentId
         targetName <- currentName
         goToTreePath source
         sourceName <- currentName
         moveEntity targetId
         return $ Just $ "Move " <> sourceName <> " to " <> targetName

{- Design note:
DO NOT USE treeSetRowDragData and treeGetRowDragData. They seem to transmit the TreePath
of the underlying TreeModel instead of the sorted path used as input. This is the Wrong Thing
in this case.

The dragDataReceived signal is supposed to provide the coordinates of the signal in the widget.
However this is always (0,0). This is probably a bug in GTK3.
-}

{- Design note: how GTK3 drag and drop works, more or less. You can also do the "dragGetData"
sequence as part of the dragMotion handler.

      Sender               GTK3                  Receiver
        |                   |                       |
        |                   |--------dragDrop------>M
        |                   |                       M
        |                   M<---dragGetData--------M
        |                   M                       |
        M<---dragDataGet----M                       |
        M                   |                       |
        M-selectionDataSet->M                       |
        |                   M---dragDataReceived--->M
        |                   |                       M
        |                   M<--selectionDataGet----M
        |                   M----{returned data}--->M
        |                   |                       M
        |                   |<-----dragFinish-------M
        |                   |                       M
-}


-- ^ Comparison function for the sorted view of the GTK tree, using natural order sorting.
-- This ensures that the GTK tree order will match the model tree order.
treeSortF :: (Integral i) => Gtk.TreeModel -> Gtk.TreeIter -> Gtk.TreeIter -> IO i
treeSortF store iter1 iter2 = do
   v1 <- getEntity store iter1
   v2 <- getEntity store iter2
   return $ case naturalOrder (v1 ^. to gtkName . nameText) (v2 ^. to gtkName . nameText) of
      LT -> -1
      EQ -> 0
      GT -> 1


-- | Target information for dragging model entities around.
dragTargets :: (MonadIO m) => m [Gtk.TargetEntry]
dragTargets = do
   entityTarget <- Gtk.targetEntryNew
         "modelEntity"
         (fromIntegral $ fromEnum Gtk.TargetFlagsSameApp)
         0  -- InfoID not used, so just 0 for now.
   return [entityTarget]


-- | Pop up a warning about a failed model edit on the event.
modelErrorWarningOn :: (Gtk.IsWidget parent) => parent -> Event ModelError -> MomentIO ()
modelErrorWarningOn parent errE = do
   stop <- reactimate1 $ modelErrorWarning parent <$> errE
   void $ Gtk.onWidgetDestroy parent stop

-- | Pop up a warning about a failed model edit.
modelErrorWarning :: (Gtk.IsWidget parent, MonadIO m) => parent -> ModelError -> m ()
modelErrorWarning parent err = errorBox (Just parent) $ case err of
      InternalError msg -> "An internal error has been detected:\n\n   " <> msg <>
         "\n\nPlease save your work in a new file and report this bug."
      UserError msg -> msg



-- | Integrate model updates into a @Behavior@, updating the widget along
-- the way. Each "ModelEdit" event also triggers a corresponding "HistoryItem" event.
-- The return model is carried by both an event (with the latest update) and a behavior (with
-- the original value).
processModelUpdates ::(EntityClass v) =>
   GtkTreeInfo v  -- ^ The GTK tree that needs to be updated in parallel with the model.
   -> Event (ModelUpdate v (Maybe Text))   -- ^ Updates to the model.
   -> Event (Model v)   -- ^ Whole new model replacing existing one.
   -> MomentIO (Event (Model v))
processModelUpdates gtkTree updates modelBlats = do
      let
         GtkTreeInfo _ _ vw _ = gtkTree
         newModels = updateNewModel <$> filterE isUpdate updates
         logChanges = updateEditLog <$> filterE isUpdate updates
         modelE = unionWith const newModels modelBlats
      -- Debug code: check that the updated model corresponds with the widget tree.
      -- stop3 <- reactimate1 $ checkGtkTree gtkTree <$> newModels
      stop1 <- reactimate1 $ replaceGtkTree gtkTree <$> modelBlats
      stop2 <- reactimate1 $ updateGtkTree vw gtkTree <$> logChanges
      void $ Gtk.onWidgetDestroy vw $ stop1 >> stop2
      return modelE
   where
      isUpdate = isJust . updateValue

{- Design note:

checkGtkTree should be enabled if there is doubt over whether the displayed tree in the GTK
TreeView matches the tree in the Hades Model. The most typical symptoms are that edits to the tree
cause errors about entity paths not existing or unrelated entities, or entities
mysteriously disappear from the TreeView. The updateGtkTree function is supposed to keep the two
in synch, but the code is complex and hence fragile.

The reactimate call to "checkGtkTree" above is in a race condition with the other two reactimates.
The ordering is imposed by Reactive Banana, and is deterministic but undefined. If it happens in
the wrong order then spurious warnings will be generated showing that the update has not occured.
If this happens then move the call to the other end of the list.

The test folder contains QuickCheck tests for the updateGtkTree routine.
-}


{-
-- | Check that the model is consistent with the GTK tree.
-- If discrepancies are found then it displays a diagnostic.
checkGtkTree :: (EntityClass v, MonadIO m) => GtkTreeInfo v -> Model v -> m ()
checkGtkTree treeInfo model = do
      let GtkTreeInfo _ gtkSorted _ _ = treeInfo
      (_, iter) <- Gtk.treeModelGetIter gtkSorted =<< Gtk.treePathNewFromIndices [0]
      widgetTree <- fmap gtkName <$> getEntityTree gtkSorted iter
      modelTree <- fmap gtkName <$> getGtkTree model
      when (modelTree /= widgetTree) $ liftIO $ do
         let msg = intercalate "\n" $ execWriter $ compareTree "/" modelTree widgetTree
         cannotHappen (T.pack msg) $ return ()
   where
      getGtkTree :: (EntityClass v, Monad m) => Model v -> m (Tree GtkEntity)
      getGtkTree mdl = do
            forest <- monadic $ evalModelEdit id mdl gtkEntities
            return $ Node (GtkEntity (Name $ modelName mdl) "model-home" UUID.nil) forest
         where
            gtkEntities = map (fmap $ entityToGtk (const "")) <$> modelPackageForest
      compareTree path (Node v1 cs1) (Node v2 cs2) = do
         when (v1 /= v2) $ tell [path <> ": " <> show v1 <> " /= " <> show v2]
         if length cs1 == length cs2
            then sequence_ $ zipWith (compareTree (path <> "/" <> show v1)) cs1 cs2
            else tell [path <> ": child count " <> show (length cs1) <> " /= " <> show (length cs2)]
-}
