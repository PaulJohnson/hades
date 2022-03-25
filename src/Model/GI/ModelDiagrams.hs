{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedLabels #-}

{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

-- |
module Model.GI.ModelDiagrams (
   ActivationScript,
   EntityMenu,
   DiagramExport,
   EditorOutput (..),
   editorWindows,
   exportDrawing,
   renderForExport,
   renderOnPng
) where

import Codec.Picture
import Codec.Picture.Metadata (mkDpiMetadata)
import Control.Arrow
import qualified Control.Exception as Ex
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Free
import Control.Monad.Trans.Reader
import Data.Array.MArray
import Data.Array.Unboxed hiding (bounds)
import Data.Bits
import qualified Data.ByteString.Lazy as BL
import Data.Either
import Data.IORef
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as U
import Data.UUID.Generate
import Data.Word
import qualified GI.Gdk as Gdk
import GI.Gtk (AttrOp ((:=)))
import qualified GI.Gtk as Gtk
import qualified GI.Cairo.Render as Cairo
import Hades.Abstract
import Hades.GI
import Model.Abstract.DiagramType
import Model.Abstract.PackageTree
import Model.Abstract.Properties
import Model.Reflection.Reflective
import Model.GI.Notebooks
import Model.GI.PackageTree
import Reactive.Banana.ArrowDialog
import Reactive.Banana.Combinators hiding ((<>))
import Reactive.Banana.Common
import Reactive.Banana.GI.ArrowDialog
import Reactive.Banana.GI.Common
import Reactive.Banana.GI.Connect
import Reactive.Banana.GI.ErrorBox
import Reactive.Banana.Frameworks
import System.IO.Unsafe (unsafePerformIO)


-- | Union of all the things that can happen in a "ModelScript". Apart from an Error and a Result
-- these all include a model edit state and something that returns a continuation script.
-- The result of the specified operation is passed to this continuation.
data InterpreterResult p v a =
   InterpreterError ModelError  -- ^ An error occured.
   | InterpreterUpdate (ModelUpdate v a)  -- ^ The script returned an update to the model.
   | InterpreterOpenProperties (
         (ModelEditState v, Maybe (Entity v, ReferenceValues) -> ModelScript p v v a),
         ModelId)
         -- ^ Properties dialog. State and continuation are passed to dialog as "k" parameter.
   | InterpreterOpenDialog (
         MomentIO (Either
            (ModelEditState v, ModelScript p v v a)
            (Event (ModelEditState v, ModelScript p v v a)))
      )  -- ^ Result from other dialog. @Left@ means no dialog to open, @Right@ carries output
         -- events.
   | InterpreterOpenEntity ((ModelEditState v, ModelScript p v v a), EntityWrapper p v)
      -- ^ Continuation, and entity to open in a diagram window.
   | InterpreterSelection ((ModelEditState v, ModelScript p v v a), Set ModelId)
      -- ^ Continuation, and set of paths to set as selection in the model window.
   | InterpreterSetHint ((ModelEditState v, ModelScript p v v a), Maybe Text)
      -- ^ Continuation and text to set as hint in the bottom bar.
   | InterpreterIOAction (IO (ModelScript p v v a), ModelEditState v)
      -- ^ Execute the IO action. The result is the continuation.

instance (EntityClass v) => Show (InterpreterResult p v a) where
   show (InterpreterError e) = "InterpreterError " <> show e
   show (InterpreterUpdate u) = "InterpreterUpdate " <> show (updateEditLog u)
   show (InterpreterOpenProperties (_, i)) = "InterperterOpenProperties " <> show i
   show (InterpreterOpenDialog _) = "InterpreterOpenDialog ..."
   show (InterpreterOpenEntity (_, w@DiagramWrapper {})) =
         "InterpreterOpenEntity Diagram " <> show (wrappedEntityId w)
   show (InterpreterOpenEntity (_, w@ViewWrapper {})) =
         "InterpreterOpenEntity View " <> show (wrappedEntityId w)
   show (InterpreterSelection (_, s)) = "InterpreterSelection " <> show (S.toList s)
   show (InterpreterSetHint (_, h)) = "InterpreterSetHint " <> show h
   show (InterpreterIOAction _) = "InterpreterIOAction ..."


getInterpreterError :: InterpreterResult p v a -> Maybe ModelError
getInterpreterError (InterpreterError e) = Just e
getInterpreterError _ = Nothing

getInterpreterUpdate :: InterpreterResult p v a -> Maybe (ModelUpdate v a)
getInterpreterUpdate (InterpreterUpdate v) = Just v
getInterpreterUpdate _ = Nothing

getInterpreterOpenProperties :: InterpreterResult p v a -> Maybe (
      (ModelEditState v, Maybe (Entity v, ReferenceValues) -> ModelScript p v v a),
      ModelId)
getInterpreterOpenProperties (InterpreterOpenProperties v) = Just v
getInterpreterOpenProperties _ = Nothing

getInterpreterOpenDialog :: InterpreterResult p v a -> Maybe (MomentIO (Either
         (ModelEditState v, ModelScript p v v a)
         (Event (ModelEditState v, ModelScript p v v a))
      ))
getInterpreterOpenDialog (InterpreterOpenDialog v) = Just v
getInterpreterOpenDialog _ = Nothing

getInterpreterOpenEntity :: InterpreterResult p v a
   -> Maybe ((ModelEditState v, ModelScript p v v a), EntityWrapper p v)
getInterpreterOpenEntity (InterpreterOpenEntity v) = Just v
getInterpreterOpenEntity _ = Nothing

getInterpreterSelection :: InterpreterResult p v a
   -> Maybe ((ModelEditState v, ModelScript p v v a), Set ModelId)
getInterpreterSelection (InterpreterSelection v) = Just v
getInterpreterSelection _ = Nothing

getInterpreterSetHint :: InterpreterResult p v a
   -> Maybe ((ModelEditState v, ModelScript p v v a), Maybe Text)
getInterpreterSetHint (InterpreterSetHint v) = Just v
getInterpreterSetHint _ = Nothing

getInterpreterIOAction :: InterpreterResult p v a
   -> Maybe (IO (ModelScript p v v a), ModelEditState v)
getInterpreterIOAction (InterpreterIOAction v) = Just v
getInterpreterIOAction _ = Nothing


-- | Interpret incoming "ModelScript" events in a Reactive Banana circuit, including any
-- resulting pop-up dialogs.
--
-- Returns an event for the results of a completed script, an event for updates to the selected
-- set of entities, and an event for updates to the hint text.
processModelScripts :: (EntityClass v, p ~ HadesRender, Gtk.IsWidget w) =>
   w                  -- ^ Widget to act as parent for dialogs.
   -> Gtk.IconTheme
   -> BookManager p v
   -> Behavior (Set ModelId)  -- ^ Set of selected entities.
   -> Changes (Model v)
   -> Event (ModelScript p v v a)
      -- ^ The model editing scripts to execute and the model to apply them to.
   -> MomentIO (
         Event (ModelUpdate v a),
         Event (Set ModelId),
         Event (Maybe Text),
         Event ModelError)
processModelScripts widget iconTheme book selection modelC scriptsIn = mdo
      gen <- fromPoll newGenerator
      -- Some steps are instantaneous, so they must be fed back via a new event to prevent an
      -- effect being simultaneous with its cause.
      (nextE, nextH) <- newEvent
      let
         startB = modelStartState <$> gen <*> changesB modelC
         -- scripts :: Event (ModelEditState v, ModelScript p v v a)
         scripts = foldr1 (unionWith const)
            [
               (,) <$> startB <@> scriptsIn,
               fst <$> diagrams,      -- The continuation after a diagram has been opened.
               nextE,              -- Continuations fed back through handlers.
               (\((es, contF), r) -> (es, contF r)) <$> newPropertyEvents,
               newDialogEvents    -- Continuation when dialog OK or Apply clicked.
            ]
         results = interpretScript widget iconTheme modelC <$> selection <*> hintB <@> scripts
         errors = filterJust $ getInterpreterError <$> results
         properties = filterJust $ getInterpreterOpenProperties <$> results
         dialogs = filterJust $ getInterpreterOpenDialog <$> results
         diagrams = filterJust $ getInterpreterOpenEntity <$> results
         newSelections = filterJust $ getInterpreterSelection <$> results
         hints = filterJust $ getInterpreterSetHint <$> results
         ioActions = filterJust $ getInterpreterIOAction <$> results
         output = filterJust $ getInterpreterUpdate <$> results
      -- Respond to different events.
      modelErrorWarningOn widget errors
      propertyUpdates <- execute $ popupPropertiesEvent widget iconTheme <$> properties
      newPropertyEvents <- switchE =<< accumE never (unionWith const <$> propertyUpdates)
      (nullDialogEvents, dialogUpdates) <- split <$> execute dialogs
      newDialogEvents <- switchE =<< accumE never (unionWith const <$> dialogUpdates)
      hintB <- stepper Nothing $ snd <$> hints
      diagramOpen <- execute $ activateDiagram . snd <$> diagrams
      stop1 <- reactimate1 diagramOpen  -- Make sure the "execute" happens.
      stop2 <- reactimate1 $ performIOAction nextH <$> ioActions
      stop3 <- reactimate1 $ nextH . fst <$> hints
      stop4 <- reactimate1 $ nextH . fst <$> newSelections
      stop5 <- reactimate1 $ nextH <$> nullDialogEvents
      void $ Gtk.onWidgetDestroy widget $ stop1 >> stop2 >> stop3 >> stop4 >> stop5
      return (output, snd <$> newSelections, snd <$> hints, errors)
   where
      activateDiagram wrapper = openDiagramInBook iconTheme book wrapper selection modelC
      performIOAction resultH (act, state) =
         Ex.try act >>= \case
            Left (ex :: Ex.IOException) ->
                  resultH (state, lift $ throwUser $ T.pack $ Ex.displayException ex)
            Right result -> resultH (state, result)


-- | Compile the model extensions into a dialog selector and use that to display a dialog.
popupPropertiesEvent :: (Gtk.IsWidget parent, EntityClass v) =>
   parent
   -> Gtk.IconTheme
   -> ((ModelEditState v, cont), ModelId)
   -> MomentIO (Event (
         (ModelEditState v, cont),
         Maybe (Entity v, ReferenceValues)
      ))
popupPropertiesEvent parent iconTheme (k@(state, _), uuid) = do
      let model = stateModel state
      case M.lookup uuid $ modelContents model of
         Nothing -> do
            modelErrorWarning parent $ InternalError $ T.pack $ "Missing entity: " <> show uuid
            return never
         Just ent -> do
            let
               refs = getReferenceValues uuid $ modelRelations model
               v = (ent, refs)
            r <- case reflectiveDialog (const Nothing) model $ Just v of
                  Nothing ->
                     return never
                  Just (dialog :: Dialog' (Model v) () (Maybe (Entity v, ReferenceValues))) ->
                     createDialog parent iconTheme (pure model) (k, Just v, dialog)
            return $ fmap (fmap (fromMaybe (ent, refs))) <$> r


-- | Interprets a script and returns the next externally visible action.
interpretScript :: (EntityClass v, Gtk.IsWidget parent) =>
   parent
   -> Gtk.IconTheme
   -> Changes (Model v)
   -> Set ModelId  -- ^ Current selection set.
   -> Maybe Text   -- ^ Current hint value.
   -> (ModelEditState v, ModelScript p v v a)
   -> InterpreterResult p v a
interpretScript parent iconTheme modelC selection hint (state, script) =
   case runModelEdit id state $ runFreeT $ stepScript script of
      Left err -> InterpreterError err
      Right (ModelUpdate result newModel newLogs, newState) ->
         case result of
            Free (ScriptProperties uuid nxt) ->
               InterpreterOpenProperties ((newState, UserScript . nxt), uuid)
            Free (ScriptDialog dSel v nxt) -> InterpreterOpenDialog $ do
               model <- valueB $ changesB modelC
               case dSel model v of
                  Just dialog -> do
                     clicks <- createDialog parent iconTheme modelC (nxt, v, dialog)
                     return $ Right $ (\(f, r) -> (newState, UserScript $ f r)) <$> clicks
                  Nothing ->
                     return $ Left (newState, UserScript $ nxt $ Just v)
            Free (ScriptOpenDiagram d nxt) ->
               InterpreterOpenEntity ((newState, UserScript nxt), d)
            Free (ScriptGetSelection nxt) ->
               interpretScript parent iconTheme modelC selection hint
                     (state, UserScript $ nxt selection)
            Free (ScriptSetSelection newSel nxt) ->
               InterpreterSelection ((newState, UserScript nxt), newSel)
            Free (ScriptGetHint nxt) ->
               interpretScript parent iconTheme modelC selection hint
                     (newState, UserScript $ nxt hint)
            Free (ScriptSetHint newHint nxt) ->
               InterpreterSetHint ((newState, UserScript nxt), newHint)
            Free (ScriptPerformIO act) ->
               InterpreterIOAction (UserScript <$> act, newState)
            Pure v ->
               InterpreterUpdate $ ModelUpdate v newModel newLogs


-- | Create an entity editor linked to the model that contains the
-- entity. The return result is the active entity and an event which is triggered if
-- this diagram hyperlinks to another one.
entityEditor :: (p ~ HadesRender, EntityClass v, Eq v, Reflective v) =>
   Gtk.IconTheme
   -> BookManager p v   -- ^ The notebook windows for new diagrams opened by this one.
   -> EntityWrapper p v
   -> Behavior (Set ModelId)  -- ^ The selected items in the model.
   -> Changes (Model v)  -- ^ The model that contains this diagram.
   -> MomentIO (Maybe (ActiveEntity p v))
-- DiagramWrapper case
entityEditor
      iconTheme
      manager
      wrapper@(DiagramWrapper uuid diagram dPrism prsm dSel ctx menu toolbar)
      selectionB
      modelC
   = mdo
      style <- liftIO hadesConfig
      {- Create the diagram widget and set up the reactive circuit for edits. The diagram
         outputs are executed by "hadesActionExecute", which returns a value in the ModelScript
         monad. This has to sent out unevaluated so that any updates to the model can be applied
         by the caller. Unfortunately we need the evaluated result because it contains the
         continuation in hadesActionNewState and any element property dialogs we need to show.
         The only way to get these values back is to create a new event and use "performIO"
         to call the handler. This is done in "updateDiagramInModel", which also
         runs the screen update.

         I'm not sure if this is inspired design or a terrible kludge.
      -}
      (modelUpdateE, modelUpdateH) <- newEvent
      -- Refresh this diagram.
      (refreshE, refreshH) <- newEvent
      let
         hadesUpdateE = fst <$> modelUpdateE
         propertyDialogE = filterE (not . null) $ snd <$> modelUpdateE
         zoomInit = 1
      (toolbarWidget, toolbarActions) <- makeGtkDeltaToolbar $ toolbar modelC
      -- Track zoom events.
      isCurrentDiagram <- stepper False $
            sameEntityView wrapper . activeWrapper <$> bookManagerActiveEvent manager
      let
         zoomE = unionWith const
            (view (hadesState . to streamStateT . deltaZoom) <$> hadesUpdateE)  -- Mouse zoom.
            (whenE isCurrentDiagram $ bookManagerZoom manager)  -- Zoom from the bottom bar.
      zoomB <- stepper zoomInit zoomE
      -- Track Hades state, including the Delta script continuation. A refresh event resets
      -- this to the initial state.
      hadesB <- stepper hadesInit $ unionWith const (hadesInit <$ refreshE) hadesUpdateE
      (drawingWidget, hadesInit, diagramActions) <-
            makeDrawingGtk diagram ctx menu keyCommands style zoomInit hadesB
      newModelE <- plainChanges (Just drawingWidget) $ changesB modelC
         -- newModelE must fire *after* the model behavior changes. Hence plainChanges is used
         -- to introduce a delay.
      dropEvent <- drawingDragAndDrop (hadesCanvas hadesInit) selectionB
      let
         modelEdits1 = foldr1 (unionWith (++))
            [
               return . updateZoom <$> filterApply ((/=) <$> zoomB) zoomE,
               [mkScriptAction $ updateDiagram dPrism >> yieldViews] <$ newModelE,
               diagramActions,
               dropEvent,
               toolbarActions,
               updateAppearance <$ refreshE,
               return . updatePropertyAction <$> diagramPropertyUpdates
            ]
         modelEdits = hadesActionExecute <$> hadesB <@> modelEdits1
         updateE = modelEdits <&> \script ->
            promoteScript prsm $ script >>= updateDiagramInModel modelUpdateH
      diagramPropertyUpdates <- diagramProperties drawingWidget propertyDialogE
      -- Monitor model for diagram deletion.
      let
         modelHasDiagram m = isRight $ evalModelEdit id m $ goToEntity uuid
         diagramDeleted = filterE not $ modelHasDiagram <$> changesE modelC
         exportB = Just . (renderStatic style &&& renderStaticSize) <$> hadesB
      widget <- liftIO $ Gtk.boxNew Gtk.OrientationVertical 0
      liftIO $ do
         Gtk.boxPackStart widget toolbarWidget False False 0
         Gtk.boxPackStart widget drawingWidget True True 0
      stop1 <- reactimate1 $ (Gtk.widgetDestroy widget >> stop1) <$ diagramDeleted
      w <- Gtk.toWidget widget
      c <- Gtk.toWidget $ hadesCanvas hadesInit
      let r = ActiveEntity {
            activeEntityWidget = w,
            activeEntityCanvas = c,
            activeEntityRefresh = refreshH (),
            activeEntityUpdates = return <$> updateE,
            activeEntityZoom = zoomB,
            activeEntityExport = exportB,
            activeWrapper = wrapper
         }
      -- Events which mean this is the active diagram.
      stop2 <- reactimate1 $ bookManagerActiveHandler manager r <$
            mconcat [void diagramActions, void dropEvent, void toolbarActions ]
      void $ Gtk.onWidgetDestroy widget stop2
      return $ Just r
   where
      updateZoom newZoom = mkScriptAction $ do
         deltaZoom .= newZoom
         tellAll
         yieldViews
      -- Store the modified diagram back into the model.
      updateDiagramInModel modelUpdateH actionResult = do  -- ModelScript monad
         when (isJust $ hadesActionCheckpoint actionResult) $ do
            -- Update the diagram in the model.
            let d = hadesActionNewState actionResult ^. hadesState . to streamStateT . deltaDiagram
            void $ lift $ try $ do
               -- If this fails the diagram has been deleted, so it doesn't matter.
               goToEntity uuid
               modifyValue $ dPrism . diagramEntityContents .~ d
            -- Update the diagram on the screen and feed the state back in to the diagram editor.
         performIO $ do
            hadesActionDraw actionResult
            modelUpdateH (hadesActionNewState actionResult, hadesActionProperties actionResult)
         return $ hadesActionCheckpoint actionResult
      -- Emit an event to makeGtkPopupSelect for each item in the popup list.
      diagramProperties parent events = do
         (propertyItems, itemH) <- newEvent
         stop <- reactimate1 $ events <&> mapM_ (\item -> itemH (identifier item, item))
         void $ Gtk.onWidgetDestroy parent stop
         filterJust . fmap promoteMaybe <$> mkGtkPopupSelect parent iconTheme modelC dSel propertyItems
      promoteMaybe :: (a, Maybe b) -> Maybe (a, b)
      promoteMaybe (_, Nothing) = Nothing
      promoteMaybe (v1, Just v2) = Just (v1, v2)
      -- Script action to update a property of an element on the diagram.
      updatePropertyAction (u, v) = mkScriptAction $ do
         deltaDiagram %= diagramModify (const v) u
         tellItem v
         desc <- itemDescription v
         tellCheckpoint $ "Set properties of " <> desc
         yieldViews
      -- When Refresh is clicked, update the appearance data.
      updateAppearance = return $ mkScriptAction $ do
         liftBase $ lift $ goToEntity uuid
         liftBase (lift current) <&>
            preview (_Just . entityContents . prsm . dPrism . diagramEntityData)
            >>= \case
               Nothing -> yieldViews  -- Should never happen.
               Just meta -> do
                  ctx1 <- use deltaContext
                  ctx2 <- liftBase $ lift $ updateViewContext meta ctx1
                  deltaContext .= ctx2
                  tellAll
                  yieldViews
-- ViewWrapper case
entityEditor iconTheme _ wrapper@(ViewWrapper _ _ state1 inputState gadget) _ modelC = do
      (refreshE, refreshH) <- newEvent
      model1 <- valueB $ changesB modelC
      let
         inputs = case inputState of
            Left v -> Just (v, pure v)
            Right inputF -> case inputF model1 of
               Nothing -> Nothing
               Just v -> Just (v, fromMaybe v . inputF <$> modelC)
      case inputs of
         Nothing -> return Nothing  -- View data not found so do nothing.
         Just (input1, inputC) ->  mdo
            -- Widgets
            outer <- Gtk.boxNew Gtk.OrientationVertical 3
            -- State update
            userOutputE <- switchE1 (gadgetUser outputInit) (gadgetUser <$> outputRefreshed)
            userOutputB <- stepper (gadgetInitial outputInit) userOutputE
            let
               -- State events are ignored because merging them with the model behavior would
               -- result in old model values being fed back in, cancelling the latest update.
               loopB = GadgetData True <$> (mergeState <$> userOutputB <*> changesB inputC)
               widgetFocusInit :: (Gtk.IsWidget w) => w -> IO ()
               widgetFocusInit = const $ return ()
            outputInit <- {-# SCC "Original-view" #-}
               renderGadget
                     iconTheme
                     widgetFocusInit
                     model1
                     (GadgetData True (state1, input1))
                     modelC
                     never
                     loopB
                     gadget
            outputRefreshed <- {-# SCC "outputRefreshed" #-} execute $
               (\model initial ->
                  renderGadget iconTheme widgetFocusInit model initial modelC never loopB gadget)
               <$> changesB modelC
               <*> loopB
               <@ refreshE
            -- Widget display
            forM_ (gadgetView outputInit) $ \w -> Gtk.boxPackStart outer w True True 0
            stop <- reactimate1 $ outputRefreshed <&> \newOut -> do
               Gtk.containerGetChildren outer >>= mapM_ Gtk.widgetDestroy
               forM_ (gadgetView newOut) $ \w -> Gtk.boxPackStart outer w True True 0
               Gtk.widgetShowAll outer
            void $ Gtk.onWidgetDestroy outer stop
            -- Model edits
            updateE <- {-# SCC "updateE" #-} fmap (fmap mconcat . sequence) <$>
               switchE1 (gadgetEvents outputInit) (gadgetEvents <$> outputRefreshed)
            -- Output
            widget <- Gtk.toWidget outer
            let
               result = ActiveEntity {
                     activeEntityWidget = widget,
                     activeEntityCanvas = widget,
                     activeEntityRefresh = refreshH (),
                     activeEntityUpdates = return <$> updateE,
                     activeEntityZoom = pure 1,  -- Dialog widgets have no zoom.
                     activeEntityExport = pure Nothing,
                     activeWrapper = wrapper
                  }
            return $ Just result
   where
      -- First argument is local feedback, second argument is application feedback.
      -- Local value is taken if it is invalid. Otherwise application value is taken.
      -- This ensures that feedback continues even when the gadget output is invalid.
      mergeState :: GadgetData (s, a) -> a -> (s, a)
      mergeState localVal outerVal =
         if localVal ^. gdOk then (localVal ^. gdValue . _1, outerVal) else localVal ^. gdValue


-- | Configure the canvas to accept entities dropped from the model.
drawingDragAndDrop :: (Avatar p v w d, p ~ Paint d) =>
   Gtk.DrawingArea   -- ^ Canvas where drop events can take place.
   -> Behavior (Set ModelId)
   -> MomentIO (Event [Action d])
drawingDragAndDrop canvas selection = do
      targets <- dragTargets
      Gtk.widgetDragDestSet canvas [] (Just targets) [Gdk.DragActionCopy]
      void $ Gtk.onWidgetDragMotion canvas $ \context _ _ t -> do
         goodSource <- isJust <$> Gtk.widgetDragDestFindTarget canvas context Nothing
         Gdk.dragStatus context [Gdk.DragActionCopy | goodSource] t
         return True
      dragDropEvent <- registerIOSignal4 canvas Gtk.onWidgetDragDrop $ \_ x y _ ->
         return (True, Point (fromIntegral x) (fromIntegral y))
      return $ return . mkScriptAction . (>> yieldViews) <$>
            (diagramInsertSelection <$> selection <@> dragDropEvent)


-- | Insert avatars for the current selection at the given point.
diagramInsertSelection :: (Avatar (Paint d) v w d) =>
   Set ModelId -> Point -> Delta d ()
diagramInsertSelection idSet pt =
   case S.toList idSet of
      [] -> liftBase $ lift $ throwUser "Nothing to insert."
      [modelId] -> diagramInsertEntity pt modelId
      ids -> do
         forM_ ids $ diagramInsertEntity pt
         tellCheckpoint $ "Add " <> T.pack (show (length ids)) <> " entities to diagram"


-- | Given the path of an entity in the model, create an avatar and drop it on the diagram
-- at the given point.
diagramInsertEntity :: (Avatar (Paint d) v w d) =>
   Point -> ModelId -> Delta d ()
diagramInsertEntity pt modelId =
   liftBase (lift $ goToEntity modelId >> current) >>= \case
      Nothing -> liftBase $ lift $ throwUser "Cannot add the model root to a diagram."
      Just ent -> do
         nm <- liftBase $ lift currentName
         entityAvatar ent pt >>= \case
            Nothing -> liftBase $ lift $ throwUser $ nm <> " cannot be added to this diagram."
            Just _ -> tellCheckpoint $ "Add " <> nm <> " to diagram"


-- | If the diagram is already open then jump to it. Otherwise open it as a new page and return
-- its updates to the model. Returns the IO action which actually displays the diagram.
openDiagramInBook :: (EntityClass v, p ~ HadesRender) =>
   Gtk.IconTheme
   -> BookManager p v            -- ^ The notebook where this editor is to be displayed.
   -> EntityWrapper p v       -- ^ Details of the diagram to be edited.
   -> Behavior (Set ModelId)  -- ^ Set of selected entities.
   -> Changes (Model v)       -- ^ The model that contains the diagram.
   -> MomentIO (IO ())
openDiagramInBook iconTheme manager wrapper selection model = do
      contents <- liftIO $ readIORef $ bookManagerContents manager
      case find (sameEntityView wrapper . activeWrapper) contents of
         Nothing ->   -- Create a new entry in the book manager.
            entityEditor iconTheme manager wrapper selection model >>= \case
               Nothing -> return $ return ()  -- Nothing to show.
               Just newDiagram -> do
                  books <- allBooks manager
                  if Prelude.null books
                     then return $ errorBox (Nothing :: Maybe Gtk.Widget)
                           "There are no tabbed windows. Something has gone wrong."
                     else do
                        sizes <- forM books $ \b -> do
                           n <- Gtk.notebookGetNPages b
                           return (b, n)
                        let
                           display = fst $ minimumBy (comparing snd) sizes
                              -- minimumBy is safe because "books" was checked for null.
                           widget = activeEntityWidget newDiagram
                           canvas = activeEntityCanvas newDiagram
                           newContents = newDiagram : contents
                        -- The diagram name may change, so it is a behavior
                        -- and the relevant widgets are here.
                        lbl <- newTabLabel widget $ changesB diagramName
                        startName <- valueB $ changesB diagramName
                        menuLbl <- Gtk.labelNew $ Just startName
                        behaviorLink menuLbl Gtk.labelSetLabel $ changesB diagramName
                        return $ do
                           -- Put the new diagram in the BookManager list AND THEN add the
                           -- widget to the GTK notebook. The change in the GTK notebook page
                           -- triggers the event to select the diagram update events.
                           bookManagerModelHandler manager newContents
                           n <- Gtk.notebookAppendPageMenu display widget (Just lbl) (Just menuLbl)
                           Gtk.notebookSetTabDetachable display widget True
                           Gtk.widgetShowAll widget
                           void $ Gtk.onWidgetDestroy widget $ do
                              -- Destroy labels. Doesn't seem to happen otherwise.
                              closeDiagram widget
                              Gtk.widgetDestroy lbl
                              Gtk.widgetDestroy menuLbl
                           {-
                           -- Any click will select this diagram.
                           canvasTree <- widgetTree canvas
                           forM_ canvasTree $ \w -> do
                                 void $ Gtk.onWidgetFocusInEvent w $ \_ -> do
                                    bookManagerActiveHandler manager newDiagram
                                    return False
                                 void $ Gtk.onWidgetButtonPressEvent w $ \_ -> do
                                    bookManagerActiveHandler manager newDiagram
                                    return False
                           -}
                           Gtk.notebookSetCurrentPage display n
                           Gtk.widgetGrabFocus canvas
                           bookManagerActiveHandler manager newDiagram
         Just newlyActive ->
            return $ Gtk.widgetGetParent (activeEntityWidget newlyActive) >>= \case
               Nothing -> return ()  -- Should never happen.
               Just p1 -> Gtk.castTo Gtk.Notebook p1 >>= \case
                  Nothing -> return ()  -- Should never happen.
                  Just display -> do
                     n <- Gtk.notebookPageNum display $ activeEntityWidget newlyActive
                     Gtk.notebookSetCurrentPage display n
                     Gtk.widgetGrabFocus $ activeEntityCanvas newlyActive
                        -- Also triggers this diagram to become the primary focus for HADES.
   where
      uuid = wrappedEntityId wrapper
      closeDiagram widget = do
         oldContents <- readIORef $ bookManagerContents manager
         let
            newContents = filter (not . sameWidget widget . activeEntityWidget) oldContents
         bookManagerModelHandler manager $ seq (length newContents) newContents
               -- Force evaluation of newContents to avoid lazy retention of old pages.
      diagramName =
         maybe "-deleted-" (view (entityName . nameText)) . M.lookup uuid . modelContents <$> model


-- | The outputs from the model editor.
data EditorOutput v = EditorOutput {
   editorUpdates :: Event (ModelUpdate v (Maybe Text)),
      -- ^ Updated versions of the model.
   editorDiagramExport :: Behavior (Maybe DiagramExport),
      -- ^ A snapshot of the current state of the diagram, ready for export.
   editorTreeInfo :: GtkTreeInfo v,
      -- ^ The GTK tree model. This must be updated in parallel with the model.
   editorWidget :: Gtk.Widget
      -- ^ The GUI widget containing the editor.
}


-- | Set up a model editor with the tree widget and empty notebook in a Horizontal Pane.
editorWindows :: (Gtk.IsWindow window, Editable p v, EntityClass v, Eq v, p ~ HadesRender) =>
   window  -- ^ The top level window in which the editor is displayed.
   -> Gtk.IconTheme
   -> StockFunc v  -- ^ The stock icons to use with each @v@.
   -> Model v              -- ^ The initial value of the model to display.
   -> Changes (Model v)   -- ^ The model.
   -> Event ()            -- ^ Refresh button clicks.
   -> Event (ModelScript p v v (Maybe Text))  -- ^ Other scripts sent in by the outer GUI.
   -> MomentIO (EditorOutput v)
editorWindows window iconTheme picF modelInit modelC refreshE scriptE = mdo
      width <- liftIO $ fst <$> Gtk.windowGetSize window
            -- To set sensible widths for the children.
      (initialBook, manager) <- newBookManager zoomE refreshE
      (treeInfo, treeSelectEvent, treeScripts) <- modelWidget picF modelInit modelC selectB
      let
         (GtkTreeInfo _ _ treeWidget _) = treeInfo
      -- Track active diagram state.
      stop1 <- reactimate1 $ bookManagerActiveEvent manager <&> (\active -> do
            let activeClass = "active-diagram"  -- CSS class selector for the active diagram.
            contents <- readIORef (bookManagerContents manager)
            forM_ contents $ \inactive -> do
               style <- Gtk.widgetGetStyleContext $ activeEntityCanvas inactive
               Gtk.styleContextRemoveClass style activeClass
            style <- Gtk.widgetGetStyleContext $ activeEntityCanvas active
            Gtk.styleContextAddClass style activeClass
         )
      -- Update model and export when a diagram is edited.
      diagramScriptE <- switchE $
            foldr (unionWith (++) . activeEntityUpdates) never <$> bookManagerModelUpdate manager
      diagramExport <- switchB (pure Nothing) $
            activeEntityExport <$> bookManagerActiveEvent manager
      -- Entity selection from tree widget and scripts.
      let selectE = unionWith S.union treeSelectEvent scriptSelections
      selectB <- stepper mempty selectE
      -- Property editing widget
      let propertySelectionE1 = filterJust $ selectionAdded <$> selectB <@> selectE
         -- New entity is selected. This can happen simultaneously with a model change, so the
         -- event has to be delayed until the model behavior has been updated.
      (propertySelectionE2, propertySelectionH2) <- newEvent
      stop2 <- reactimate1 $ propertySelectionH2 <$> propertySelectionE1
      let latestProperty = entityPropertyData <$> propertySelectionE2
      (propertiesFrame, propertiesUpdates, propertyScriptE) <-
         mkDialogWidget
               iconTheme
               mempty
               modelInit
               modelC
               (pure $ reflectiveDialog $ editableDisplayMenu . Just)
               (U.nil, pure Nothing)
               latestProperty
      (modelUpdates, scriptSelections, hintE, exceptionE) <-
         processModelScripts treeWidget iconTheme manager selectB modelC $
               foldr (unionWith mergeScripts) never [
                     treeScripts,
                     updateFromProperty <$> propertiesUpdates,
                     scriptE,
                     foldr mergeScripts (return Nothing) <$>
                        unionWith (++) propertyScriptE diagramScriptE
                  ]
      -- Refresh all open windows when Refresh is clicked or an exception occurs.
      stop3 <- reactimate1 $ refreshWindows manager treeInfo <$> changesB modelC <@ refreshE
      stop4 <- reactimate1 $ refreshWindows manager treeInfo <$> changesB modelC <@ exceptionE
      -- Connect up bottom bar to script hints and diagram state zoom.
      hintB <- stepper Nothing $
         foldr1 (unionWith const) [hintE, Nothing <$ refreshE, Nothing <$ exceptionE]
      zoomB <- switchB (pure 1) $ activeEntityZoom <$> bookManagerActiveEvent manager
      (bottomBar, zoomE) <- makeBottomBar hintB zoomB
      -- Set up GTK widget hierarchy inside mainBox.
      diagramFrame <- Gtk.frameNew Nothing
      Gtk.frameSetShadowType diagramFrame Gtk.ShadowTypeIn
      Gtk.containerAdd diagramFrame initialBook
      diagramBox <- Gtk.boxNew Gtk.OrientationVertical 0
      Gtk.boxPackStart diagramBox diagramFrame True True 0
      Gtk.boxPackEnd diagramBox bottomBar False False 0
      modelFrame <- Gtk.frameNew Nothing
      Gtk.frameSetShadowType modelFrame Gtk.ShadowTypeIn
      scrolled <- Gtk.scrolledWindowNew noAdjustment noAdjustment
      Gtk.containerAdd scrolled treeWidget
      Gtk.containerAdd modelFrame scrolled
      modelPaned <- Gtk.panedNew Gtk.OrientationVertical
      Gtk.panedPack1 modelPaned modelFrame True True
      Gtk.panedPack2 modelPaned propertiesFrame True True
      mainBox <- Gtk.panedNew Gtk.OrientationHorizontal
      Gtk.panedPack1 mainBox diagramBox True True
      Gtk.panedPack2 mainBox modelPaned True True
      Gtk.panedSetPosition mainBox $ (width * 8) `div` 10
      void $ Gtk.onWidgetDestroy mainBox $ stop1 >> stop2 >> stop3 >> stop4
      Gtk.widgetShowAll mainBox
      w <- Gtk.toWidget mainBox
      return $ EditorOutput modelUpdates diagramExport treeInfo w
   where
      noAdjustment :: Maybe Gtk.Adjustment
      noAdjustment = Nothing
      -- Refresh action
      refreshWindows manager treeInfo model = do
         replaceGtkTree treeInfo model
         mapM_ activeEntityRefresh =<< readIORef (bookManagerContents manager)
      -- When a new entity is selected look up the value and references for the property dialog.
      selectionAdded oldSet newSet = S.lookupMin $ S.difference newSet oldSet
      -- entityPropertyData :: (EntityClass v) =>
      --    ModelId -> (ModelId, Behavior (Maybe (Entity v, ReferenceValues)))
      entityPropertyData modelId = (modelId, extractData <$> changesB modelC)
         where
            extractData model = do  -- Maybe monad
               ent <- fromRight Nothing $ evalModelEdit id model $ goToEntity modelId >> current
               let references = getReferenceValues (entityId ent) $ modelRelations model
               return (ent, references)
      -- ModelEdit action to update an entity from the property widget events.
      updateFromProperty (_, Nothing) = return Nothing
      updateFromProperty (modelId, Just (ent, newRefs)) = do
         changed <- lift $ do
            goToEntity modelId
            c1 <- setVariantData $ ent ^. entityProperties
            oldRefs <- queryRelations $ getReferenceValues modelId
            c2 <- if oldRefs == newRefs
                     then return False
                     else do
                        putReferenceValues ent newRefs
                        return True
            return $ c1 || c2
         if changed
            then return $ Just $ "Modified properties of " <> ent ^. entityName . nameText
            else return Nothing
      mergeScripts :: (Monad m) => m (Maybe Text) -> m (Maybe Text) -> m (Maybe Text)
      mergeScripts s1 s2 = do
         r1 <- s1
         r2 <- s2
         return $ case (r1, r2) of
            (Just str1, Just str2) -> Just $ str1 <> ", " <> str2
            _ -> r1 <> r2




{-
Debugging code. Not currently used, so commented out to prevent warnings.

printSelection :: (Named v) => Model v -> S.Set ModelPath -> IO ()
printSelection model sel = do
   putStrLn "Selection = "
   forM_ sel $ \path ->
      case evalModelEdit id model $ goToTreePath path >> currentName of
         Left err -> putStrLn $ "   Path " <> show path <> " is broken: " <> show err
         Right ent ->
            putStrLn $ "   Path " <> show path <> " point to " <> show ent
-}



-- | Create a tab label widget containing the string and a close button. When the
-- close button is clicked the widget is destroyed.
newTabLabel :: (Gtk.IsWidget widget) => widget -> Behavior Text -> MomentIO Gtk.Box
newTabLabel widget textB = do
   startStr <- valueB textB
   label <- liftIO $ Gtk.labelNew $ Just startStr
   behaviorLink label Gtk.labelSetLabel textB
   boxWidget <- Gtk.boxNew Gtk.OrientationHorizontal 1
   button <- Gtk.buttonNewWithLabel ("✖" :: Text)  -- U+2716: Heavy Multiplication X
   Gtk.setButtonRelief button Gtk.ReliefStyleNone
   Gtk.boxPackStart boxWidget label True True 0
   Gtk.boxPackEnd boxWidget button False False 0
   void $ Gtk.onButtonClicked button $ Gtk.widgetDestroy widget
   Gtk.widgetShowAll boxWidget
   return boxWidget


-- | Export a drawing to a file in one of the supported formats.
exportDrawing :: (Gtk.IsWidget parent) =>
   parent    -- ^ Widget in parent window for dialog.
   -> Gtk.IconTheme
   -> DiagramExport    -- ^ Drawing to be exported.
   -> IO ()
exportDrawing parent iconTheme (drawing, boxWidget) = do
      runDialog parent iconTheme typeDialog () (head exportTypes) >>= \case
         -- "head" is safe because exportTypes is constant
         Nothing -> return ()
         Just (typName, typ, glob) -> do
            picFilter <- Gtk.fileFilterNew
            Gtk.fileFilterSetName picFilter $ Just typName
            Gtk.fileFilterAddPattern picFilter glob
            chooser <- Gtk.new Gtk.FileChooserNative [
                  #doOverwriteConfirmation := True,
                  #action := Gtk.FileChooserActionSave,
                  #title := "Export Diagram",
                  #acceptLabel := "Export"
               ]
            Gtk.fileChooserAddFilter chooser picFilter
            parentWin <- Gtk.castTo Gtk.Window =<< Gtk.widgetGetToplevel parent
            forM_ parentWin $ \w -> Gtk.set chooser [#transientFor := w]
            result <- Gtk.nativeDialogRun chooser
            if result == fromIntegral (fromEnum Gtk.ResponseTypeAccept)
               then do
                  mTarget <- Gtk.fileChooserGetFilename chooser
                  Gtk.nativeDialogDestroy chooser
                  case mTarget of
                     Nothing -> return ()
                     Just target -> void $ catchFileError_ (Just parent) $ do
                           let
                              width = boxWidth boxWidget
                              height = boxHeight boxWidget
                           case typ of
                              SVG -> Cairo.withSVGSurface
                                       (checkExtension "svg" target)
                                       width
                                       height
                                       $ \srf -> Cairo.renderWith srf drawing
                              Postscript -> Cairo.withPSSurface
                                       (checkExtension "ps" target)
                                       width
                                       height
                                       $ \srf -> Cairo.renderWith srf drawing
                              PDF -> Cairo.withPDFSurface
                                       (checkExtension "pdf" target)
                                       width
                                       height
                                       $ \srf -> Cairo.renderWith srf drawing
                              PNG -> Cairo.withImageSurface Cairo.FormatARGB32
                                       (ceiling width) (ceiling height) $
                                       \srf -> do
                                          Cairo.renderWith srf drawing
                                          Cairo.surfaceWriteToPNG srf $ checkExtension "png" target
               else
                  Gtk.nativeDialogDestroy chooser
   where
      exportTypes :: [(Text, ExportType, Text)]
      exportTypes = [
            ("Scalable Vector Graphics (SVG)", SVG, "*.svg"),
            ("Portable Network Graphics (PNG)", PNG, "*.png"),
            ("Postscript (PS)", Postscript, "*.ps"),
            ("Portable Document Format (PDF)", PDF, "*.pdf")
         ]
      typeDialog = Dialog "Diagram Export Type" OkButton $ proc t -> do
            _ <- message1 "Select the type of image file to export:" -< ()
            radio $ const $ map (\v -> (v ^. _1, v)) exportTypes -< t



-- | Render the image to a PNG file which is returned as a lazy ByteString. Drawing operations
-- are in points.
renderOnPng ::
   Double    -- ^ Resolution in pixels per inch.
   -> (Double, Double)    -- ^ Width and height in points.
   -> Cairo.Render ()  -- ^ Drawing for the PNG. Drawing unit is points regardless of resolution.
   -> IO BL.ByteString
renderOnPng res (w, h) drawing = do
      let
         zoom1 = res / 72  -- 72 points per inch.
         w1 = max 1 $ ceiling $ w * zoom1
         h1 = max 1 $ ceiling $ h * zoom1
      Cairo.withImageSurface Cairo.FormatARGB32 w1 h1 $ \surface -> do
         Cairo.renderWith surface $ do
            Cairo.scale zoom1 zoom1
            drawing
         d1 <- Cairo.imageSurfaceGetPixels surface :: IO (Cairo.SurfaceData Int Word32)
         d2 <- freeze d1 :: IO (UArray Int Word32)
         stride <- Cairo.imageSurfaceGetStride surface
         let
            stride4 = stride `div` 4  -- Taken from docs for Graphics.Rendering.Cairo.
            readPix :: Int -> Int -> PixelRGBA8
            readPix x y =
               let (a, r, g, b) = getBytes $ d2 ! (y * stride4 + x)
               in PixelRGBA8 r g b a
            pixels = generateImage readPix w1 h1
         return $ encodePngWithMetadata (mkDpiMetadata (round res)) pixels

   where
      getBytes :: Word32 -> (Word8, Word8, Word8, Word8)
      getBytes pixel = let
            b0 = fromIntegral pixel
            b1 = fromIntegral $ pixel `shiftR` 8
            b2 = fromIntegral $ pixel `shiftR` 16
            b3 = fromIntegral $ pixel `shiftR` 24
         in (b3, b2, b1, b0)


-- | Render a model for export. The result is a render operation scaled to points (1/72 inch) so
-- that text appears the correct size. The result also includes the size of the diagram in points.
-- The diagram is moved to the top left corner and 10
-- points of padding is added around each side.
renderForExport :: (HadesRender ~ Paint d, Connectable d, Base d ~ ModelScript p v w) =>
   Traversal' v w   -- ^ Relationship between diagram and model entities.
   -> Model v       -- ^ Model containing this diagram.
   -> ViewContext d   -- ^ Application context, including how the diagram is to be rendered.
   -> Diagram d     -- ^ Diagram to render.
   -> (Cairo.Render (), (Double, Double))
renderForExport prsm model ctx d =
      let
         viewOrder = reverse $ d ^. diagramOrder
         dState = unsafePerformIO $ mkDiagramState ctx 1 d
            -- Safe because the diagram is not edited, so there is no risk of reuse of UUIDs.
         result = outViews . fst <$> startStreamT (stepDelta drawForever) dState
            -- Interpret "result" as a ModelScript action.
         (paint, bounds) = case evalModelEdit prsm model $ runFreeT $ stepScript result of
            Left err ->
               cannotHappen (T.pack $ show err) (return (), NoBox)
            Right (Pure (ViewSet drawing)) ->
               let vs = mapMaybe (`M.lookup` drawing) viewOrder
               in (mapM_ viewDraw vs, growBox 10 $ mconcat $ map viewBox vs)
                  -- Add 10 pixel slop around diagram.
            Right _ ->
               cannotHappen "getDrawingStatic: impossible UI action." (return (), NoBox)
         boundsScaled = scaleBox staticScale bounds
         w = boxWidth boundsScaled
         h = boxHeight boundsScaled
         (xOff, yOff) = case boundsScaled of
            NoBox -> (0, 0)
            BoundBox p _ -> (p ^. pX, p ^. pY)
         render = do
            style <- liftIO hadesConfig
            Cairo.translate (-xOff) (-yOff)
            Cairo.scale staticScale staticScale
            runReaderT paint style
            Cairo.showPage
      in (render, (w, h))
   where
      drawForever = forever $ tellAll >> void yieldViews
      staticScale = 72/96  -- Convert from 96 dpi to 72 points per inch.
      scalePoint s (Point px py) = Point (s*px) (s*py)
      scaleBox _ NoBox = NoBox
      scaleBox s (BoundBox p1 p2) = BoundBox (scalePoint s p1) (scalePoint s p2)
