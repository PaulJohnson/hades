{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedLabels #-}
{-
Copyright © Paul Johnson 2020. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.
-}

module App.ModelMain (
   MainDescriptor (..),
   modelMain,
   getProgramDataFolder
) where

import App.Welcome
import Control.Arrow
import Control.Category
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Trans
import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import Data.Either
import Data.Function (on)
import Data.IORef
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Data.Time
import Data.Tree
import qualified Data.UUID as U
import Data.UUID.Generate
import Data.Version
import Evidence.Model
import qualified GI.Gio as Gio
import qualified GI.GLib as GLib
import GI.Gtk (AttrOp ((:=)))
import qualified GI.Gtk as Gtk
import Hades.GI
import Model.Abstract.DiagramType
import Model.Abstract.Import
import Model.Abstract.ModelExport
import Model.Abstract.PackageTree as Pkg
import Reactive.Banana.GI.DataIconTheme
import Model.GI.Export
import Model.GI.ModelDiagrams
import Model.GI.PackageTree
import Model.Reflection.Dialogs
import Model.Reflection.NamedRelation (renameRelation, edgeFromRelation, edgeToRelation)
import Model.Reflection.References
import Model.Reflection.Reflective
import Model.Reflection.Types
import Model.Reflection.Values
import qualified Options.Applicative as Opt
import Paths_hades
import Prelude hiding (id, (.))
import Reactive.Banana.ArrowDialog
import Reactive.Banana.Combinators hiding ((<>))
import Reactive.Banana.Common
import Reactive.Banana.Frameworks
import Reactive.Banana.GI.ArrowDialog
import Reactive.Banana.GI.Connect
import Reactive.Banana.GI.ErrorBox
import Reactive.Banana.GI.Menu
import Reactive.Banana.Menu
import System.Directory
import System.Environment
import System.FilePath
import System.Hades.Autosave
import System.Hades.DataFiles
import System.Hades.EditLock
import Reactive.Banana.GI.Common


-- | True if the model has changed and hence may need to be saved.
type Changed = Bool


{- | Parameters for the "modelMain" routine.

There are two sets of icons used by the model editor:

1. Icons used by specialised entity types for toolbar buttons and the model tree.
These are passed in the 'programIconDirs' field, from where they are added to the GTK default
icon theme search path.

2. Icons associated with field values in the \"Data\" menu. These are stored in a separate global
icon theme accessed by `getDataIconTheme`. This theme is initially empty. The caller should
populate this theme using 'Gtk.iconThemeSetSearchPath' and 'Gtk.iconThemeSetCustomTheme'.
-}
data MainDescriptor v = MainDescriptor {
   programName :: AppName,  -- ^ Short alphanumeric identifier, used in pathnames and the like.
   programTitle :: Text,  -- ^ Shown in the title bar of the main window.
   programXtn :: Text,   -- ^ Save file extension (without the dot).
   programStock :: Entity v -> Text,  -- ^ Stock icon names for type @v@.
   programIconDirs :: [FilePath],  -- Folders to be added to the default icon theme search path.
   programFirstLoad :: Maybe FilePath,
      -- ^ File to load when first started, relative to data directory @share/hades/@.
   programAfterLoad :: ModelEdit v v ()
      -- ^ This will be applied after a model is opened. Use it to update models in old
      -- formats. It MUST be idempotent.
}


data CmdOpts = CmdOpts {
      fileToLoad :: Maybe FilePath,
      fontConfig :: Maybe FilePath
   }

cmdOpts :: Opt.ParserInfo CmdOpts
cmdOpts = Opt.info basic Opt.fullDesc
   where
      basic =  CmdOpts <$>
         Opt.optional (Opt.strArgument (Opt.help "File to edit")) <*>
         Opt.optional (Opt.strOption (
               Opt.long "font-config" <>
               Opt.short 'f' <>
               Opt.help "Path for fontConfig files"))




{- | The application state is stored in two lists; undo and redo. Combined with the current state
(held elsewhere) this is somewhat akin to a list zipper structure. However the models and texts
move differently.

>           Undo log              Redo log
>    t __ t __ t __ t --------- t __ t __ t __ t
>    v    v    v    v -\     /- v    v    v    v
>                      \    /
>                   Application

When an undo is performed the value is moved from the head of the undo log into the application
and the current application value is moved into the redo log. Meanwhile the text from the
head of the undo log is moved directly from the undo to the redo, so that text is now associated
with the value that the application will get from a redo.

Thus when the application looks forward along the redo log or backwards along the undo log it
always sees the change text which leads to the associated value.  -}
data ModelHistory v = ModelHistory {
      undoLog :: [HistoryItem v],
      redoLog :: [HistoryItem v]
   }


{-  Debugging function. Commented out until needed.

showHistory :: ModelHistory v -> String
showHistory h = "History {"
      <> show (map undoText $ undoLog h) <> ", "
      <> show (map undoText $ redoLog h) <> "}"
-}


emptyHistory :: ModelHistory v
emptyHistory = ModelHistory [] []


-- | Perform an undo. Returns @Nothing@ if there is no available undo step.
undo ::
   Model v            -- ^ Current state of the model.
   -> ModelHistory v  -- ^ The current undo state.
   -> Maybe (Model v, ModelHistory v)
undo model log1 =
   case undoLog log1 of
      [] -> Nothing
      u1 : undos ->
         let
            newItem = HistoryItem model (undoText u1)
            log2 = ModelHistory undos $ newItem : redoLog log1
         in Just (undoValue u1, log2)


-- | Perform a redo. Returns @Nothing@ if there is no available redo step.
redo ::
   Model v            -- ^ Current state of the model.
   -> ModelHistory v  -- ^ The current undo state.
   -> Maybe (Model v, ModelHistory v)
redo v log1 =
   case redoLog log1 of
      [] -> Nothing
      r1 : redos ->
         let
            newItem = HistoryItem v (undoText r1)
            log2 = ModelHistory (newItem : undoLog log1) redos
         in Just (undoValue r1, log2)


-- | Add the item to the undo state. Clears the redo log because there is no concept of redoing
-- from the new state. If the new item has the same text as the most recent undo then it is
-- ignored. From the user point of view this avoids long strings of undo items of the
-- form \"Modified Foo\". Behind the scenes it also hides the fact that some entity editors emit
-- multiple updates during a single edit.
recordUndo ::
   Int          -- ^ Limit on the number of items stored in the Undo list.
   -> HistoryItem v
   -> ModelHistory v
   -> ModelHistory v
recordUndo n h log1 = ModelHistory (take n addUndo) []
   where
      addUndo = case undoLog log1 of
         [] -> [h]
         latest : _ ->
            if undoText latest == undoText h
               then undoLog log1  -- Ignore more recent change in favour of older version.
               else h : undoLog log1


{- Design note:

This implementation risks creating a memory leak if the undo list is not forced, as it will
simply build up an unlimited chain of thunks until the "take" is evaluated. The GUI will therefore
need to ensure that it does something in IO with the whole list produced by this function.
-}



-- | Hard coded limit on the number of items in the Undo history.
maxUndoLength :: Int
maxUndoLength = 30


-- | When the application is first started this initializes the undo and redo GUI actions
-- to each emit an event value of "1".
historyActions :: (Gio.IsActionMap a) => a -> MomentIO (Event Int, Event Int)
historyActions actionMap = do
   undoAction <- Gio.simpleActionNew "undo" Nothing
   redoAction <- Gio.simpleActionNew "redo" Nothing
   Gio.actionMapAddAction actionMap undoAction
   Gio.actionMapAddAction actionMap redoAction
   undoClicked <- registerIOSignal1 undoAction Gio.onSimpleActionActivate $ \_ -> return ((), 1)
   redoClicked <- registerIOSignal1 redoAction Gio.onSimpleActionActivate $ \_ -> return ((), 1)
   return (undoClicked, redoClicked)


-- | Updates the menus attached to the undo and redo buttons, and returns events for them.
-- The number is the number of changes to be undone or redone respectively. The \"other widgets\"
-- are other buttons or menu items that should be enbaled or disabled as appropriate.
--
-- This returns its IO actions as a separate value so that they can reliably be executed when
-- this function is called from "execute".
historyMenus ::
   Gtk.MenuToolButton     -- ^ Undo button.
   -> [Gtk.Widget]        -- ^ Other undo widgets
   -> Gtk.MenuToolButton  -- ^ Redo button.
   -> [Gtk.Widget]        -- ^ Other redo widgets.
   -> ModelHistory v
   -> MomentIO (Event Int, Event Int, IO ())
historyMenus undoButton otherUndo redoButton otherRedo history = do
      (undoE, undoM) <- if not $ null $ undoLog history
         then do
            (undoMenu, undoSelection) <- mkGtkMenu $ hMenu $ undoLog history
            let act = do
                  Gtk.menuToolButtonSetMenu undoButton undoMenu
                  Gtk.widgetSetSensitive undoButton True
                  forM_ otherUndo (`Gtk.widgetSetSensitive` True)
            return (undoSelection, act)
         else
            let act = do
                  Gtk.widgetSetSensitive undoButton False  -- SetMenu should take a Maybe for this.
                  forM_ otherUndo (`Gtk.widgetSetSensitive` False)
            in return (never, act)
      (redoE, redoM) <- if not $ null $ redoLog history
         then do
            (redoMenu, redoSelection) <- mkGtkMenu $ hMenu $ redoLog history
            let act = do
                  Gtk.menuToolButtonSetMenu redoButton redoMenu
                  Gtk.widgetSetSensitive redoButton True
                  forM_ otherRedo (`Gtk.widgetSetSensitive` True)
            return (redoSelection, act)
         else
            let act = do
                  Gtk.widgetSetSensitive redoButton False
                  forM_ otherRedo (`Gtk.widgetSetSensitive` False)
            in return (never, act)
      return (undoE, redoE, undoM >> redoM)
   where
      hMenu items = Menu [zipWith menuItem (map undoText items) [1..]]


-- | When an Undo or Redo is clicked or selected an event is triggered. This routine returns
-- the new state of the history and model. The history should be passed to "historyMenus" and the
-- model used to replace the current one.
processHistoryClick ::
   Event Int      -- ^ Undo event.
   -> Event Int   -- ^ Redo event.
   -> Behavior (Model v)    -- ^ The state of the model.
   -> Behavior (ModelHistory v)  -- ^ The model history.
   -> Event (Model v, ModelHistory v)
processHistoryClick undoE redoE modelB historyB =
      let
         undoResult = filterJust $ step undo <$> modelB <*> historyB <@> undoE
         redoResult = filterJust $ step redo <$> modelB <*> historyB <@> redoE
      in unionWith const undoResult redoResult
   where
      step f model h n
         | n < 1     = Just (model, h)
         | otherwise = case f model h of
            Just (m2, h2) -> step f m2 h2 (n-1)
            Nothing -> Nothing


-- | Standard main function for model editors.
modelMain ::
   (Editable p v, EntityClass v, ToJSON v, FromJSON v, Reflective v,
      HasDiagrams p v, p ~ HadesRender, HasEvidence v) =>
   MainDescriptor v -> IO ()
modelMain descriptor = do
   iconTheme <- getDataIconTheme
   -- Set working directory to be the user's document folder.
   catch
      (getUserDocumentsDirectory >>= setCurrentDirectory)
      (\(_ :: SomeException) -> return ())  -- Ignore any failure here.
   -- Set environment variable to disable dynamically disappearing scrollbars
   setEnv "GTK_OVERLAY_SCROLLING" "0"
   -- Parse command line and set fontConfig environment if instructed.
   options <- Opt.execParser cmdOpts
   case fontConfig options of
      Nothing -> return ()
      Just path -> do
         setEnv "FONTCONFIG_PATH" path
         setEnv "FC_DEBUG" "1024"  -- Debug code: monitor config file loading.
   -- Initialise GTK and add application-specific settings.
   void $ Gtk.init Nothing
   dataDir <- getProgramDataFolder
   -- Set up application GUI
   builder <- Gtk.builderNewFromFile $ T.pack $ dataDir </> "editor-ui.glade"
   window <- Gtk.builderGetObject builder "app-window" >>= \case
      Nothing -> fail "Cannot get app-window"
      Just win -> Gtk.unsafeCastTo Gtk.ApplicationWindow win
   do  -- Set up GTK parameters
      theme <- Gtk.iconThemeGetDefault
      evidenceIcons  <- getEvidenceDataFolder
      let
         hadesIcons = dataDir
         iconPaths = programIconDirs descriptor ++ [hadesIcons, evidenceIcons]
      mapM_ (Gtk.iconThemeAppendSearchPath theme) iconPaths
      let pri = fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION
      screen <- Gtk.windowGetScreen window
      bc <- bananaCss
      Gtk.styleContextAddProviderForScreen screen bc pri
      hadesCss <- Gtk.cssProviderNew
      getDataFileName "share/hades/hades-gtk.css" >>= Gtk.cssProviderLoadFromPath hadesCss . T.pack
      Gtk.styleContextAddProviderForScreen screen hadesCss pri
      Gtk.windowSetDefaultIconName "diametric"
      Gtk.set window [
            Gtk.windowRole := ("\\hades\\Main" :: Text),
            Gtk.windowTitle := programTitle descriptor <> ": <unknown>"]
   bx <- Gtk.builderGetObject builder "main-box" >>= \case
      Nothing -> fail "Cannot get application main-box widget."
      Just bx -> Gtk.unsafeCastTo Gtk.Box bx
   undoButtonWidget <- Gtk.builderGetObject builder "undo-button" >>= \case
      Nothing -> fail "Cannot get undo button."
      Just b -> Gtk.unsafeCastTo Gtk.MenuToolButton b
   redoButtonWidget <- Gtk.builderGetObject builder "redo-button" >>= \case
      Nothing -> fail "Cannot get undo button."
      Just b -> Gtk.unsafeCastTo Gtk.MenuToolButton b
   undoMenuItem <- Gtk.builderGetObject builder "menu-undo" >>= \case
      Nothing -> fail "Cannot get undo menu item."
      Just i -> Gtk.unsafeCastTo Gtk.Widget i
   redoMenuItem <- Gtk.builderGetObject builder "menu-redo" >>= \case
      Nothing -> fail "Cannot get redo menu item."
      Just i -> Gtk.unsafeCastTo Gtk.Widget i
   -- modelContainer holds the model tree.
   modelFrame <- Gtk.frameNew Nothing
   Gtk.boxPackEnd bx modelFrame True True 0
   -- Show the Welcome screen unless this has been disabled or we have a command-line filename.
   initialFilePath <- case fileToLoad options of
      Just path -> return $ Just path
      Nothing -> welcomeScreen window iconTheme False
   initialModel <- case initialFilePath of
      Nothing -> return (Nothing, emptyModel "Model")
      Just target ->
         catchFileError (Just window) (LB.readFile target) >>= \case
            Nothing -> return (Nothing, emptyModel "Model")
            Just text ->
               case eitherDecode' text of
                  Left str -> do
                     errorBox (Just window) $ T.pack $
                        "File \"" <> target <> "\" was corrupt and could not be read.\n\n"
                        <> "Technical details: " <> str
                     return (Nothing, emptyModel "Model")
                  Right doc ->
                     if isJust $ fileToLoad options
                        then  -- User file; see if we can get an edit lock.
                           forEditing window Nothing (return $ Just (target, doc)) >>= \case
                              Just (l, _) -> return (Just l, doc)
                              Nothing -> return (Nothing, emptyModel "Model")
                        else  -- Built-in example file. Treat as unsaved initial model.
                           return (Nothing, doc)
   -- Set working directory to be the user's document folder. This is needed for file dialogs to
   -- be based in the right folder. It is done *after* we read the initial file given in the
   -- command line argument because that is probably given relative to the original working
   -- directory.
   catch
      (getUserDocumentsDirectory >>= setCurrentDirectory)
      (\(_ :: SomeException) -> return ())  -- Ignore any failure here.
   -- IORefs used to manage start-up and panic from outside Reactive Banana's MomentIO monad.
   startupHandlerRef <- newIORef $ const $ return ()
   latestModel <- newIORef $ snd initialModel
   let
   -- The body of the program: a Reactive Banana circuit in the MomentIO monad.
      hadesNet = mdo  -- MomentIO monad
         -- Window quit
         quitE <- registerIOSignal1 window Gtk.onWidgetDeleteEvent $ \_ -> return (True, Just ())
         (undo1Click, redo1Click) <- historyActions window
         void $ confirmUnsaved window changedB (statusB <@ quitE) quitProgram
         -- Program start event, which is also used to recover after a panic.
         (programStart, programStartHandler) <- newEvent
         liftIO $ writeIORef startupHandlerRef programStartHandler
         -- Connect up the GTK actions to the associated Reactive Banana events.
         (fileEvents, fileActions) <- unzip <$> sequence [
               actionNew window (Just $ emptyModel "Model") changedB,
               actionExit window changedB statusB,
               actionOpen descriptor window changedB statusB,
               actionSave descriptor window saveEnableB statusB modelB,
               actionSaveAs descriptor window statusB modelB,
               actionRecovery descriptor window changedB statusB,
               actionAbout descriptor window modelB,
               actionDisplayFile "helpManual" $ dataDir </> "documentation" </> manualFile,
                  -- Manual is actually PDF, but we use a HTML redirect because of issue with
                  -- opening PDFs on Windows.
               actionDisplayFile "helpSupport" $ dataDir </> "documentation" </> "Support.html",
               actionDisplayFile "helpCredits" $ dataDir </> "documentation" </> "Credits.html"
            ]
         mapM_ (Gio.actionMapAddAction window) fileActions
         actionExportDiagram window iconTheme exportB >>= Gio.actionMapAddAction window
         gen <- fromPoll newGenerator  -- Used to execute import events.
         -- Track the current file in the window title.
         let
            fileEvent = foldr1 (unionWith const) $ programStart : fileEvents
            newModelE = runAfterLoad descriptor <$> gen <@> filterJust (snd <$> fileEvent)
         statusB <- stepper (fst initialModel) $ fst <$> fileEvent
         let
            readOnlyB = readOnlyStatus <$> statusB
            saveEnableB = (&&) <$> (not <$> readOnlyB) <*> changedB
         behaviorLink window Gtk.windowSetTitle $ statusB <&> (\t ->
               programTitle descriptor <> ": " <>
               T.pack (fromMaybe "<unknown>" $ t ^? _Just . fileStatusPath) <>
               (if readOnlyStatus t then " (read only)" else "")
            )
         -- Create an editor for each new model that comes out of the file events.
         newEditorE <- execute $ newModelE <&> (\newModel -> mdo
               liftIO previewFolderClean
               -- Connect up the dialog actions.
               -- This also drops any previous instances of these actions from old editors.
               (fieldEvents, fieldAction) <-
                     actionFields window iconTheme (modelFields <$> changesB modelC)
               Gio.actionMapAddAction window fieldAction
               (refTypeEvents, refTypeAction) <-
                     actionRefTypes window iconTheme (modelRefTypes <$> changesB modelC)
               Gio.actionMapAddAction window refTypeAction
               selectionExport <- actionExportSelection descriptor window modelC
               Gio.actionMapAddAction window selectionExport
               (importEvent, importAction) <- actionImportData descriptor window modelC
               Gio.actionMapAddAction window importAction
               (findEvent, findAction) <- actionFind
               Gio.actionMapAddAction window findAction
               (refreshEvent, refreshAction) <- actionRefresh
               Gio.actionMapAddAction window refreshAction

               output <- editorWindows
                     window
                     iconTheme
                     (programStock descriptor)

                     newModel
                     modelC
                     refreshEvent $
                     foldr (unionWith const) never [importEvent, findEvent, newFileLoadScript]
               -- Model update processing.
               modelE <- processModelUpdates
                                 (editorTreeInfo output)
                                 updates
                                 (fst <$> historySelection)
               modelC <- makeChanges newModel $ whenE (not <$> readOnlyB) modelE
               let
                  fieldUpdate oldModel newFields =
                        ModelUpdate {
                           updateValue = Just "Modify data fields",
                           updateNewModel = updatedModel,
                           updateEditLog = [ModelOther oldModel updatedModel]
                        }
                     where updatedModel = oldModel {modelFields = newFields}
                  refTypeUpdate oldModel newRefs =
                        ModelUpdate {
                           updateValue = Just "Modify reference types",
                           updateNewModel = updatedModel,
                           updateEditLog = [ModelOther oldModel updatedModel]
                        }
                     where updatedModel = oldModel {modelRefTypes = newRefs}
                  modelDataUpdates = foldr (unionWith const) never [
                        fieldUpdate <$> changesB modelC <@> fieldEvents,
                        refTypeUpdate <$> changesB modelC <@> refTypeEvents
                     ]
               -- Connect up the extension menu.
               modelExtensionUpdates <- extensionMenu window iconTheme builder modelC
               -- Model updates.
               let
                  modelUpdates = editorUpdates output
                  modelChanged = True <$ modelHistoryItems
                  modelUnchanged = False <$ fileEvent
                  updates = foldr1 (unionWith const) [
                        modelUpdates, modelDataUpdates, modelExtensionUpdates]
               changedB <- stepper False $ unionWith (||) modelChanged modelUnchanged
               -- Autosave process fork. Save every 5 minutes and keep the last 5 autosave files.
               writeAutosave <- liftIO $ autosaveProcess (programName descriptor) (5 * 60) 5
               reactimate $ writeAutosave <$>
                     (maybe "" (view fileStatusPath) <$> statusB) <@>
                     changesE modelC
               -- Keep a copy of the latest successful model edit for panic recovery.
               reactimate $ writeIORef latestModel <$> changesE modelC
               -- History mechanism for undo and redo.
               let
                  modelHistoryItems = HistoryItem <$> changesB modelC <@>
                     filterE (/= "") (filterJust (updateValue <$> updates))
                        -- Note: this provides the *old* version of the model, not the updated one.
                  undoE1 = unionWith (+) undoE undo1Click
                  redoE1 = unionWith (+) redoE redo1Click
                  historySelection = processHistoryClick undoE1 redoE1 (changesB modelC) historyB
                  historyMenus1 =
                     historyMenus undoButtonWidget [undoMenuItem] redoButtonWidget [redoMenuItem]
                  newHistory = flip (recordUndo maxUndoLength) <$> historyB <@> modelHistoryItems
                  historyUpdates = unionWith const newHistory $ snd <$> historySelection
               historyMenus1 emptyHistory >>= liftIO . view _3 -- Clear the history widgets.
               newHistoryEvents <- execute $ historyMenus1 <$> historyUpdates
               undoE <- switchE $ view _1 <$> newHistoryEvents
               redoE <- switchE $ view _2 <$> newHistoryEvents
               reactimate $ view _3 <$> newHistoryEvents
               historyB <- stepper emptyHistory historyUpdates
               -- Replace the child widget of the modelFrame.
               Gtk.binGetChild modelFrame >>= \case
                  Just oldWidget -> Gtk.containerRemove modelFrame oldWidget
                  Nothing -> return ()
               Gtk.containerAdd modelFrame $ editorWidget output
               Gtk.widgetShowAll modelFrame
               return (changesB modelC, output, changedB)
            )
         modelB <- switchB (pure $ snd initialModel) $ newEditorE <&> view _1
         exportB <- switchB (pure Nothing) $ newEditorE <&> view (_2 . to editorDiagramExport)
         changedB <- switchB (pure False) $ newEditorE <&> view _3
         -- Trigger the execution of openStartDiagram *after* everything else has happened.
         newFileLoadScript <- mapEventIO (const $ return openStartDiagram) newEditorE
         return ()
   compile hadesNet >>= actuate
   Gtk.widgetShowAll window
   let executeProgram =
         catch Gtk.main $ \e -> do
               errorBox (Just window) $ T.pack $
                  "An internal error has occured. I will try to recover your work. If I succeed \
                  \then please save it in a new file. You may also be able to recover an older \
                  \version by restarting the program and using the recovery option in the File \
                  \menu.\n\n\
                  \Please report this bug.\n\n\
                  \Technical details: " ++ show (e :: SomeException) ++ "\n\n\
                  \Note: if the term \"user\" appears above, this is a reference to the \
                  \programmer, not you."
               saved <- readIORef latestModel
               h <- readIORef startupHandlerRef
               h (Nothing, Just saved)  -- Deliberately force user to pick a new file name.
               executeProgram
   h <- readIORef startupHandlerRef
   h $ second Just initialModel
   executeProgram

{-
Design note: the autosave code in modelMain could in theory have done something like:

   autosave = changes $ (,) <$> statusB <*> model

However this would have unpredictable race conditions when a new model is loaded; it could
potentially store the old model under the new name, or the new model under the old name. Therefore
the autosave event is triggered only when the model changes, and the current pathname is picked
up at that point. Using plainChanges is the most straightforward way of doing this.
-}


-- | Open any diagrams with names beginning "Start" at the top level of the model.
openStartDiagram :: (EntityClass v, HasDiagrams p v) => ModelScript p v v (Maybe Text)
openStartDiagram = do
   ents <- lift $ do
      goToRoot
      ch <- map (_1 %~ view nameText) . M.toList <$> currentChildren
      let entIds = map snd $ filter (("Start" `T.isPrefixOf`) . fst) ch
      forM entIds $ \uid -> do
         goToEntity uid
         current
   forM_ (catMaybes ents) $ \ent ->
         lift (getDiagramWrapper ent) >>= \case
            Nothing -> return ()  -- "Start" entity is not a diagram.
            Just {} -> openDiagram $ entityId ent
   return Nothing  -- This script makes no changes.


-- | Run an action to update the loaded model into a new format.
runAfterLoad :: MainDescriptor v -> Generator -> Model v -> Model v
runAfterLoad desc gen model =
   case execModelEdit id (modelStartState gen model) (programAfterLoad desc) of
      Left err -> cannotHappen ("Format update failed:\n" <> T.pack (show err)) model
      Right (newModel, _) -> newModel


-- | Previous versions used many different relations for arrows. These have now been combined
-- into the \"@Arrow Head@\" and \"@Arrow Tail@\" relations. This function renames all the old
-- ones.
--
-- This code should be deleted once it is no longer required.
fixOldRelations :: (EntityClass v) => Model v -> Model v
fixOldRelations model = model {modelRelations = renameOld $ modelRelations model}
   where
      renameOld =
         renameRelation "Before arrow" edgeFromRelation
         . renameRelation "After arrow" edgeToRelation
         . renameRelation "Trigger arrow out" edgeFromRelation
         . renameRelation "Trigger arrow in" edgeToRelation
         . renameRelation "Guard arrow out" edgeFromRelation
         . renameRelation "Guard arrow in" edgeToRelation
         . renameRelation "Hazard arrow out" edgeFromRelation
         . renameRelation "Hazard arrow in" edgeToRelation
         . renameRelation "Arrow From" edgeFromRelation
         . renameRelation "Arrow To" edgeToRelation



-- | Most of the application UI (command buttons, menus etc) are defined in XML using Glade.
-- However the extension menu is defined using the "Reflective" interface and is then attached
-- to the @ExtensionFields@ menu item.
extensionMenu :: (Gtk.IsWidget parent, EntityClass v) =>
   parent
   -> Gtk.IconTheme
   -> Gtk.Builder
   -> Changes (Model v)
   -> MomentIO (Event (ModelUpdate v (Maybe Text)))
extensionMenu parent iconTheme builder modelC = do
      menu <- Gtk.new Gtk.Menu []
      model <- valueBLater $ changesB modelC
      let
         vs = nubBy ((==) `on` reflectiveName) $
               sortOn (variantUserLabel . reflectiveName) reflectiveDefaults
               -- reflectiveName can have duplicates. Hence "nubBy".
         _ = evalModelEdit id model $ forM vs mkEntity
               -- Tell compiler that vs contains the same type as model.
      events <- forM (zip vs [0..]) $ \(v, n) -> do
         item <- Gtk.new Gtk.MenuItem
               [#label := variantUserLabel $ reflectiveName v, #useUnderline := False]
         Gtk.menuAttach menu item 0 1 n (n+1)
         registerIOSignal item Gtk.onMenuItemActivate $
               return ((), reflectiveName v)
      parentItem <- liftIO $ Gtk.builderGetObject builder "ExtensionFields" >>= \case
         Nothing -> fail "Error: cannot get ExtensionFields menu item"
         Just i -> Gtk.unsafeCastTo Gtk.MenuItem i
      Gtk.widgetShowAll menu
      Gtk.menuItemSetSubmenu parentItem $ Just menu
      let
         extensionEvent = foldr (unionWith const) never events
         modelB = changesB modelC
      updateEvent <- mkGtkPopup parent iconTheme modelC $ extFields <$> modelB <@> extensionEvent
      return $ filterJust $ mkUpdate <$> changesB modelC <@> updateEvent
   where
      -- Marshal arguments for mkGtkPopup
      extFields :: (EntityClass v) =>
         Model v -> Variant v -> (Variant v, [FieldId], Dialog' (Model v) () [FieldId])
      extFields model variant =
         let
            fields = M.findWithDefault [] variant $ modelExtensions model
            dialog = extensionDialog (modelFields model) variant
         in (variant, fields, dialog)
      -- Marshal dialog output into model update.
      mkUpdate _ (_, Nothing) = Nothing
      mkUpdate model (variant, Just v) =
         Just ModelUpdate {
            updateValue = Just $ "Modify " <> variantUserLabel variant <> " extension fields",
            updateNewModel =
               model {modelExtensions = M.insert variant v $ modelExtensions model},
            updateEditLog = []  -- No change to tree widget.
         }


saveFileFilters :: MainDescriptor v -> IO [Gtk.FileFilter]
saveFileFilters descriptor = do
   saveFilter <- Gtk.fileFilterNew
   Gtk.fileFilterSetName saveFilter $
         Just $ programTitle descriptor <> " files *." <> programXtn descriptor
   Gtk.fileFilterAddPattern saveFilter $ "*." <> programXtn descriptor
   anyFilter <- Gtk.fileFilterNew
   Gtk.fileFilterAddPattern anyFilter "*"
   Gtk.fileFilterSetName anyFilter $ Just "Any file"
   return [saveFilter, anyFilter]


exportFileFilters :: MainDescriptor v -> IO [Gtk.FileFilter]
exportFileFilters descriptor = do
      saveFilter <- Gtk.fileFilterNew
      Gtk.fileFilterSetName saveFilter $
            Just $ programTitle descriptor <> " export files *." <> extension
      Gtk.fileFilterAddPattern saveFilter $ "*." <> extension
      anyFilter <- Gtk.fileFilterNew
      Gtk.fileFilterSetName anyFilter $ Just "Any file"
      Gtk.fileFilterAddPattern anyFilter "*"
      return [saveFilter, anyFilter]
   where
      extension = T.dropEnd 1 (programXtn descriptor) <> "x"


-- | If the current value has been changed since the last save then confirm before proceeding.
confirmUnsaved :: (Gtk.IsWidget parent) =>
   parent               -- ^ Widget in parent window for confirmation dialog.
   -> Behavior Changed  -- ^ Has the model changed since the last save? If not then just proceed.
   -> Event a           -- ^ Event that needs to be confirmed.
   -> (a -> IO b)       -- ^ Action to execute if confirmed.
   -> MomentIO (Event b)
confirmUnsaved parent changedB event action = do
   result <- mapEventIO id $ confirm1 <$> changedB <@> event
   return $ filterJust result
   where
      confirm1 flag value =
         if flag
         then do
            dialog <- Gtk.new Gtk.MessageDialog [
                  #messageType := Gtk.MessageTypeWarning,
                  #buttons := Gtk.ButtonsTypeYesNo,
                  #text :=
                     "The current model has not been saved and any changes will be lost.\n\n\
                       \Are you sure?"
               ]
            parentWin <- Gtk.castTo Gtk.Window =<< Gtk.widgetGetToplevel parent
            forM_ parentWin $ \w -> Gtk.set dialog [#transientFor := w]
            result <- Gtk.dialogRun dialog
            Gtk.widgetDestroy dialog
            if result == fromIntegral (fromEnum Gtk.ResponseTypeYes)
               then Just <$> action value else return Nothing
         else Just <$> action value

{- $fileOps
All the file operations have the same return type so that they can be convienently
put in a list and registered in the GUI together. When any of these operations is invoked the
event is triggered. The event will contain a new filename and/or a new model value. On this event
the changed flag can be cleared.
-}

-- | Create a new model.
actionNew ::(Gtk.IsWidget parent) =>
   parent     -- ^ Parent widget for confirmation dialog.
   -> v
   -> Behavior Changed  -- ^ Do we need to ask for confirmation?
   -> MomentIO (Event (Maybe FileStatus, v), Gio.SimpleAction)
actionNew parent newValue changedB = do
   action <- Gio.simpleActionNew "new" Nothing
   newClicked <- registerIOSignal1 action Gio.onSimpleActionActivate $
      \_ -> return ((), (Nothing, newValue))
   newConfirmed <- confirmUnsaved parent changedB newClicked return
   return (newConfirmed, action)


-- | Load an existing model from a file.
actionOpen :: (EntityClass v, FromJSON v, Gtk.IsWidget parent) =>
   MainDescriptor v
   -> parent             -- ^ Widget in parent window.
   -> Behavior Changed   -- ^ Do we need to ask for confirmation?
   -> Behavior (Maybe FileStatus)  -- ^ Current file status.
   -> MomentIO (Event (Maybe FileStatus, Maybe (Model v)), Gio.SimpleAction)
actionOpen descriptor parent changedB statusB = do
   action <- Gio.simpleActionNew "open" Nothing
   openClicked <- registerIOSignal1 action Gio.onSimpleActionActivate
      (\_ -> return ((), ()))
   openConfirmed <- confirmUnsaved parent changedB (statusB <@ openClicked) $ \status -> do
         filters <- saveFileFilters descriptor
         forEditing parent status
            (openJsonFile parent filters (status ^? _Just . fileStatusPath)) >>= \case
               Nothing -> return Nothing
               Just (newStatus, doc) -> return $ Just (Just newStatus, Just $ fixOldRelations doc)
   return (filterJust openConfirmed, action)


-- | Save the current model under the current filename.
actionSave :: (EntityClass v, ToJSON v, Gtk.IsWidget parent) =>
   MainDescriptor v
   -> parent                        -- ^ Parent widget
   -> Behavior Bool                 -- ^ Enabled?
   -> Behavior (Maybe FileStatus)   -- ^ Current edit lock and path name, if any.
   -> Behavior (Model v)             -- ^ Model state
   -> MomentIO (Event (Maybe FileStatus, Maybe (Model v)), Gio.SimpleAction)
actionSave descriptor parent enableB statusB modelB = do
   let
      actionInput = (,) <$> statusB <*> modelB
   action <- Gio.simpleActionNew "save" Nothing
   Gtk.set action [#enabled := False]  -- Initially no edits have been made, so can't save them.
   enableE <- changes $ enableB <&> (\b -> Gtk.set action [#enabled := b])
   reactimate' enableE
   saveClicked <- registerIOSignal1 action Gio.onSimpleActionActivate
      (\_ -> return ((), ()))
   (fileSaved, savedHandler) <- newEvent
   reactimate $ (actionInput <@ saveClicked) <&> (\(status, model) -> do
         filters <- saveFileFilters descriptor
         if readOnlyStatus status
            then errorBox (Just parent) "This model is read-only."
            else do
               newStatus <- forEditing parent status $ saveFile
                     parent
                     filters
                     (checkExtension $ T.unpack $ programXtn descriptor)
                     (status ^? _Just . fileStatusPath)
                     model
               savedHandler (fst <$> newStatus, Nothing)
      )
   return (fileSaved, action)

-- | Prompt for a new file name and save the model under that.
actionSaveAs :: (EntityClass v, ToJSON v, Gtk.IsWidget parent) =>
   MainDescriptor v
   -> parent                        -- ^ Parent widget for dialog box.
   -> Behavior (Maybe FileStatus)
   -> Behavior (Model v)
   -> MomentIO (Event (Maybe FileStatus, Maybe (Model v)), Gio.SimpleAction)
actionSaveAs descriptor parent statusB modelB = do
   let actionInput = (,) <$> statusB <*> modelB
   action <- Gio.simpleActionNew "saveAs" Nothing
   saveClicked <- registerIOSignal1 action Gio.onSimpleActionActivate
      (\_ -> return ((), ()))
   (fileSaved, savedHandler) <- newEvent
   reactimate $ (actionInput <@ saveClicked) <&> (\(status, model) -> do
         filters <- saveFileFilters descriptor
         newStatus <- forEditing parent status $ saveAsFile
               parent
               filters
               (checkExtension $ T.unpack $ programXtn descriptor)
               (status ^? _Just . fileStatusPath)
               model
         savedHandler (fst <$> newStatus, Nothing)
      )
   return (fileSaved, action)


actionRecovery :: (EntityClass v, FromJSON v, Gtk.IsWidget parent) =>
   MainDescriptor v
   -> parent   -- ^ Parent widget for dialog.
   -> Behavior Changed   -- ^ Do we need to ask for confirmation?
   -> Behavior (Maybe FileStatus)  -- ^ Current edit lock status and file name.
   -> MomentIO (Event (Maybe FileStatus, Maybe (Model v)), Gio.SimpleAction)
actionRecovery descriptor parent changedB statusB = do
      action <- Gio.simpleActionNew "recovery" Nothing
      recoveryClicked <- registerIOSignal1 action Gio.onSimpleActionActivate
         (\_ -> return ((), ()))
      recoveryConfirmed <- confirmUnsaved parent changedB (statusB <@ recoveryClicked) $ const $ do
         files <- sortOn (Down . autosaveWhen) <$> autosaveFiles (programName descriptor)
         tbl <- catMaybes <$> forM files (\f ->
               autosaveGetContents (autosavePath f) >>= \case
                  Nothing ->
                     return Nothing
                  Just content ->
                     return $ Just (autosavePath f, autosaveFor content, autosaveWhen f)
            )
         let dflt = if null tbl then "" else head tbl ^. _1
         return ((), dflt, recoveryDialog tbl)
      theme <- Gtk.iconThemeGetDefault
      recoveryEvent <- mkGtkPopup parent theme (pure ()) recoveryConfirmed
      (recoveryComplete, handler) <- newEvent
      reactimate $ ((,) <$> statusB <@> recoveryEvent) <&> (\case
         (_, (_, Nothing)) -> return ()
         (oldStatus, (_, Just path)) -> do
            autosaveGetContents path >>= \case
               Nothing -> errorBox (Just parent) "Could not recover autosave file."
               Just result -> case eitherDecode' $ autosaveData result of
                  Left err -> errorBox
                        (Just parent)
                        $ T.pack $ "Could not recover autosave file:\n\n" <> err
                  Right doc -> do
                     let v = if autosaveFor result == ""
                                 then Nothing
                                 else Just (autosaveFor result, doc)
                     lockResult <- forEditing parent oldStatus $ return v
                     handler (fst <$> lockResult, Just doc)
         )
      return (recoveryComplete, action)
   where
      recoveryDialog :: [(FilePath, FilePath, UTCTime)] -> Dialog' e w FilePath
      recoveryDialog contents =
         let
            spec = comboBox $ const $ flip map contents $ \(path, content, time) ->
               let
                  nm = if null content then "<unnamed file>" else content
                  tStr = T.pack $ formatTime defaultTimeLocale "%d/%m/%y %R" time
               in ComboItem {
                     menuItemLabel = tStr <> ": " <> T.pack nm,
                     menuItemIcon = Nothing,
                     menuItemColour = Nothing,
                     menuItemValue = path
                  }
         in Dialog "Recover from auto-save" OkButton $ simpleFrame "File to recover: " spec



-- | Prompt for a new file name and export the current diagram under that.
actionExportDiagram :: (Gtk.IsWidget parent) =>
   parent     -- ^ Parent widget for confirmation dialog.
   -> Gtk.IconTheme
   -> Behavior (Maybe DiagramExport)
   -> MomentIO Gio.SimpleAction
actionExportDiagram parent iconTheme exportB = do
   action <- Gio.simpleActionNew "exportDiagram" Nothing
   exportClicked <- registerIOSignal1 action Gio.onSimpleActionActivate
      (\_ -> return ((), ()))
   reactimate $ exportDrawing parent iconTheme <$> filterJust (exportB <@ exportClicked)
   return action


-- | Pop up the field list dialog.
actionFields :: (Gtk.IsWidget parent) =>
   parent         -- ^ Parent widget for dialogs.
   -> Gtk.IconTheme
   -> Behavior FieldTable
   -> MomentIO (Event FieldTable, Gio.SimpleAction)
actionFields window iconTheme tbl = do
      newIds <- fromPoll uuidStreamIO
      action <- Gio.simpleActionNew "fields" Nothing
      typesClicked <- registerIOSignal1 action Gio.onSimpleActionActivate $ \_ -> return ((), ())
      updates1 <- mkGtkPopupSelect
            window
            iconTheme
            (pure ())
            (constantDialog (fieldDialog :: Dialog' e () [Field]))
            (((),) . sortOn (T.toCaseFold . view fieldName) . M.elems <$> tbl <@ typesClicked)
      let
         updates2 = replaceNullIds <$> newIds <@> filterJust (snd <$> updates1)
      return (fromFieldList <$> updates2, action)
   where
      fromFieldList = M.fromList . map (\f -> (fieldId f, f))
      replaceNullIds :: [FieldId] -> [Field] -> [Field]
      replaceNullIds _ [] = []
      replaceNullIds [] _ = cannotHappen "Field dialog ran out of new field IDs." []
      replaceNullIds fids@(fid:fids1) (f : fields) =
         if U.null $ fieldId f
            then f {fieldId = fid} : replaceNullIds fids1 fields
            else f : replaceNullIds fids fields



-- | Pop up the reference type list dialog.
actionRefTypes :: (Gtk.IsWidget parent, Reflective a) =>
   parent   -- ^ Parent widget for dialogs.
   -> Gtk.IconTheme
   -> Behavior (RefTypeTable a)
   -> MomentIO (Event (RefTypeTable a), Gio.SimpleAction)
actionRefTypes parent iconTheme tbl = do
   action <- Gio.simpleActionNew "refTypes" Nothing
   typesClicked <- registerIOSignal1 action Gio.onSimpleActionActivate $ \_ -> return ((), ())
   updates <- mkGtkPopupSelect
         parent
         iconTheme
         (pure ())
         (constantDialog referenceListDialog)
         ((True, ) . refTypeTableToList <$> tbl <@ typesClicked)
   return (filterJust $ fmap refTypeTableFromList . snd <$> updates, action)


-- | Import the data into a selected location in the model. Returns an edit action that inserts
-- the data.
actionImportData :: (Gtk.IsWidget parent, EntityClass v, FromJSON v, HasEvidence v) =>
   MainDescriptor v
   -> parent    -- ^ Parent widget for dialog.
   -> Changes (Model v)
   -> MomentIO (Event (ModelScript p v v (Maybe Text)), Gio.SimpleAction)
actionImportData descriptor parent modelC = do
      action <- Gio.simpleActionNew "importData" Nothing
      importClicked <- registerIOSignal1 action Gio.onSimpleActionActivate $ const $
            return ((), ((), defaultValue))
      theme <- Gtk.iconThemeGetDefault
      importE <- mkGtkPopupSelect parent theme modelC importDialog importClicked
      (resultE, resultH) <- newEvent
      reactimate $ importE <&> (\case
         ((), Nothing) -> return ()  -- Action cancelled.
         ((), Just selection) -> do
            let
               targetId = case S.toList $ either (view _1) (view _1) selection of
                  [] -> U.nil  -- Nothing selected, so dump it in root.
                  t : _ -> t  -- Multiple selections should never happen.
            filters <- exportFileFilters descriptor
            case selection of  -- ModelEdit action
               Left (_, metaFlag) -> openJsonFile parent filters Nothing >>= \case
                  Nothing -> return ()  -- Action cancelled.
                  Just (path, v) -> resultH $ lift $ do
                     importEntities targetId (exportRelations v) (exportContents v)
                     when metaFlag $ importMetadata v
                     return $ Just $ T.pack $ "Imported from " <> path
               Right (_, template, clash) -> openExcelFile parent Nothing >>= \case
                  Nothing -> return ()  -- Action cancelled.
                  Just (path, book) -> resultH $ do
                     lift $ goToEntity targetId
                     lift (importExcel clash book template) >>= \case
                        [] -> return $ Just $ T.pack $ "Imported from " <> path  -- No warnings.
                        warns -> do
                           let
                              warnDialog = constantDialog $ Dialog "Warning" OkButton $
                                 message1 $
                                       "Warnings during import:\n\n" <>
                                       T.intercalate "\n" warns <>
                                       "\n\nPress OK to proceed, Cancel to stop."
                           openDialog warnDialog () >>= \case
                              Just _ -> return $ Just $ T.pack $ "Imported from " <> path
                              Nothing -> lift $ throwUser "Import cancelled. No changes made."
         )
      return (resultE, action)
   where
      defaultValue = Left (S.empty, False)
      mkSelectorItem ent = (
            ent ^. entityName . nameText,
            Nothing,
            if isJust $ ent ^? entityContents . _Package then Just $ entityId ent else Nothing
         )
      importDialog :: (EntityClass v, HasEvidence v) =>
            DialogSelector' (Model v) w (Either
                  (S.Set U.UUID, Bool)
                  (S.Set U.UUID, v, NameClashAction))
      importDialog m _ = Just $ Dialog "Import Data" OkButton $ validate validityCheck $
            unionTab [("Import Model", modelImport), ("Import Excel", excelImport)]
         where
            forest = case evalModelEdit id m $ goToRoot >> modelPackageForest of
               Left err -> cannotHappen ("Element import: " <> T.pack (show err)) mempty
               Right v -> fmap mkSelectorItem <$> v
            tree = Node (modelName m, Nothing, Just U.nil) forest
            modelImport = PrismaticGadget (S.empty, False) _Left $ proc (loc1, flg1) -> do
               _ <- message1 "Select location for import." -< ()
               loc2 <- simpleFrame "Import location:" $ treeSelector $ const [tree] -< loc1
               flg2 <- accum $ form Vertical [("Import extensions?", focusing id tickBox)] -< flg1
               returnA -< (loc2, flg2)
            excelImport = PrismaticGadget (S.empty, blankEvidence, CancelClash) _Right $
               proc (loc1, blank1, clash1) -> do
                  _ <- message1 "Select import location and type." -< ()
                  loc2 <- simpleFrame "Import Location:" $ treeSelector $ const [tree] -< loc1
                  (blank2, clash2) <- accum $ form Vertical [
                           ("Import type:", focusing _1 variantMenu),
                           ("On name clash:", focusing _2 boundedCombo)
                        ] -< (blank1, clash1)
                  returnA -< (loc2, blank2, clash2)
            variantMenu = comboBox $ const $ sortOn menuItemLabel $
               map
                  (\r -> ComboItem (variantUserLabel $ reflectiveName r) Nothing Nothing r)
                  reflectiveDefaults
            blankEvidence = Evidence (Name "") ^. re _Evidence
            validityCheck v = S.size (either (view _1) (view _1) v) == 1


{- Design Note:

The import dialog uses Either and tuples as a quick-and-dirty import type. If this gets any more
complicated then it should be given its own dedicated type.
-}


-- | Dialog for exporting the selected items.
actionExportSelection :: (Gtk.IsWidget parent, EntityClass v, ToJSON v) =>
   MainDescriptor v
   -> parent    -- ^ Parent widget for dialogs.
   -> Changes (Model v)
   -> MomentIO Gio.SimpleAction
actionExportSelection descriptor parent modelC = do
      action <- Gio.simpleActionNew "exportData" Nothing
      exportSelectionClicked <- registerIOSignal1 action Gio.onSimpleActionActivate $ \_ ->
            return ((), ((), (S.empty, False)))
      theme <- Gtk.iconThemeGetDefault
      exportE <- mkGtkPopupSelect parent theme modelC exportDialog exportSelectionClicked
      let
         exportData =
               snd $
               queryModel id (changesB modelC) (filterJust $ snd <$> exportE) $
               uncurry makeExport
      reactimate $ exportData <&> (\dat -> do
            filters <- exportFileFilters descriptor
            void $ writeAsFile
                  parent
                  "Export Data to File"
                  "Export"
                  filters
                  (checkExtension $ T.unpack extension)
                  Nothing
                  dat
         )
      return action
   where
      extension = T.dropEnd 1 (programXtn descriptor) <> "x"
      mkSelectorItem ent = (ent ^. entityName . nameText, Nothing, Just $ entityId ent)
      exportDialog m _ = Just $
         let
            forest = case evalModelEdit id m $ goToRoot >> modelPackageForest of
               Left err -> cannotHappen ("Element export: " <> T.pack (show err)) mempty
               Right v -> fmap mkSelectorItem <$> v
         in
            Dialog "Export Data" OkButton $ proc (selected1, flag1) -> do
               _ <- message1 "Select items to export. \
                  \The children of the selected items will be included automatically." -< ()
               selected2 <- simpleFrame "Export Entities" $ treeSelector $ const forest -< selected1
               flag2 <- accum $ form Vertical [("Export extensions?", focusing id tickBox)] -< flag1
               returnA -< (selected2, flag2)


-- | Find text in the model.
actionFind :: (EntityClass v) =>
   MomentIO (Event (ModelScript p v v (Maybe Text)), Gio.SimpleAction)
actionFind = do
   action <- Gio.simpleActionNew "find" Nothing
   findClicked <- registerIOSignal1 action Gio.onSimpleActionActivate $ \_ -> return ((), ())
   return (findTextTool <$ findClicked, action)


-- | Refresh any matrices that are currently displayed. Actually, this sends a refresh signal
-- to all tabs, its just that only matrices actually do anything with it.
actionRefresh :: MomentIO (Event (), Gio.SimpleAction)
actionRefresh = do
   action <- Gio.simpleActionNew "refresh" Nothing
   refreshClicked <- registerIOSignal1 action Gio.onSimpleActionActivate $ \_ -> return ((), ())
   return (refreshClicked, action)


-- | Exit the program
actionExit :: (Gtk.IsWidget parent) =>
   parent     -- ^ Parent widget for confirmation dialog.
   -> Behavior Changed
   -> Behavior (Maybe FileStatus)
   -> MomentIO (Event a, Gio.SimpleAction)
actionExit window changedB statusB = do
   action <- Gio.simpleActionNew "quit" Nothing
   exitClicked <- registerIOSignal1 action Gio.onSimpleActionActivate $ \_ -> return ((), ())
   void $ confirmUnsaved window changedB (statusB <@ exitClicked) quitProgram
   return (never, action)


-- | An action that opens the named file.
actionDisplayFile ::
   Text   -- ^ The UI name for this action.
   -> FilePath   -- ^ The file to open on activation.
   -> MomentIO (Event a, Gio.SimpleAction)
actionDisplayFile lbl path = do
   uri <- GLib.filenameToUri path Nothing
   action <- Gio.simpleActionNew lbl Nothing
   void $ Gio.onSimpleActionActivate action $ \_ ->
      catchAny
         (Gtk.showUriOnWindow (Nothing :: Maybe Gtk.Window) uri 0)
               -- Zero is magic timestamp value for "now".
         (\err -> errorBox (Nothing :: Maybe Gtk.Widget) (T.pack $ show err))
   return (never, action)
   where
      catchAny :: IO a -> (SomeException -> IO a) -> IO a
      catchAny = Control.Exception.catch


-- | An action that displays information about the program.
actionAbout :: (EntityClass v, Gtk.IsWidget parent) =>
   MainDescriptor v
   -> parent  -- ^ Parent widget for dialog.
   -> Behavior (Model v)
   -> MomentIO (Event a, Gio.SimpleAction)
actionAbout desc parent modelB = do
      action <- Gio.simpleActionNew "helpAbout" Nothing
      aboutClicked <- registerIOSignal1 action Gio.onSimpleActionActivate $ \_ -> return ((), ())
      reactimate $ displayAbout <$> modelB <@ aboutClicked
      return (never, action)
   where
      displayAbout model = do
         dataDir <- getProgramDataFolder
         licenseBytes <- liftIO $ LB.readFile $ dataDir </> "documentation" </> "LICENSE.txt"
         let
            licenseText = decodeUtf8With lenientDecode $ LB.toStrict licenseBytes
         about <- Gtk.new Gtk.AboutDialog [
               #comments :=
                  "The best modelling tool for safety cases and hazards.\n\
                  \Current model contains " <>
                  T.pack (show $ M.size $ modelContents model) <> " entities.",
               #copyright := "Copyright © Paul Johnson 2020. Licensed under BSD3 terms. \
                  \See License for details.",
               #iconName := "diametric",
               #license := licenseText,
               #logoIconName := "diametric",
               #programName := programTitle desc,
               #version := "Version " <> T.pack (showVersion version),
               #website := "",
               #websiteLabel := ""
            ]
         parentWin <- Gtk.castTo Gtk.Window =<< Gtk.widgetGetToplevel parent
         forM_ parentWin $ \w -> Gtk.set about [#transientFor := w]
         void $ Gtk.dialogRun about
         Gtk.widgetDestroy about


-- | Quit the program.
quitProgram :: Maybe FileStatus -> IO ()
quitProgram status = do
   mapM_ dropEditLock $ status ^? _Just . fileStatusLock . _Just
   Gtk.mainQuit
   return ()
