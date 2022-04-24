{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecursiveDo #-}

{-
Copyright © Paul Johnson 2019. See LICENSE file for details.
-}

-- | This is the legacy dialog code. It is being phased out. Various parts are bitrotted and
-- should not be used in any new code.
module Reactive.Banana.GI.Dialog (
   mkGtkPopupSelect,
   mkGtkPopup,
   mkDialogWidget,
   createDialog,
   renderSubDialog,
   datePrism
) where

import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Function
import qualified Data.GI.Base.Signals as Gtk
import qualified Data.GI.Base.GType as Gtk
import qualified Data.GI.Gtk as MV  -- for ModelView
import Data.Int
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Tree
import qualified Data.Vector.Lens as V
import Data.VTree (VTree)
import qualified Data.VTree as V
import Data.Word
import qualified GI.Gdk as Gdk
import qualified GI.GObject as G
import GI.Gtk (AttrOp ((:=)))
import qualified GI.Gtk as Gtk
import qualified GI.Pango.Enums as Pango
import Reactive.Banana
import Reactive.Banana.Common
import Reactive.Banana.Dialog
import Reactive.Banana.Frameworks
import Reactive.Banana.GI.Common
import Reactive.Banana.GI.Connect
import Reactive.Banana.GI.DataIconTheme
import Reactive.Banana.GI.ErrorBox
import Reactive.Banana.GI.Menu
import Reactive.Banana.GI.Table
import System.Random


-- | Pop up one of a range of dialog boxes depending on the event value. Typically @s@ is
-- a sum type, and the selector will return a different dialog depending on which variant
-- it receives.
--
-- The @Event@ is the trigger for the popup to appear. A new popup will be spawned for
-- every trigger event, with the initial values taken from the @s@ parameter in the event. The
-- @k@ parameter allows the application to distinguish between the output events for different
-- invocations.
--
-- The returned @Event@ is triggered by the @OK@, @Apply@ and @Cancel@ buttons in the dialog.
-- If @@Cancel@ is pressed then the event contains @Nothing@. Othewise it contains
-- a setter function that will make the specified updates to its arguments. Typically the
-- application will pass in the value of @s@ that originally triggered the popup, but the
-- setter can also be applied to multiple values, such as all items selected in some other part
-- of the GUI.
mkGtkPopupSelect :: (Gtk.IsWidget parent) =>
   parent   -- ^ The dialog will be kept above the window with this widget.
   -> DialogSelector s
   -> Event (Bool, (k, s))   -- ^ Flag is True if this dialog should have an \"Apply\" button.
   -> MomentIO (Event (k, Maybe (s->s)))
mkGtkPopupSelect parent selector trigger = do
      result <- execute $ popupAction <$> trigger
      newEvents <- accumE never (unionWith const <$> result)
      switchE newEvents
   where
      popupAction (applyFlag, (k, v)) =
         case selector v of
            Nothing -> return never
            Just (DialogWrapper prsm dialog) ->
               case v ^? prsm of
                  Nothing -> return never
                  Just v1 -> do
                     innerEvent <- createDialog parent (applyFlag, k, v1, dialog)
                     return $ ((_2 . _Just) %~ over prsm) <$> innerEvent


-- | Similar to @mkGtkPopupSelect@, except with the Dialog being part of the event.
mkGtkPopup :: (Gtk.IsWidget parent) =>
   parent
   -> Event (Bool, k, s, MomentIO (Dialog s))
   -> MomentIO (Event (k, Maybe (s->s)))
mkGtkPopup parent trigger = do
      result <- execute $ createDialog parent <$> trigger
      newEvents <- accumE never (unionWith const <$> result)
      switchE newEvents


-- | Creates a widget which continuously displays a value determined by an Event and returns
-- edits to the value as events. The Key value @k@ carries an identifier for the value being
-- edited. When it changes a new dialog is created. The returned widget is a "Gtk.Frame",
-- so the caller does not need to do anything with the result except insert it in the widget
-- hierarchy.
--
-- If the initial key-value pair is not @Nothing@ then this function will immediately inspect the
-- dialog selector behavior. This can cause problems if the function is called when the
-- application is still starting up and the selector has no value.
--
-- The dialog will be recomputed if the input key changes or if the selector event is triggered.
-- This allows the application to filter updates to the dialog.
mkDialogWidget :: (Eq k, Eq v) =>
   (Gtk.Widget -> IO ()) -- ^ Run on all newly created widgets.
   -> Changes (DialogSelector v)
      -- ^ Events trigger a new dialog to be displayed. Also sampled when the input key changes.
   -> Maybe (k, Changes v)   -- ^ Initial widget display. @Nothing@ gives a blank.
   -> Event (k, Changes v)   -- ^ Key-value pairs for widget updates.
   -> MomentIO (Gtk.Widget, Event (k, v))
mkDialogWidget newWidgetIO selectorC initialM inputE = do
      scrolled <- Gtk.scrolledWindowNew noAdjustment noAdjustment
      liftIO $ Gtk.toWidget scrolled >>= newWidgetIO
      frame <- Gtk.frameNew Nothing
      liftIO $ Gtk.toWidget frame >>= newWidgetIO
      Gtk.set frame [#margin := 3]
      inputB <- stepper initialM $ Just <$> inputE
      let
         newKeyE = Just <$> filterApply (keyChanged <$> inputB) inputE
      -- Event stream from edits to the initial value, if any.
      (initialEvent, delayedIO) <- case initialM of
         Nothing -> return (never, return ())
         Just initial -> do
            sel <- valueB $ changesB selectorC
            createWidget frame sel $ Just initial
      liftIO delayedIO
      creation <- execute $ changesE $ createWidget frame <$> selectorC <*> Changes newKeyE inputB
      stop <- reactimate1 $ snd <$> creation
      void $ Gtk.onWidgetDestroy frame stop
      updateEE <- accumE never $ unionWith const . fst <$> creation
      updateE <- switchE updateEE
      Gtk.containerAdd scrolled frame
      Gtk.scrolledWindowSetPropagateNaturalHeight scrolled True
      Gtk.scrolledWindowSetPropagateNaturalWidth scrolled True
      Gtk.widgetShowAll scrolled
      w <- Gtk.toWidget scrolled
      return (w, unionWith const initialEvent updateE)
   where
      keyChanged Nothing _ = True
      keyChanged (Just (k1, _)) (k2, _) = k1 /= k2
      -- createWidget :: Gtk.Frame -> (k, Behavior v) -> MomentIO (Event (k, v), IO ())
      createWidget parent _ Nothing = return (never, clearWidget parent)
      createWidget parent selector (Just (key, valC)) = do
         initial <- valueB $ changesB valC
         case selector initial of
            Nothing ->
               return (never, clearWidget parent)
            Just (DialogWrapper trav dialog1) ->
               case initial ^? trav of
                  Nothing ->
                     return (never, clearWidget parent)
                           -- Should not happen. "trav" does not match wrapper.
                  Just start2 -> do
                     let currentC = fromMaybe start2 . preview trav <$> valC
                     Dialog title dialog2 <- dialog1
                     (widget, eventF, _) <-
                        renderSubDialog newWidgetIO start2 currentC dialog2
                     -- Widget is insensitive if trav <$> valB is Nothing.
                     let inputValid = isJust . preview trav <$> changesB valC
                     behaviorLink widget Gtk.widgetSetSensitive inputValid
                     let
                        result = flip ($) <$> changesB valC <@> (over trav <$> eventF)
                           -- Apply the edit to the current value.
                        delayedIO = do
                           -- Callbacks triggered during execute cause hanging.
                           clearWidget parent
                           Gtk.frameSetLabel parent $ Just title
                           Gtk.set widget [#margin := 3]
                           Gtk.containerAdd parent widget
                           Gtk.widgetShowAll parent
                     return ((key,) <$> result, delayedIO)
      -- Clear the frame contents.
      clearWidget parent = do
         Gtk.binGetChild parent >>= \case
            -- Just oldWidget -> Gtk.containerRemove parent oldWidget
            Just oldwidget -> Gtk.widgetDestroy oldwidget
            Nothing -> return ()
         Gtk.frameSetLabel parent Nothing


-- | Create the dialog as specified.
createDialog :: (Gtk.IsWidget parent) =>
   parent -> (Bool, k, s, MomentIO (Dialog s)) -> MomentIO (Event (k, Maybe (s->s)))
createDialog parent (applyFlag, key, initial, mkDialog) = mdo
      d <- mkDialog
      -- Create the window.
      target <- Gtk.new Gtk.Dialog [#modal := True, #title := dialogTitle d]
      style <- Gtk.widgetGetStyleContext target
      Gtk.styleContextAddClass style "hades-dialog"
      modalFor <- widgetWindow parent
      forM_ modalFor $ \win -> Gtk.set target [#transientFor := win]
      Gtk.dialogSetDefaultResponse target $ r Gtk.ResponseTypeOk
      box <- Gtk.dialogGetContentArea target
      -- Get the dialog widget tree and change event. Put widget tree into window.
      (widget, setter, enableOk) <- renderSubDialog mempty initial value $ dialogMain d
      Gtk.set widget [#margin := 5]
      Gtk.widgetSetMarginEnd widget 10  -- Space for scroll bars
      scroll <- Gtk.new Gtk.ScrolledWindow [
            #propagateNaturalHeight := True,
            #propagateNaturalWidth := True
         ]
      Gtk.containerAdd scroll widget
      Gtk.boxPackStart box scroll True True 5
       -- Event triggered when OK is pressed.
      (resultE, resultH) <- newEvent
      void $ Gtk.onDialogResponse target $ handleResponse target resultH
      -- Plumb the new values coming up via 'setter' to the 'value' sent down the tree.
      accumulator <- accumE id $ (.) <$> setter
      accumulatorC <- makeChanges id accumulator
      let
         value = accumulatorC <&> ($ initial)
      void $ Gtk.dialogAddButton target "_Cancel" $ r Gtk.ResponseTypeCancel
      okButton <- Gtk.dialogAddButton target "_Ok" $ r Gtk.ResponseTypeOk
      initialValid <- valueB enableOk
      behaviorLink okButton Gtk.setWidgetSensitive enableOk
      Gtk.setWidgetSensitive okButton initialValid
      when applyFlag $ do
         applyButton <- Gtk.dialogAddButton target "_Apply" $ r Gtk.ResponseTypeApply
         behaviorLink applyButton Gtk.setWidgetSensitive enableOk
         Gtk.setWidgetSensitive applyButton initialValid
      Gtk.widgetShowAll target
      return $ (\a r1 -> (key, if r1 then Just a else Nothing))
            <$> changesB accumulatorC
            <@> resultE
   where
      r x = fromIntegral $ fromEnum x  -- Convert response type to Word32
      handleResponse target handler v =
         handleResponse1 target handler $ toEnum $ fromIntegral v
      handleResponse1 target handler Gtk.ResponseTypeOk = do
         handler True
         Gtk.widgetDestroy target
      handleResponse1 _ handler Gtk.ResponseTypeApply =
         handler True
      handleResponse1 target handler Gtk.ResponseTypeCancel = do
         handler False
         Gtk.widgetDestroy target
      handleResponse1 target handler Gtk.ResponseTypeNone = do
         handler False
         Gtk.widgetDestroy target
      handleResponse1 target handler Gtk.ResponseTypeDeleteEvent = do
         handler False
         Gtk.widgetDestroy target
      handleResponse1 target handler _ =
         cannotHappen "createDialog: unknown button clicked." $ do
         handler False
         Gtk.widgetDestroy target


-- | Render a Dialog as the appropriate kind of GTK Widget. Also returns a Behavior with
-- the setter for all the fields modified by the user.
renderSubDialog ::
   (Gtk.Widget -> IO ())   -- ^ Applied to every widget when it is created.
   -> s                    -- ^ Initial value to display.
   -> Changes s            -- ^ Updates to display value.
   -> SubDialog s          -- ^ The dialog to display.
   -> MomentIO (Gtk.Widget, Event (s -> s), Behavior Bool)
renderSubDialog newWidgetIO initial value sub =
   case sub of
      Exec extract subDialog ->   renderExec newWidgetIO initial value extract subDialog
      ExecEvent initialSub ev ->  renderExecEvent newWidgetIO initial value initialSub ev
      VBox dss ->                 renderVBox newWidgetIO initial value dss
      HBox dss ->                 renderHBox newWidgetIO initial value dss
      Grid colH rowH cells ->     renderGrid newWidgetIO initial value colH rowH cells
      Frame enable frameLens d -> renderFrame newWidgetIO enable frameLens d initial value
      Elements es ->              renderElements newWidgetIO initial value es
      BigElement e ->             renderBigElement newWidgetIO initial value e
      ButtonBar buttons ->        renderButtonBar newWidgetIO buttons
      UnionTab unionLens tabs ->  renderUnionTab newWidgetIO initial value unionLens tabs
      Message msg ->              renderMessage newWidgetIO initial value msg
      ValidityCheck p sub1 ->      renderValidityCheck newWidgetIO initial value p sub1
      ValidityMessage p sub1 ->    renderValidityMessage newWidgetIO initial value p sub1


{- Design Note:

The "newWidgetIO" parameters are a bit of a kluge. The DSM application needs a way to set a
focus callback on all widgets within a dialog. For static dialogs this is not a problem (the
application could just walk the tree), but with dynamic dialogs newly created widgets needed
the same callback.

This implementation is rather fragile: this module has to take care to ensure that the newWidgetIO
action is called for all widgets as they are created. If some are missed then the result could be
inconsistent behaviour depending on exactly where the user clicks. There is also an issue with
internal widgets created by certain containers.
-}


-- | Render the dialog corresponding to each new value of @a@.
renderExec :: (Eq a) =>
   (Gtk.Widget -> IO ())
   -> s
   -> Changes s
   -> (s -> a)
   -> (a -> SubDialog s)
   -> MomentIO (Gtk.Widget, Event (s -> s), Behavior Bool)
renderExec newWidgetIO initial value extract subDialog = do
      parent <- Gtk.frameNew Nothing  -- Merely exists as a holder for widgets created by events.
      liftIO $ Gtk.toWidget parent >>= newWidgetIO
      Gtk.setFrameShadowType parent Gtk.ShadowTypeNone
      Gtk.setContainerBorderWidth parent 0
      let
         updates = changesE $ filterChangesWith ((/=) `on` snd) $ (id &&& extract) <$> value
         subs = second subDialog <$> updates
      (initialF, initialIO, initialValidity) <- doExec parent (initial, subDialog $ extract initial)
      liftIO initialIO  -- Put the initial sub-dialog in the empty frame.
      newWidgets <- execute $ doExec parent <$> subs
      results <- switchE $ view _1 <$> newWidgets
      stop <- reactimate1 $ view _2 <$> newWidgets  -- Widget IO in execute causes lock-ups.
      validity <- switchB initialValidity $ view _3 <$> newWidgets
      void $ Gtk.onWidgetDestroy parent stop
      Gtk.widgetShowAll parent
      w <- Gtk.toWidget parent
      return (w, unions [initialF, results], validity)
   where
      doExec parent (newValue, sub) = do
         (newWidget, newF, validity) <- renderSubDialog newWidgetIO newValue value sub
         let
            replace = do  -- IO monad
               Gtk.binGetChild parent >>= \case
                  Nothing -> return ()
                  Just w -> Gtk.widgetDestroy w
               Gtk.containerAdd parent newWidget
               t <- widgetTree newWidget
               mapM_ newWidgetIO t
               Gtk.widgetShowAll parent
         return (newF, replace, validity)


-- | Render the dialog for each new event.
renderExecEvent ::
   (Gtk.Widget -> IO ())
   -> s
   -> Changes s
   -> SubDialog s
   -> Event (s, SubDialog s)
   -> MomentIO (Gtk.Widget, Event (s -> s), Behavior Bool)
renderExecEvent newWidgetIO initial value initialSub subs = do
      parent <- Gtk.frameNew Nothing  -- Merely exists as a holder for widgets created by events.
      liftIO $ Gtk.toWidget parent >>= newWidgetIO
      Gtk.setFrameShadowType parent Gtk.ShadowTypeNone
      Gtk.setContainerBorderWidth parent 0
      (initialF, initialIO, initialValidity) <- doExec parent (initial, initialSub)
      liftIO initialIO  -- Put the initial sub-dialog in the empty frame.
      newWidgets <- execute $ doExec parent <$> subs
      results <- switchE $ view _1 <$> newWidgets
      stop <- reactimate1 $ view _2 <$> newWidgets  -- Widget IO in execute causes lock-ups.
      validity <- switchB initialValidity $ view _3 <$> newWidgets
      void $ Gtk.onWidgetDestroy parent stop
      Gtk.widgetShowAll parent
      w <- Gtk.toWidget parent
      return (w, unions [initialF, results], validity)
   where
      doExec parent (newValue, sub) = do
         (newWidget, newF, validity) <- renderSubDialog newWidgetIO newValue value sub
         let
            replace = do  -- IO monad
               Gtk.binGetChild parent >>= \case
                  Nothing -> return ()
                  Just w -> Gtk.widgetDestroy w
               Gtk.containerAdd parent newWidget
               t <- widgetTree newWidget
               mapM_ newWidgetIO t
               Gtk.widgetShowAll parent
         return (newF, replace, validity)

{- Design Note:

The "Exec" and "ExecEvent" dialog primitives are ad-hoc responses to application requirements.
A better solution is needed. This will probably involve an extra monadic layer in between
"Dialog" and "SubDialog" allowing one part of the dialog to feed values into another.
 -}


-- | Create the widgets described by the Dialog lists and add them to the box.
renderSubDialogSection :: (Gtk.IsContainer c) =>
   (Gtk.Widget -> IO ())
   -> c                -- ^ Container to hold the new widgets.
   -> IO Gtk.Widget -- ^ Separator widget to go between dialog sublists.
   -> s             -- ^ Initial value to be displayed.
   -> Changes s    -- ^ Dialog value.
   -> [[SubDialog s]] -- ^ Dialog specification.
   -> MomentIO (Event (s -> s), Behavior Bool)
renderSubDialogSection newWidgetIO box sep initial value dialogss = do
      (actions, setters, validities) <- unzip3              -- :: ([IO ()], [Event (v -> v)])
            . map (_1 %~ Gtk.containerAdd box)              -- :: IO [(IO (), Event (v -> v))]
            <$> sequence                                    -- :: IO [(w, Event (v -> v))]
                 (intercalate [(, never, pure True) <$> liftIO sep] -- :: [IO (w, Event (v -> v))]
                  $ map (map (renderSubDialog newWidgetIO initial value)) dialogss)
                                                            -- :: [[IO (w, Event (s -> s))]]
      sequence_ actions
      return (unions setters, foldr (liftA2 (&&)) (pure True) validities)
      -- If any setter is Nothing then the result must be Nothing. Hence we use sequence rather
      -- than the Monoid instance for Maybe.


-- | Create a GTK VBox of sub-widgets.
renderVBox ::
   (Gtk.Widget -> IO ())
   -> s
   -> Changes s
   -> [[SubDialog s]]
   -> MomentIO (Gtk.Widget, Event (s -> s), Behavior Bool)
renderVBox newWidgetIO initial value dss = do
   box <- Gtk.new Gtk.Grid [#orientation := Gtk.OrientationVertical, #rowSpacing := 5 ]
   (setter, validity) <- renderSubDialogSection
         newWidgetIO
         box
         (Gtk.separatorNew Gtk.OrientationHorizontal >>= Gtk.toWidget)
         initial
         value
         dss
   w <- Gtk.toWidget box
   liftIO $ newWidgetIO w
   return (w, setter, validity)


-- | Create a GTK HBox of sub-widgets.
renderHBox ::
   (Gtk.Widget -> IO ())
   -> s
   -> Changes s
   -> [[SubDialog s]]
   -> MomentIO (Gtk.Widget, Event (s -> s), Behavior Bool)
renderHBox newWidgetIO initial value dss = do
   box <- Gtk.new Gtk.Grid [#orientation := Gtk.OrientationHorizontal, #columnSpacing := 5 ]
   (setter, validity) <- renderSubDialogSection
         newWidgetIO
         box
         (Gtk.separatorNew Gtk.OrientationVertical >>= Gtk.toWidget)
         initial
         value
         dss
   w <- Gtk.toWidget box
   liftIO $ newWidgetIO w
   return (w, setter, validity)


-- | Create a GTK Grid of elements.
renderGrid ::
   (Gtk.Widget -> IO ())
   -> s
   -> Changes s
   -> [Text]
   -> [Text]
   -> [[DialogElement s]]
   -> MomentIO (Gtk.Widget, Event (s -> s), Behavior Bool)
renderGrid newWidgetIO initial value colH rowH rows = do
   grid <- Gtk.new Gtk.Grid [#orientation := Gtk.OrientationHorizontal, #columnSpacing := 1]
   forM_ (zip [1..] colH) $ \(n, txt) -> do
      lbl <- Gtk.labelNew $ Just txt
      Gtk.gridAttach grid lbl n 0 1 1
   forM_ (zip [1..] rowH) $ \(n, txt) -> do
      lbl <- Gtk.labelNew $ Just txt
      Gtk.gridAttach grid lbl 0 n 1 1
   evs <- forM (zip [1..] rows) $ \(rowNum, row) ->
      forM (zip [1..] row) $ \(colNum, cell) -> do
         (w, e) <- renderElement initial value cell
         Gtk.gridAttach grid w colNum rowNum 1 1
         return e
   w <- Gtk.toWidget grid
   liftIO $ newWidgetIO w
   return (w, unions $ concat evs, pure True)


-- | Create a named frame containing a sub-widget.
renderFrame ::
   (Gtk.Widget -> IO ()) -> (s -> Bool) -> Lens' s a -> Dialog a -> s -> Changes s
   -> MomentIO (Gtk.Widget, Event (s -> s), Behavior Bool)
renderFrame newWidgetIO enable frameLens contents initial value = do
      box <- Gtk.frameNew $ Just $ dialogTitle contents
      let
         enabledNow = enable initial
         frameInit = initial ^. frameLens
         frameValue = value <&> (^. frameLens)
      (widget, setter, validity) <-
         renderSubDialog newWidgetIO frameInit frameValue $ dialogMain contents
      Gtk.set widget [#margin := 5]
      liftIO $ Gtk.containerAdd box widget
      void $ Gtk.onWidgetShow box $ Gtk.setWidgetSensitive box enabledNow
      behaviorLink box Gtk.setWidgetSensitive $ enable <$> changesB value
      w <- Gtk.toWidget box
      liftIO $ newWidgetIO w
      return (w, setter <&> over frameLens, validity)


-- | Return a table of element labels and widgets, and the accumulated setter function for the
-- current values.
renderElements ::
   (Gtk.Widget -> IO ()) -> s -> Changes s -> [DialogElement s]
   -> MomentIO (Gtk.Widget, Event (s -> s), Behavior Bool)
renderElements newWidgetIO initial value items = do
   table <- Gtk.new Gtk.Grid [#rowSpacing := 3, #columnSpacing := 3]
   let addWidgets row (w1, w2, b) = do
         Gtk.gridAttach table w1 0 row 1 1
         Gtk.gridAttach table w2 1 row 1 1
         return b
   widgetPairs <- forM items $ renderLabelledElement initial value
   setter <- unions <$> zipWithM addWidgets [0..] widgetPairs
   w <- Gtk.toWidget table
   t <- widgetTree w
   liftIO $ mapM_ newWidgetIO t
   return (w, setter, pure True)


-- | Returns label and field widgets corresponding to the dialog element.
renderLabelledElement ::
   s              -- ^ Initial value to be displayed.
   -> Changes s  -- ^ Updates to the value of the element.
   -> DialogElement s   -- ^ GUI specification for the element.
   -> MomentIO (Gtk.Widget, Gtk.Widget, Event (s -> s))
renderLabelledElement initial value item = do
   label <- Gtk.labelNew $ Just $ elementLabel1 item
   Gtk.set label [#halign := Gtk.AlignEnd, #valign := Gtk.AlignCenter]
   (field, setter) <- renderElement initial value item
   behaviorLink label Gtk.setWidgetSensitive $ elementEnable1 item <$> changesB value
   w <- Gtk.toWidget label
   return (w, field, setter)


-- | Creates a frame labelled with the element label and places the element within it.
renderBigElement ::
   (Gtk.Widget -> IO ()) -> s -> Changes s -> DialogElement s
   -> MomentIO (Gtk.Widget, Event (s -> s), Behavior Bool)
renderBigElement newWidgetIO initial value item = do
   box <- Gtk.frameNew $ Just $ elementLabel1 item
   (widget, setter) <- renderElement initial value item
   liftIO $ Gtk.containerAdd box widget
   let enabledNow = elementEnable1 item initial
   void $ Gtk.onWidgetShow box $ Gtk.setWidgetSensitive box enabledNow
   behaviorLink box Gtk.setWidgetSensitive $ elementEnable1 item <$> changesB value
   w <- Gtk.toWidget box
   t <- widgetTree w
   liftIO $ mapM_ newWidgetIO t
   return (w, setter, pure True)


-- | Create a toolbar of buttons, each of which is associated with an edit event.
renderButtonBar ::
   (Gtk.Widget -> IO ()) -> [(Text, s -> s)] -> MomentIO (Gtk.Widget, Event (s -> s), Behavior Bool)
renderButtonBar newWidgetIO buttons = do
   box <- Gtk.buttonBoxNew Gtk.OrientationHorizontal
   Gtk.buttonBoxSetLayout box Gtk.ButtonBoxStyleCenter
   events <- forM buttons $ \(nm, func) -> do
      b <- Gtk.new Gtk.Button [#label := nm]
      Gtk.boxPackStart box b False False 0
      registerIOSignal b Gtk.onButtonClicked $ return ((), func)
   w <- Gtk.toWidget box
   t <- widgetTree w
   liftIO $ mapM_ newWidgetIO t
   return (w, unions events, pure True)


-- | Create a tab box where each tab is associated with a prism into the data being edited.
renderUnionTab ::
   (Gtk.Widget -> IO ())
   -> s
   -> Changes s
   -> Lens' s a
   -> [UnionTabData a]
   -> MomentIO (Gtk.Widget, Event (s -> s), Behavior Bool)
renderUnionTab newWidgetIO initial value unionLens tabs = do
      book <- Gtk.new Gtk.Notebook [
            #margin := 5,
            #scrollable := True,
            #enablePopup := True
         ]
      setters <- forM tabs (renderTab book)
      let initN = fromIntegral $ length $ takeWhile (not . tabMatches initial) tabs
      Gtk.widgetShowAll book
      Gtk.notebookSetCurrentPage book initN
      tabClicks <- registerIOSignal2 book Gtk.onNotebookSwitchPage $ \_ n ->
            return ((), fromIntegral n)
      let
         tabResults = observeE $ clickValue setters <$> tabClicks
         tabSetter = fst <$> tabResults
      initialValidity <- snd <$> clickValue setters (fromIntegral initN)
      tabValidity <- switchB initialValidity $ snd <$> tabResults
      w <- Gtk.toWidget book
      liftIO $ newWidgetIO w
      return (w, unions $ tabSetter : map (changesE . fst) setters, tabValidity)
   where
      tabMatches x (UnionTabData _ prsm _ _) = isJust $ x ^? unionLens . prsm
      clickValue :: (MonadMoment m) =>
         [(Changes (s -> s), Behavior Bool)] -> Int -> m (s -> s, Behavior Bool)
      clickValue setters n =
         case drop n setters of  -- Like (!!) but safe.
            (newSetter, newValidity) : _ -> valueB (changesB newSetter) <&> (, newValidity)
            [] -> cannotHappen "renderUnionTab: missing setter" $ return (id, pure True)
      -- renderTab :: Gtk.Notebook -> UnionTabData a -> MomentIO (Changes (s -> s))
      renderTab book (UnionTabData title prsm defaultValue subdialog) = mdo
         let initialSub = fromMaybe defaultValue $ initial ^? unionLens . prsm
         (widget, setEvent, validity) <- renderSubDialog newWidgetIO initialSub current subdialog
         label <- Gtk.labelNew $ Just title
         tabNum <- Gtk.notebookAppendPage book widget (Just label)
         Gtk.notebookSetCurrentPage book $ fromIntegral tabNum
         setAccum <- accumE id $ (.) <$> setEvent
         setC <- makeChanges id setAccum
         let
            current = fromMaybe <$>
               (setC <&> ($ initialSub)) <*>
               (preview (unionLens . prsm) <$> value)
               -- Value behavior from outside overrides current settings iff it is for this prism.
            initialValue = fromMaybe defaultValue $ initial ^? unionLens . prsm
         return (setC <&> (\f -> unionLens .~ (f initialValue ^. re prsm)), validity)


renderMessage ::
   (Gtk.Widget -> IO ()) -> s -> Changes s -> (s -> Text)
   -> MomentIO (Gtk.Widget, Event (s -> s), Behavior Bool)
renderMessage newWidgetIO initial value msgFunc = do
   label <- Gtk.labelNew $ Just $ msgFunc initial
   stop <- reactimate1 $ Gtk.labelSetText label . msgFunc <$> changesE value
   void $ Gtk.onWidgetDestroy label stop
   w <- Gtk.toWidget label
   liftIO $ newWidgetIO w
   return (w, never, pure True)


-- | If the value meets the predicate then pass the result upwards. Otherwise flag an error.
renderValidityCheck ::
   (Gtk.Widget -> IO ())
   -> s
   -> Changes s
   -> (s -> Bool)
   -> SubDialog s
   -> MomentIO (Gtk.Widget, Event (s -> s), Behavior Bool)
renderValidityCheck newWidgetIO initial value predicate contents = do
      (inner, setter, subValidity) <- renderSubDialog newWidgetIO initial value contents
      (widget, validity) <- withError inner
      return (widget, setter, (&&) <$> validity <*> subValidity)
   where
      -- withError :: Gtk.Widget -> Event s -> MomentIO (Gtk.Widget, Behavior Bool)
      withError widget = do
         style <- Gtk.widgetGetStyleContext widget
         let
            initialValidity = predicate initial
            updateValidity = predicate <$> value
         showValid style initialValidity
         stop <- reactimate1 (showValid style <$> changesE updateValidity)
         void $ Gtk.onWidgetDestroy widget stop
         return (widget, changesB updateValidity)
      showValid style True = Gtk.styleContextRemoveClass style "entry-error"
      showValid style False = Gtk.styleContextAddClass style "entry-error"


renderValidityMessage ::
   (Gtk.Widget -> IO ())
   -> s
   -> Changes s
   -> (s -> [Text])
   -> SubDialog s
   -> MomentIO (Gtk.Widget, Event (s -> s), Behavior Bool)
renderValidityMessage newWidgetIO initial value validF contents = do
      (inner, setter, subValidity) <- renderSubDialog newWidgetIO initial value contents
      (widget, validity) <- withError inner
      return (widget, setter, (&&) <$> validity <*> subValidity)
   where
      -- withError :: Gtk.Widget -> Event s -> MomentIO Gtk.Widget
      withError widget = do
         box <- Gtk.boxNew Gtk.OrientationVertical 3
         style <- Gtk.widgetGetStyleContext box
         let
            initialErr = listToMaybe $ validF initial
            updateErr = listToMaybe . validF <$> value
         msg <- Gtk.labelNew initialErr
         liftIO $ Gtk.toWidget msg >>= newWidgetIO
         Gtk.boxPackStart box widget True True 0
         Gtk.boxPackStart box msg False False 0
         showValid style $ isNothing initialErr
         Gtk.widgetSetVisible msg $ isJust initialErr
         stop <- reactimate1 $ changesE updateErr <&> \case
            Just txt -> do
               showValid style False
               Gtk.labelSetLabel msg txt
               Gtk.widgetShow msg
            Nothing -> do
               showValid style True
               Gtk.labelSetLabel msg ""
               Gtk.widgetHide msg
         void $ Gtk.onWidgetDestroy msg stop
         w <- Gtk.toWidget box
         liftIO $ newWidgetIO w
         return (w, isNothing <$> changesB updateErr)
      showValid style True = Gtk.styleContextRemoveClass style "entry-error"
      showValid style False = Gtk.styleContextAddClass style "entry-error"


-- | Creates the unadorned widget for a single element. A "LensElement" also
-- returns a behavior with the value expressed as a setter function. For a "SimpleElement" this
-- behavior is @pure id@.
renderElement ::
   s -> Changes s -> DialogElement s -> MomentIO (Gtk.Widget, Event (s -> s))
renderElement initial valC (LensElement eEnable _ eSpec eLens eChanged) = do
   (widget, value) <- renderSpecifier
         eSpec
         (initial ^. eLens)
         (valC <&> (^. eLens))
         eChanged
   let enabledNow = eEnable initial
   void $ Gtk.onWidgetShow widget $ Gtk.setWidgetSensitive widget enabledNow
   behaviorLink widget Gtk.setWidgetSensitive $ eEnable <$> changesB valC
   return (widget, over eLens <$> value)
renderElement initial valC (SimpleElement eEnable _ eSpec eInit eChanged) = do
   (valueE, handler) <- newEvent
   valueC <- makeChanges eInit valueE
   (widget, _) <- renderSpecifier
         eSpec
         eInit
         valueC
         (\v -> eChanged v >> handler v)
   let enabledNow = eEnable initial
   void $ Gtk.onWidgetShow widget $ Gtk.setWidgetSensitive widget enabledNow
   behaviorLink widget Gtk.setWidgetSensitive $ eEnable <$> changesB valC
   return (widget, never)
renderElement initial valC (IconDecorated iconF inner) = do
   (innerWidget, result) <- renderElement initial valC inner
   box <- Gtk.boxNew Gtk.OrientationHorizontal 0
   icon <- Gtk.imageNewFromIconName (Just $ iconF initial) $
         fromIntegral $ fromEnum Gtk.IconSizeButton
   Gtk.boxPackStart box icon False False 0
   Gtk.boxPackStart box innerWidget True True 0
   eventLink icon Gtk.setImageIconName $ iconF <$> changesE valC
   w <- Gtk.toWidget box
   return (w, result)
renderElement initial valC (ColourDecorated colourF inner) = do
   (innerWidget, result) <- renderElement initial valC inner
   widgetLinkColour innerWidget (colourF initial) never $ colourF <$> changesB valC
   return (innerWidget, result)
renderElement initial valC (LinkDecorated linkF inner) = do
   (innerWidget, result) <- renderElement initial valC inner
   box <- Gtk.boxNew Gtk.OrientationHorizontal 0
   let target = fromMaybe "" $ linkF initial
   button <- Gtk.linkButtonNewWithLabel target (Just "")
   img <- Gtk.imageNewFromIconName (Just "emblem-symbolic-link") $
      fromIntegral $ fromEnum Gtk.IconSizeButton
   Gtk.buttonSetImage button $ Just img
   Gtk.buttonSetAlwaysShowImage button True
   Gtk.widgetSetTooltipText button $ Just target
   Gtk.boxPackStart box button False False 0
   eventLink button Gtk.linkButtonSetUri $ fromMaybe "" . linkF <$> changesE valC
   Gtk.boxPackStart box innerWidget True True 0
   w <- Gtk.toWidget box
   return (w, result)


-- | Returns the appropriate GTK widget and a @Behavior@ of its values.
renderSpecifier ::
   DialogSpecifier a
   -> a
   -> Changes a  -- ^ Displayed value
   -> Handler a  -- ^ Handler for value changes.
   -> MomentIO (Gtk.Widget, Event (a -> a))
renderSpecifier (TextBoxSpec prsm) initial value userHandler = do
      let width = min 20 $ max 5 $ T.length $ initial ^. re prsm
      entry <- Gtk.new Gtk.Entry [#hexpand := True, #widthChars := fromIntegral width]
      styleCtx <- Gtk.widgetGetStyleContext entry
      (w, b) <- makeField
         entry
         entryChangedSignal
         updateEntry
         (initial ^. re prsm)
         (value <&> (^. re prsm))
         userHandler $
         do
            t <- Gtk.entryGetText entry
            case t ^? prsm of
               Nothing -> do
                  Gtk.styleContextAddClass styleCtx "entry-error"
                  return Nothing
               Just v -> do
                  Gtk.styleContextRemoveClass styleCtx "entry-error"
                  return $ Just v
      return (w, b)
   where
      -- Update only if text has changed. Do not overwrite leading or trailing spaces in the widget.
      updateEntry w newTxt = do
         oldTxt <- Gtk.entryGetText w
         when (T.strip oldTxt /= T.strip newTxt) $ Gtk.setEntryText w newTxt


renderSpecifier (MemoBoxSpec size vExpand) initial value userHandler = do
      scroll <- Gtk.scrolledWindowNew noAdjustment noAdjustment
      style <- Gtk.widgetGetStyleContext scroll
      Gtk.styleContextAddClass style $ memoBoxCss size
      Gtk.set scroll [
         #propagateNaturalHeight := True,
         #propagateNaturalWidth := True,
         #hexpand := True,
         #vexpand := vExpand ]
      Gtk.setScrolledWindowShadowType scroll Gtk.ShadowTypeIn
      vw <- Gtk.textViewNew
      Gtk.set vw [
         #hexpand := True,
         #vexpand := vExpand,
         #wrapMode := Gtk.WrapModeWord,
         #inputHints := [Gtk.InputHintsSpellcheck]]
      Gtk.textViewSetWrapMode vw Gtk.WrapModeWord
      Gtk.containerAdd scroll vw
      buffer <- Gtk.textViewGetBuffer vw
      Gtk.textBufferSetText buffer initial (-1)
      stop <- reactimate1 $ (\v -> do
               old <- fromMaybe "" <$> Gtk.getTextBufferText buffer
               when (v /= old) $ Gtk.textBufferSetText buffer v (-1)
            ) <$> changesE value
      void $ Gtk.onWidgetDestroy vw stop
      Gtk.widgetQueueResize vw
      textEdits <- registerIOSignal buffer Gtk.onTextBufferEndUserAction $ do
            newText <- fromMaybe "" <$> Gtk.getTextBufferText buffer
            userHandler newText
            return ((), newText)
      w <- Gtk.toWidget scroll
      return (w, const <$> textEdits)

renderSpecifier (FixedTextSpec displayF nested) initial value userHandler = do
   let width = max 5 $ T.length $ displayF initial
   entry <- Gtk.new Gtk.Entry [#hexpand := True, #widthChars := fromIntegral width]
   Gtk.setEntryText entry $ displayF initial
   Gtk.setEntryEditable entry False
   behaviorLink entry Gtk.setEntryText $ displayF <$> changesB value
   editedValue <- case nested of
      Nothing -> return never
      Just selector -> do
         activated <- registerIOSignal1 entry Gtk.onWidgetButtonPressEvent $ \ev -> do
            b <- Gdk.getEventButtonButton ev
            t <- Gdk.getEventButtonType ev
            if b == 1 && t == Gdk.EventType2buttonPress
               then return (True, Just ())
               else return (False, Nothing)
         mkGtkPopupSelect entry selector $
               (\v -> (True, ((), v))) <$> (changesB value <@ activated)
   let
      result = filterJust $ snd <$> editedValue
   stop <- reactimate1 $ (\v f -> userHandler $ f v) <$> changesB value <@> result
   void $ Gtk.onWidgetDestroy entry stop
   w <- Gtk.toWidget entry
   return (w, result)

renderSpecifier (FixedMemoSpec displayF nested) initial value userHandler = do
   label <- Gtk.labelNew $ Just $ displayF initial
   Gtk.labelSetLineWrap label True
   Gtk.labelSetYalign label 0.0  -- Align to top.
   Gtk.labelSetXalign label 0.0  -- Align whole label to left.
   box <- Gtk.eventBoxNew  -- Label has no GDK window, so use this instead.
   Gtk.eventBoxSetVisibleWindow box False
   Gtk.containerAdd box label
   Gtk.widgetAddEvents box [Gdk.EventMaskButtonPressMask, Gdk.EventMaskButtonReleaseMask]
   style <- Gtk.widgetGetStyleContext label
   Gtk.styleContextAddClass style "fixed-memo"
   stop1 <- reactimate1 $ Gtk.labelSetLabel label . displayF <$> changesE value
   editedValue <- case nested of
      Nothing -> return never
      Just selector -> do
         activated <- registerIOSignal1 box Gtk.onWidgetButtonPressEvent $ \ev -> do
            b <- Gdk.getEventButtonButton ev
            t <- Gdk.getEventButtonType ev
            if b == 1 && t == Gdk.EventType2buttonPress
               then return (True, Just ())
               else return (False, Nothing)
         mkGtkPopupSelect label selector $ (\v -> (True, ((), v))) <$> (changesB value <@ activated)
   let
      result = filterJust $ snd <$> editedValue
   stop2 <- reactimate1 $ (\v f -> userHandler $ f v) <$> changesB value <@> result
   void $ Gtk.onWidgetDestroy box $ stop1 >> stop2
   w <- Gtk.toWidget box
   return (w, result)

{-
Design note: This only supports multiple selection because when single selection modes are used
GTK3 executes the selectionChanged callback before the rest of the Reactive Banana network is set
up, causing the program to hang. No workaround could be found, so the facility is not available.
Use dialog validity checks instead.
-}
renderSpecifier (TreeSelectorSpec items) initial value userHandler = do
      let items1 = trimForest items
      store <- Gtk.treeStoreNew [Gtk.gtypeString, Gtk.gtypeString]
         -- Contents are name and optional tooltip
      pairs <- addStoreForest store Nothing items1
      -- "store" contains names. "pairs" maps store paths to actual values.
      let
         idx = M.fromList pairs
         revIndex = M.fromList $ map (\(x,y) -> (y,x)) pairs
      scroll <- Gtk.scrolledWindowNew noAdjustment noAdjustment
      Gtk.set scroll [
            #propagateNaturalHeight := True,
            #propagateNaturalWidth := True
         ]
      Gtk.setScrolledWindowShadowType scroll Gtk.ShadowTypeIn
      vw <- Gtk.treeViewNewWithModel store
      Gtk.set vw [
            #hexpand := True,
            #vexpand := True,
            #heightRequest := 300  -- Not good to hard code a size, but default is too small.
         ]
      Gtk.treeViewSetHeadersVisible vw False
      column <- Gtk.treeViewColumnNew
      void $ Gtk.treeViewAppendColumn vw column
      txt <- Gtk.cellRendererTextNew
      Gtk.setCellRendererTextEditable txt False
      Gtk.treeViewColumnPackEnd column txt True
      Gtk.cellLayoutAddAttribute column txt "text" 0
      Gtk.treeViewSetEnableTreeLines vw True
      path <- Gtk.treePathNewFromIndices [0]
      void $ Gtk.treeViewExpandRow vw path False
      Gtk.treeViewSetTooltipColumn vw 1
      selection <- Gtk.treeViewGetSelection vw
      Gtk.treeSelectionSetMode selection Gtk.SelectionModeMultiple
      setSelection vw revIndex selection initial
      Gtk.treeSelectionSetSelectFunction selection $ Just $ selectionFunc idx
      selectionE <- registerIOSignal selection Gtk.onTreeSelectionChanged $ do
         (gtkPaths, _) <- Gtk.treeSelectionGetSelectedRows selection
         paths <- S.fromList . mapMaybe (`M.lookup` idx) . catMaybes <$>
               mapM Gtk.treePathGetIndices gtkPaths
         userHandler paths
         return ((), paths)
      stop <- reactimate1 $ setSelection vw revIndex selection <$> changesE value
      void $ Gtk.onWidgetDestroy vw stop
      Gtk.containerAdd scroll vw
      w <- Gtk.toWidget scroll
      return (w, const <$> selectionE)
   where
      selectionFunc :: Map [Int32] a -> Gtk.TreeSelectionFunc
      selectionFunc tbl _ _ path old = do
         p2 <- (fromMaybe []) <$> Gtk.treePathGetIndices path
         let r = (p2 `M.member` tbl) || old  -- Always allow deselection
         return r
      setSelection :: (Ord a, MonadIO m) =>
         Gtk.TreeView -> Map a [Int32] -> Gtk.TreeSelection -> Set a -> m ()
      setSelection vw tbl sel vs = do
         (oldGtkPaths, _) <- Gtk.treeSelectionGetSelectedRows sel
         oldPaths <- S.fromList . catMaybes <$> mapM Gtk.treePathGetIndices oldGtkPaths
         let newPaths = mapMaybe (`M.lookup` tbl) $ S.toList vs
         when (S.fromList newPaths /= oldPaths) $ do
            paths <- mapM Gtk.treePathNewFromIndices newPaths
            Gtk.treeSelectionUnselectAll sel
            forM_ paths $ \path -> do
               Gtk.treeViewExpandToPath vw path
               Gtk.treeSelectionSelectPath sel path
      trimForest :: Forest (Text, Maybe Text, Maybe a) -> Forest (Text, Maybe Text, Maybe a)
      trimForest = mapMaybe trimTree
      trimTree :: Tree (Text, Maybe Text, Maybe a) -> Maybe (Tree (Text, Maybe Text, Maybe a))
      trimTree (Node v sub) = case trimForest sub of
            [] -> if isJust $ v ^. _3 then Just $ Node v [] else Nothing
            trees -> Just $ Node v trees
      -- Insert the tree into the store, returning a list of pairs of values and integers.
      addStoreTree :: (MonadIO m) =>
         Gtk.TreeStore -> Maybe Gtk.TreeIter -> Tree (Text, Maybe Text, Maybe a) -> m [([Int32], a)]
      addStoreTree store iter1 (Node v1 sub) = do
         iter2 <- case iter1 of  -- iter is changed by "treeStoreInsert". Hence work with a clone.
            Just i -> Just <$> Gtk.treeIterCopy i
            Nothing -> return noTreeIter
         newRow <- Gtk.treeStoreInsert store iter2 (-1)
         (pair1, row) <- case v1 ^. _3 of
            Nothing -> do
               row <- liftIO $ sequence [Gtk.toGValue $ Just $ v1 ^. _1, Gtk.toGValue $ v1 ^. _2]
               return ([], row)
            Just v2 -> do
               row <- liftIO $ sequence [Gtk.toGValue $ Just $ v1 ^. _1, Gtk.toGValue $ v1 ^. _2]
               path <- fromMaybe [] <$>
                     (Gtk.treePathGetIndices =<< Gtk.treeModelGetPath store newRow)
               return ([(path, v2)], row)
         Gtk.treeStoreSet store newRow [0, 1] row
         pairs <- addStoreForest store (Just newRow) sub
         return $ pair1 ++ pairs
      addStoreForest :: (MonadIO m) =>
         Gtk.TreeStore
         -> Maybe Gtk.TreeIter
         -> Forest (Text, Maybe Text, Maybe a)
         -> m [([Int32], a)]
      addStoreForest store iter forest = concat <$> mapM (addStoreTree store iter) forest

renderSpecifier TickBox initial value userHandler = do
   button <- liftIO Gtk.checkButtonNew
   (w, b) <- makeField button
      Gtk.onToggleButtonToggled
      Gtk.toggleButtonSetActive
      initial
      value
      userHandler
      $ do
         b <- Gtk.toggleButtonGetActive button
         return $ Just b
   return (w, b)

renderSpecifier (DateSpec fmt) initial value userHandler = do
   let prsm = datePrism fmt
   entry <- Gtk.new Gtk.Entry [#hexpand := True]
   styleCtx <- Gtk.widgetGetStyleContext entry
   (w, b) <- makeField
      entry
      entryChangedSignal
      Gtk.setEntryText
      (initial ^. re prsm)
      (value <&> (^. re prsm))
      userHandler
      $ do
         t <- Gtk.entryGetText entry
         case t ^? datePrism fmt of
            Nothing -> do
               Gtk.styleContextAddClass styleCtx "entry-error"
               return Nothing
            Just v -> do
               Gtk.styleContextRemoveClass styleCtx "entry-error"
               return $ Just v
   (overlay, calendar) <- createDatePopover (datePrism fmt) entry
   void $ Gtk.onCalendarDaySelectedDoubleClick calendar $ do
      (year, month, day) <- Gtk.calendarGetDate calendar
      let date = fromGregorian (fromIntegral year) (fromIntegral month + 1) (fromIntegral day)
      Gtk.entrySetText entry $ date ^. re (datePrism fmt)
      Gtk.entryGrabFocusWithoutSelecting entry  -- Makes sure that the FocusOut event will happen.
      Gtk.popoverPopdown overlay
   return (w, b)

renderSpecifier (DateSpecMaybe fmt) initial value userHandler = do
   let prsm = prismToMaybe $ datePrism fmt
   entry <- Gtk.new Gtk.Entry [#hexpand := True]
   styleCtx <- Gtk.widgetGetStyleContext entry
   (w, b) <- makeField
      entry
      entryChangedSignal
      Gtk.setEntryText
      (initial ^. re prsm)
      (value <&> (^. re prsm))
      userHandler
      $ do
         t <- Gtk.entryGetText entry
         case t ^? prismToMaybe (datePrism fmt) of
            Nothing -> do
               Gtk.styleContextAddClass styleCtx "entry-error"
               return Nothing
            Just v -> do
               Gtk.styleContextRemoveClass styleCtx "entry-error"
               return $ Just v
   (overlay, calendar) <- createDatePopover (datePrism fmt) entry
   void $ Gtk.onCalendarDaySelectedDoubleClick calendar $ do
      (year, month, day) <- Gtk.calendarGetDate calendar
      let date = fromGregorian (fromIntegral year) (fromIntegral month + 1) (fromIntegral day)
      Gtk.entrySetText entry $ date ^. re (datePrism fmt)
      Gtk.entryGrabFocusWithoutSelecting entry  -- Makes sure that the FocusOut event will happen.
      Gtk.popoverPopdown overlay
   return (w, b)

renderSpecifier ColourSpec initial value userHandler = do
      entry <- Gtk.new Gtk.Entry [#hexpand := True]
      styleCtx <- Gtk.widgetGetStyleContext entry
      widgetLinkColour entry (Just initial) never $ Just <$> changesB value
      Gtk.entrySetIconFromIconName
         entry
         Gtk.EntryIconPositionSecondary
         $ Just "applications-science"
      (w, b) <- makeField
         entry
         entryChangedSignal
         setEntryValue
         initial
         value
         userHandler
         $ do
            t <- Gtk.entryGetText entry
            case t ^? colourPrism of
               Nothing -> do
                  Gtk.styleContextAddClass styleCtx "entry-error"
                  return Nothing
               Just v -> do
                  Gtk.styleContextRemoveClass styleCtx "entry-error"
                  return $ Just v
      void $ Gtk.onEntryIconPress entry $ \_ _ -> do
         chooser <- createColorDialog entry
         (toEnum . fromIntegral) <$> Gtk.dialogRun chooser >>= \case
            Gtk.ResponseTypeOk -> do
               colour <- rgbaToColour =<< Gtk.colorChooserGetRgba chooser
               setEntryValue entry colour
               Gtk.widgetGrabFocus entry   -- So that FocusOutEvent will be triggered.
            _ -> return ()
         Gtk.widgetDestroy chooser
      return (w, b)
   where
      setEntryValue w c = Gtk.setEntryText w $ c ^. re colourPrism

renderSpecifier ColourSpecMaybe initial value userHandler = do
      entry <- Gtk.new Gtk.Entry [#hexpand := True]
      styleCtx <- Gtk.widgetGetStyleContext entry
      widgetLinkColour entry initial never $ changesB value
      Gtk.entrySetIconFromIconName
         entry
         Gtk.EntryIconPositionSecondary
         $ Just "applications-science"
      (w, b) <- makeField
         entry
         entryChangedSignal
         setEntryValue
         initial
         value
         userHandler
         $ do
            t <- Gtk.entryGetText entry
            case t ^? prismToMaybe colourPrism of
               Nothing -> do
                  Gtk.styleContextAddClass styleCtx "entry-error"
                  return Nothing
               Just v -> do
                  Gtk.styleContextRemoveClass styleCtx "entry-error"
                  return $ Just v
      void $ Gtk.onEntryIconPress entry $ \_ _ -> do
         chooser <- createColorDialog entry
         (toEnum . fromIntegral) <$> Gtk.dialogRun chooser >>= \case
            Gtk.ResponseTypeOk -> do
               colour <- rgbaToColour =<< Gtk.colorChooserGetRgba chooser
               Gtk.setEntryText entry $ colour ^. re colourPrism
               Gtk.widgetGrabFocus entry
            _ -> return ()
         Gtk.widgetDestroy chooser
      return (w, b)
   where
      setEntryValue w Nothing = Gtk.setEntryText w ""
      setEntryValue w (Just c) = Gtk.setEntryText w $ c ^. re colourPrism

renderSpecifier (TableSpec flags columns nested) initial value userHandler = do
   iconTheme <- getDataIconTheme
   (vw, store, activated, _) <- mkTableView
         iconTheme
         flags
         (isJust nested)
         columns
         initial
         (changesE value)
         (changesB value)
   changed <- registerIOSignal2 store Gtk.afterTreeModelRowChanged $
      \ _ _ -> ((), ) <$> MV.seqStoreToList store
   deleted <- registerIOSignal1 store Gtk.afterTreeModelRowDeleted $
      \ _ -> ((), ) <$> MV.seqStoreToList store
   inserted <- registerIOSignal2 store Gtk.afterTreeModelRowInserted $
      \ _ _ -> ((), ) <$> MV.seqStoreToList store
   case nested of
      Nothing -> return ()
      Just selector -> do
         editedRow <- mkGtkPopupSelect vw selector $ (True,) <$> activated
         stop <- reactimate1 $ editedRow <&> (\(n, mf) ->
               forM_ mf $ \f -> do
                  row <- MV.seqStoreGetValue store n
                  MV.seqStoreSetValue store n $ f row
            )
         void $ Gtk.onWidgetDestroy vw stop
   let
      editedTable = foldr1 (unionWith const) [changed, deleted, inserted]
   stop <- reactimate1 $ userHandler <$> editedTable
   void $ Gtk.onWidgetDestroy vw stop
   widget <- Gtk.toWidget vw
   return (widget, const <$> editedTable)

renderSpecifier (ForestTableSpec groups) initial value userHandler = do
      let
         initial1 = map (view V.treeIso) initial
         value1 = view V.forestIso <$> value
      grid <- Gtk.gridNew
      ev <- flip evalStateT 0 $ do
         headerGen grid groups
         forM (zip [0..] initial1) $ \(n, g) ->
            fmap (over (ix n)) <$> treeGen grid 0 groups g (preview (ix n) <$> value1)
      Gtk.widgetShowAll grid
      w <- Gtk.toWidget grid
      let editE = (\f -> view $ V.forestIso . to f . from V.forestIso) <$> unions ev
      resultE <- accumE id $ (.) <$> editE
      stop <- reactimate1 $ (\v f -> userHandler $ f v) <$> changesB value <@> resultE
      void $ Gtk.onWidgetDestroy grid stop
      return (w, resultE)
   where
      headerGen :: Gtk.Grid -> [(Text, [DialogElement a])] -> StateT Int32 MomentIO ()
      headerGen _ [] = return ()
      headerGen grid [(_, group1)] = do
         forM_ (zip [0..] group1) $ \(col, spec) -> addHead grid 1 1 0 col $ elementLabel1 spec
         modify (+1)
      headerGen grid (group1 : rest) = do
         n <- fromIntegral . length <$>
               zipWithM (addHead grid 2 1 0) [0..] (map elementLabel1 $ snd group1)
         let ns = scanl (+) n $ map (fromIntegral . length) rest
         forM_ (zip ns rest) $ \(col1, (txt, group2)) -> do
            addHead grid 1 (fromIntegral $ length group2) 0 col1 txt
            zipWithM_ (addHead grid 1 1 1) [col1..] $ map elementLabel1 group2
         modify (+2)
      addHead grid rowSpan colSpan row col txt = do
         label <- Gtk.labelNew $ Just txt
         style <- Gtk.widgetGetStyleContext label
         Gtk.styleContextAddClass style "table-header"
         Gtk.gridAttach grid label col row colSpan rowSpan
      treeGen ::
         Gtk.Grid
         -> Int32
         -> [(Text, [DialogElement a])]
         -> VTree a
         -> Changes (Maybe (VTree a))
         -> StateT Int32 MomentIO (Event (VTree a -> VTree a))
      treeGen _ _ [] _ _ = return never
      treeGen grid col ((_, group1) : rest) initialTree changesTree = do
         let w = fromIntegral $ length group1
         startRow <- get
         branchEvs <- forM (zip [0..] $ initialTree ^. V.branches . from V.vector) $
            \(n, branch) -> do
               ev <- treeGen grid (col+w) rest branch
                        (preview (_Just . V.branches . ix n) <$> changesTree)
               return $ over (V.branches . ix n) <$> ev
         endRow <- get
         h <- if endRow - startRow == 0
            then do
               modify (+1)  -- At least one row required for this level, even if no children.
               return 1
            else return $ endRow - startRow
         itemEvs <- forM (zip [col..] group1) $ \(col1, spec) -> do
            ev <- addCell grid h startRow col1 spec
                     (initialTree ^. V.root)
                     (preview (_Just . V.root) <$> changesTree)
            return $ over V.root <$> ev
         return $ unions $ branchEvs ++ itemEvs
      addCell :: Gtk.Grid -> Int32 -> Int32 -> Int32 -> DialogElement a -> a -> Changes (Maybe a)
            -> StateT Int32 MomentIO (Event (a -> a))
      addCell grid rowSpan row col spec initial1 value1 = lift $ do
         (widget, event) <- renderElement initial1 (fromMaybe initial1 <$> value1) spec
         style <- Gtk.widgetGetStyleContext widget
         Gtk.styleContextAddClass style "table-cell"
         Gtk.gridAttach grid widget col row 1 rowSpan
         return event

renderSpecifier
         (ForestEditorSpec labelFunc menuFunc legalParent dSel)
         initial
         value
         userHandler = mdo
      info <- liftIO $ getStdRandom random  -- Unique identifier for this tree editor instance.
      -- Configure the tree view with one column for the item text.
      vw <- Gtk.treeViewNew
      column <- Gtk.treeViewColumnNew
      render <- Gtk.cellRendererTextNew
      store <- createStore column render vw initial
      (editEvent, editHandler) <- newEvent
      let sendEditEvent = do
            contents <- MV.forestStoreGetForest store
            editHandler contents
            userHandler contents
      Gtk.treeViewSetHeadersVisible vw False
      Gtk.treeViewColumnPackStart column render True
      void $ Gtk.treeViewAppendColumn vw column
      stop1 <- reactimate1 $ updateIfNeeded store <$> changesE value
      -- Configure the view for drag and drop. ForestStore DND isn't working so do this manually.
      targets <- dragTargets info
      Gtk.treeViewEnableModelDragSource vw
            [Gdk.ModifierTypeButton1Mask]
            targets
            [Gdk.DragActionCopy, Gdk.DragActionMove]
      Gtk.treeViewEnableModelDragDest vw targets [Gdk.DragActionCopy, Gdk.DragActionMove]
      void $ vw `Gtk.on` #dragMotion $ dragMotionHandler info store vw
      void $ vw `Gtk.on` #dragDrop $ dragDropHandler sendEditEvent info store vw
      -- Activation of tree elements.
      activations <- filterJust <$> registerIOSignal2 vw Gtk.onTreeViewRowActivated (
         \path1 _ -> do
            path2 <- Gtk.treePathCopy path1  -- path1 gets deallocated after exit.
            MV.forestStoreLookup store path2 >>= \case
               Nothing -> cannotHappen
                     "Render ForestEditorSpec: path for activated item does not exist."
                     $ return ((), Nothing)
               Just (Node frag _) -> return ((), Just (path2, frag)))
      edits <- case dSel of
         Just s -> mkGtkPopupSelect vw s $ (True, ) <$> activations
         Nothing -> return never
      stop2 <- reactimate1 $ edits <&> (\(path, mFunc) -> case mFunc of
            Nothing -> return ()
            Just f -> do
               void $ MV.forestStoreChange store path f
               sendEditEvent
         )
      -- Context menus
      menuClick <- filterJust <$> registerIOSignal1
            vw
            Gtk.onWidgetButtonPressEvent
            (buttonHandler store vw)
      menuEvent <- execute $ mkGtkMenu <$> menuClick
      popupMenuOn $ fst <$> menuEvent
      menuEventOut <- switchE $ snd <$> menuEvent
      stop3 <- reactimate1 $ processMenuSelection sendEditEvent vw column store <$> menuEventOut
      -- Final widget set-up
      Gtk.treeViewSetEnableTreeLines vw True
      Gtk.treeViewExpandAll vw
      scroll <- Gtk.new Gtk.ScrolledWindow [
            #propagateNaturalHeight := True,
            #propagateNaturalWidth := True,
            #hexpand := True,
            #vexpand := True
         ]
      Gtk.containerAdd scroll vw
      w <- Gtk.toWidget scroll
      Gtk.widgetShowAll w
      void $ Gtk.onWidgetDestroy vw $ stop1 >> stop2 >> stop3
      return (w, const <$> editEvent)
   where
      createStore column render vw v = do
         store <- MV.forestStoreNew v
         MV.cellLayoutSetAttributes column render store $ \v1 -> [
               #ellipsize := Pango.EllipsizeModeEnd,
               #editable := False,
               #text := labelFunc v1
            ]
         Gtk.treeViewSetModel vw $ Just store
         Gtk.treeViewExpandAll vw
         return store
      updateIfNeeded store newVal = do
         oldVal <- MV.forestStoreGetForest store
         when (newVal /= oldVal) $ do
               MV.forestStoreClear store
               path <- Gtk.treePathNew  -- Root
               MV.forestStoreInsertForest store path 0 newVal
      targetName :: Word32 -> Text
      targetName info = "treePath-" <> T.pack (show info)
      dragTargets info = do
         entityTarget <- Gtk.targetEntryNew
               (targetName info)
               (fromIntegral $ fromEnum Gtk.TargetFlagsSameWidget)
               0  -- Can't use the "info" field, so appending it to the target name.
         return [entityTarget]
      checkTargets info context = do
         targets <- Gdk.dragContextListTargets context >>= mapM Gdk.atomName
         return $ targetName info `elem` targets
      dragPositionTarget vw model x1 y1 = do
         (x2, y2) <- Gtk.treeViewConvertWidgetToBinWindowCoords vw x1 y1
         (_, mPath, _, _, cellY) <- Gtk.treeViewGetPathAtPos vw x2 y2
         rowRect <- Gtk.treeViewGetBackgroundArea vw mPath noTreeViewColumn
         case mPath of
            Nothing -> return Nothing
            Just path -> do
               rowH <- Gdk.getRectangleHeight rowRect
               let rowH3 = rowH `div` 3
               MV.forestStoreLookup model path >>= \case
                  Nothing -> cannotHappen
                        "Render ForestEditorSpec: clicked item not found in tree."
                        $ return Nothing
                  Just (Node v _) -> do
                     let
                        pos
                           | legalParent v =
                              if cellY < rowH3 then Gtk.TreeViewDropPositionBefore else
                                 if cellY > rowH - rowH3 then Gtk.TreeViewDropPositionAfter else
                                    Gtk.TreeViewDropPositionIntoOrBefore
                           | cellY < (rowH `div` 2) = Gtk.TreeViewDropPositionBefore
                           | otherwise = Gtk.TreeViewDropPositionAfter
                     return $ Just (path, pos)
      dragMotionHandler info model vw context x1 y1 t = do
         validDrop <- checkTargets info context
         when validDrop $ do
            dragPositionTarget vw model x1 y1 >>= \case
               Just (path, pos) -> Gtk.treeViewSetDragDestRow vw (Just path) pos
               Nothing -> Gtk.treeViewSetDragDestRow vw Nothing Gtk.TreeViewDropPositionAfter
            Gdk.dragStatus context [Gdk.DragActionMove] t
         return validDrop
      dragDropHandler editHandler info model vw context x1 y1 t = do
         validDrop <- checkTargets info context
         G.signalStopEmissionByName vw "drag_drop"  -- Identifier as string: yuck.
               -- Need to do this to stop the default GTK tree drop processing.
         if validDrop
            then do
               (parentRef, position) <- dragPositionTarget vw model x1 y1 >>= \case
                  Just (path, pos) -> do
                     path2 <- reverse . fromMaybe [] <$> Gtk.treePathGetIndices path
                     case pos of
                        Gtk.TreeViewDropPositionBefore ->
                           case path2 of
                              [] -> return (Nothing, -1)
                              [n] -> return (Nothing, n)
                              n : _ -> do
                                 void $ Gtk.treePathUp path  -- Side effect on path.
                                 pathRef <- Gtk.treeRowReferenceNew model path
                                 return (Just pathRef, n)
                        Gtk.TreeViewDropPositionAfter ->
                           case path2 of
                              [] -> return (Nothing, -1)
                              [n] -> return (Nothing, n)
                              n:_ -> do
                                 void $ Gtk.treePathUp path  -- Side effect on path.
                                 pathRef <- Gtk.treeRowReferenceNew model path
                                 return (Just pathRef, n+1)
                        _ -> do
                           pathRef <- Gtk.treeRowReferenceNew model path
                           return (Just pathRef, -1)
                  Nothing -> return (Nothing, -1)
               sel <- Gtk.treeViewGetSelection vw
               (rows, _) <- Gtk.treeSelectionGetSelectedRows sel
               rowRefs <- mapM (Gtk.treeRowReferenceNew model) rows
               moved <- forM rowRefs $ \sourceRef -> do
                  v <- runMaybeT $ do
                     source <- MaybeT $ Gtk.treeRowReferenceGetPath sourceRef
                     tree <- MaybeT $ MV.forestStoreLookup model source
                     parent <- case parentRef of
                        Just r -> MaybeT $ Gtk.treeRowReferenceGetPath r
                        Nothing -> MaybeT $ Just <$> Gtk.treePathNew  -- Default points at root.
                     return (tree, parent)
                  case v of
                     Nothing -> cannotHappen
                           "Render ForestEditorSpec: item being moved not found in tree."
                           $ return False
                     Just (tree, parent) -> do
                        MV.forestStoreInsertTree
                              model
                              parent
                              (fromIntegral position)
                              tree
                        -- Source path may have changed due to insertion, so get the new one.
                        Gtk.treeRowReferenceGetPath sourceRef >>= \case
                           Nothing -> cannotHappen
                              "Render ForestEditorSepc: old position missing." $
                              return ()
                           Just source -> void $ MV.forestStoreRemove model source
                        return $ not $ null rows
               let moved1 = or moved
               Gtk.dragFinish context moved1 False t  -- Delete flag false because we have done it.
               when moved1 editHandler
               return moved1
            else
               return False
      buttonHandler store vw event = do
         button <- Gdk.getEventButtonButton event
         x1 <- round <$> Gdk.getEventButtonX event
         y1 <- round <$> Gdk.getEventButtonY event
         (x2, y2) <- Gtk.treeViewConvertWidgetToBinWindowCoords vw x1 y1
         if button == 3  -- Right button
            then Gtk.treeViewGetPathAtPos vw x2 y2 >>= \case
                  (_, Just path1, _, _, _) ->
                     MV.forestStoreLookup store path1 >>= \case
                        Nothing -> cannotHappen
                              "Render ForestEditorSpec: clicked item not found in tree."
                              $ return (False, Nothing)
                        Just (Node v _) -> do
                           path2 <- Gtk.treePathCopy path1  -- path1 gets deallocated after exit.
                           return (True, Just $ (Just path2, ) <$> menuFunc (Just v)) -- Clicked
                  _ -> return (True, Just $ (Nothing, ) <$> menuFunc Nothing)  -- Not on tree.
            else return (False, Nothing)  -- Pass other buttons to standard processing.
      processMenuSelection editHandler vw col store (Just path1, TreeAddBefore v) = do
         n <- (last . (0:)) . fromMaybe [] <$> Gtk.treePathGetIndices path1
               -- Position = 0 if no indices there. Insert before root = insert at start of root.
         path2 <- Gtk.treePathCopy path1
         void $ Gtk.treePathUp path2
         MV.forestStoreInsert store path2 (fromIntegral n) v
         editHandler
         Gtk.treeViewRowActivated vw path1 col
      processMenuSelection editHandler vw col store (Just path1, TreeAddIn v) = do
         MV.forestStoreInsert store path1 (-1) v
         (_, i) <- Gtk.treeModelGetIter store path1
         n <- Gtk.treeModelIterNChildren store $ Just i
         editHandler
         Gtk.treePathAppendIndex path1 $ fromIntegral n-1
         Gtk.treeViewRowActivated vw path1 col
      processMenuSelection editHandler vw col store (Just path1, TreeAddAfter v) = do
         n <- (last . ((-2):)) . fromMaybe [] <$> Gtk.treePathGetIndices path1
               -- Position = end if no indices there.
         void $ Gtk.treePathUp path1
         MV.forestStoreInsert store path1 (fromIntegral n+1) v
         editHandler
         if n < 0
            then do  -- Corner case: insert after root.
               (_, i) <- Gtk.treeModelGetIter store path1
               n1 <- Gtk.treeModelIterNChildren store $ Just i
               Gtk.treePathAppendIndex path1 $ fromIntegral n1-1
            else   -- Normal case: insert after existing node at path1
               Gtk.treePathAppendIndex path1 (fromIntegral n+1)
         Gtk.treeViewRowActivated vw path1 col
      processMenuSelection editHandler _ _ store (Just path1, TreeDelete) = do
         void $ MV.forestStoreRemove store path1
         editHandler
      processMenuSelection editHandler vw col store (Nothing, TreeAddAfter v) = do
         path <- Gtk.treePathNew
         MV.forestStoreInsert store path (-1) v
         n <- Gtk.treeModelIterNChildren store Nothing
         editHandler
         Gtk.treePathAppendIndex path $ fromIntegral n-1
         Gtk.treeViewRowActivated vw path col
      processMenuSelection editHandler vw col store (Nothing, TreeAddBefore v) = do
         path <- Gtk.treePathNew
         MV.forestStoreInsert store path 0 v
         editHandler
         Gtk.treePathAppendIndex path 0
         Gtk.treeViewRowActivated vw path col
      processMenuSelection _ _ _ _ _ = return ()   -- Do nothing.



-- | Create a new field widget with the plubming for setting the value and triggering events. The
-- returned behavior will always be a setter unless the default is invalid, in which case it will
-- start out as @id@.
makeField :: (Gtk.IsWidget w) =>
   w                 -- ^ Widget.
   -> (w -> IO () -> MomentIO Gtk.SignalHandlerId)
         -- ^ Signal for a change in widget display value.
   -> (w -> b -> IO ())  -- ^ Setter for widget display value.
   -> b              -- ^ The initial value to display.
   -> Changes b     -- ^ The value to display.
   -> Handler a      -- ^ Callback action for changed values.
   -> IO (Maybe a)   -- ^ Get current user value from widget, if valid.
   -> MomentIO (Gtk.Widget, Event (a -> a))
makeField widget signal setter initial value handler getValue = do
   liftIO $ setter widget initial
   valueChanged <- registerIOSignal widget signal $ do
      v <- getValue
      mapM_ handler v
      return ((), const <$> v)
   unlessFocused widget value $ setter widget
   w <- Gtk.toWidget widget
   return (w, filterJust valueChanged)


-- | The old @Entry changed@ signal has disappeared, so this simulates it by triggering on
-- insertions and deletions in the buffer. In fact there are two signal handlers, but only
-- one is returned, in accordance with the type. Since these are ignored anyway it doesn't matter.
entryChangedSignal :: Gtk.Entry -> IO () -> MomentIO Gtk.SignalHandlerId
entryChangedSignal entry action = do
   buf <- Gtk.entryGetBuffer entry
   void $ Gtk.onEntryBufferDeletedText buf $ \_ _ -> action
   Gtk.onEntryBufferInsertedText buf $ \_ _ _ -> action


-- Specialisations of Nothing to satisfy the type system.
noAdjustment :: Maybe Gtk.Adjustment
noAdjustment = Nothing

noTreeViewColumn :: Maybe Gtk.TreeViewColumn
noTreeViewColumn = Nothing

noTreeIter :: Maybe Gtk.TreeIter
noTreeIter = Nothing
