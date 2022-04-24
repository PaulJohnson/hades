{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-
Copyright © Paul Johnson 2019. See LICENSE file for details.
-}

{- |

Interpret an abstract GUI interface in GTK.
-}

module Reactive.Banana.GI.ArrowDialog (
   mkGtkPopupSelect,
   mkGtkPopup,
   mkDialogWidget,
   runDialog,
   createDialog,
   GadgetData (..),
   gdOk,
   gdValue,
   GadgetOut (..),
   cleanOutput,
   renderGadget,
   boxWidgets
) where

import Control.Arrow
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Maybe
import qualified Data.ByteString as BS
import qualified Data.Colour.CIE as C
import qualified Data.Colour.SRGB as C
import Data.Either
import Data.Function
import qualified Data.GI.Base.Signals as Gtk
import qualified Data.GI.Base.GType as Gtk
import qualified Data.GI.Gtk as MV  -- for ModelView
import Data.Int
import Data.IORef
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Semigroup
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import Data.Tree
import Data.Word
import qualified GI.Gdk as Gdk
import qualified GI.GdkPixbuf as Gdk
import qualified GI.Gio as Gio
import qualified GI.GObject as G
import GI.Gtk (AttrOp ((:=)))
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango
import Prelude hiding ((.), id)
import Reactive.Banana.ArrowDialog
import Reactive.Banana.Combinators
import Reactive.Banana.Common
import Reactive.Banana.Frameworks
import Reactive.Banana.GadgetPrimitives
import Reactive.Banana.GI.Common
import Reactive.Banana.GI.Connect
import Reactive.Banana.GI.ErrorBox
import Reactive.Banana.GI.Menu
import Reactive.Banana.GI.Table
import System.IO
import System.Random hiding (split)



-- | Pop up one of a range of dialog boxes depending on the event value. Typically @i@ is
-- a sum type and the selector will return a different dialog depending on which variant
-- it receives.
--
-- The @Event@ is the trigger for the popup to appear. A new popup will be spawned for
-- every trigger event with the initial values taken from the @i@ parameter in the event. The
-- @k@ parameter allows the application to distinguish between the output events for different
-- invocations.
--
-- The returned @Event@ is triggered by the  buttons in the dialog. If @@Cancel@ or @Close@ is
-- pressed then the event contains @Nothing@.
mkGtkPopupSelect :: (Gtk.IsWidget parent) =>
   parent   -- ^ The dialog will be kept above the window with this widget.
   -> Gtk.IconTheme
   -> Changes e
   -> DialogSelector' e w a
   -> Event (k, a)
   -> MomentIO (Event (k, Maybe a))
mkGtkPopupSelect parent iconTheme env selector trigger = do
      result <- execute $ popupAction <$> changesB env <@> trigger
      newEvents <- accumE never (unionWith const <$> result)
      switchE newEvents
   where
      popupAction e (k, v) =
         case selector e v of
            Nothing -> return never
            Just dialog -> createDialog parent iconTheme env (k, v, dialog)


-- | Similar to @mkGtkPopupSelect@, except with the Dialog being part of the event.
mkGtkPopup :: (Gtk.IsWidget parent) =>
   parent
   -> Gtk.IconTheme
   -> Changes e
   -> Event (k, a, Dialog' e w a)
   -> MomentIO (Event (k, Maybe a))
mkGtkPopup parent iconTheme env trigger = do
   result <- execute $ createDialog parent iconTheme env <$> trigger
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
mkDialogWidget :: (Eq k, Eq i) =>
   Gtk.IconTheme
   -> (forall widget . (Gtk.IsWidget widget) => widget -> IO ()) -- ^ Run on all newly created widgets.
   -> e    -- ^ Initial value of environment.
   -> Changes e  -- ^ Current value of the environment.
   -> Behavior (DialogSelector e w i o)
      -- ^ Events trigger a new dialog to be displayed. Also sampled when the input key changes.
   -> (k, Behavior i)   -- ^ Initial widget display.
   -> Event (k, Behavior i)   -- ^ Key-value pairs for widget updates.
   -> MomentIO (Gtk.Widget, Event (k, o), Event [w])
mkDialogWidget iconTheme newWidgetIO envInit envC selectorB initial inputE = do
      scrolledWidget <- Gtk.scrolledWindowNew noAdjustment noAdjustment
      liftIO $ newWidgetIO scrolledWidget
      frameWidget <- Gtk.frameNew Nothing
      liftIO $ newWidgetIO frameWidget
      Gtk.set frameWidget [#margin := 3]
      inputB <- stepper initial inputE
      sel <- valueB selectorB
      let
         -- newKeyE :: Event (k, Changes (Maybe v))
         newKeyE = filterApply (keyChanged <$> inputB) inputE
         -- dialogE :: Event (DialogSelector e w i o, (k, Changes (Maybe v)))
         dialogE = (,) <$> selectorB <@> newKeyE
      -- Event stream from edits to the initial value, if any.
      (initialEvent, initialSent, delayedIO) <- createWidget frameWidget envInit (sel, initial)
      liftIO delayedIO
      -- Event stream from edits to updated values.
      creation <- execute $ createWidget frameWidget <$> changesB envC <@> dialogE
      stop <- reactimate1 $ view _3 <$> creation
      void $ Gtk.onWidgetDestroy frameWidget stop
      sent <- switchE1 initialSent $ view _2 <$> creation
      updateEE <- accumE never $ unionWith const . view _1 <$> creation
      updateE <- switchE updateEE
      Gtk.containerAdd scrolledWidget frameWidget
      Gtk.scrolledWindowSetPropagateNaturalHeight scrolledWidget True
      Gtk.scrolledWindowSetPropagateNaturalWidth scrolledWidget True
      Gtk.widgetShowAll scrolledWidget
      w <- Gtk.toWidget scrolledWidget
      return (w, unionWith const initialEvent updateE, sent)
   where
      keyChanged (k1, _) (k2, _) = k1 /= k2
      -- The gadget is chosen from the current value and does not change until the next call
      -- to createWidget (i.e. when the key changes)
      createWidget parent env (selector, (key, valB)) = do
         initial1 <- valueB valB
         case selector env initial1 of
            Nothing -> do
               let delayedIO = do
                     clearWidget parent
                     Gtk.frameSetLabel parent Nothing
               return (never, never, delayedIO)
            Just (Dialog title _ gadget) -> do
               output <- renderGadget
                     iconTheme
                     newWidgetIO
                     env
                     (GadgetData True initial1)
                     envC
                     never
                     (GadgetData True <$> valB)
                     gadget
               widget <- boxWidgets newWidgetIO Vertical $ gadgetView output
               let
                  result = view gdValue <$> filterE (view gdOk) (gadgetUser output)
                  delayedIO = do
                     -- Callbacks triggered during execute cause hanging.
                     clearWidget parent
                     Gtk.frameSetLabel parent $ Just title
                     forM_ widget $ \w -> do
                        Gtk.set w [#margin := 3]
                        Gtk.containerAdd parent w
                     Gtk.widgetShowAll parent
               return ((key,) <$> result, gadgetEvents output, delayedIO)
      -- Clear the frame contents.
      clearWidget parent = do
         Gtk.binGetChild parent >>= \case
            -- Just oldWidget -> Gtk.containerRemove parent oldWidget
            Just oldwidget -> Gtk.widgetDestroy oldwidget
            Nothing -> return ()
         Gtk.frameSetLabel parent Nothing


-- | Run the dialog as a simple pop-up in the IO monad.
runDialog :: (Gtk.IsWidget parent) =>
   parent -> Gtk.IconTheme -> Dialog' e w a -> e -> a -> IO (Maybe a)
runDialog parent iconTheme d env initial = do
      store <- newIORef initial  -- Wormhole for result to escape from reactive net.
      target <- Gtk.dialogNew
      style1 <- Gtk.widgetGetStyleContext target
      Gtk.styleContextAddClass style1 "hades-dialog"
      modalFor1 <- widgetWindow parent
      forM_ modalFor1 $ \win -> Gtk.set target [#transientFor := win]
      Gtk.windowSetTitle target $ dialogTitle d
      Gtk.dialogSetDefaultResponse target $ r $ case dialogButtons d of
         CloseButton _ -> Gtk.ResponseTypeClose
         _ -> Gtk.ResponseTypeOk
      let
         net = mdo  -- MomentIO monad.
            -- Get the dialog widget tree and change event. Put widget tree into window.
            output <- renderGadget
                  iconTheme
                  wInit
                  env
                  (GadgetData True initial)
                  (pure env)
                  never
                  (GadgetData True <$> feedback)
                  (dialogGadget d)
                     -- Input is always assumed valid. The gadget can decide otherwise.
            widget <- boxWidgets wInit Vertical $ gadgetView output
            forM_ widget $ \w -> do   -- widget is Maybe, so this is done at most once.
               Gtk.set w [#margin := 5]
               Gtk.widgetSetMarginEnd w 10  -- Space for scroll bars
               contentBox <- Gtk.dialogGetContentArea target
               Gtk.boxPackStart contentBox w True True 5
            -- Plumb the new values coming up via 'setter' to the 'value' sent down the tree.
            feedback <- stepper (_gdValue $ gadgetInitial output) $ _gdValue <$> gadgetUser output
            liftIO $ writeIORef store $ _gdValue $ gadgetInitial output
            stop <- reactimate1 $
               writeIORef store . _gdValue <$> gadgetUser (cleanOutput output)
            void $ Gtk.onWidgetDestroy target stop
            case dialogButtons d of
               CloseButton txt ->
                  void $ Gtk.dialogAddButton target txt $ r Gtk.ResponseTypeClose
               _ -> do
                  void $ Gtk.dialogAddButton target "_Cancel" $ r Gtk.ResponseTypeCancel
                  okButton <- Gtk.dialogAddButton target "_Ok" $ r Gtk.ResponseTypeOk
                  eventLink okButton Gtk.setWidgetSensitive $ _gdOk <$> gadgetUser output
                  Gtk.setWidgetSensitive okButton $ _gdOk $ gadgetInitial output
            Gtk.widgetShowAll target
      compiled <- compile net
      actuate compiled
      r1 <- Gtk.dialogRun target
      pause compiled
      Gtk.widgetDestroy target
      case toEnum $ fromIntegral r1 of
         Gtk.ResponseTypeOk -> Just <$> readIORef store
         Gtk.ResponseTypeClose -> Just <$> readIORef store
         _ -> return Nothing
   where
      wInit :: (Gtk.IsWidget w) => w -> IO ()
      wInit = const $ return ()
      r x = fromIntegral $ fromEnum x  -- Convert response type to Word32


-- | Create the dialog as specified.
createDialog :: (Gtk.IsWidget parent) =>
   parent -> Gtk.IconTheme -> Changes e -> (k, a, Dialog' e w a) -> MomentIO (Event (k, Maybe a))
createDialog parent iconTheme envC (key, initial, d) = mdo
      -- Create the window.
      target <- Gtk.new Gtk.Dialog [#modal := True, #title := dialogTitle d]
      style <- Gtk.widgetGetStyleContext target
      Gtk.styleContextAddClass style "hades-dialog"
      modalFor <- widgetWindow parent
      forM_ modalFor $ \win -> Gtk.set target [#transientFor := win]
      Gtk.dialogSetDefaultResponse target $ r $ case dialogButtons d of
         CloseButton _ -> Gtk.ResponseTypeClose
         _ -> Gtk.ResponseTypeOk
      box <- Gtk.dialogGetContentArea target
      -- Get the dialog widget tree and change event. Put widget tree into window.
      envInit <- valueB $ changesB envC
      output <- renderGadget
            iconTheme
            wInit
            envInit
            (GadgetData True initial)
            envC
            never
            (GadgetData True <$> feedback)
            (dialogGadget d)
      outputB <- stepper (gadgetInitial output) $ filterE _gdOk $ gadgetUser output
      feedback <- stepper (_gdValue $ gadgetInitial output) $ _gdValue <$> gadgetUser output
      widget <- boxWidgets wInit Vertical $ gadgetView output
      forM_ widget $ \w -> do   -- widget is Maybe, so this is done at most once.
         Gtk.set w [#margin := 5]
         Gtk.widgetSetMarginEnd w 10  -- Space for scroll bars
         Gtk.boxPackStart box w True True 5
      -- Event triggered when OK is pressed.
      (resultE, resultH) <- newEvent
      void $ Gtk.onDialogResponse target $ handleResponse target resultH
      case dialogButtons d of
         CloseButton txt ->
            void $ Gtk.dialogAddButton target txt $ r Gtk.ResponseTypeClose
         _ -> do
            let enableOk = _gdOk <$> gadgetUser output
            void $ Gtk.dialogAddButton target "_Cancel" $ r Gtk.ResponseTypeCancel
            okButton <- Gtk.dialogAddButton target "_Ok" $ r Gtk.ResponseTypeOk
            eventLink okButton Gtk.setWidgetSensitive enableOk
            Gtk.setWidgetSensitive okButton $ _gdOk $ gadgetInitial output
            when (dialogButtons d == OkApplyButton) $ do
               applyButton <- Gtk.dialogAddButton target "_Apply" $ r Gtk.ResponseTypeApply
               Gtk.setWidgetSensitive applyButton $ view gdOk $ gadgetInitial output
               eventLink applyButton Gtk.setWidgetSensitive enableOk
      Gtk.widgetShowAll target
      return $ (\a r1 -> (key, if r1 then Just $ a ^. gdValue else Nothing)) <$> outputB <@> resultE
   where
      wInit :: (Gtk.IsWidget w) => w -> IO ()
      wInit = const $ return ()
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
      handleResponse1 target handler Gtk.ResponseTypeClose = do
         handler True
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


-- | GTK gadget renderings return this type.
--
-- Gadget output combines an event and behaviour in way which resembles "Changes" but is
-- sufficiently different not to use one. The user "Event" is triggered when the user actually
-- makes a change to the data. When this data is fed back in to this or any other gadget it
-- travels as a "Behavior", so the Event data may be different to the current Behavior.
--
-- The gadget dialog functions maintain a global input behaviour from the output events. This
-- provides the feedback for the gadget to update itself.
--
-- Display widgets therefore need to reactimate only on the behavior, but not propogate
-- events which are the result of updates from the behavior.
data GadgetOut w o = GadgetOut {
      gadgetView :: [Gtk.Widget],  -- ^ The widgets to display, if any.
      gadgetEvents :: Event [w],   -- ^ Data sent via the side channel.
      gadgetInitial :: GadgetData o,   -- ^ The value of the initial output, if any.
      gadgetUser :: Event (GadgetData o),  -- ^ User updates to the output.
      gadgetValue :: Behavior (GadgetData o) -- ^ Application updates to the output.
   }

instance Functor (GadgetOut w) where
   fmap f (GadgetOut w ev i u b) =
      GadgetOut w ev (gdValue %~ f $ i) ((gdValue %~ f) <$> u) ((gdValue %~ f) <$> b)

instance Applicative (GadgetOut w) where
   pure v = GadgetOut [] never (GadgetData True v) never (pure $ GadgetData True v)
   GadgetOut w1 ev1 initF userF appF <*> GadgetOut w2 ev2 initV userV appV =
      let
         Changes user appB = (<*>) <$> Changes userF appF <*> Changes userV appV
            -- Semantics happen to match Changes, so reuse the code.
      in GadgetOut {
            gadgetView = w1 ++ w2,
            gadgetEvents = ev1 <> ev2,
            gadgetInitial = initF <*> initV,
            gadgetUser = user,
            gadgetValue = appB
         }

instance (Monoid v) => Monoid (GadgetOut w v) where
   mempty = GadgetOut [] never mempty never $ pure mempty

instance (Semigroup v) => Semigroup (GadgetOut w v) where
   GadgetOut w1 ev1 init1 user1 app1 <> GadgetOut w2 ev2 init2 user2 app2 =
      GadgetOut {
            gadgetView = w1 ++ w2,
            gadgetEvents = ev1 <> ev2,
            gadgetInitial = init1 <> init2,
            gadgetUser = user1 <> user2,
            gadgetValue = app1 <> app2
         }


-- | Filter invalid values from the user event stream.
cleanOutput :: GadgetOut w i -> GadgetOut w i
cleanOutput g = g {gadgetUser = filterE _gdOk $ gadgetUser g}


-- | Make the output validity conditional on an external condition. If the @Bool@ is false then
-- all the gadget values will be flagged as invalid.
qualifyOutput :: Bool -> GadgetOut w i -> GadgetOut w i
qualifyOutput ok o = o {
      gadgetInitial = gdOk %~ (ok &&) $ gadgetInitial o,
      gadgetUser = (gdOk %~ (ok &&)) <$> gadgetUser o,
      gadgetValue = (gdOk %~ (ok &&)) <$> gadgetValue o
   }


-- | Merge ouptputs that carry functions, including their validity flags. Widgets are concatenated.
mergeOutputs :: [GadgetOut w (a -> a)] -> GadgetOut w (a -> a)
mergeOutputs [] = GadgetOut [] never (GadgetData True id) never $ pure $ GadgetData True id
mergeOutputs outs =
      foldr1 merge2 outs
      -- foldr1 is safe because outs is never null.
   where
       -- Merge a pair of outputs. The tricky bit is ensuring that old invalidity flags do not
      -- cause a permanent latch in the user output.
      merge2 out1 out2 = GadgetOut {
            gadgetView = gadgetView out1 ++ gadgetView out2,
            gadgetEvents = gadgetEvents out1 <> gadgetEvents out2,
            gadgetInitial = gadgetInitial out1 `merge` gadgetInitial out2,
            gadgetUser = unionWith merge
               (merge <$> gadgetValue out2 <@> gadgetUser out1)
               (merge <$> gadgetValue out1 <@> gadgetUser out2),
            gadgetValue = merge <$> gadgetValue out1 <*> gadgetValue out2
         }
      merge (GadgetData b1 f1) (GadgetData b2 f2) = GadgetData (b1 && b2) (f1 . f2)


-- | Data derived from a "ClickableItem" which is displayed in GTK.
data GtkClickable a = GtkClickable {
      gclickRange :: (Word32, Word32), -- ^ UTF8 byte indices for start and end.
      gclickBase :: ClickableItem a  -- ^ Base data.
   }


-- | Display the items in the label as a comma-separated list, and return a function from the
-- UTF8 index (as used by GTK) to the corresponding GtkClickable.
displayClickables ::
   Gtk.Label
   -> GadgetData [ClickableItem a]
   -> IO (Word32 -> Maybe (GtkClickable a))
displayClickables _ (GadgetData False _) = return $ const Nothing
displayClickables label (GadgetData True items) = do
      oldTxt <- Gtk.labelGetText label
      when (oldTxt /= display) $ Gtk.labelSetLabel label display
      let gtks = zipWith GtkClickable itemRanges items
      return (\idx -> find (inRange idx . gclickRange) gtks)
   where
      itemLengths = map (BS.length . T.encodeUtf8 . clickText) items
      itemRanges = tail $ scanl itemRange (0,-sepLen) itemLengths
      inRange idx (x1, x2) = x1 <= idx && idx <= x2
      display = T.intercalate itemSep $ map clickText items
      itemRange (_, prev) x = (prev + sepLen, prev + sepLen + fromIntegral x)
      itemSep = ", "
      sepLen = fromIntegral $ T.length itemSep


-- | Render a gadget as one or more GTK widgets.
renderGadget ::
   Gtk.IconTheme
   -> (forall widget . (Gtk.IsWidget widget) => widget -> IO ()) -- ^ Run on all newly created widgets.
   -> e   -- ^ The initial environment.
   -> GadgetData i   -- ^ The initial input.
   -> Changes e  -- ^ Updates to the environment. This must start equal to the first argument.
   -> Event (GadgetData i)  -- ^ User updates to the input.
   -> Behavior (GadgetData i)  -- ^ Application updates. Must start equal to the second argument.
   -> Gadget e w i o
   -> MomentIO (GadgetOut w o)

renderGadget _ _ _ initial _ e b Null = {-# SCC "renderGadget_Null" #-}
   return $ GadgetOut {
         gadgetView = [],
         gadgetEvents = never,
         gadgetInitial = initial,
         gadgetUser = e,
         gadgetValue = b
      }

renderGadget _ _ _ initial _ e b (Pure f) = {-# SCC "renderGadget_Pure" #-}
   return $ GadgetOut {
         gadgetView = [],
         gadgetEvents = never,
         gadgetInitial = f <$> initial,
         gadgetUser = fmap f <$> e,
         gadgetValue = fmap f <$> b
      }

renderGadget iconTheme wInit env initial envC e b (Dot g1 g2) = {-# SCC "renderGadget_Dot" #-} do
      r2 <- renderGadget iconTheme wInit env initial envC e b g2
      r1 <- renderGadget
            iconTheme
            wInit
            env
            (r2 ^. to gadgetInitial)
            envC
            (gadgetUser r2)
            (gadgetValue r2)
            g1
      return $ GadgetOut {
            gadgetView = gadgetView r2 ++ gadgetView r1,   -- r2 feeds into r1 so r2 goes above.
            gadgetEvents = gadgetEvents r1 <> gadgetEvents r2,
            gadgetInitial = gadgetInitial r1,
            gadgetUser = gadgetUser r1,
            gadgetValue = gadgetValue r1
         }

renderGadget iconTheme wInit env initial envC e b (Prod g1 g2) = {-# SCC "renderGadget_Prod" #-} do
   r1 <- renderGadget iconTheme wInit env (fst <$> initial) envC (fmap fst <$> e) (fmap fst <$> b) g1
   r2 <- renderGadget iconTheme wInit env (snd <$> initial) envC (fmap snd <$> e) (fmap snd <$> b) g2
   return $ (,) <$> r1 <*> r2

-- Loop :: Gadget (i, s) (o, s) -> Gadget i o
renderGadget iconTheme wInit env initial envC e b (Loop g) = {-# SCC "renderGadget_Loop" #-} mdo
      let
         loopInitialInput = (,) <$> initial <*> (snd <$> gadgetInitial output)
         newE = fb <@> e
         newB = fb <*> b
      output <- renderGadget iconTheme wInit env loopInitialInput envC newE newB g
      fb <- fmap (\s i -> (,) <$> i <*> s) <$> stepper
               (snd <$> loopInitialInput)
               (fmap snd <$> gadgetUser output)
      return $ fst <$> output

renderGadget iconTheme wInit env initial envC e b (Focusing lns g) = {-# SCC "renderGadget_Focusing" #-} do
   let extract x = x ^. getting lns
   out <- renderGadget
         iconTheme
         wInit
         env
         (extract <$> initial)
         envC
         (fmap extract <$> e)
         (fmap extract <$> b) g
   return $ set lns <$> out

renderGadget iconTheme wInit env initial envC e b (Prismatic d prsm g) = {-# SCC "renderGadget_Prismatic" #-}
   withPrism prsm $ \inject extract -> do
      let extract1 = fromRight d . extract
      out <- renderGadget
            iconTheme
            wInit
            env
            (extract1 <$> initial)
            envC
            (fmap extract1 <$> e)
            (fmap extract1 <$> b) g
      return $ inject <$> out

renderGadget iconTheme wInit env initial envC e b (PrismaticOver d prsm g) =
   {-# SCC "renderGadget_PrismaticOver" #-} do
      let extract x = fromMaybe d $ x ^? getting prsm
      out <- renderGadget
            iconTheme
            wInit
            env
            (extract <$> initial)
            envC
            (fmap extract <$> e)
            (fmap extract <$> b)
            g
      return $ (\f v -> view (re prsm) $ f $ extract v) <$> out

renderGadget iconTheme wInit env initial envC e b (Traversing d trv g) =
   {-# SCC "renderGadget_Traversing" #-} do
      let extract x = fromMaybe d $ x ^? getting trv
      out <- renderGadget
            iconTheme
            wInit
            env
            (extract <$> initial)
            envC
            (fmap extract <$> e)
            (fmap extract <$> b)
            g
      return $ set trv <$> out

renderGadget iconTheme wInit env initial envC e b (TraversingOver d trv g) =
   {-# SCC "renderGadget_TraversingOver" #-} do
      let extract x = fromMaybe d $ x ^? getting trv
      out <- renderGadget
            iconTheme
            wInit
            env
            (extract <$> initial)
            envC
            (fmap extract <$> e)
            (fmap extract <$> b) g
      return $ over trv <$> out

renderGadget iconTheme wInit env initial envC e b (Accum g) = {-# SCC "renderGadget_Accum" #-} mdo
      inner <- renderGadget iconTheme wInit env initial envC e b g
      let outE = applyUpdate <$> b <@> gadgetUser inner
      return inner {
            gadgetInitial = applyUpdate initial $ gadgetInitial inner,
            gadgetUser = outE,
            gadgetValue = b
         }
   where
      applyUpdate (GadgetData _ old) (GadgetData ok newF) = GadgetData ok $ newF old

renderGadget _ _ _ _ _ e b (Initially v) = {-# SCC "renderGadget_Initiall" #-}
   return $ GadgetOut [] never (GadgetData True v) e b

renderGadget iconTheme wInit env initial envC e b (GetInitial f) = {-# SCC "renderGadget_GetInitial" #-} do
   let g = initial ^. gdValue . to f
   renderGadget iconTheme wInit env initial envC e b g

renderGadget _ _ env _ envC _ _ GetEnv = {-# SCC "renderGadget_GetEnv" #-} return GadgetOut {
      gadgetView = [],
      gadgetEvents = never,
      gadgetInitial = GadgetData True env,
      gadgetUser = never,  -- Environment changes are nothing to do with the user.
      gadgetValue = GadgetData True <$> changesB envC
   }

renderGadget iconTheme wInit env initial envC e b (GetInitialEnv f) = {-# SCC "renderGadget_GetInitialEnv" #-}
      renderGadget iconTheme wInit env initial envC e b $ f env

renderGadget iconTheme wInit env initial envC e b (WithEnv envG g) = {-# SCC "renderGadget_WithEnv" #-} do
         envOut <- cleanOutput <$> renderGadget iconTheme wInit env initial envC e b envG
         let
            i2 = gadgetInitial envOut ^. gdValue
            e2 = _gdValue <$> gadgetUser envOut
         newEnvC <- makeChanges i2 e2
         renderGadget iconTheme wInit i2 initial newEnvC e b g

renderGadget _ _ _ initial _ e b (Send f) = {-# SCC "renderGadget_Send" #-} return $ GadgetOut {
         gadgetView = [],
         gadgetEvents = return <$> filterJust (f . _gdValue <$> filterE _gdOk e),
         -- "return" puts item into a list.
         gadgetInitial = initial,
         gadgetUser = e,
         gadgetValue = b
      }

renderGadget iconTheme wInit env initial envC e b (SendMap f g) = {-# SCC "renderGadget_SendMap" #-} do
         inner <- renderGadget iconTheme wInit env initial envC e b g
         return inner {gadgetEvents = filterE (not . null) $ mapMaybe f <$> gadgetEvents inner}

renderGadget iconTheme wInit _ initial envC e b (Exec gadgetF) = {-# SCC "renderGadget_Exec" #-} do
      parent <- Gtk.boxNew Gtk.OrientationVertical 5  -- A holder for widgets created by events.
      liftIO $ wInit parent
      currentArg <- stepper (fst <$> initial) $ fmap fst <$> e
      let
         changed = snd <$> filterE
               (\(v1, v2) -> _gdValue v1 /= fst (_gdValue v2))
               ((,) <$> currentArg <@> e)

         startGadget = getGadget initial
         gadgetUpdates = getGadget <$> changed
      (initialOut, initialIO) <- doExec parent startGadget
      liftIO initialIO  -- Put up the initial gadget in the empty frame.
      newWidgets <- execute $ doExec parent <$> gadgetUpdates
      sent <- switchE1 (gadgetEvents initialOut) $ gadgetEvents . fst <$> newWidgets
      resultE <- switchE1
            (gadgetUser initialOut)
            (gadgetUser . fst <$> newWidgets)
      stop <- reactimate1 $ snd <$> newWidgets
      void $ Gtk.onWidgetDestroy parent stop
      w <- Gtk.toWidget parent
      let outE = unionWith const
               (gadgetInitial . fst <$> newWidgets)  -- Newly created inner widgets trigger updates.
               resultE
      outB <- switchB (gadgetValue initialOut) $ gadgetValue . fst <$> newWidgets
      return GadgetOut {
            gadgetView = [w],
            gadgetEvents = sent,
            gadgetInitial = gadgetInitial initialOut,
            gadgetUser = outE,
            gadgetValue = outB
         }
   where
      getGadget d@(GadgetData ok val) = GadgetData ok (gadgetF $ fst val, snd <$> d)
      -- doExec :: (Gtk.IsWidget w) => w -> (Gadget i o, i) -> MomentIO (GadgetOut o, IO ())
      doExec parent (GadgetData ok (sub, newValue)) = do
         env <- valueB $ changesB envC
         o <- renderGadget iconTheme wInit env newValue envC (fmap snd <$> e) (fmap snd <$> b) sub
         let
            replace = do  -- IO monad
               childs <- Gtk.containerGetChildren parent
               forM_ childs Gtk.widgetDestroy
               forM_ (gadgetView o) $ \w -> Gtk.boxPackStart parent w True True 0
               Gtk.widgetShowAll parent
         return (qualifyOutput ok o, replace )

renderGadget iconTheme wInit env initial envC e b (Cond opts) = {-# SCC "renderGadget_Cond" #-} do
      -- Outputs are numbered so we can detect changes.
      (outputs :: [(i -> Bool, (GadgetOut w o, Int))]) <-
         forM (zip [(1 :: Int) ..] opts) $ \(n, PrismaticGadget d p g) ->
            withPrism p $ \setter getter -> do
               let
                  initial1 = fromRight d . getter <$> initial
                  e1 = filterJust $
                        sequenceA . (gdValue %~ (either (const Nothing) Just . getter)) <$> e
                  b1 = (gdValue %~ (fromRight d . getter)) <$> b
               o <- renderGadget iconTheme wInit env initial1 envC e1 b1 $ g >>> arr setter
               return (isRight . getter, (o, n))
      let
         defaultOutput = case outputs of
            o1:_ -> o1 ^. _2
            [] -> error "renderGadget Cond: empty list."
      parent <- Gtk.boxNew Gtk.OrientationVertical 5  -- A holder for widgets created by events.
      liftIO $ wInit parent
      let
         chooseOutput :: GadgetData i -> (GadgetOut w o, Int)
         chooseOutput v = maybe defaultOutput snd $ find (($ _gdValue v) . fst) outputs
         startOutput :: (GadgetOut w o, Int)
         startOutput = chooseOutput initial
         userChoice :: Event (GadgetOut w o, Int)
         userChoice = chooseOutput <$> e
         resultE :: Event (GadgetData o)
         resultE = foldr
               (unionWith const . gadgetUser . view (_2 . _1))
               never
               outputs
      currentChoice <- stepper startOutput userChoice
      outB <- switchB (gadgetValue $ fst startOutput) (gadgetValue . fst <$> userChoice)
      replaceWidgets parent (-1) startOutput
      stop1 <- reactimate1 $ replaceWidgets parent . snd <$> currentChoice <@> userChoice
      void $ Gtk.onWidgetDestroy parent stop1
      w <- Gtk.toWidget parent
      return $ GadgetOut {
            gadgetView = [w],
            gadgetEvents = mconcat $ map (view $ _2 . _1 . to gadgetEvents) outputs,
            gadgetInitial = gadgetInitial $ startOutput ^. _1,
            gadgetUser = unionWith const resultE $ observeE $
                  valueB . gadgetValue . fst <$> userChoice,
            gadgetValue = outB
         }
   where
      replaceWidgets parent n1 (o, n2) = when (n1 /= n2) $ do
            childs <- Gtk.containerGetChildren parent
            mapM_ (Gtk.containerRemove parent) childs
            forM_ (gadgetView o) $ \w -> Gtk.boxPackStart parent w True True 0
            Gtk.containerCheckResize parent
            Gtk.widgetShowAll parent

renderGadget _ _ _ _ _ _ _ (UnionTab []) =  -- Empty tab list.
   error "renderGadget UnionTab: empty list."

renderGadget iconTheme wInit env initial envC e b (UnionTab tabs) = {-# SCC "renderGadget_UnionTab" #-} do
      (appPageEvent, appH) <- newEvent  -- When the incoming Behavior triggers a page switch.
      book <- Gtk.new Gtk.Notebook [
            #margin := 5,
            #scrollable := True,
            #enablePopup := True
         ]
      liftIO $ wInit book
      outputs <- forM tabs (renderTab book)
      Gtk.widgetShowAll book
      goToPage book mempty $ inputNumber initial
      -- Use an IORef to block or enable signals according to where the change is coming from.
      signalEnabled <- liftIO $ newIORef True
      let
         blockSignalsIn act =
            writeIORef signalEnabled False >> act >> writeIORef signalEnabled True
      tabClicks <- filterJust <$> registerIOSignal2 book Gtk.afterNotebookSwitchPage (\_ n -> do
            enabledFlag <- readIORef signalEnabled
            return ((), if enabledFlag then Just $ fromIntegral n else Nothing))
      stop1 <- reactimate1 $ goToPage book mempty . inputNumber <$> e
            -- Propogates change event from book signal.
      ch <- changes $ blockSignalsIn . goToPage book appH . inputNumber <$> b
      stop2 <- reactimate1' ch
            -- Does not propogate change event for application behavior.
      void $ Gtk.onWidgetDestroy book $ stop1 >> stop2
      let
         initialOutput = clickValue outputs $ inputNumber initial
         tabE = clickValue outputs <$> tabClicks
         appEvent = clickValue outputs <$> appPageEvent
      outE <- switchE1 (gadgetUser initialOutput) $ gadgetUser <$> tabE
      outB <- switchB (gadgetValue initialOutput) $ gadgetValue <$> appEvent
      w <- Gtk.toWidget book
      return $ GadgetOut {
            gadgetView = [w],
            gadgetInitial = gadgetInitial initialOutput,
            gadgetEvents = mconcat $ map gadgetEvents outputs,
            gadgetUser = unionWith const outE $ observeE $ valueB . gadgetValue <$> tabE,
            gadgetValue = outB
         }
   where
      goToPage :: (MonadIO m) => Gtk.Notebook -> (Int -> IO ()) -> Int -> m ()
      goToPage book handler n = do
         c <- fromIntegral <$> Gtk.notebookGetCurrentPage book
         when (c /= n) $ do
            Gtk.notebookSetCurrentPage book $ fromIntegral n
            liftIO $ handler n
      inputNumber (GadgetData _ v) = length $ takeWhile (not . tabMatches v) tabs
      tabMatches x (_, PrismaticGadget _ prsm _) = isJust $ x ^? getting (clonePrism prsm)
      clickValue :: [a] -> Int -> a
      clickValue gs n =
         case drop n gs of  -- Like (!!) but safe.
            g : _ -> g
            [] -> cannotHappen "renderGadget UnionTab: missing tab" $ head gs
               -- Safe because the list of tabs is not empty.
      renderTab book (title, PrismaticGadget d prsm g) = withPrism prsm $ \setter getter -> do
         let
            initial1 = fromRight d . getter <$> initial
            e1 = filterJust $ sequenceA . (gdValue %~ (either (const Nothing) Just . getter)) <$> e
            b1 = (gdValue %~ (fromRight d . getter)) <$> b
         subOutput <- renderGadget iconTheme wInit env initial1 envC e1 b1 $ g >>> arr setter
         widget <- boxWidgets wInit Vertical (gadgetView subOutput) >>= \case
            Nothing -> Gtk.labelNew (Just "") >>= Gtk.toWidget  -- Blank
            Just w -> return w
         label <- Gtk.labelNew $ Just title
         liftIO $ wInit label
         void $ Gtk.notebookAppendPageMenu book widget (Just label) noWidget
         return subOutput

renderGadget iconTheme wInit env initial envC e b (Enabled g) = {-# SCC "renderGadget_Enabled" #-} do
      result <- renderGadget iconTheme wInit env (fst <$> initial) envC (fmap fst <$> e) (fmap fst <$> b) g
      forM_ (gadgetView result) $ \w -> do
         Gtk.widgetSetSensitive w $ initial ^. gdValue . _2
         eventLink w Gtk.widgetSetSensitive $ snd . _gdValue <$> e
         behaviorLink w Gtk.widgetSetSensitive $ snd . _gdValue <$> b
      return result

renderGadget iconTheme wInit env initial envC e b (Optional _ (TextBox p)) =
      renderGadget iconTheme wInit env initial envC e b $ TextBox $ prismToMaybe . p

renderGadget iconTheme wInit env initial envC e b (Optional _ DisplayText) =
      renderGadget iconTheme wInit env initial envC e b $
         arr (\t -> if T.null t then Nothing else Just t) <<< DisplayText <<< arr (fromMaybe "")

renderGadget iconTheme wInit env initial envC e b (Optional _ (MemoBox expand size)) =
      fmap outF <$> renderGadget
            iconTheme
            wInit
            env
            (fromMaybe "" <$> initial)
            envC
            (fmap (fromMaybe "") <$> e)
            (fmap (fromMaybe "") <$> b)
            (MemoBox expand size)
   where
      outF :: Text -> Maybe Text
      outF "" = Nothing
      outF txt = Just txt

renderGadget iconTheme wInit env initial envC e b (Optional _ (Combo itemF)) =
      renderGadget iconTheme wInit env initial envC e b $ Combo $ (nothingItem :) . map (fmap Just) . itemF
   where
      nothingItem = ComboItem "" Nothing Nothing Nothing

renderGadget iconTheme wInit env initial envC e b (Optional _ TickBox) =
   renderGadget iconTheme wInit env initial envC e b $ Combo $ const [
         ComboItem "" Nothing Nothing Nothing,
         ComboItem "Yes" (Just "object-select-symbolic") Nothing (Just True),  -- Green tick
         ComboItem "No" (Just "list-remove-symbolic") Nothing (Just False)  -- Red cross
      ]

renderGadget iconTheme wInit env initial envC e b (Optional _ (FixedText textF Nothing)) =
      renderGadget iconTheme wInit env initial envC e b $ FixedText (maybe "" . textF) Nothing

renderGadget iconTheme wInit env initial envC e b (Optional d (FixedText textF (Just selector1))) =
      renderGadget iconTheme wInit env initial envC e b $ FixedText (maybe "" . textF) $ Just selector2
   where
      selector2 = promoteDialogSelector d _Just selector1

renderGadget iconTheme wInit env initial envC e b (Optional _ (FixedMemo expand size textF Nothing)) =
      renderGadget iconTheme wInit env initial envC e b $ FixedMemo expand size (maybe "" . textF) Nothing

renderGadget iconTheme wInit env initial envC e b
         (Optional d (FixedMemo expand size textF (Just selector1))) =
      renderGadget iconTheme wInit env initial envC e b $
            FixedMemo expand size (maybe "" . textF) $ Just selector2
   where
      selector2 = promoteDialogSelector d _Just selector1

renderGadget _ wInit _ initial _ e1 b1 (Optional _ (DateBox fmt)) =
   {-# SCC "renderGadget_Optional_DateBox" #-} do
      let prsm = datePrism fmt
      (w, e2) <-
         textBoxWithPopup wInit initial e1 b1 (prismToMaybe $ clonePrism prsm) $ \entry -> do
            (overlay, calendar) <- createDatePopover (datePrism fmt) entry
            liftIO $ wInit overlay
            liftIO $ wInit calendar
            void $ Gtk.onWidgetDestroy entry $ do
               Gtk.widgetDestroy overlay
               Gtk.widgetDestroy calendar
            registerIOSignal calendar Gtk.onCalendarDaySelectedDoubleClick $ do
               (year, month, day) <- Gtk.calendarGetDate calendar
               Gtk.popoverPopdown overlay
               let date = fromGregorian
                     (fromIntegral year)
                     (fromIntegral month + 1)
                     (fromIntegral day)
               return ((), date ^. re (datePrism fmt))
      return $ GadgetOut {
            gadgetView = [w],
            gadgetEvents = never,
            gadgetInitial = initial,
            gadgetUser = e2,
            gadgetValue = b1
         }

renderGadget iconTheme wInit _ initial _ e b (Optional _ (IconBox predicate)) =
   {-# SCC "renderGadget_Optional_IconBox" #-} do
      currentName <- liftIO $ newIORef $ initial ^. gdValue
      icon1 <- safeLoadIcon iconTheme (fromMaybe noIconName $ initial ^. gdValue) iconSize
      pic <- Gtk.imageNewFromPixbuf icon1
      Gtk.set pic [#sensitive := True]
      button <- Gtk.new Gtk.Button [#image := pic, #alwaysShowImage := True]
      liftIO $ wInit pic
      liftIO $ wInit button
      stop1 <- reactimate1 $ updateIcon currentName pic <$> e
      ch <- changes $ updateIcon currentName pic <$> b
      stop2 <- reactimate1' ch
      void $ Gtk.onWidgetDestroy button $ stop1 >> stop2
      (nameE, handler) <- newEvent
      void $ Gtk.onButtonPressed button $ do
         icons <- withWaitCursor button $ iconThemeContents iconTheme False predicate
         iconDialog pic icons $ \newIcon -> do
            let newValue = if newIcon == noIconName then Nothing else Just newIcon
            Gtk.set pic [#iconName := newIcon]
            handler $ GadgetData True newValue
      w1 <- Gtk.toWidget button
      return $ GadgetOut {
            gadgetView = [w1],
            gadgetEvents = never,
            gadgetInitial = initial,
            gadgetUser = unionWith const nameE e,
            gadgetValue = b
         }
   where
      updateIcon _ _ (GadgetData False _) = return ()
      updateIcon ref pic (GadgetData True v) = do
         oldName <- readIORef ref
         when (oldName /= v) $ do
            writeIORef ref v
            icon1 <- safeLoadIcon iconTheme (fromMaybe noIconName v) iconSize
            Gtk.imageSetFromPixbuf pic icon1

renderGadget _ wInit _ initial _ e1 b1 (Optional _ ColourBox) =
   {-# SCC "renderGadget_Optional_ColourBox" #-} do
      entry <- Gtk.new Gtk.Entry [#hexpand := True]
      liftIO $ wInit entry
      Gtk.entrySetIconFromIconName
         entry
         Gtk.EntryIconPositionSecondary
         $ Just "applications-science"
      (w, e2) <- makeField
         entry
         entryChangedSignal
         setEntryValue
         initial
         e1
         b1
         $ readVal entry
      void $ Gtk.onEntryIconPress entry $ \_ _ -> do
         chooser <- createColorDialog entry
         r <- toEnum . fromIntegral <$> Gtk.dialogRun chooser
         case r of
            Gtk.ResponseTypeOk -> do
               colour <- rgbaToColour =<< Gtk.colorChooserGetRgba chooser
               Gtk.setEntryText entry $ colour ^. re colourPrism
               Gtk.widgetGrabFocus entry
            _ -> return ()
         Gtk.widgetDestroy chooser
      widgetLinkColour entry (initial ^. gdValue) (_gdValue <$> e2) (_gdValue <$> b1)
      return GadgetOut {
            gadgetView = [w],
            gadgetEvents = never,
            gadgetInitial = initial,
            gadgetUser = e2,
            gadgetValue = b1
         }
   where
      readVal :: Gtk.Entry -> IO (Maybe (Maybe Colour))
      readVal entry = do
         t <- Gtk.entryGetText entry
         styleCtx <- Gtk.widgetGetStyleContext entry
         case t ^? prismToMaybe colourPrism of
            Nothing -> do
               Gtk.styleContextAddClass styleCtx "entry-error"
               return Nothing
            Just v -> do
               Gtk.styleContextRemoveClass styleCtx "entry-error"
               return $ Just v
      setEntryValue w (Just c) = updateText w $ c ^. re colourPrism
      setEntryValue w _ = updateText w ""
      updateText w txt = do
         old <- Gtk.getEntryText w
         when (old /= txt) $ Gtk.setEntryText w txt

renderGadget iconTheme wInit env initial envC e b (Optional _ s@FilePathSelector{}) =
      fmap outFunc <$>
         renderGadget iconTheme wInit env (inFunc <$> initial) envC (fmap inFunc <$> e) (fmap inFunc <$> b) s
   where
      inFunc Nothing = ""
      inFunc (Just str) = str
      outFunc :: String -> Maybe String
      outFunc "" = Nothing
      outFunc str = Just str

renderGadget _ wInit _ initial _ e b (ImageDisplay sz) = {-# SCC "renderGadget_ImageDisplay" #-} do
      imgWidget <- Gtk.imageNew
      liftIO $ wInit imgWidget
      liftIO $ updateImage imgWidget initial
      stop1 <- reactimate1 $ updateImage imgWidget <$> e
      ch <- changes $ updateImage imgWidget <$> b
      stop2 <- reactimate1' ch
      void $ Gtk.onWidgetDestroy imgWidget $ stop1 >> stop2
      w <- Gtk.toWidget imgWidget
      return GadgetOut {
            gadgetView = [w],
            gadgetEvents = never,
            gadgetInitial = initial,
            gadgetUser = e,
            gadgetValue = b
         }
   where
      updateImage _ (GadgetData False _) = return ()
      updateImage w (GadgetData True Nothing) = Gtk.imageSetFromPixbuf w noPixbuf
      updateImage w (GadgetData True (Just bytes)) = do
         stream <- Gio.memoryInputStreamNewFromData bytes Nothing
         Gdk.pixbufNewFromStream stream (Nothing :: Maybe Gio.Cancellable) >>= \case
            Nothing -> return ()
            Just pix1 -> do
               height <- fromIntegral <$> Gdk.pixbufGetHeight pix1
               width <- fromIntegral <$> Gdk.pixbufGetWidth pix1
               if height <= maxHeight && width <= maxWidth
                  then Gtk.imageSetFromPixbuf w $ Just pix1
                  else do
                     let
                        scale = min (maxHeight / height) (maxWidth / width)
                        newWidth = round $ width * scale
                        newHeight = round $ height * scale
                     pix2 <- Gdk.pixbufScaleSimple pix1 newWidth newHeight Gdk.InterpTypeBilinear
                     Gtk.imageSetFromPixbuf w pix2  -- No-op if out of memory. But that is unlikely.
      maxHeight, maxWidth :: Double
      (maxHeight, maxWidth) = case sz of
         MemoSmall -> (100, 150)
         MemoMedium -> (200, 300)
         MemoLarge -> (400, 600)

renderGadget iconTheme wInit env initial envC e b (Optional d g) = {-# SCC "renderGadget_Optional" #-} do
      -- The general case for Optional decorates the inner widget with a check button. If there
      -- are no inner widgets then it just passes Nothing straight through.
      inner <- renderGadget
            iconTheme
            wInit
            env
            (fromMaybe d <$> initial)
            envC
            (fmap (fromMaybe d) <$> e)
            (fmap (fromMaybe d) <$> b)
            g
      let initialEnabled = isJust $ initial ^. gdValue
      widgetM <- boxWidgets wInit Vertical $ gadgetView inner
      case widgetM of
         Nothing -> return $ Just <$> inner
         Just widget -> do
            button <- Gtk.checkButtonNew
            liftIO $ wInit button
            boxWidget <- Gtk.boxNew Gtk.OrientationHorizontal 0
            liftIO $ wInit boxWidget
            Gtk.boxPackStart boxWidget button False False 0
            Gtk.boxPackStart boxWidget widget True True 0
            Gtk.toggleButtonSetActive button initialEnabled
            blockSignals <- liftIO $ newIORef False
            stop1 <- reactimate1 $ updateButton button . isJust . _gdValue <$> e
            ch <- changes $ b <&> (\v -> do
                  writeIORef blockSignals True
                  updateButton button $ isJust $ _gdValue v
                  writeIORef blockSignals False
               )
            stop2 <- reactimate1' ch
            void $ Gtk.onWidgetDestroy button $ stop1 >> stop2
            clicked <- filterJust <$> registerIOSignal button Gtk.onToggleButtonToggled ( do
                  blk <- readIORef blockSignals
                  if blk
                     then return ((), Nothing)
                     else ((),) . Just <$> Gtk.toggleButtonGetActive button
               )
            Gtk.widgetSetSensitive widget initialEnabled
            eventLink widget Gtk.widgetSetSensitive clicked
            enabled1 <- stepper initialEnabled clicked
            innerValue <- stepper (gadgetInitial inner) $ gadgetUser inner
            Gtk.widgetShowAll boxWidget
            w <- Gtk.toWidget boxWidget
            return $ GadgetOut {
                  gadgetView = [w],
                  gadgetEvents = gadgetEvents inner,
                  gadgetInitial = outF initialEnabled $ gadgetInitial inner,
                  gadgetUser = unionWith const
                     (outF <$> enabled1 <@> gadgetUser inner)
                     (flip outF <$> innerValue <@> clicked),
                  gadgetValue = outF <$> enabled1 <*> gadgetValue inner
               }
   where
      updateButton button v = do
         old <- Gtk.toggleButtonGetActive button
         when (old /= v) $ Gtk.toggleButtonSetActive button v
      outF True (GadgetData ok v) = GadgetData ok $ Just v
      outF False _ = GadgetData True Nothing

renderGadget iconTheme wInit env initial envC e b (Intercept dblFlag selector g) =
   {-# SCC "renderGadget_Intercept" #-} mdo
      (activated, handler :: () -> IO ()) <- newEvent
      inner <- renderGadget
            iconTheme
            (buttonIntercept handler)
            env
            initial
            envC
            (unionWith const e $ GadgetData True <$> filterJust (snd <$> r))
            b
            g
      r <- case gadgetView inner of
         w:_ -> mkGtkPopupSelect w iconTheme envC selector $ ((),) . _gdValue <$> b <@ activated
         _ -> return never
      return inner
   where
      buttonIntercept :: (Gtk.IsWidget widget) => (() -> IO ()) -> widget -> IO ()
      buttonIntercept handler w = do
         w1 <- Gtk.toWidget w
         wInit w1
         void $ Gtk.onWidgetButtonPressEvent w1 $ \ev -> do
            t <- Gdk.getEventButtonType ev
            btn <- Gdk.getEventButtonButton ev
            if btn == 1 && (
                  not dblFlag && t == Gdk.EventTypeButtonPress ||
                  dblFlag && t == Gdk.EventType2buttonPress)
               then do
                  handler ()
                  return True
               else
                  return False

renderGadget iconTheme wInit env initial envC e b (Icon iconF g) = {-# SCC "renderGadget_Icon" #-} do
      boxWidget <- Gtk.boxNew Gtk.OrientationHorizontal 0
      inner <- renderGadget iconTheme wInit env initial envC e b g
      liftIO $ wInit boxWidget
      icon1 <- safeLoadIcon iconTheme (iconF $ initial ^. gdValue) iconSize
      imageWidget <- case icon1 of   -- If not found display broken icon rather than nothing.
         Just i -> Gtk.imageNewFromPixbuf $ Just i
         Nothing -> Gtk.imageNewFromIconName Nothing (fromIntegral $ fromEnum Gtk.IconSizeButton)
      Gtk.imageSetPixelSize imageWidget 16
      liftIO $ wInit imageWidget
      Gtk.boxPackStart boxWidget imageWidget False False 0
      innerWidget <- boxWidgets wInit Vertical $ gadgetView inner
      forM_ innerWidget $ \w -> Gtk.boxPackStart boxWidget w True True 0
      eventLink imageWidget Gtk.setImageIconName $ iconF . _gdValue <$> e
      behaviorLink imageWidget Gtk.setImageIconName $ iconF . _gdValue <$> b
      w <- Gtk.toWidget boxWidget
      return $ inner {gadgetView = [w]}

renderGadget iconTheme wInit env initial envC e b (Coloured colourF g) =
   {-# SCC "renderGadget_Coloured" #-} do
      inner <- renderGadget iconTheme wInit env initial envC e b g
      forM_ (gadgetView inner) $ \w ->
         widgetLinkColour
               w
               (colourF $ initial ^. gdValue)
               (colourF . _gdValue <$> e)
               (colourF . _gdValue <$> b)
      return inner

renderGadget iconTheme wInit env initial envC e b (Linked linkF g) =
   {-# SCC "renderGadget_Linked" #-} do
      inner <- renderGadget iconTheme wInit env initial envC e b g
      boxWidget <- Gtk.boxNew Gtk.OrientationHorizontal 0
      liftIO $ wInit boxWidget
      button <- Gtk.linkButtonNewWithLabel "" (Just "")
      liftIO $ wInit button
      img <- Gtk.imageNewFromIconName (Just "emblem-symbolic-link") $
         fromIntegral $ fromEnum Gtk.IconSizeButton
      liftIO $ wInit img
      Gtk.buttonSetImage button $ Just img
      Gtk.buttonSetAlwaysShowImage button True
      Gtk.boxPackStart boxWidget button False False 0
      setLink button $ linkF $ initial ^. gdValue
      eventLink button setLink $ linkF . _gdValue <$> e
      behaviorLink button setLink $ linkF . _gdValue <$> b
      innerWidget <- boxWidgets wInit Vertical $ gadgetView inner
      forM_ innerWidget $ \w -> Gtk.boxPackStart boxWidget w True True 0
      w <- Gtk.toWidget boxWidget
      return $ inner {gadgetView = [w]}
   where
      setLink btn Nothing = do
         Gtk.linkButtonSetUri btn ""
         Gtk.widgetSetTooltipText btn Nothing
         Gtk.widgetSetSensitive btn False
      setLink btn (Just str) = do
         Gtk.linkButtonSetUri btn str
         Gtk.widgetSetTooltipText btn $ Just str
         Gtk.widgetSetSensitive btn True

renderGadget iconTheme wInit env initial envC e b (Styled textF g) = {-# SCC "renderGadget_Styled" #-} do
      inner <- renderGadget iconTheme wInit env initial envC e b g
      case gadgetView inner of
         w:_ -> do
            currentStyle <- liftIO $ newIORef Nothing
            ctxs <- mapM Gtk.widgetGetStyleContext (gadgetView inner)
            liftIO $ updateStyle currentStyle ctxs $ textF $ initial ^. gdValue
            stop1 <- reactimate1 $ updateStyle currentStyle ctxs . textF . _gdValue <$> e
            ch <- changes $ updateStyle currentStyle ctxs . textF . _gdValue <$> b
            stop2 <- reactimate1' ch
            void $ Gtk.onWidgetDestroy w $ stop1 >> stop2
         [] -> return ()
      return inner
   where
      updateStyle currentStyle ctxs newStyle = do
         oldStyle <- readIORef currentStyle
         when (oldStyle /= newStyle) $ do
            forM_ oldStyle $ \txt ->
               forM_ ctxs $ \c -> Gtk.styleContextRemoveClass c txt
            writeIORef currentStyle newStyle
            forM_ newStyle $ \txt ->
               forM_ ctxs $ \c -> Gtk.styleContextAddClass c txt

renderGadget iconTheme wInit env initial envC e b (Frame textF g) = {-# SCC "renderGadget_Frame" #-} do
   inner <- renderGadget iconTheme wInit env initial envC e b g
   boxWidget <- Gtk.frameNew $ textF $ initial ^. gdValue
   liftIO $ wInit boxWidget
   innerWidget <- boxWidgets wInit Vertical $ gadgetView inner
   forM_ innerWidget $ \w -> do
      Gtk.set w [#margin := 5]
      Gtk.containerAdd boxWidget w
   eventLink boxWidget Gtk.frameSetLabel $ textF . _gdValue <$> e
   behaviorLink boxWidget Gtk.frameSetLabel $ textF . _gdValue <$> b
   w <- Gtk.toWidget boxWidget
   return $ inner {gadgetView = [w]}

renderGadget iconTheme wInit env initial envC e b (Form orient items) = {-# SCC "renderGadget_Form" #-} do
      tableWidget <- Gtk.new Gtk.Grid [#rowSpacing := 3, #columnSpacing := 3]
      liftIO $ wInit tableWidget
      let
         addWidgets _ (_, Nothing, v) = return v
         addWidgets row (w1, Just w2, v) = do
            case orient of
               Vertical -> do
                  Gtk.gridAttach tableWidget w1 0 row 1 1
                  Gtk.gridAttach tableWidget w2 1 row 1 1
               Horizontal -> do
                  Gtk.gridAttach tableWidget w1 row 0 1 1
                  Gtk.gridAttach tableWidget w2 row 1 1 1
            return v
      widgetPairs <- mapM renderLabelledGadget items
      results <- zipWithM addWidgets [0..] widgetPairs
      w <- Gtk.toWidget tableWidget
      return (mergeOutputs results) {gadgetView = [w]}
   where
      renderLabelledGadget (txt, g) = do
         label <- Gtk.labelNew $ Just txt
         liftIO $ wInit label
         case orient of
            Vertical -> Gtk.set label [#halign := Gtk.AlignEnd, #valign := Gtk.AlignCenter]
            Horizontal -> Gtk.set label [#halign := Gtk.AlignStart, #valign := Gtk.AlignCenter]
         result <- renderGadget iconTheme wInit env initial envC e b g
         w1 <- Gtk.toWidget label
         w2 <- boxWidgets wInit Vertical $ gadgetView result
         return (w1, w2, result)

renderGadget iconTheme wInit env initial envC e b (TabForm tabs) = {-# SCC "renderGadget_TabForm" #-} do
      book <- Gtk.new Gtk.Notebook [
            #margin := 5,
            #scrollable := True,
            #enablePopup := True
         ]
      liftIO $ wInit book
      results <- forM tabs (addGadget book)
      Gtk.widgetShowAll book
      Gtk.notebookSetCurrentPage book 0
      w <- Gtk.toWidget book
      return (mergeOutputs results) {gadgetView = [w]}
   where
      addGadget book (l, g) = do
         result <- renderGadget iconTheme wInit env initial envC e b g
         widget <- boxWidgets wInit Vertical (gadgetView result) >>= \case
            Nothing -> Gtk.labelNew (Just "") >>= Gtk.toWidget  -- Blank
            Just w -> return w
         lbl <- Gtk.labelNew $ Just l
         liftIO $ wInit lbl
         void $ Gtk.notebookAppendPageMenu book widget (Just lbl) noWidget
         return result

renderGadget iconTheme wInit env initial envC e b (Box orient gss) = {-# SCC "renderGadget_Box" #-} do
   (boxWidget, addSep) <-
      case orient of
         Vertical -> do
            boxWidget <- Gtk.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 5 ]
            liftIO $ wInit boxWidget
            let addSep = do
                  sep <- Gtk.separatorNew Gtk.OrientationHorizontal
                  Gtk.boxPackStart boxWidget sep False False 0
            return (boxWidget, addSep)
         Horizontal -> do
            boxWidget <- Gtk.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 5 ]
            liftIO $ wInit boxWidget
            let addSep = do
                  sep <- Gtk.separatorNew Gtk.OrientationVertical
                  Gtk.boxPackStart boxWidget sep False False 0
            return (boxWidget, addSep)
   outputss <- mapM (mapM $ renderGadget iconTheme wInit env initial envC e b) gss
   let
      widgetss = filter (not . null) $ map (concatMap gadgetView) outputss  -- List of groups
      addGroup = mapM_ $ \w -> Gtk.boxPackStart boxWidget w True True 0
      output = mergeOutputs $ concat outputss
   sequence_ $ intersperse addSep $ map addGroup widgetss
   w <- Gtk.toWidget boxWidget
   return output {gadgetView = [w]}

renderGadget iconTheme wInit env initial envC e b (Grid colH rowH rows) = {-# SCC "renderGadget_Grid" #-} do
      gridWidget <- Gtk.new Gtk.Grid
         [#orientation := Gtk.OrientationHorizontal, #columnSpacing := 1]
      liftIO $ wInit gridWidget
      forM_ (zip [1..] colH) $ \(n, txt) -> do
         lbl <- Gtk.labelNew $ Just txt
         Gtk.gridAttach gridWidget lbl n 0 1 1
      forM_ (zip [1..] rowH) $ \(n, txt) -> do
         lbl <- Gtk.labelNew $ Just txt
         Gtk.gridAttach gridWidget lbl 0 n 1 1
      evs <- forM (zip [1..] rows) $ \(rowNum, row) ->
         forM (zip [1..] row) $ \(colNum, cell) -> do
            cellOut <- renderGadget iconTheme wInit env initial envC e b cell
            cellWidget <- boxWidgets wInit Vertical $ gadgetView cellOut
            forM_ cellWidget $ \w -> Gtk.gridAttach gridWidget w colNum rowNum 1 1
            return cellOut
      w <- Gtk.toWidget gridWidget
      return $ (mergeOutputs $ concat evs) {gadgetView = [w]}

renderGadget iconTheme wInit env initial envC e b (Validate predicate g) =
   {-# SCC "renderGadget_Validate" #-} do
      result <- renderGadget iconTheme wInit env initial envC e b g
      currentState <- liftIO $ newIORef True
      let
         validInitial = validated env initial (gadgetInitial result) ^. gdOk
         validE = validated <$> changesB envC <*> b <@> gadgetUser result
         validB = validated <$> changesB envC <*> b <*> gadgetValue result
      forM_ (gadgetView result) $ \widget -> do
         style <- Gtk.widgetGetStyleContext widget
         liftIO $ showValid currentState style validInitial
         stop1 <- reactimate1 (showValid currentState style . view gdOk <$> validE)
         ch <- changes $ showValid currentState style . view gdOk <$> validB
         stop2 <- reactimate1' ch
         void $ Gtk.onWidgetDestroy widget $ stop1 >> stop2
      return result {
            gadgetInitial = gdOk %~ (&& validInitial) $ gadgetInitial result,
            gadgetUser = validE,
            gadgetValue = validB
         }
   where
      validated env1 v1 (GadgetData valid v2) =
         GadgetData (valid && predicate env1 (_gdValue v1) v2) v2
      showValid current style new = do
         c <- readIORef current
         when (c /= new) $ do
            showValid1 style new
            writeIORef current new
      showValid1 style False = Gtk.styleContextAddClass style "entry-error"
      showValid1 style True = Gtk.styleContextRemoveClass style "entry-error"

renderGadget iconTheme wInit env initial envC e b (ValidateText messageF g) =
   {-# SCC "renderGadget_ValidateText" #-} do
      result <- renderGadget iconTheme wInit env initial envC e b g
      boxWidgets wInit Vertical (gadgetView result) >>= \case
         Nothing -> return result
         Just appWidget -> do
            boxWidget <- Gtk.boxNew Gtk.OrientationVertical 3
            liftIO $ wInit boxWidget
            let
               initialErr = messageF1 env initial $ gadgetInitial result
               updateE = messageF1 <$> changesB envC <*> b <@> gadgetUser result
               updateB = messageF1 <$> changesB envC <*> b <*> gadgetValue result
               initialValid = validF env initial (gadgetInitial result) ^. gdOk
               validE = validF <$> changesB envC <*> b <@> gadgetUser result
               validB = validF <$> changesB envC <*> b <*> gadgetValue result
            msg <- Gtk.labelNew initialErr
            liftIO $ wInit msg
            style <- Gtk.widgetGetStyleContext msg
            Gtk.boxPackStart boxWidget appWidget True True 0
            Gtk.boxPackStart boxWidget msg False False 0
            showValid style $ isNothing initialErr
            Gtk.widgetSetVisible msg $ isJust initialErr
            stop1 <- reactimate1 $ updateMessage style msg <$> updateE
            ch <- changes $ updateMessage style msg <$> updateB
            stop2 <- reactimate1' ch
            void $ Gtk.onWidgetDestroy msg $ stop1 >> stop2
            w <- Gtk.toWidget boxWidget
            return result {
                  gadgetView = [w],
                  gadgetInitial = gdOk %~ (&& initialValid) $ gadgetInitial result,
                  gadgetUser = validE,
                  gadgetValue = validB
               }
   where
      updateMessage style msg (Just txt) = do
         showValid style False
         Gtk.labelSetLabel msg txt
         Gtk.widgetShow msg
      updateMessage style msg Nothing = do
         showValid style True
         Gtk.labelSetLabel msg ""
         Gtk.widgetHide msg
      messageF1 env1 v1 v2 = messageF env1 (_gdValue v1) (_gdValue v2)
      validF env1 v1 (GadgetData valid v2) =
         GadgetData (valid && isNothing (messageF env1 (_gdValue v1) v2)) v2
      showValid style True = Gtk.styleContextRemoveClass style "entry-error"
      showValid style False = Gtk.styleContextAddClass style "entry-error"

renderGadget _ wInit env initial envC e b (TextBox prsmF) = {-# SCC "renderGadget_TextBox" #-} do
      currentPrism <- liftIO $ newIORef $ prsmF env
      let
         initialText = view (re $ prsmF env) <$> initial
         width = min 20 $ max 5 $ T.length $ initialText ^. gdValue
      entry <- Gtk.new Gtk.Entry [#hexpand := True, #widthChars := fromIntegral width]
      stop <- reactimate1 $ writeIORef currentPrism . prsmF <$> changesE envC
      void $ Gtk.onWidgetDestroy entry stop
      liftIO $ wInit entry
      styleCtx <- Gtk.widgetGetStyleContext entry
      (w, resultE) <- makeField
            entry
            entryChangedSignal
            updateEntry
            initialText
            ((\env1 -> gdValue %~ view (re $ prsmF env1)) <$> changesB envC <@> e)
            ((\env1 -> gdValue %~ view (re $ prsmF env1)) <$> changesB envC <*> b)
            $ do
               p <- readIORef currentPrism
               t <- Gtk.entryGetText entry
               case t ^? p of
                  Nothing -> do
                     Gtk.styleContextAddClass styleCtx "entry-error"
                     return Nothing
                  Just v -> do
                     Gtk.styleContextRemoveClass styleCtx "entry-error"
                     return $ Just v
      let resultE1 = unionWith const resultE e
      return $ GadgetOut {
            gadgetView = [w],
            gadgetEvents = never,
            gadgetInitial = initial,
            gadgetUser = resultE1,
            gadgetValue = b
         }
   where
      -- Update only if text has changed. Do not overwrite leading or trailing spaces in the widget.
      -- Do not overwrite if the widget has focus, as this implies that the user is trying to type.
      updateEntry w newTxt = do
         userEdit <- Gtk.getWidgetHasFocus w
         unless userEdit $ do  -- Don't fight with the user about the text they are typing.
            oldTxt <- Gtk.entryGetText w
            when (T.strip oldTxt /= T.strip newTxt) $ Gtk.setEntryText w newTxt

renderGadget _ wInit _ initial _ e b DisplayText = {-# SCC "renderGadget_DisplayText" #-} do
      -- Construct label in a button.
      label <- Gtk.new Gtk.Label [
            #label := initial ^. gdValue,
            #wrap := True,
            #halign := Gtk.AlignFill,
            #xalign := 0,
            #selectable := True]
      btn <- Gtk.new Gtk.Button [#halign := Gtk.AlignFill, #relief := Gtk.ReliefStyleNone]
      Gtk.containerAdd btn label
      frame1 <- Gtk.frameNew Nothing
      Gtk.containerAdd frame1 btn
      -- Configure internal behavior.
      pop <- Gtk.popoverNew $ Just btn
      entry <- Gtk.new Gtk.Entry [#hexpand := True, #widthChars := 40]
      void $ Gtk.on entry #activate $ Gtk.popoverPopdown pop
      clickE <- registerIOSignal btn Gtk.onButtonClicked $ return ((), ())
      stop1 <- reactimate1 $ popupField pop entry . view gdValue <$> b <@ clickE
      void $ Gtk.onWidgetDestroy btn $ do  -- Popover and its entry are not destroyed by default.
         stop1
         Gtk.widgetDestroy pop
         Gtk.widgetDestroy entry
      Gtk.containerAdd pop entry
      Gtk.widgetShowAll entry
      liftIO $ wInit label
      liftIO $ wInit btn
      liftIO $ wInit pop
      liftIO $ wInit entry
      (_, resultE) <-
         makeField entry Gtk.onEntryActivate (updateWidgets label) initial e b $
            Just <$> Gtk.entryGetText entry
      w <- Gtk.toWidget frame1
      return $ GadgetOut {
            gadgetView = [w],
            gadgetEvents = never,
            gadgetInitial = initial,
            gadgetUser = resultE,
            gadgetValue = b
         }
   where
      updateWidgets label entry txt = do
         oldText <- Gtk.labelGetLabel label
         when (txt /= oldText) $ do
            Gtk.set label [#label := txt]
            Gtk.set entry [#text := txt]
      popupField pop entry txt = do
         Gtk.entrySetText entry txt
         Gtk.popoverPopup pop

renderGadget _ wInit _ initial _ e b (MemoBox size vExpand) = {-# SCC "renderGadget_MemoBox" #-} do
      scroll <- Gtk.scrolledWindowNew noAdjustment noAdjustment
      liftIO $ wInit scroll
      style1 <- Gtk.widgetGetStyleContext scroll
      Gtk.styleContextAddClass style1 $ memoBoxCss size
      Gtk.set scroll [
         #propagateNaturalHeight := True,
         #propagateNaturalWidth := True,
         #hexpand := True,
         #vexpand := vExpand ]
      Gtk.setScrolledWindowShadowType scroll Gtk.ShadowTypeIn
      vw <- Gtk.textViewNew
      liftIO $ wInit vw
      Gtk.set vw [
         #hexpand := True,
         #vexpand := vExpand,
         #wrapMode := Gtk.WrapModeWord,
         #inputHints := [Gtk.InputHintsSpellcheck]]
      Gtk.textViewSetWrapMode vw Gtk.WrapModeWord
      Gtk.containerAdd scroll vw
      buffer <- Gtk.textViewGetBuffer vw
      Gtk.setTextBufferText buffer $ initial ^. gdValue
      blockSignals <- liftIO $ newIORef False
      stop1 <- reactimate1 $ updateBuffer buffer <$> e
      ch <- changes $ b <&> (\v -> do
            writeIORef blockSignals True
            updateBuffer buffer v
            writeIORef blockSignals False
         )
      stop2 <- reactimate1' ch
      void $ Gtk.onWidgetDestroy vw $ stop1 >> stop2
      Gtk.widgetQueueResize vw
      textEdits <- filterJust <$> registerIOSignal buffer Gtk.onTextBufferEndUserAction ( do
            bFlag <- readIORef blockSignals
            if bFlag
               then return ((), Nothing)
               else do
                  newText <- Gtk.getTextBufferText buffer
                  return ((), GadgetData True <$> newText)
         )
      w <- Gtk.toWidget scroll
      return  GadgetOut {
            gadgetView = [w],
            gadgetEvents = never,
            gadgetInitial = initial,
            gadgetUser = textEdits,
            gadgetValue = b
         }
   where
      updateBuffer _ (GadgetData False _) = return ()
      updateBuffer buf (GadgetData True v) = do
         old <- Gtk.getTextBufferText buf
         when (Just v /= old) $ Gtk.setTextBufferText buf v

renderGadget _ wInit _ initial _ e b DisplayMemo = {-# SCC "renderGadget_DisplayMemo" #-} do
      -- Construct label in a button.
      label <- Gtk.new Gtk.Label [
            #label := initial ^. gdValue,
            #wrap := True,
            #halign := Gtk.AlignFill,
            #xalign := 0,
            #selectable := True]
      btn <- Gtk.new Gtk.Button [#halign := Gtk.AlignFill, #relief := Gtk.ReliefStyleNone]
      Gtk.containerAdd btn label
      frame1 <- Gtk.frameNew Nothing
      Gtk.containerAdd frame1 btn
      -- Configure internal behavior.
      pop <- Gtk.popoverNew $ Just btn
      popBox <- Gtk.boxNew Gtk.OrientationVertical 1
      popButton <- Gtk.buttonNewWithLabel "Ok"
      entry <- Gtk.textViewNew
      Gtk.set entry [
         #hexpand := True,
         #wrapMode := Gtk.WrapModeWord,
         #inputHints := [Gtk.InputHintsSpellcheck]]
      Gtk.textViewSetWrapMode entry Gtk.WrapModeWord
      scroll <- Gtk.scrolledWindowNew
            (Nothing :: Maybe Gtk.Adjustment)
            (Nothing :: Maybe Gtk.Adjustment)
      Gtk.containerAdd scroll entry
      Gtk.widgetGetStyleContext scroll >>= (`Gtk.styleContextAddClass` "memo-large")
      Gtk.boxPackStart popBox scroll True True 0
      Gtk.boxPackStart popBox popButton False False 0
      Gtk.widgetShowAll popBox
      buffer <- Gtk.textViewGetBuffer entry
      Gtk.setTextBufferText buffer $ initial ^. gdValue
      clickE <- registerIOSignal btn Gtk.onButtonClicked $ return ((), ())
      stop1 <- reactimate1 $ popupField pop buffer . view gdValue <$> b <@ clickE
      void $ Gtk.onWidgetDestroy btn $ do  -- Popover and its entry are not destroyed by default.
         stop1
         Gtk.widgetDestroy pop
         Gtk.widgetDestroy entry
         Gtk.widgetDestroy popButton
         Gtk.widgetDestroy popBox
      Gtk.containerAdd pop popBox
      Gtk.widgetShowAll entry
      liftIO $ wInit label
      liftIO $ wInit btn
      liftIO $ wInit pop
      liftIO $ wInit popBox
      liftIO $ wInit popButton
      liftIO $ wInit entry
      (_, resultE) <-
         makeField popButton Gtk.onButtonClicked (updateWidgets label buffer) initial e b $ do
            Gtk.popoverPopdown pop
            Gtk.getTextBufferText buffer
      w <- Gtk.toWidget frame1
      return $ GadgetOut {
            gadgetView = [w],
            gadgetEvents = never,
            gadgetInitial = initial,
            gadgetUser = resultE,
            gadgetValue = b
         }
   where
      updateWidgets label buffer _ txt = do
         oldText <- Gtk.labelGetLabel label
         when (txt /= oldText) $ do
            Gtk.set label [#label := txt]
            Gtk.set buffer [#text := txt]
      popupField pop buf txt = do
         Gtk.textBufferSetText buf txt (-1)
         Gtk.popoverPopup pop

renderGadget iconTheme wInit env initial _ e b (Combo entriesF) = {-# SCC "renderGadget_Combo" #-} do
      store <- MV.seqStoreNew $ makeEntries env
      MV.customStoreSetColumn store (MV.makeColumnIdString 0) (view _1) -- Item name
      MV.customStoreSetColumn store (MV.makeColumnIdString 1) (view _2) -- Icon name
      MV.customStoreSetColumn store (MV.makeColumnIdString 2) (view _3) -- Colour in CSS format.
      MV.customStoreSetColumn store (MV.makeColumnIdBool 3) (view _4) -- True for white
      menuWidget <- Gtk.comboBoxNewWithModel store
      -- Kludge to stop combo boxes responding to scroll events.
      void $ Gtk.onWidgetScrollEvent menuWidget $ const $ return True
      -- End kludge
      setContents store env
      liftIO $ wInit menuWidget
      menuText <- Gtk.cellRendererTextNew
      menuIcon <- Gtk.cellRendererPixbufNew
      when (any (isJust . menuItemIcon) (entriesF env)) $ do
         Gtk.cellLayoutPackStart menuWidget menuIcon False
         MV.cellLayoutSetDataFunction menuWidget menuIcon store $ \i -> do
            safeLoadIcon iconTheme (i ^. _2) iconSize >>= \case
               Just pb -> Gtk.set menuIcon [#pixbuf := pb]
               Nothing -> Gtk.set menuIcon [#iconName := ""]
      Gtk.cellLayoutPackStart menuWidget menuText True
      MV.cellLayoutSetDataFunction menuWidget menuText store $ \(nm, _, clr, fg, _) -> do
         if clr /= ""
            then Gtk.set menuText [
                  #background := clr,
                  #foreground := if fg then "white" else "black" ]
            else do
               Gtk.clearCellRendererTextForeground menuText
               Gtk.clearCellRendererTextBackground menuText
         Gtk.set menuText [#text := nm]
      let
         displayVal v = do
            entries <- MV.seqStoreToList store
            return $ fromIntegral $ fromMaybe (-1) $ elemIndex v $ map (view _5) entries
      (w, resultE) <- makeField
            menuWidget
            Gtk.onComboBoxChanged
            (\w v -> do
               sel <- Gtk.comboBoxGetActive menuWidget
               n <- displayVal v
               when (n /= sel) $ displayVal v >>= Gtk.comboBoxSetActive w)
            initial
            e
            b
            $ do
               sel <- Gtk.comboBoxGetActive menuWidget
               entries <- MV.seqStoreToList store
               let limit = genericLength entries
               if sel >= 0 && sel < limit
                  then return $ Just $ genericIndex entries sel ^. _5
                  else return Nothing
      return GadgetOut {
            gadgetView = [w],
            gadgetEvents = never,
            gadgetInitial = initial,
            gadgetUser = resultE,
            gadgetValue = b
         }
   where
      setContents store env1 = do
         MV.seqStoreClear store
         forM_ (makeEntries env1) $ \i -> MV.seqStoreAppend store i
      makeEntries env1 =
         map
            (\(ComboItem nm i clr v) ->
               (nm, fromMaybe blankIconName i, maybe "" toRGBA clr, maybe True toTextRGB clr, v))
            (entriesF env1)
      toRGBA c =
         let C.RGB r g b1 = C.toSRGB24 $ getColour c
         in T.pack $ "rgba(" <> show r <> "," <> show g <> "," <> show b1 <> ",1)"
      toTextRGB c = C.luminance (getColour c) < 0.5

renderGadget _ wInit env initial _ e b (Radio f) = {-# SCC "renderGadget_Radio" #-} do
      let items = f env
      blocker <- liftIO $ newIORef False
      (buttons, clicks) <- unzip <$> mapM (mkButton blocker) items
      let setGroup = setButtons buttons (map snd items)
      stop1 <- reactimate1 $ setGroup <$> e
      ch <- changes $ b <&> \v -> do
         writeIORef blocker True
         setGroup v
         writeIORef blocker False
      stop2 <- reactimate1' ch
      case buttons of
         [] -> return ()
         b1:bs -> do
            mapM_ (`Gtk.radioButtonJoinGroup` Just b1) bs
            void $ Gtk.onWidgetDestroy b1 $ stop1 >> stop2
      widgets <- mapM Gtk.toWidget buttons
      return GadgetOut {
            gadgetView = widgets,
            gadgetEvents = never,
            gadgetInitial = initial,
            gadgetUser = foldr (unionWith const) never clicks,
            gadgetValue = b
         }
   where
      setButtons _ _ (GadgetData False _) = return ()
      setButtons btns vals (GadgetData True v) =
         zipWithM_ (\btn btnVal -> Gtk.toggleButtonSetActive btn $ btnVal == v) btns vals
      mkButton blocker (txt, v) = do
         btn <- Gtk.radioButtonNewWithLabel ([] :: [Gtk.RadioButton]) txt
         liftIO $ wInit btn
         Gtk.toggleButtonSetActive btn $ v == (initial ^. gdValue)
         usr <- registerIOSignal btn Gtk.onToggleButtonToggled $ do
            blocked <- readIORef blocker
            if blocked then return ((), Nothing) else do
               r <- Gtk.toggleButtonGetActive btn
               return $ if r then ((), Just $ GadgetData True v) else ((), Nothing)
         return (btn, filterJust usr)

renderGadget _ wInit _ initial _ e b TickBox = {-# SCC "renderGadget_TickBox" #-} do
      button <- Gtk.checkButtonNew
      liftIO $ wInit button
      (w, resultE) <-
         makeField button Gtk.afterToggleButtonToggled updateToggle initial e b $
            Just <$> Gtk.toggleButtonGetActive button
      return GadgetOut {
            gadgetView = [w],
            gadgetEvents = never,
            gadgetInitial = initial,
            gadgetUser = resultE,
            gadgetValue = b
         }
   where
      updateToggle btn v = do
         old <- Gtk.toggleButtonGetActive btn
         when (old /= v) $ Gtk.toggleButtonSetActive btn v

renderGadget _ wInit env initial envC e b (Message textF) = {-# SCC "renderGadget_Message" #-} do
   label <- Gtk.labelNew $ Just $ textF env $ initial ^. gdValue
   liftIO $ wInit label
   let
      update env1 (GadgetData True v1) = do
         old <- Gtk.labelGetText label
         let new = textF env1 v1
         when (old /= new) $ Gtk.labelSetText label new
      update _ (GadgetData False _) = return ()
   stop1 <- reactimate1 $ update <$> changesB envC <@> e
   ch <- changes $ update <$> changesB envC <*> b
   stop2 <- reactimate1' ch
   void $ Gtk.onWidgetDestroy label $ stop1 >> stop2
   w <- Gtk.toWidget label
   return GadgetOut {
         gadgetView = [w],
         gadgetEvents = never,
         gadgetInitial = initial,
         gadgetUser = e,
         gadgetValue = b
      }

renderGadget iconTheme wInit env initial envC e b (Scrolled g) = {-# SCC "renderGadget_Scrolled" #-} do
   result <- renderGadget iconTheme wInit env initial envC e b g
   widget <- boxWidgets wInit Vertical $ gadgetView result
   scroll <- forM widget $ \w -> do
      scroll <- Gtk.new Gtk.ScrolledWindow [
            #propagateNaturalHeight := True,
            #propagateNaturalWidth := True
         ]
      liftIO $ wInit scroll
      Gtk.containerAdd scroll w
      Gtk.toWidget scroll
   return result {gadgetView = maybeToList scroll}

renderGadget iconTheme wInit env initial envC e b (FixedText displayF nested) =
   {-# SCC "renderGadget_FixedText" #-} do
      let
         initText = displayF env $ initial ^. gdValue
         width = min 20 $ max 5 $ T.length initText
      entry <- Gtk.new Gtk.Entry [#hexpand := True, #widthChars := fromIntegral width]
      liftIO $ wInit entry
      Gtk.setEntryText entry initText
      Gtk.setEntryEditable entry False
      editedValue <- fmap (GadgetData True) . filterJust . fmap snd <$> case nested of
         Nothing -> return never
         Just selector -> do
            -- We can assume that the Entry widget already sets the ButtonPressMask for its window.
            activated <- registerIOSignal1 entry Gtk.onWidgetButtonPressEvent $ \ev -> do
               t <- Gdk.getEventButtonType ev
               btn <- Gdk.getEventButtonButton ev
               if btn == 1 && t == Gdk.EventType2buttonPress
                  then return (True, Just ())
                  else return (False, Nothing)
            mkGtkPopupSelect entry iconTheme envC selector $ ((),) . _gdValue <$>
                  b <@ filterJust activated
      let
         allEvents = unionWith const editedValue e
         update w (env1, GadgetData True v1) = Gtk.setEntryText w $ displayF env1 v1
         update _ (_, GadgetData False _) = return ()
      eventLink entry update $ (,) <$> changesB envC <@> allEvents
      behaviorLink entry update $ (,) <$> changesB envC <*> b
      w <- Gtk.toWidget entry
      return $ GadgetOut [w] never initial editedValue b

renderGadget iconTheme wInit env initial envC e b (FixedMemo size vExpand displayF nested) =
   {-# SCC "renderGadget_FixedMemo" #-} do
      scroll <- Gtk.scrolledWindowNew noAdjustment noAdjustment
      liftIO $ wInit scroll
      style <- Gtk.widgetGetStyleContext scroll
      Gtk.styleContextAddClass style $ memoBoxCss size
      Gtk.set scroll [
            #propagateNaturalHeight := True,
            #propagateNaturalWidth := True
         ]
      Gtk.setScrolledWindowShadowType scroll Gtk.ShadowTypeIn
      vw <- Gtk.new Gtk.TextView [
            #hexpand := True,
            #vexpand := vExpand,
            #wrapMode := Gtk.WrapModeWord,
            #editable := False
         ]
      liftIO $ wInit vw
      Gtk.containerAdd scroll vw
      buffer <- Gtk.textViewGetBuffer vw
      let
         updateBuffer _ (GadgetData False _) = return ()
         updateBuffer env1 (GadgetData True v) = do
            let txt = displayF env1 v
            old <- Gtk.getTextBufferText buffer
            when (old /= Just txt) $ Gtk.textBufferSetText buffer txt (-1)
      Gtk.textBufferSetText buffer (displayF env $ initial ^. gdValue) (-1)
      (editedValue :: Event (GadgetData a)) <- case nested of
         Nothing -> return never
         Just selector -> do
            activated <- registerIOSignal1 vw Gtk.onWidgetButtonPressEvent $ \ev -> do
               btn <- Gdk.getEventButtonButton ev
               t <- Gdk.getEventButtonType ev
               if btn == 1 && t == Gdk.EventType2buttonPress
                  then return (True, Just ())
                  else return (False, Nothing)
            fmap (GadgetData True) . filterJust . fmap snd <$> mkGtkPopupSelect
                  vw
                  iconTheme
                  envC
                  selector
                  (((),) . _gdValue <$> b <@ activated)
      let allEvents = unionWith const editedValue e
      stop1 <- reactimate1 $ updateBuffer <$> changesB envC <@> allEvents
      ch <- changes $ updateBuffer <$> changesB envC <*> b
      stop2 <- reactimate1' ch
      void $ Gtk.onWidgetDestroy vw $ stop1 >> stop2
      w <- Gtk.toWidget scroll
      return GadgetOut {
            gadgetView = [w],
            gadgetEvents = never,
            gadgetInitial = initial,
            gadgetUser = editedValue,
            gadgetValue = b
         }

renderGadget _ wInit _ initial _ e b ClickableList = {-# SCC "renderGadget_ClickableList" #-} do
      evBox <- Gtk.new Gtk.EventBox [#visibleWindow := True]
      label <- Gtk.labelNew Nothing
      ctx <- Gtk.widgetGetStyleContext label
      Gtk.styleContextAddClass ctx "clickable-list"
      Gtk.containerAdd evBox label
      Gtk.set label [
            #wrap := True,
            #justify := Gtk.JustificationLeft,
            #hexpand := True,
            #xalign := 0]
      liftIO $ wInit label
      liftIO $ wInit evBox
      clickMapRef <- liftIO $ displayClickables label initial >>= newIORef
      Gtk.widgetAddEvents evBox [Gdk.EventMaskButtonPressMask]
      stop1 <- reactimate1 $ e <&> (displayClickables label >=> writeIORef clickMapRef)
      ch <- changes $ b <&> (displayClickables label >=> writeIORef clickMapRef)
      stop2 <- reactimate1' ch
      void $ Gtk.onWidgetDestroy label $ stop1 >> stop2
      mouseEvent <- registerIOSignal1 evBox Gtk.onWidgetButtonPressEvent $ \ev -> do
         (lx, ly) <- Gtk.labelGetLayoutOffsets label
         t <- Gdk.getEventButtonType ev
         btn <- Gdk.getEventButtonButton ev
         (px, py) <- (,) <$> Gdk.getEventButtonX ev <*> Gdk.getEventButtonY ev
         let posn1 = (px - fromIntegral lx, py - fromIntegral ly)
         getLabelItem label clickMapRef posn1 >>= \case
            Just item ->
               if btn == 1
                  then if t == Gdk.EventTypeButtonPress
                     then return (True, Just (1 :: Int, item))  -- Single click
                     else if t == Gdk.EventType2buttonPress
                        then return (True, Just (2, item))  -- Double click
                        else return (False, Nothing)
                  else if btn == 3 && t == Gdk.EventTypeButtonPress
                     then return (True, Just (3, item))  -- Right click.
                     else return (False, Nothing)
            Nothing -> return (False, Nothing)
      let
         mouseEvent1 = filterJust mouseEvent
         singleClickEvent = clickSingle . snd <$> filterE ((1 ==) . fst) mouseEvent1
         doubleClickEvent = clickDouble . snd <$> filterE ((2 ==) . fst) mouseEvent1
         menuEvent = clickMenu . snd <$> filterE ((3 ==) . fst) mouseEvent1
      menuDisplay <- execute $ mkGtkMenu <$> filterJust menuEvent
      popupMenuOn $ fst <$> menuDisplay
      menuResult <- switchE $ snd <$> menuDisplay
      void $ Gtk.onWidgetDestroy label stop1
      let resultE = GadgetData True <$> foldr1 (unionWith const) [
               singleClickEvent,
               doubleClickEvent,
               Just <$> menuResult
            ]
      w <- Gtk.toWidget evBox
      return GadgetOut {
            gadgetView = [w],
            gadgetEvents = never,
            gadgetInitial = GadgetData True Nothing,
            gadgetUser = resultE,
            gadgetValue = pure $ GadgetData True Nothing
         }
   where
      getLabelItem :: Gtk.Label -> IORef (Word32 -> Maybe (GtkClickable a)) -> (Double, Double)
         -> IO (Maybe (ClickableItem a))
      getLabelItem lbl ref (x, y) = do
         idxFunc <- readIORef ref
         layout <- Gtk.labelGetLayout lbl
         x1 <- Pango.unitsFromDouble x
         y1 <- Pango.unitsFromDouble y
         (validIdx, idx, _) <- Pango.layoutXyToIndex layout x1 y1
         if validIdx
            then return $ gclickBase <$> idxFunc (fromIntegral idx)
            else return Nothing

renderGadget _ wInit _ initial _ e b (DateBox fmt) = {-# SCC "renderGadget_DateBox" #-} do
   let
      prsm :: Prism' Text Day
      prsm = datePrism fmt
   (w, ev) <- textBoxWithPopup wInit initial e b prsm $ \entry -> do
      (overlay, calendar) <- createDatePopover prsm entry
      void $ Gtk.onWidgetDestroy entry $ do
         Gtk.widgetDestroy overlay
         Gtk.widgetDestroy calendar
      registerIOSignal calendar Gtk.onCalendarDaySelectedDoubleClick $ do
         (year, month, day) <- Gtk.calendarGetDate calendar
         let date = fromGregorian (fromIntegral year) (fromIntegral month + 1) (fromIntegral day)
         Gtk.popoverPopdown overlay
         return ((), date ^. re prsm)
         -- Design note: callback ought to be integrated into createDatePopover once it no longer
         -- has to support the legacy dialogs.
   return GadgetOut {
         gadgetView = [w],
         gadgetEvents = never,
         gadgetInitial = initial,
         gadgetUser = ev,
         gadgetValue = b
      }

renderGadget iconTheme wInit _ initial _ e b (IconBox predicate) = {-# SCC "renderGadget_IconBox" #-} do
      currentName <- liftIO $ newIORef $ initial ^. gdValue
      icon1 <- safeLoadIcon iconTheme (initial ^. gdValue) iconSize
      pic <- Gtk.imageNewFromPixbuf icon1
      Gtk.set pic [#sensitive := True]
      liftIO $ wInit pic
      button <- Gtk.new Gtk.Button [#image := pic, #alwaysShowImage := True]
      liftIO $ wInit button
      (nameE, handler) <- newEvent
      stop1 <- reactimate1 $ updateImage currentName pic <$> e
      ch <- changes $ updateImage currentName pic <$> b
      stop2 <- reactimate1' ch
      void $ Gtk.onWidgetDestroy button $ stop1 >> stop2
      void $ Gtk.onButtonPressed button $ do
         -- Debug code
         ps <- Gtk.iconThemeGetSearchPath iconTheme
         putStrLn $ "IconBox theme search path = " <> show ps
         -- End debug
         icons <- withWaitCursor button $ iconThemeContents iconTheme False predicate
         iconDialog pic icons $ \newIcon -> do
            Gtk.set pic [#iconName := newIcon]
            handler $ GadgetData True newIcon
      w <- Gtk.toWidget button
      return GadgetOut {
            gadgetView = [w],
            gadgetEvents = never,
            gadgetInitial = initial,
            gadgetUser = unionWith const nameE e,
            gadgetValue = b
         }
   where
      updateImage _ _ (GadgetData False _) = return ()
      updateImage ref pic (GadgetData True v) = do
         oldName <- readIORef ref
         when (oldName /= v) $ do
            writeIORef ref v
            safeLoadIcon iconTheme v iconSize >>= \case
               Just icon1 -> Gtk.imageSetFromPixbuf pic $ Just icon1
               Nothing -> return ()

renderGadget _ wInit _ initial _ e b ColourBox = {-# SCC "renderGadget_ColourBox" #-} do
      entry <- Gtk.new Gtk.Entry [#hexpand := True]
      liftIO $ wInit entry
      widgetLinkColour
            entry
            (Just $ initial ^. gdValue)
            (Just . _gdValue <$> e)
            (Just . _gdValue <$> b)
      Gtk.entrySetIconFromIconName
         entry
         Gtk.EntryIconPositionSecondary
         $ Just "applications-science"
      (w, result) <- makeField
         entry
         entryChangedSignal
         setEntryValue
         initial
         e
         b
         $ getValue entry
      void $ Gtk.onEntryIconPress entry $ \_ _ -> do
         chooser <- createColorDialog entry
         r <- toEnum . fromIntegral <$> Gtk.dialogRun chooser
         case r of
            Gtk.ResponseTypeOk -> do
               colour <- rgbaToColour =<< Gtk.colorChooserGetRgba chooser
               setEntryValue entry colour
               Gtk.widgetGrabFocus entry   -- So that FocusOutEvent will be triggered.
            _ -> return ()
         Gtk.widgetDestroy chooser
      return GadgetOut {
            gadgetView = [w],
            gadgetEvents = never,
            gadgetInitial = initial,
            gadgetUser = result,
            gadgetValue = b
         }
   where
      getValue :: Gtk.Entry -> IO (Maybe Colour)
      getValue entry = do
         styleCtx <- Gtk.widgetGetStyleContext entry
         t <- Gtk.entryGetText entry
         case t ^? colourPrism of
            Nothing -> do
               Gtk.styleContextAddClass styleCtx "entry-error"
               return Nothing
            Just v -> do
               Gtk.styleContextRemoveClass styleCtx "entry-error"
               return $ Just v
      setEntryValue w c = do
         old <- Gtk.getEntryText w
         let new = c ^. re colourPrism
         when (old /= new) $ Gtk.setEntryText w new

renderGadget _ wInit env initial _ e b (TreeSelector forestF) =
   {-# SCC "renderGadget_TreeSelector" #-} do
      let items1 = trimForest $ forestF env
      store <- Gtk.treeStoreNew [Gtk.gtypeString, Gtk.gtypeString]
         -- Contents are name and optional tooltip
      pairs <- addStoreForest store Nothing items1
      -- "store" contains names. "pairs" maps store paths to actual values.
      let
         idx = M.fromList pairs
         revIndex = M.fromList $ map (\(x,y) -> (y,x)) pairs
         -- setSelection :: (Ord a) =>
         --    Gtk.TreeView -> Gtk.TreeSelection -> Set a -> IO ()
         setSelection _ _ (GadgetData False _) = return ()
         setSelection vw sel (GadgetData True vs) = do
            (oldGtkPaths, _) <- Gtk.treeSelectionGetSelectedRows sel
            oldPaths <- S.fromList . catMaybes <$> mapM Gtk.treePathGetIndices oldGtkPaths
            let newPaths = mapMaybe (`M.lookup` revIndex) $ S.toList vs
            when (S.fromList newPaths /= oldPaths) $ do
               paths <- mapM Gtk.treePathNewFromIndices newPaths
               Gtk.treeSelectionUnselectAll sel
               forM_ paths $ \path -> do
                  Gtk.treeViewExpandToPath vw path
                  Gtk.treeSelectionSelectPath sel path
      scroll <- Gtk.scrolledWindowNew noAdjustment noAdjustment
      Gtk.set scroll [
            #propagateNaturalHeight := True,
            #propagateNaturalWidth := True
         ]
      liftIO $ wInit scroll
      Gtk.setScrolledWindowShadowType scroll Gtk.ShadowTypeIn
      vw <- Gtk.treeViewNewWithModel store
      Gtk.set vw [
            #hexpand := True,
            #vexpand := True,
            #heightRequest := 300  -- Not good to hard code a size, but default is too small.
         ]
      liftIO $ wInit vw
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
      liftIO $ setSelection vw selection initial
      Gtk.treeSelectionSetSelectFunction selection $ Just $ selectionFunc idx
      blockSignals <- liftIO $ newIORef False
      selectionE <- filterJust <$> registerIOSignal selection Gtk.onTreeSelectionChanged ( do
            blockFlag <- readIORef blockSignals
            if blockFlag
               then return ((), Nothing)
               else do
                  (gtkPaths, _) <- Gtk.treeSelectionGetSelectedRows selection
                  paths <- S.fromList . mapMaybe (`M.lookup` idx) . catMaybes <$>
                        mapM Gtk.treePathGetIndices gtkPaths
                  return ((), Just $ GadgetData True paths)
         )
      stop1 <- reactimate1 $ setSelection vw selection <$> e
      ch <- changes $ b <&> (\v -> do
            writeIORef blockSignals True
            setSelection vw selection v
            writeIORef blockSignals False
         )
      stop2 <- reactimate1' ch
      void $ Gtk.onWidgetDestroy vw $ stop1 >> stop2
      Gtk.containerAdd scroll vw
      w <- Gtk.toWidget scroll
      return GadgetOut {
            gadgetView = [w],
            gadgetEvents = never,
            gadgetInitial = initial,
            gadgetUser = selectionE,
            gadgetValue = b
         }
   where
      selectionFunc :: Map [Int32] a -> Gtk.TreeSelectionFunc
      selectionFunc tbl _ _ path old = do
         p2 <- fromMaybe [] <$> Gtk.treePathGetIndices path
         let r = (p2 `M.member` tbl) || old  -- Always allow deselection
         return r
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
         Gtk.TreeStore -> Maybe Gtk.TreeIter -> Forest (Text, Maybe Text, Maybe a)
         -> m [([Int32], a)]
      addStoreForest store iter forest = concat <$> mapM (addStoreTree store iter) forest

renderGadget iconTheme wInit _ initial envC e b (Table flags columns nested) =
   {-# SCC "renderGadget_Table" #-} do
      (vw, store, activated, userEdit) <-
         mkTableView
               iconTheme
               flags
               (isJust nested)
               columns
               (_gdValue initial)
               (_gdValue <$> e)
               (_gdValue <$> b)
      liftIO $ wInit vw
      case nested of
         Nothing -> return ()
         Just selector -> do
            editedRow <- mkGtkPopupSelect vw iconTheme envC selector activated
            stop <- reactimate1 $ editedRow <&> \(n, mv) ->
                  forM_ mv $ \v -> MV.seqStoreSetValue store n v
            void $ Gtk.onWidgetDestroy vw stop
      widget <- Gtk.toWidget vw
      return GadgetOut {
            gadgetView = [widget],
            gadgetEvents = never,
            gadgetInitial = initial,
            gadgetUser = GadgetData True <$> userEdit,
            gadgetValue = b
         }

renderGadget iconTheme wInit env initial envC e b (ForestTable headerGroups lensForest) =
   {-# SCC "renderGadget_ForestTable" #-} do
      -- Create the widget hierarchy.
      vbox <- Gtk.boxNew Gtk.OrientationVertical 0
      base <- Gtk.layoutNew (Nothing :: Maybe Gtk.Adjustment) (Nothing :: Maybe Gtk.Adjustment)
      hAdjust <- Gtk.scrollableGetHadjustment base
      headerBase <- Gtk.layoutNew (Just hAdjust) (Nothing :: Maybe Gtk.Adjustment)
      (header1, headers) <- headerWidgets headerGroups
      mapM_ (Gtk.containerAdd headerBase) header1
      forM_ headers $ \(w1, ws) -> do
         Gtk.containerAdd headerBase w1
         mapM_ (Gtk.containerAdd headerBase) ws
      Gtk.widgetShowAll headerBase
      headerWidth1 <- mapM headerWidthRequests header1
      headerWidthsRest <- mapM headerGroupWidths headers
      let headerWidths = headerWidth1 : headerWidthsRest
      gadgets <- gadgetForest (map (map snd . snd) headerGroups) (lensForest env)
      mapM_ (mapM_ (mapM_ (mapM_ (Gtk.containerAdd base) . gadgetView))) gadgets
      scroll <- Gtk.scrolledWindowNew (Just hAdjust) (Nothing :: Maybe Gtk.Adjustment)
      Gtk.containerAdd scroll base
      Gtk.set scroll
            [#expand := True, #propagateNaturalHeight := True, #propagateNaturalWidth := True,
            #halign := Gtk.AlignFill, #valign := Gtk.AlignFill]
      Gtk.boxPackStart vbox headerBase False False 0
      Gtk.boxPackStart vbox scroll True True 0
      Gtk.widgetShowAll vbox
      -- Compute the initial sizes.
      widthGroups <- zipWith (zipWith bothMax) headerWidths <$> columnWidthsForest gadgets
      let widthss = allocateColumns Nothing widthGroups
      headHeight <- headersAdjust header1 headers widthss
      heights <- map treeHeightAdjust <$>
         getHeightsForest gadgets (allocateColumns Nothing widthGroups)
      baseSize headerBase base 0 widthss headHeight heights
      forestAdjust 0 0 gadgets widthss heights
      -- Set up resize callback on base.
      void $ Gtk.on base #sizeAllocate $ \rect -> do
         space <- Gdk.getRectangleWidth rect
         let widthss2 = allocateColumns (Just $ space - 5) widthGroups
         headHeight2 <- headersAdjust header1 headers widthss2
         heights2 <- map treeHeightAdjust <$> getHeightsForest gadgets widthss2
         baseSize headerBase base space widthss2 headHeight2 heights2
         forestAdjust 0 0 gadgets widthss2 heights2
      -- Assemble the resulting GadgetOut.
      widget <- Gtk.toWidget vbox
      return (mergeOutputs $ concatMap (concat . flatten) gadgets) {gadgetView = [widget]}
   where
      headerWidgets ::
         [(Text, [(Text, a)])] -> MomentIO ([Gtk.Widget], [(Gtk.Widget, [Gtk.Widget])])
      headerWidgets [] = return ([], [])
      headerWidgets [(_, columns)] = ( , []) <$> mapM (headerWidget . fst) columns
      headerWidgets ((_, group1) : groups) = do
         widgets1 <- mapM (headerWidget . fst) group1
         widgetss <- forM groups $ \(groupName, columns) -> do
            groupLabel <- headerWidget groupName
            columnLabels <- mapM (headerWidget . fst) columns
            return (groupLabel, columnLabels)
         return (widgets1, widgetss)
      headerWidget :: Text -> MomentIO Gtk.Widget
      headerWidget txt = do
         label <- Gtk.labelNew $ Just txt
         let longest = longestWord txt
         Gtk.set label [#wrap := True, #justify := Gtk.JustificationCenter,
               #wrapMode := Pango.WrapModeWordChar, #ellipsize := Pango.EllipsizeModeMiddle,
               #widthChars := 5 `min` longest, #maxWidthChars := longest, #lines := 3,
               #widthRequest := 10 * (5 `min` longest)]
         Gtk.labelSetLineWrap label True
         Gtk.labelSetJustify label Gtk.JustificationCenter
         Gtk.labelSetLineWrapMode label Pango.WrapModeWordChar
         style <- Gtk.widgetGetStyleContext label
         Gtk.styleContextAddClass style "table-header"
         Gtk.toWidget label
      headerGroupWidths :: (Gtk.Widget, [Gtk.Widget]) -> MomentIO [(Int32, Int32)]
      headerGroupWidths (top, cols)= do
            (topMin, topReq) <- headerWidthRequests top
            (colMins, colReqs) <- unzip <$> mapM headerWidthRequests cols
            return $ zip (expand topMin colMins) (expand topReq colReqs)
         where
            expand t xs =
               let
                  s = sum xs
                  d = t - s
               in if s > 0 && d > 0
                     then
                        let f = (1.0 :: Double) + (fromIntegral d / fromIntegral s)
                        in map (ceiling . (f *) . fromIntegral) xs
                     else xs
      gadgetTree gs gss (Node (k, lns1) forest) = do
         v <- forM gs $ \g -> do
               let g2 = focusingOver (getLens lns1) $
                     send (Just . Right . (k,)) <<< sendMap (Just . Left) g
               renderGadget iconTheme wInit env initial envC e b g2 >>= boxGadget
         forestOut <- gadgetForest gss forest
         return $ Node v forestOut
      gadgetForest [] _ = return []
      gadgetForest (gs : gss) nodes = forM nodes $ gadgetTree gs gss
      columnWidthsTree :: (MonadIO m) => Tree [GadgetOut w o] -> m [[(Int32, Int32)]]
      columnWidthsTree (Node gs childs) = do
         widths <- forM gs $ \g ->
            foldr bothMax (0,0) <$> mapM Gtk.widgetGetPreferredWidth (gadgetView g)
         widthss <- columnWidthsForest childs
         return $ widths : widthss
      columnWidthsForest :: (MonadIO m) => Forest [GadgetOut w o] -> m [[(Int32, Int32)]]
      columnWidthsForest nodes =
         foldr (longZip (longZip bothMax)) [] <$> mapM columnWidthsTree nodes
      headerWidthRequests :: (MonadIO m) => Gtk.Widget -> m (Int32, Int32)
      headerWidthRequests widget = do
         minReq <- fst <$> Gtk.widgetGetPreferredWidth widget
         prefReq <- snd <$> Gtk.widgetGetPreferredWidthForHeight widget 30  -- Single line
         return (minReq, prefReq)
      allocateColumns :: Maybe Int32 -> [[(Int32, Int32)]] -> [[Int32]]
      allocateColumns available requests =
         let
            allCols = concat requests
            totalMin = sum $ map fst allCols
            totalReq = sum $ map snd allCols
            colCount = length allCols
         in case available of
            Just w ->
               let
                  minExtra, reqExtra :: Double
                  minExtra = fromIntegral (w - totalMin) / fromIntegral colCount
                  reqExtra = fromIntegral (w - totalReq) / fromIntegral colCount
               in if
                  | w > totalReq -> -- Allocate excess space between widgets.
                     map (map $ (floor reqExtra +) . snd) requests
                  | w > totalMin -> -- Allocate space in excess of min between widgets
                     map (map $ (floor minExtra +) . fst) requests
                  | otherwise ->  -- Minimum sizes only.
                     map (map fst) requests
            Nothing ->  -- No space limit, so give columns everything they ask for.
               map (map snd) requests
      getHeightsTree :: (MonadIO m) => Tree [GadgetOut w o] -> [[Int32]] -> m (Tree Int32)
      getHeightsTree (Node gadgets childs) [] = do   -- Should never happen.
         hs <- forM gadgets (\g ->
            sum <$> forM (gadgetView g) (fmap fst . Gtk.widgetGetPreferredHeight))
         childHeights <- getHeightsForest childs []
         return $ Node (maximum $ 0:hs) childHeights
      getHeightsTree (Node gadgets childs) (widths : widthss) = do
            hs <- zipWithM gadgetHeight gadgets widths
            childHeights <- getHeightsForest childs widthss
            return $ Node (maximum $ 0:hs) childHeights
         where
            gadgetHeight g width =
               sum <$> forM (gadgetView g) (\widget ->
                  fst <$> Gtk.widgetGetPreferredHeightForWidth widget width)
      getHeightsForest :: (MonadIO m) => Forest [GadgetOut w o] -> [[Int32]] -> m (Forest Int32)
      getHeightsForest forest widthss = forM forest $ \node -> getHeightsTree node widthss
      treeHeightAdjust (Node h childs) =
         let newChilds = map treeHeightAdjust childs
         in Node (max h $ sum $ map rootLabel newChilds) newChilds
      headersAdjust :: (MonadIO m) =>
            [Gtk.Widget] -> [(Gtk.Widget, [Gtk.Widget])] -> [[Int32]] -> m Int32
      headersAdjust _ _ [] = return 0  -- Should never happen.
      headersAdjust group1 groups (widths1 : widthss) = do
            h1 <- maximum . (0:) . map fst <$>  -- Height of first group of header widgets
                  zipWithM Gtk.widgetGetPreferredHeightForWidth group1 widths1
            (hs2, hs3) <- unzip <$> zipWithM groupHeights groups widthss
            let
               h2 = maximum $ 0:hs2   -- Height of spanning header widgets.
               h3 = maximum $ 0:hs3   -- Height of sub-header widgets
               height = max h1 (h2 + h3)  -- Total height of header rows.
               h2a = max h2 $ height - h3  -- Spanning header wigets expanded to match h1.
               xs = scanl (+) (sum widths1) (map sum widthss)  -- Start x for each group.
            headerGroupAdjust 0 0 height group1 widths1
            headerGroupAdjust (sum widths1) 0 h2a (map fst groups) (map sum widthss)
            mapM_ (\(g, ws, x) -> headerGroupAdjust x h2a h3 g ws) $
                  zip3 (map snd groups) widthss xs
            return height
         where
            groupHeights (spanWidget, widgets) widths = do
               spanH <- Gtk.widgetGetPreferredHeightForWidth spanWidget $ sum widths
               subH <- maximum . (0:) . map fst <$>
                  zipWithM Gtk.widgetGetPreferredHeightForWidth widgets widths
               return (fst spanH, subH)
      headerGroupAdjust x y h widgets widths = mapM_ (adj h) $ zip3 widgets xs widths
         where
            xs = scanl (+) x widths
            adj height (w, x1, width) = do
               p <- Gtk.new Gdk.Rectangle [#x := x1, #y := y, #width := width, #height := height]
               Gtk.widgetSizeAllocate w p
      treeAdjust :: (MonadIO m) =>
         Int32 -> Int32    -- X and Y for top left of this tree.
         -> Tree [GadgetOut w o]  -- Gadgets to place.
         -> [[Int32]] -> Tree Int32  -- Widths and heights.
         -> m ()
      treeAdjust _ _ _ [] _ = return ()
      treeAdjust x y (Node gadgets childs) (widths : widthss) (Node height heights) = do
            let
               xs = scanl (+) x widths
               childX = x + sum widths
            mapM_ adjustGadget $ zip3 gadgets widths xs
            forestAdjust childX y childs widthss heights
         where
            adjustGadget (gadget, width, x1) = do
               p <- Gtk.new Gdk.Rectangle [#x := x1, #y := y, #width := width, #height := height]
               forM_ (gadgetView gadget) (`Gtk.widgetSizeAllocate` p)
      forestAdjust x y nodes widthss heights = do
         let
            ys = scanl (+) y $ map rootLabel heights
         forM_ (zip3 ys nodes heights) $ \(y1, c1, h1) ->
            treeAdjust x y1 c1 widthss h1
      baseSize headerBase base space widthss headHeight heights = do
         let
            width = sum $ concat widthss
            height = sum $ map rootLabel heights
         Gtk.layoutSetSize headerBase (fromIntegral width) (fromIntegral headHeight)
         Gtk.widgetSetSizeRequest headerBase 1 (fromIntegral headHeight)
         Gtk.layoutSetSize base (fromIntegral width) (fromIntegral height)
         Gtk.widgetSetSizeRequest base (max space $ fromIntegral width) (fromIntegral height)
      -- Gadgets must have 0 or 1 widget.
      boxGadget g = do
         w1 <- maybeToList <$> boxWidgets wInit Vertical (gadgetView g)
         return g {gadgetView = w1}
      liftBoth f (v1a,v2a) (v1b, v2b) = (f v1a v1b, f v2a v2b)
      bothMax = liftBoth max
      longZip _ [] xs = xs
      longZip _ xs [] = xs
      longZip f (x1:xs1) (x2:xs2) = f x1 x2 : longZip f xs1 xs2
      longestWord = fromIntegral . maximum . (0:) . map T.length . T.words

renderGadget _ wInit _ _ _ e b (ButtonBar buttons) = {-# SCC "renderGadget_ButtonBar" #-} do
      buttonBox <- Gtk.buttonBoxNew Gtk.OrientationHorizontal
      liftIO $ wInit buttonBox
      Gtk.buttonBoxSetLayout buttonBox Gtk.ButtonBoxStyleCenter
      clicks <- forM buttons $ \(nm, func) -> do
         btn <- Gtk.new Gtk.Button [#label := nm]
         liftIO $ wInit buttonBox
         Gtk.boxPackStart buttonBox btn False False 0
         registerIOSignal btn Gtk.onButtonClicked $ return ((), func)
      w <- Gtk.toWidget buttonBox
      let
         e1 = fmap const <$> e
         clicks1 = e1 : map (GadgetData True <$>) clicks
      return GadgetOut {
            gadgetView = [w],
            gadgetEvents = never,
            gadgetInitial = GadgetData True id,
            gadgetUser = foldr (unionWith u) never clicks1,
            gadgetValue = fmap const <$> b
         }
   where
      u (GadgetData True f1) (GadgetData True f2) = GadgetData True $ f1 . f2
      u (GadgetData False _) v@(GadgetData True _) = v
      u v@(GadgetData True _) (GadgetData False _) = v
      u (GadgetData False _) (GadgetData False _) = GadgetData False id

renderGadget _ wInit _ _ envC _ b (ButtonIO label act) = {-# SCC "renderGadget_ButtonIO" #-} do
      button <- Gtk.new Gtk.Button [#label := label]
      liftIO $ wInit button
      click <- registerIOSignal button Gtk.onButtonClicked $ return ((), act)
      result <- mapEventIO id $ runAction button <$> changesB envC <*> b <@> click
      let result1 = GadgetData True <$> result
      resultB <- stepper (GadgetData True Nothing) result1
      w <- Gtk.toWidget button
      return GadgetOut {
            gadgetView = [w],
            gadgetEvents = never,
            gadgetInitial = GadgetData True Nothing,
            gadgetUser = result1,
            gadgetValue = resultB
         }
   where
      runAction :: (Gtk.IsWidget parent) =>
         parent -> e -> GadgetData i -> (e -> i -> IO b) -> IO (Maybe b)
      runAction _ _ (GadgetData False _) _ = return Nothing
      runAction parent e1 (GadgetData True i) act1 =
         catch (Just <$> act1 e1 i) $ \err -> do
            void $ errorBox (Just parent) $ errMsg err
            return Nothing
      errMsg :: SomeException -> Text
      errMsg err = "Could not complete the operation.\n\n\
            \Technical details: " <> T.pack (show err)

renderGadget _ wInit _ initial _ e _ (Image path) = {-# SCC "renderGadget_Image" #-} do
      imageWidget <- Gtk.imageNewFromFile path
      liftIO $ wInit imageWidget
      Gtk.widgetShow imageWidget
      click <- registerIOSignal1 imageWidget Gtk.onWidgetButtonPressEvent $ \ev -> do
         btn <- Gdk.getEventButtonButton ev
         if btn == 1
            then do
               x <- round <$> Gdk.getEventButtonX ev
               y <- round <$> Gdk.getEventButtonY ev
               return (True, Just (x, y))
            else return (False, Nothing)
      let result = unionWith const (GadgetData True <$> filterJust click) e
      resultB <- stepper initial result
      w <- Gtk.toWidget imageWidget
      return GadgetOut {
            gadgetView = [w],
            gadgetEvents = never,
            gadgetInitial = initial,
            gadgetUser = result,
            gadgetValue = resultB
         }

renderGadget iconTheme wInit env initial envC e b (ForestEditor labelFunc menuFunc legalParent dSel) =
   {-# SCC "renderGadget_ForestEditor" #-} mdo
      info <- liftIO $ getStdRandom random  -- Unique identifier for this tree editor instance.
      -- Configure the tree view with one column for the item text.
      vw <- Gtk.treeViewNew
      liftIO $ wInit vw
      column <- Gtk.treeViewColumnNew
      render <- Gtk.cellRendererTextNew
      store <- createStore column render vw $ initial ^. gdValue
      stop1 <- reactimate1 $ changesE envC <&> (\env1 ->
         MV.cellLayoutSetAttributes column render store $ \v1 -> [
               #ellipsize := Pango.EllipsizeModeEnd,
               #editable := False,
               #text := labelFunc env1 v1
            ])
      (editEvent, editHandler) <- newEvent
      blockSignals <- liftIO $ newIORef False
      let
         sendEditEvent = do
            blockFlag <- readIORef blockSignals
            unless blockFlag $ MV.forestStoreGetForest store >>= editHandler
      editB <- stepper (initial ^. gdValue) editEvent
      Gtk.treeViewSetHeadersVisible vw False
      Gtk.treeViewColumnPackStart column render True
      void $ Gtk.treeViewAppendColumn vw column
      -- Update tree from incoming data.
      stop2 <- reactimate1 $ updateIfNeeded store vw <$> e
      ch <- changes $ b <&> (\v -> do
            writeIORef blockSignals True
            updateIfNeeded store vw v
            writeIORef blockSignals False
         )
      stop3 <- reactimate1' ch
      -- Configure the view for drag and drop. ForestStore DND isn't working so do this manually.
      targets <- dragTargets info
      Gtk.treeViewEnableModelDragSource vw
            [Gdk.ModifierTypeButton1Mask]
            targets
            [Gdk.DragActionCopy, Gdk.DragActionMove]
      Gtk.treeViewEnableModelDragDest vw targets [Gdk.DragActionCopy, Gdk.DragActionMove]
      void $ Gtk.on vw #dragMotion $ dragMotionHandler info store vw
      void $ Gtk.on vw #dragDrop $ dragDropHandler sendEditEvent info store vw
      -- Insertion of new tree elements
      (insertion, insertHandler) <- newEvent
      insertsNoDialog <- case dSel of
         Just s -> return $ (_2 %~ Just) . snd <$> filterE
               (\(env1, (_, v)) -> isNothing $ s env1 v)
               ((,) <$> changesB envC <@> insertion)
         Nothing -> return never
      inserts <- case dSel of
         Just s -> mkGtkPopupSelect vw iconTheme envC (disableApply1 s) insertion
         Nothing -> return never
      stop6 <- reactimate1 $ unionWith const inserts insertsNoDialog <&>
         (\((path, n), mVal) -> case mVal of
            Nothing -> return ()  -- Cancel pressed.
            Just v -> do
               void $ MV.forestStoreInsert store path n v
               sendEditEvent
         )
      -- Activation of existing tree elements.
      activations <- filterJust <$> registerIOSignal2 vw Gtk.onTreeViewRowActivated (
         \path1 _ -> do
            path2 <- Gtk.treePathCopy path1  -- path1 gets deallocated after exit.
            MV.forestStoreLookup store path2 >>= \case
               Nothing -> cannotHappen
                     "Render ForestEditorSpec: path for activated item does not exist."
                     $ return ((), Nothing)
               Just (Node frag _) -> return ((), Just (path2, frag)))
      edits <- case dSel of
         Just s -> mkGtkPopupSelect vw iconTheme envC s activations
         Nothing -> return never
      stop4 <- reactimate1 $ edits <&> (\(path, mVal) -> case mVal of
            Nothing -> return ()
            Just v -> do
               void $ MV.forestStoreSetValue store path v
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
      stop5 <- reactimate1 $ processMenuSelection insertHandler sendEditEvent store <$> menuEventOut
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
      void $ Gtk.onWidgetDestroy vw $ stop1 >> stop2 >> stop3 >> stop4 >> stop5 >> stop6
      return GadgetOut {
            gadgetView = [w],
            gadgetEvents = never,
            gadgetInitial = initial,
            gadgetUser = GadgetData True <$> editEvent,
            gadgetValue = GadgetData True <$> editB
         }
   where
      createStore column render vw v = do
         store <- MV.forestStoreNew v
         MV.cellLayoutSetAttributes column render store $ \v1 -> [
               #ellipsize := Pango.EllipsizeModeEnd,
               #editable := False,
               #text := labelFunc env v1
            ]
         Gtk.treeViewSetModel vw $ Just store
         Gtk.treeViewExpandAll vw
         return store
      updateIfNeeded _ _ (GadgetData False _) = return ()
      updateIfNeeded store vw (GadgetData True newVal) = do
         oldVal <- MV.forestStoreGetForest store
         when (newVal /= oldVal) $ do
               MV.forestStoreClear store
               path <- Gtk.treePathNew  -- Root
               MV.forestStoreInsertForest store path 0 newVal
               Gtk.treeViewExpandAll vw
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
      processMenuSelection insertHandler _ _ (Just path1, TreeAddBefore v) = do
         n <- (last . (0:)) . fromMaybe [] <$> Gtk.treePathGetIndices path1
               -- Position = 0 if no indices there. Insert before root = insert at start of root.
         path2 <- Gtk.treePathCopy path1
         void $ Gtk.treePathUp path2
         insertHandler ((path2, fromIntegral n), v)
      processMenuSelection insertHandler _ _ (Just path1, TreeAddIn v) =
         insertHandler ((path1, -1), v)
      processMenuSelection insertHandler _ _ (Just path1, TreeAddAfter v) = do
         n <- (last . ((-2):)) . fromMaybe [] <$> Gtk.treePathGetIndices path1
               -- Position = end if no indices there.
         void $ Gtk.treePathUp path1
         insertHandler ((path1, fromIntegral n+1), v)
      processMenuSelection _ editHandler store (Just path1, TreeDelete) = do
         void $ MV.forestStoreRemove store path1
         editHandler
      processMenuSelection insertHandler _ _ (Nothing, TreeAddAfter v) = do
         path <- Gtk.treePathNew
         insertHandler ((path, -1), v)
      processMenuSelection insertHandler _ _ (Nothing, TreeAddBefore v) = do
         path <- Gtk.treePathNew
         insertHandler ((path, 0), v)
      processMenuSelection _ _ _ _ = return ()   -- Do nothing.

renderGadget _ wInit _ initial _ e b (FilePathSelector title purpose filterList confirmFlag) =
   {-# SCC "renderGadget_FilePathSelector" #-} do
      entry <- Gtk.new Gtk.Entry [#hexpand := True, #widthChars := 30]
      liftIO $ wInit entry
      Gtk.setEntryText entry $ T.pack $ initial ^. gdValue
      Gtk.setEntryEditable entry False
      -- We can assume that the Entry widget already sets the ButtonPressMask for its window.
      activated <- registerIOSignal1 entry Gtk.onWidgetButtonPressEvent $ \ev -> do
         btn <- Gdk.getEventButtonButton ev
         if btn == 1 then return (True, Just ()) else return (False, Nothing)
      editedValue <- filterJust <$> mapEventIO (\() -> fileDialog entry) (filterJust activated)
      let
         allEvents = unionWith const editedValue e
         update w (GadgetData True v1) = Gtk.setEntryText w $ T.pack v1
         update _ (GadgetData False _) = return ()
      eventLink entry update allEvents
      behaviorLink entry update b
      w <- Gtk.toWidget entry
      return $ GadgetOut [w] never initial editedValue b
   where
      fileDialog entry = do
         d <- Gtk.new Gtk.FileChooserNative [
               #action := case purpose of
                  FilePathOpen -> Gtk.FileChooserActionOpen
                  FilePathSave -> Gtk.FileChooserActionSave
                  FilePathFolder -> Gtk.FileChooserActionSelectFolder,
               #title := title
            ]
         parentWin <- Gtk.widgetGetToplevel entry >>= Gtk.castTo Gtk.Window
         forM_ parentWin $ \w -> Gtk.set d [#transientFor := w]
         forM_ filterList $ \(lbl, filters) -> do
            filt <- Gtk.fileFilterNew
            Gtk.fileFilterSetName filt $ Just lbl
            forM_ filters $ \case
               FilePathGlob glob -> Gtk.fileFilterAddPattern filt glob
               FilePathMime mime -> Gtk.fileFilterAddMimeType filt mime
               FilePathImages -> Gtk.fileFilterAddPixbufFormats filt
            Gtk.fileChooserAddFilter d filt
         Gtk.fileChooserSetDoOverwriteConfirmation d confirmFlag
         path <- Gtk.getEntryText entry
         unless (T.null path) $ void $ Gtk.fileChooserSetFilename d $ T.unpack path
         result <- Gtk.nativeDialogRun d
         if result == fromIntegral (fromEnum Gtk.ResponseTypeAccept)
            then do
               mTarget <- Gtk.fileChooserGetFilename d
               Gtk.nativeDialogDestroy d
               return $ GadgetData True <$> mTarget
            else do
               Gtk.nativeDialogDestroy d
               return Nothing

renderGadget iconTheme wInit env initial envC e b (Trace lbl inF outF g) = {-# SCC "renderGadget_Trace" #-} do
      result <- renderGadget iconTheme wInit env initial envC e b g
      liftIO $ hPutStrLn stderr $
            "Initialising " <> lbl <>  ": " <>
            inF (initial ^. gdValue) <> " -> " <>
            outF1 initial (gadgetInitial result)
      let
         inE = inF1 <$> e
         outE = outF1 <$> b <@> gadgetUser result
         inB = inF1 <$> b
         outB = outF1 <$> b <*> gadgetValue result
      reactimate $ hPutStrLn stderr . ((lbl <> " output event: ") <>) <$> outE
      reactimate $ hPutStrLn stderr . ((lbl <> " input event: ") <>) <$> inE
      ch1 <- changes $ hPutStrLn stderr . ((lbl <> " input behav: ") <>) <$> inB
      reactimate' ch1
      ch2 <- changes $ hPutStrLn stderr . ((lbl <> " output behav: ") <>) <$> outB
      reactimate' ch2
      return result
   where
      inF1 (GadgetData ok i) =
         if ok then inF i else "Invalid: " <> inF i
      outF1 (GadgetData ok1 i) (GadgetData ok2 o) =
         show (ok1, ok2) <> outF i o


-- | Text box with an extra source of events.
textBoxWithPopup ::
   (forall w . (Gtk.IsWidget w) => w -> IO ()) -- ^ Run on all newly created widgets.
   -> GadgetData a  -- ^ Initial value for display.
   -> Event (GadgetData a)  -- ^ User-driven updates.
   -> Behavior (GadgetData a)  -- ^ Background updates.
   -> Prism' Text a   -- ^ Relationship between displayed text and value.
   -> (Gtk.Entry -> MomentIO (Event Text))   -- ^ Should create a popover for the entry.
   -> MomentIO (Gtk.Widget, Event (GadgetData a))
textBoxWithPopup wInit initial e b prsm popoverF = do
      entry <- Gtk.new Gtk.Entry [#hexpand := True]
      liftIO $ wInit entry
      styleCtx <- Gtk.widgetGetStyleContext entry
      (w, outE) <- makeField
         entry
         entryChangedSignal
         updateText
         (view (re prsm) <$> initial)
         (fmap (view $ re prsm) <$> e)
         (fmap (view $ re prsm) <$> b)
         $ do
            t <- Gtk.entryGetText entry
            case t ^? prsm of
               Nothing -> do
                  Gtk.styleContextAddClass styleCtx "entry-error"
                  return Nothing
               Just v -> do
                  Gtk.styleContextRemoveClass styleCtx "entry-error"
                  return $ Just v
      textE <- popoverF entry
      stop <- reactimate1 $ textE <&> \newText -> do
         oldText <- Gtk.entryGetText entry
         when (oldText /= newText) $ Gtk.entrySetText entry newText
      void $ Gtk.onWidgetDestroy entry stop
      return (w, outE)
   where
      updateText w new = do
         old <- Gtk.entryGetText w
         when (new /= old) $ Gtk.setEntryText w new


-- | CSS class names for memo box sizes.
memoBoxCss :: MemoBoxSize -> Text
memoBoxCss MemoSmall = "memo-small"
memoBoxCss MemoMedium = "memo-medium"
memoBoxCss MemoLarge = "memo-large"


-- | The old @Entry changed@ signal has disappeared, so this simulates it by triggering on
-- insertions and deletions in the buffer. In fact there are two signal handlers, but only
-- one is returned in accordance with the type. Since these are ignored anyway it doesn't matter.
entryChangedSignal :: Gtk.Entry -> IO () -> MomentIO Gtk.SignalHandlerId
entryChangedSignal entry action = do
   buf <- Gtk.entryGetBuffer entry
   void $ Gtk.onEntryBufferDeletedText buf $ \_ _ -> action
   Gtk.onEntryBufferInsertedText buf $ \_ _ _ -> action


-- | Create a new field widget with the plubming for setting the value and triggering events.
makeField :: (Gtk.IsWidget w) =>
   w                 -- ^ Widget.
   -> (w -> IO () -> MomentIO Gtk.SignalHandlerId)
         -- ^ Signal for a change in widget display value.
   -> (w -> b -> IO ())  -- ^ Setter for widget display value.
   -> GadgetData b             -- ^ The initial value to display.
   -> Event (GadgetData b)     -- ^ The value to display.
   -> Behavior (GadgetData b)
   -> IO (Maybe a)   -- ^ Get current user value from widget, if valid.
   -> MomentIO (Gtk.Widget, Event (GadgetData a))
makeField widget signal setter initial e b getValue = do
   liftIO $ setter widget $ initial ^. gdValue
   blockSignals <- liftIO $ newIORef False
   valueChanged <- filterJust <$> registerIOSignal widget signal (do
         blockFlag <- readIORef blockSignals
         if blockFlag
            then return ((), Nothing)
            else ((),) . Just . fmap (GadgetData True) <$> getValue
      )
   stop1 <- reactimate1 $ (\(GadgetData f v) -> when f $ setter widget v) <$> e
   ch <- changes $ b <&> (\(GadgetData f v) -> when f $ do
         writeIORef blockSignals True
         setter widget v
         writeIORef blockSignals False
      )
   stop2 <- reactimate1' ch
   void $ Gtk.onWidgetDestroy widget $ stop1 >> stop2
   w <- Gtk.toWidget widget
   return (w, filterJust valueChanged)


-- | Wrap a list of widgets up in a box. If the list contains just one widget
-- then no box is created and the widget is returned on its own. If the list is empty then @Nothing@
-- is returned.
boxWidgets :: (MonadIO m) =>
   (forall w . (Gtk.IsWidget w) => w -> IO ()) -- ^ Run on all newly created widgets.
   -> Orientation
   -> [Gtk.Widget]
   -> m (Maybe Gtk.Widget)
boxWidgets _ _ [] = return Nothing
boxWidgets _ _ [w] = return $ Just w
boxWidgets wInit orient ws = do
   let
      gtkOrient = case orient of
         Vertical -> Gtk.OrientationVertical
         Horizontal -> Gtk.OrientationHorizontal
   boxWidget <- Gtk.boxNew gtkOrient 3
   liftIO $ wInit boxWidget
   forM_ ws $ \w -> Gtk.boxPackStart boxWidget w True True 0
   w <- Gtk.toWidget boxWidget
   return $ Just w


-- Specialisations of Nothing to satisfy the type system.
noAdjustment :: Maybe Gtk.Adjustment
noAdjustment = Nothing

noWidget :: Maybe Gtk.Widget
noWidget = Nothing

noTreeViewColumn :: Maybe Gtk.TreeViewColumn
noTreeViewColumn = Nothing

noTreeIter :: Maybe Gtk.TreeIter
noTreeIter = Nothing

noPixbuf :: Maybe Gdk.Pixbuf
noPixbuf = Nothing

-- Size of data icons in dialogs.
iconSize :: Int
iconSize = 48
