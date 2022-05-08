{-# LANGUAGE RecursiveDo #-}

{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

{- |

This module provides a GTK and Cairo implementation of the abstract diagram editing given by
the "Delta" monad in "Hades.Abstract.Delta". The "makeDrawingGtk" function takes a diagram with
elements of type @v@ and displays it in a scrolling window. Mouse events are interpreted to edit
the diagram, with diagram updates made available through events carrying "HadesActionResult"
values. These values are wrapped in an application monad @m@, so by extracting the result the
associated actions will be applied.
-}
module Hades.GI.Rendering (
   -- ** Hades on GTK
   HadesGtk,
   hadesState,
   hadesViews,
   hadesCanvas,
   makeDrawingGtk,
   makeBottomBar,
   zoomLimit,
   -- ** Actions on the Diagram
   HadesActionResult (..),
   hadesActionExecute,
   renderStatic,
   renderStaticSize
) where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID
import qualified GI.Cairo.Render as Cairo
import qualified GI.Cairo.Render.Connector as Cairo
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import Hades.Abstract
import Hades.GI.BasicShapes
import Hades.GI.MouseMachine
import Reactive.Banana.Combinators hiding ((<>))
import Reactive.Banana.Common
import Reactive.Banana.Frameworks
import Reactive.Banana.GI.Connect
import Reactive.Banana.GI.Menu
import Reactive.Banana.Menu
import Text.Read (readMaybe)


-- | Top level state for a Hades diagram editor in the GTK environment.
data HadesGtk v = HadesGtk {
      _hadesState      :: DeltaNext v,           -- ^ "runDelta" input and output.
      _hadesViews      :: ViewSet v,             -- ^ Currently drawn "View"s
      _hadesTransients :: ViewSet v,             -- ^ Currently drawn transient views.
      hadesCanvas      :: Gtk.DrawingArea,         -- ^ Canvas for the Hades editor.
      hadesPaint       :: IORef (HadesRender ()) -- ^ Action to paint diagram updates.
   }

{- Design Note:

The hadesPaint IORef is used to communicate between the diagram update code and the "on" event
which is triggered when the canvas is to be repainted. This is a kludge, but it works and its
not visible outside this module.

In the longer term this code needs to be ported to Reactive Banana, which would use events
and behaviors for the same thing.

Its also a design issue that the lens components of HadesGtk are in fact abstract, while the
canvas and paint members are GTK-specific. This suggests that the type should be refactored to
move the abstract stuff back over to the Hades.Abstract hierarchy.
-}

-- | "runDelta" input and output.
hadesState :: Lens' (HadesGtk v) (DeltaNext v)
hadesState = lens _hadesState $ \h v -> h{_hadesState = v}

-- | Currently drawn "View"s
hadesViews :: Lens' (HadesGtk v) (ViewSet v)
hadesViews = lens _hadesViews $ \h v -> h{_hadesViews = v}

-- | Currently drawn transient Views.
hadesTransients :: Lens' (HadesGtk v) (ViewSet v)
hadesTransients = lens _hadesTransients $ \h t -> h{_hadesTransients = t}


-- Hades diagram coordinates of the mouse position.
hadesPosition :: HadesGtk v -> (Double, Double) -> Point
hadesPosition hades (px,py) = Point (px/zm) (py/zm)
   where
      zm = hades ^. hadesState . to streamStateT . deltaZoom


-- | Find the top-most "View" object in the diagram at the point.
hadesTarget :: (Viewable v) => HadesGtk v -> Point -> Maybe (UUID, View v)
hadesTarget hades pt = find ((`viewTouched` pt) . snd) (ts ++ vs)
   where
      (ViewSet viewMap) = hades ^. hadesViews
      (ViewSet transientMap) = hades ^. hadesTransients
      order = hades ^. hadesState . to streamStateT . deltaDiagram . diagramOrder
      vs = mapMaybe (\u -> (u, ) <$> M.lookup u viewMap) order
      ts =  M.toList transientMap


-- | The current boundbox of the diagram.
hadesDiagramBounds :: HadesGtk v -> BoundBox
hadesDiagramBounds hades = viewSetBoundBox $ hades ^. hadesViews
   -- Note: this may be too slow. Perhaps we need to memoise it.


-- | The current size (width, height) of the diagram in pixels.
hadesDiagramSize :: HadesGtk v -> (Int, Int)
hadesDiagramSize hades = case hadesDiagramBounds hades of
      BoundBox _ (Point w h) -> (ceiling $ w * zm, ceiling $ h * zm)
      NoBox -> (0,0)
   where
      zm = hades ^. hadesState . to streamStateT . deltaZoom


{- | Create an empty diagram within a GTK DrawingArea. The DrawingArea is added to the Container
inside a scrolled viewport.

Returns the scrolled window containing the drawing and updates to the drawing state as events
containing actions. The caller is responsible for applying these actions to the diagram behavior
and thereby updating the behavior.
-}
makeDrawingGtk ::
   forall v . (Connectable v, Paint v ~ HadesRender) =>
   Diagram v         -- ^ The diagram to start editing.
   -> ViewContext v  -- ^ The application context for this diagram. Read by @use deltaContext@.
   -> Maybe (Point -> Menu (Action v))  -- ^ Context menu for the diagram background.
   -> KeyCommands v    -- ^ The command keys for this diagram.
   -> HadesStyle   -- ^ The style in which this diagram is to be rendered.
   -> Scale   -- ^ The initial zoom factor for the display.
   -> Behavior (HadesGtk v)  -- ^ The current state of the drawing.
   -> MomentIO (
         Gtk.Widget,
         HadesGtk v,
         Event [Action v])
makeDrawingGtk diagram ctx backgroundMenu keyCommands style z hadesB = mdo
      -- Create the diagram state.
      canvas <- Gtk.drawingAreaNew
      gtkStyle <- Gtk.widgetGetStyleContext canvas
      Gtk.widgetSetName canvas "hades-diagram"
      Gtk.styleContextAddClass gtkStyle "hades-diagram"
      Gtk.widgetSetCanFocus canvas True
      Gtk.widgetSetFocusOnClick canvas True
      paintRef <- liftIO $ newIORef $ return ()
      dState <- (liftIO $ mkDiagramState ctx z diagram) :: MomentIO (DeltaState v)
      let
         machine :: DeltaNext v
         machine = startStreamT1 (stepDelta . processAction drawSelectionBox keyCommands) dState
         hadesInit :: HadesGtk v
         hadesInit = HadesGtk {
               _hadesState = machine,
               _hadesViews = mempty,
               _hadesTransients = mempty,
               hadesCanvas = canvas,
               hadesPaint = paintRef
            }
      -- Set up Reactive Banana circuit
      mouseCommands <- mouseCommandEvent canvas
      let
         (diagramActions, screenActions) = split $ mouseToAction <$> hadesB <@> mouseCommands
         (mouseActions, menus) = split diagramActions
         (scrollEvent, zoomEvent) = split screenActions
      gtkMenus <- execute $ mkGtkMenu <$> filterJust menus
      popupMenuOn $ fst <$> gtkMenus
      menuOutputs <- switchE $ snd <$> gtkMenus
      let
         hadesActions = foldr1 (unionWith (++)) [
            return <$> mouseActions,
            return <$> menuOutputs,
            zoomAction <$> zoomEvent,
            mapEvents]
      -- Create the DrawingArea and set it up in the frame.
      scroll <- makeScrollingWidget canvas $ hadesDiagramSize <$> hadesB
      stop <- dragScroll scroll scrollEvent
      void $ Gtk.onWidgetDestroy scroll stop
      void $ Gtk.onWidgetDraw canvas $ \gtkCtx -> do
         drawAction <- liftIO $ readIORef paintRef
         renderWithHades gtkCtx style drawAction
         return True
      -- Get the initial diagram drawn on the canvas
      mapEvents <- registerIOSignal scroll Gtk.afterWidgetMap $
         return ((), [mkScriptAction $ tellAll >> yieldViews])
                           -- Refresh when diagram gets focus.
      w <- Gtk.toWidget scroll
      return (w, hadesInit, hadesActions)
   where
      zoomAction d = [mkScriptAction $ do
         deltaZoom *= d
         deltaZoom %= (max (1/zoomLimit) . min zoomLimit)
         tellAll
         yieldViews]
      mouseToAction :: HadesGtk v -> MouseCommand ->
         Either
            (Either (Action v) (Maybe (Menu (Action v))))
            (Either
               (Double, Double)  -- Diagram scroll distance
               Double)            -- Mouse wheel zoom factor
      mouseToAction _ (MouseZoom d) = Right $ Right d
      mouseToAction hades mc@MouseCommand {} = case mouseType mc of
            SingleClick -> Left $ Left $ Action Select loc mods targetView
            DoubleClick -> Left $ Left $ Action Activate loc mods targetView
            RightClick -> Left $ Right $ ($ loc) <$> menu   -- Context menu.
            MouseDrag (mdx, mdy) -> if modCtrl mods
               then Right $ Left (mdx, mdy)
               else Left $ Left $ Action (Drag (mdx/zm, mdy/zm)) loc mods targetView
            MouseEndDrag (mdx, mdy) ->
               Left $ Left $ Action (EndDrag (mdx/zm, mdy/zm)) loc mods targetView
            MouseKey k -> Left $ Left $ Action (Key k) loc mods targetView
         where
            loc = hadesPosition hades $ mouseLocation mc
            mods = Modifiers {
                  modShift = Gdk.ModifierTypeShiftMask `elem` gtkMods,
                  modCtrl =  Gdk.ModifierTypeControlMask `elem` gtkMods,
                  modAlt =  Gdk.ModifierTypeMod1Mask `elem` gtkMods
               }
            gtkMods = mouseModifiers mc
            targetView = hadesTarget hades loc
            menu = (viewMenu . snd <$> targetView) <|> backgroundMenu
            zm = hades ^. hadesState . to streamStateT . deltaZoom
      dragScroll scroll scrollEvent = do
         hAdj <- Gtk.scrolledWindowGetHadjustment scroll
         vAdj <- Gtk.scrolledWindowGetVadjustment scroll
         stopH <- reactimate1 $ adjustDrag hAdj . fst <$> scrollEvent
         stopV <- reactimate1 $ adjustDrag vAdj . snd <$> scrollEvent
         return $ stopH >> stopV  -- Returned to caller, not executed now.
      adjustDrag adj d = do
         p <- Gtk.adjustmentGetValue adj
         Gtk.adjustmentSetValue adj $ p - d


-- | The result of applying an "Action" to the Hades state.
data HadesActionResult v = HadesActionResult {
      hadesActionNewState :: HadesGtk v,
         -- ^ The updated diagram state.
      hadesActionDraw :: IO (),
         -- ^ Execute this to update the diagram on the screen.
      hadesActionCheckpoint :: Maybe Text,
         -- ^ If this action resulted in an undo checkpoint then the text describes it.
      hadesActionProperties :: [v]
         -- ^ If one or more actions call for a properties dialog then these are the elements for
         -- the dialogs. The dialog updates will feed event actions back into "makeDrawingGtk".
   }


-- | Executes Hades Actions and accumulates the results in monad @Base v@.
hadesActionExecute :: (Viewable v, Paint v ~ HadesRender) =>
   HadesGtk v             -- ^ Initial state.
   -> [Action v]    -- ^ Actions that update the diagram state.
   -> Base v (HadesActionResult v)
hadesActionExecute initial = foldM procInput (wrapResult initial)
   where
      -- Accumulate changes and associated actions.
      procInput (HadesActionResult hades1 drw1 check1 props1) act = do
            (HadesActionResult hades2 drw2 check2 props2) <- hadesAction hades1 act
            return $ HadesActionResult
                  hades2
                  (drw1 >> drw2)
                  (check1 <> check2)
                  (props1 ++ props2)
      -- Convert to HadesActionResult
      wrapResult h = HadesActionResult h (return ()) Nothing []


-- | Applies a user "Action" to a "HadesGtk" state. The result encapsulates the new state
-- and any action in the inner monad that may be called for.
hadesAction :: (Viewable v, Paint v ~ HadesRender) =>
   HadesGtk v -> Action v -> Base v (HadesActionResult v)
hadesAction hades action = do
      (output, newState) <- applyInputT (hades ^. hadesState) action
      let
         -- Compute the updates to the views in the HadesGtk state based on the output.
         ViewSet updates = outViews output
         ViewSet oldViews = hades ^. hadesViews
         unchangedViews = oldViews `M.difference` updates
         ViewSet transientUpdates = outTransients output
         ViewSet oldTransients = hades ^. hadesTransients
         unchangedTransients = oldTransients `M.difference` transientUpdates
         newViews = nonNullViews updates
         newTransients = nonNullViews transientUpdates
         -- The IO action to draw the updates. Adds an arbitrary bit of slop around the
         -- changes for safety.
         drawAction = do
               writeIORef (hadesPaint newHades) $ repaint newHades
               Gtk.widgetQueueDraw $ hadesCanvas newHades
         -- The updated HadesGtk state resulting from the output.
         newHades = (
               (hadesState .~ newState) .
               (hadesViews .~ ViewSet (newViews `M.union` unchangedViews)) .
               (hadesTransients .~ ViewSet (newTransients `M.union` unchangedTransients))
            ) hades
      return $ HadesActionResult
            newHades
            drawAction
            (outCheckpoint output)
            (outProperties output)
   where
      nonNullViews = M.filter ((/= mempty) . viewBox)


-- | Redraw the diagram in response to a Gtk.redraw signal.
repaint :: (Paint v ~ HadesRender) => HadesGtk v -> HadesRender ()
repaint hades = do
      let
         (ViewSet vs) = hades ^. hadesViews
         (ViewSet ts) = hades ^. hadesTransients
         order = hades ^. hadesState . to streamStateT . deltaDiagram . diagramOrder
         viewList = mapMaybe (`M.lookup` vs) order
         z = hades ^. hadesState . to streamStateT . deltaZoom
      style <- Gtk.widgetGetStyleContext $ hadesCanvas hades
      w <- Gtk.widgetGetAllocatedWidth $ hadesCanvas hades
      h <- Gtk.widgetGetAllocatedHeight $ hadesCanvas hades
      lift $ do
         Cairo.toRender $ \ctx ->
            Gtk.renderBackground style ctx 0 0 (fromIntegral w) (fromIntegral h)
         Cairo.scale z z
      mapM_ viewDraw $ reverse viewList
      mapM_ viewDraw $ M.elems ts



-- | Draw a box with thin dashed lines for area selection.
drawSelectionBox :: Point -> Point -> HadesRender ()
drawSelectionBox p1 p2 = lift $ do
      Cairo.setSourceRGB 0 0 0
      let (dx, dy) = pointDiff p1 p2
      Cairo.rectangle (adjust $ p1 ^. pX) (adjust $ p1 ^. pY) dx dy
      Cairo.save
      Cairo.identityMatrix
      Cairo.setLineWidth 0.5
      Cairo.setDash [3, 3] 0
      Cairo.stroke
      Cairo.restore
   where
      adjust v = fromIntegral (round v :: Integer) + 0.5


-- | Puts a widget into a GTK Viewport inside a GTK ScrolledWindow. The size of the
-- widget is controlled by a Behavior so it can be varied over time. Use @always (-1,-1)@
-- for the default size of the widget.
makeScrollingWidget :: (Gtk.IsWidget widget) =>
   widget         -- ^ The widget to be made scrollable.
   -> Behavior (Int, Int)  -- ^ The width and height of the widget.
   -> MomentIO Gtk.ScrolledWindow
makeScrollingWidget widget size = do
      -- A scrolledWindow can take a non-scrolling widget and will automatically add a viewport.
      -- However GTK then hooks up the scroll bars to the container focus adjustments, so that focus
      -- grabs cause the scroll bars to move to the top left hand corner of the widget. This is the
      -- Wrong Thing for a big diagram, so we avoid this "helpful" behaviour by manually creating a
      -- viewport.
      vw <- Gtk.viewportNew noAdjustment noAdjustment
      Gtk.containerAdd vw widget
      scroll <- Gtk.scrolledWindowNew noAdjustment noAdjustment
      Gtk.containerAdd scroll vw
      Gtk.scrolledWindowSetPolicy scroll Gtk.PolicyTypeAutomatic Gtk.PolicyTypeAutomatic
      Gtk.scrolledWindowSetShadowType scroll Gtk.ShadowTypeIn
      behaviorLink widget Gtk.setWidgetWidthRequest $ fromIntegral . fst <$> size
      behaviorLink widget Gtk.setWidgetHeightRequest $ fromIntegral . snd <$> size
      return scroll
   where
      noAdjustment :: Maybe Gtk.Adjustment
      noAdjustment = Nothing


-- | The bottom bar for a diagram. Currently contains a hint text and a scale widget.
--
-- The resulting widget will follow the input scale and generate events when it changes.
makeBottomBar :: Behavior (Maybe Text) -> Behavior Scale -> MomentIO (Gtk.Widget, Event Scale)
makeBottomBar hintB scaleB = mdo
      bar <- Gtk.boxNew Gtk.OrientationHorizontal 0
      -- Set up hint widget.
      hintBox <- Gtk.labelNew Nothing
      Gtk.labelSetSelectable hintBox False
      behaviorLink hintBox Gtk.labelSetText $ fromMaybe "" <$> hintB
      Gtk.boxPackStart bar hintBox False False 0
      -- Set up scale widgets.
      slider <- Gtk.scaleNewWithRange Gtk.OrientationHorizontal (-1) 1 0.1
      textBox <- Gtk.entryNew
      scaleBox <- Gtk.boxNew Gtk.OrientationHorizontal 2
      Gtk.scaleSetDrawValue slider False
      Gtk.widgetSetSizeRequest slider 100 (-1)
      Gtk.rangeSetValue slider 0
      Gtk.entrySetText textBox ("100" :: Text)
      Gtk.entrySetMaxLength textBox 3
      Gtk.entrySetWidthChars textBox 3
      label1 <- Gtk.labelNew $ Just ("Scale =" :: Text)
      label2 <- Gtk.labelNew $ Just ("%" :: Text)
      Gtk.boxPackStart scaleBox label1 False False 0
      Gtk.boxPackStart scaleBox textBox False False 0
      Gtk.boxPackStart scaleBox label2 False False 0
      Gtk.boxPackEnd scaleBox slider False False 0
      textE <- filterJust <$> registerIOSignal textBox Gtk.onEditableChanged (do
         str <- Gtk.editableGetChars textBox 0 (-1)
         case readMaybe $ T.unpack str of
            Just (n :: Int) ->
               let s = txtToSlide n
               in if s >= -1 && s <= 1 then return ((), Just n) else return ((), Nothing)
            Nothing -> return ((), Nothing)
         )
      sliderE <- registerIOSignal slider Gtk.onRangeValueChanged $ do
         z <- Gtk.rangeGetValue slider
         return ((), z)
      behaviorLink slider Gtk.rangeSetValue $ logBase zoomLimit <$> scaleB
      eventLink textBox Gtk.setEntryText $ T.pack . show . slideToTxt <$> sliderE
      eventLink slider Gtk.rangeSetValue $ txtToSlide <$> textE
      let scaleE = textE <&> (/100) . fromIntegral
      Gtk.boxPackEnd bar scaleBox False False 5
      w <- Gtk.toWidget bar
      return (w, scaleE)
   where
      txtToSlide :: Int -> Double
      txtToSlide v = logBase zoomLimit (fromIntegral v/100)
      slideToTxt :: Double -> Int
      slideToTxt v = round $ 100 * exp (v * log zoomLimit)


-- | Limit on how large or small the rendered diagram can be on the screen.
zoomLimit :: Double
zoomLimit = 4


-- | Compensate for the change from pixels to points when exporting a diagram.
staticScale :: Double
staticScale = 72/96  -- 72 points per inch, 96 pixels per inch.


-- | Create a static rendering of a diagram, for instance for file export or printing. The
-- missing context is taken from the Cairo Surface. The diagram is moved up to the top left
-- of the rendering space.
renderStatic :: (Paint v ~ HadesRender) =>
   HadesStyle -> HadesGtk v -> Cairo.Render ()
renderStatic style hades = do
   let
      (ViewSet vs) = hades ^. hadesViews
      order = hades ^. hadesState . to streamStateT . deltaDiagram . diagramOrder
      viewList = mapMaybe (`M.lookup` vs) order
      (xOff, yOff) = case renderStaticSize hades of
         NoBox -> (0, 0)
         BoundBox p _ -> (p ^. pX, p ^. pY)
   Cairo.translate (-xOff) (-yOff)
   Cairo.scale staticScale staticScale
   runReaderT (mapM_ viewDraw $ reverse viewList) style
   Cairo.showPage


-- The maximum X and Y coordinates of the diagram views, with a bit of slop.
renderStaticSize :: HadesGtk v -> BoundBox
renderStaticSize hades = scaleBox staticScale $ growBox 5 $ viewSetBoundBox $ hades ^. hadesViews
   where
      scalePoint s (Point px py) = Point (s*px) (s*py)
      scaleBox _ NoBox = NoBox
      scaleBox s (BoundBox p1 p2) = BoundBox (scalePoint s p1) (scalePoint s p2)
