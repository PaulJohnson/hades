{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

{- |

Convert GTK mouse events into Mouse Commands embedded in Reactive Banana events.

Mouse commands are not quite the same as HADES Actions, but the mapping is intended to be
straightforward.
-}
module Hades.GI.MouseMachine (
  MouseCommandType (..),
  MouseCommand (..),
  mouseCommandEvent
) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.Void
import Hades.Abstract.AutoBanana
import Hades.Abstract.AutoMonad
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import Reactive.Banana
import Reactive.Banana.Common
import Reactive.Banana.Frameworks
import Reactive.Banana.GI.Connect


-- | Data about the most recent raw mouse event.
data MouseEvent =
    ButtonPress MouseState
    | ButtonRelease MouseState
    | ButtonDrag MouseDragState
    | ButtonZoom Double
    | KeyPress KeyState
  deriving Show


-- | Relevant information about a GTK mouse event in raw GTK types.
data MouseState = MouseState {
    gsButton :: Int,
    gsClick  :: Gdk.EventType,
    gsPos    :: (Double, Double),
    gsMods   :: [Gdk.ModifierType]
  } deriving Show


-- | Relevant information about a GTK mouse motion event in raw GTK types.
newtype MouseDragState = MouseDragState {
    dragPos :: (Double, Double)
  } deriving Show


data KeyState = KeyState {
    ksKey :: Char,   -- The approximate key pressed, as a Unicode character.
    ksMods :: [Gdk.ModifierType]  -- Modifier keys (shift, ctrl, etc).
  } deriving Show


-- | Get the state of the mouse after a button event.
mouseState :: (MonadIO m) => Gdk.EventButton -> m MouseState
mouseState b = MouseState <$>
        (fromIntegral <$> Gdk.getEventButtonButton b) <*>
        Gdk.getEventButtonType b <*>
        ((,) <$> Gdk.getEventButtonX b <*> Gdk.getEventButtonY b) <*>
        Gdk.getEventButtonState b


-- | Get the data associated with a drag event.
dragState :: (MonadIO m) => Gdk.EventMotion -> m MouseDragState
dragState m = MouseDragState <$> ((,) <$> Gdk.getEventMotionX m <*> Gdk.getEventMotionY m)


-- | Get the data associated with a key press event.
keyState :: (MonadIO m) => Gdk.EventKey -> m KeyState
keyState k = do
    v <- Gdk.getEventKeyKeyval k
    c1 <- chr . fromIntegral <$> Gdk.keyvalToUnicode v
    let
      c2 = case c1 of
        '\NUL' -> case v of
          0xFF08 -> '\b'   -- Backspace
          0xFF09 -> '\t'   -- Tab
          0xFF0D -> '\r'   -- Return
          0xFF1B -> '\ESC' -- Escape
          0xFFFF -> '\DEL' -- Delete
          _ -> '\NUL'
        _ -> c1
    m <- Gdk.getEventKeyState k
    return $ KeyState c2 m

{- Design Note:

GDK on Windows has a more restricted table of Unicode values for keys than on Linux. Particular
keys, including "Delete" and "Return" are not included because doing so was found to break text
entry on some foreign versions of Windows. See the following patch for details.

https://github.com/GNOME/gtk/commit/fd6ce9975e4cc9c090d07e7a6b0013d02b49ce26

We don't want to expose GDK key codes outside this module, so this is a workaround to detect
this case and manually substitute the relevant Unicode characters.
-}



data MouseCommandType =
    SingleClick
    | DoubleClick
    | RightClick
    | MouseDrag (Double, Double)
    | MouseEndDrag (Double, Double)
    | MouseKey Char
  deriving Show


-- | Mouse events that are relevant to diagram editing.
data MouseCommand =
  MouseCommand {  -- A mouse action that will affect the diagram.
      mouseType :: MouseCommandType,
      mouseLocation :: (Double, Double),
      mouseModifiers :: [Gdk.ModifierType]
    }
  | MouseZoom Double  -- A zoom action.
  deriving Show


-- | Stream processor that converts mouse events into commands.
mouseMachine :: MouseEvent -> StateStream MouseEvent (Maybe MouseCommand) () e Void
mouseMachine (ButtonRelease _) = ignore
mouseMachine (ButtonDrag _) = ignore
mouseMachine (ButtonZoom d) = yield (Just $ MouseZoom d) >>= mouseMachine
mouseMachine (KeyPress (KeyState c m)) =
  yield (Just $ MouseCommand (MouseKey c) (0,0) m) >>= mouseMachine
mouseMachine (ButtonPress mouse1) =   -- Must call mouseMachine or ignore.
    yield Nothing >>= \case
      ButtonZoom _ -> ignore  -- Trying to zoom half way through a click.
      KeyPress _ -> ignore    -- Trying to do a key command half way through a click.
      ButtonPress mouse2 ->  -- Could be a double click.
        if gsButton mouse1 == leftButton && gsButton mouse2 == leftButton
            && gsClick mouse2 == Gdk.EventType2buttonPress
        then
          yieldCommand DoubleClick mouse2 >>= mouseMachine -- Tail recursion for mice!
        else ignore
      ButtonRelease mouse2 -> -- Could be a single click or a right click.
        if gsClick mouse1 == Gdk.EventTypeButtonPress && gsButton mouse1 == gsButton mouse2
          then if gsButton mouse2 == leftButton
            then yieldCommand SingleClick mouse2 >>= mouseMachine
            else if gsButton mouse2 == rightButton
            then yieldCommand RightClick mouse2 >>= mouseMachine
            else ignore
          else ignore
      ButtonDrag mouse2 ->
        -- if isRealDrag mouse1 mouse2 && gsButton mouse1 == Gtk.LeftButton
        if gsButton mouse1 == leftButton
          then dragging mouse2
          else ignore
  where
    leftButton = 1
    rightButton = 3
    dragging mouse2 = do
      let
        (x1, y1) = gsPos mouse1
        (x2, y2) = dragPos mouse2
        delta = (x2-x1, y2-y1)
      yieldCommand (MouseDrag delta) mouse1 >>= \case
        ButtonZoom _ -> dragging mouse2
        ButtonPress _ -> dragging mouse2   -- Ignore spurious button press during dragging.
        KeyPress _ -> dragging mouse2    -- Ignore keyboard commands during dragging.
        ButtonRelease mouse3 ->
          if gsButton mouse3 == gsButton mouse1
            then yieldCommand (MouseEndDrag delta) mouse1 >>= mouseMachine
            else ignore
        ButtonDrag mouse3 -> dragging mouse3



-- | Ignore whatever input event got to this point and restart mouseMachine from the beginning.
--
-- Tail recursion for mice!
ignore :: StateStream MouseEvent (Maybe MouseCommand) () e Void
ignore = yield Nothing >>= mouseMachine

-- | Yield a MouseCommand and return the next input event.
yieldCommand :: MouseCommandType -> MouseState
  -> StateStream MouseEvent (Maybe MouseCommand) s e MouseEvent
yieldCommand mt ms = yield (Just $ MouseCommand mt (gsPos ms) (gsMods ms))




-- | Install event handlers on an object (e.g. a Hades canvas).
-- Subsequent GTK mouse events will be translated into the corresponding Banana mouse events.
--
-- This also causes the widget to grab focus whenever a mouse button is clicked.
mouseCommandEvent :: (Gtk.IsWidget widget) => widget -> MomentIO (Event MouseCommand)
mouseCommandEvent widget = do
  Gtk.widgetAddEvents widget [
      Gdk.EventMaskButton1MotionMask,
      Gdk.EventMaskButtonPressMask,
      Gdk.EventMaskButtonReleaseMask,
      Gdk.EventMaskKeyPressMask,
      Gdk.EventMaskScrollMask]
  buttonDownEvent <- registerIOSignal1 widget Gtk.onWidgetButtonPressEvent $
    fmap ((True, ) . ButtonPress) . mouseState
  buttonUpEvent <- registerIOSignal1 widget Gtk.onWidgetButtonReleaseEvent $
    fmap ((True, ) . ButtonRelease) . mouseState
  dragEvent <- registerIOSignal1 widget Gtk.onWidgetMotionNotifyEvent $
    fmap ((True, ) . ButtonDrag) . dragState
  keyEvent <- registerIOSignal1 widget Gtk.onWidgetKeyPressEvent $
    fmap ((True, ) . KeyPress) . keyState
  zoomEvent <- registerIOSignal1 widget Gtk.onWidgetScrollEvent $ \scroll -> do
    mods <- Gdk.getEventScrollState scroll
    if Gdk.ModifierTypeControlMask `elem` mods
      then do
        d <- Gdk.getEventScrollDirection scroll >>= \case
            Gdk.ScrollDirectionUp -> return 0.99
            Gdk.ScrollDirectionDown -> return 1.01
            Gdk.ScrollDirectionLeft -> return 0.99
            Gdk.ScrollDirectionRight -> return 1.01
            Gdk.ScrollDirectionSmooth -> do
              dy <- Gdk.getEventScrollDeltaY scroll
              return $ 1  + dy / 100  -- Hope this is the right direction.
            _ -> return 1
        return (True, Just $ ButtonZoom d)
      else
        return (False, Nothing)
  let mouseEvent = foldr1 (unionWith const)
        [buttonDownEvent, buttonUpEvent, dragEvent, keyEvent, filterJust zoomEvent]
  stop <- reactimate1 $ Gtk.widgetGrabFocus widget <$ mouseEvent
  void $ Gtk.onWidgetDestroy widget stop
    -- Manual focus grab because we are intercepting events before the focus mechanism gets them.
  filterJust <$> applyEvents (snd $ startStream (yield Nothing >>= mouseMachine) ()) mouseEvent
