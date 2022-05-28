{-
Copyright © Paul Johnson 2019. See LICENSE file for details.
-}

{- |

These functions provide low level connections between GTK objects and Reactive Banana.

The \"register\" family of functions is used to convert GTK input hooks into Reactive Banana
events. On the output side the \"link\" family of functions is used to update a GTK attribute
when an event fires or a behavior changes.
-}
module Reactive.Banana.GI.Connect (
  -- * Generating Events.
  registerIOSignal,
  registerIOSignal1,
  registerIOSignal2,
  registerIOSignal3,
  registerIOSignal4,
  registerIOSignal5,
  registerIOSignal6,
  -- registerActionEntry,
  -- * Setting Widget Attributes
  eventLink,
  behaviorLink
) where

import Control.Monad
import Control.Monad.IO.Class
import Data.GI.Base.Signals
import qualified GI.Gtk as Gtk
import Reactive.Banana
import Reactive.Banana.Common
import Reactive.Banana.Frameworks


-- | Register a GTK signal as an input.
--
-- If you only want to know that the signal occured then use @return ((), ())@ as the action.
-- However a more typical use-case is that you receive a signal that something has changed and
-- then need to use the action to query the new value to put into the @Event@.
registerIOSignal ::
  object
  -> (object -> IO a -> MomentIO SignalHandlerId)
    -- ^ Signal function, such as "onButtonClicked".
  -> IO (a, b)  -- ^ IO action executed to obtain the signal result and the Event value.
  -> MomentIO (Event b)
registerIOSignal obj sig act = do
  (event, runHandlers) <- newEvent
  void $ sig obj $ do
    (r, v) <- act
    runHandlers v
    return r
  return event


-- | Register a GTK signal with one argument.
registerIOSignal1 ::
  object
  -> (object -> (a -> IO b) -> MomentIO SignalHandlerId)
  -> (a -> IO (b, c))
  -> MomentIO (Event c)
registerIOSignal1 obj sig act = do
  (event, runHandlers) <- newEvent
  void $ sig obj $ \arg1 -> do
    (r, v) <- act arg1
    runHandlers v
    return r
  return event


-- | Register a GTK signal with two arguments.
registerIOSignal2 ::
  object
  -> (object -> (a -> b -> IO c) -> MomentIO SignalHandlerId)
  -> (a -> b -> IO (c, d))
  -> MomentIO (Event d)
registerIOSignal2 obj sig act = do
  (event, runHandlers) <- newEvent
  void $ sig obj $ \arg1 arg2 -> do
    (r, v) <- act arg1 arg2
    liftIO $ runHandlers v
    return r
  return event


-- | Register a GTK signal with three arguments.
registerIOSignal3 ::
  object
  -> (object -> (a -> b -> c -> IO d) -> MomentIO SignalHandlerId)
  -> (a -> b -> c -> IO (d, e))
  -> MomentIO (Event e)
registerIOSignal3 obj sig act = do
  (event, runHandlers) <- newEvent
  void $ sig obj $ \arg1 arg2 arg3 -> do
    (r, v) <- act arg1 arg2 arg3
    liftIO $ runHandlers v
    return r
  return event


-- | Register a GTK signal with four arguments.
registerIOSignal4 ::
  object
  -> (object -> (a -> b -> c -> d -> IO e) -> MomentIO SignalHandlerId)
  -> (a -> b -> c -> d -> IO (e, f))
  -> MomentIO (Event f)
registerIOSignal4 obj sig act = do
  (event, runHandlers) <- newEvent
  void $ sig obj $ \arg1 arg2 arg3 arg4 -> do
    (r, v) <- act arg1 arg2 arg3 arg4
    liftIO $ runHandlers v
    return r
  return event


-- | Register a GTK signal with five arguments.
registerIOSignal5 ::
  object
  -> (object -> (a -> b -> c -> d -> e -> IO f) -> MomentIO SignalHandlerId)
  -> (a -> b -> c -> d -> e -> IO (f, g))
  -> MomentIO (Event g)
registerIOSignal5 obj sig act = do
  (event, runHandlers) <- newEvent
  void $ sig obj $ \arg1 arg2 arg3 arg4 arg5 -> do
    (r, v) <- act arg1 arg2 arg3 arg4 arg5
    liftIO $ runHandlers v
    return r
  return event


-- | Register a GTK signal with four arguments.
registerIOSignal6 ::
  object
  -> (object -> (a -> b -> c -> d -> e -> f -> IO g) -> MomentIO SignalHandlerId)
  -> (a -> b -> c -> d -> e -> f -> IO (g, h))
  -> MomentIO (Event h)
registerIOSignal6 obj sig act = do
  (event, runHandlers) <- newEvent
  void $ sig obj $ \arg1 arg2 arg3 arg4 arg5 arg6 -> do
    (r, v) <- act arg1 arg2 arg3 arg4 arg5 arg6
    liftIO $ runHandlers v
    return r
  return event


-- | When an event occurs push the value to the specified attribute.
eventLink :: (Gtk.IsWidget object) => object -> (object -> a -> IO ()) -> Event a -> MomentIO ()
eventLink obj setter evs = do
  stop <- reactimate1 $ setter obj <$> evs
  void $ Gtk.onWidgetDestroy obj stop

-- | Keep the attribute updated with the specified Behaviour.
behaviorLink :: (Gtk.IsWidget object) =>
  object -> (object -> a -> IO ()) -> Behavior a -> MomentIO ()
behaviorLink obj setter b = do
  fe <- changes b
  stop <- reactimate1' $ fmap (fmap (setter obj)) fe
  void $ Gtk.onWidgetDestroy obj stop
