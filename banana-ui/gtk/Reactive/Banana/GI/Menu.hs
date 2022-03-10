{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the banana-ui-gtk library. The banana-ui-gtk library is
proprietary and confidential. Copying is prohibited 
-}

-- |
--
-- Generate GTK menu widgets within the Reactive Banana MomentIO monad.

module Reactive.Banana.GI.Menu (
   mkGtkMenu,
   popupMenuOn
) where

import Control.Monad
import Data.List
import qualified GI.Gtk.Objects as Gtk
import Reactive.Banana
import Reactive.Banana.Common
import Reactive.Banana.Frameworks
import Reactive.Banana.Menu
import Reactive.Banana.GI.Connect


-- | Convert a Reactive Banana menu into a GTK menu. The associated event
-- is triggered by the user clicking on an item in the menu.
mkGtkMenu :: Menu a -> MomentIO (Gtk.Menu, Event a)
mkGtkMenu (Menu items) = do
   result <- Gtk.menuNew
   let
      addSections = map (addMenuSection result) items
      addSep = do
         sep <- Gtk.separatorMenuItemNew
         Gtk.menuShellAppend result sep
         return never
   evs <- sequence $
      intersperse addSep addSections
   Gtk.widgetShowAll result
   return (result, firstEvent evs)


-- | Add a section of the menu (between two separators) to the menu.
addMenuSection :: Gtk.Menu -> [MenuItem a] -> MomentIO (Event a)
addMenuSection target items = firstEvent <$> forM items (\item -> do
      (gtkItem, ev) <- mkGtkMenuItem item
      liftIO $ Gtk.menuShellAppend target gtkItem
      return ev)


-- | Convert a Hades @MenuItem@ into a GTK @MenuItem@
mkGtkMenuItem :: MenuItem a -> MomentIO (Gtk.MenuItem, Event a)
mkGtkMenuItem (MenuItem label item) = do
   result <- liftIO $ Gtk.menuItemNewWithLabel label
   case item of
      Left sub -> do
         (submenu, event) <- mkGtkMenu sub
         liftIO $ Gtk.menuItemSetSubmenu result $ Just submenu
         return (result, event)
      Right v -> do
         event <- registerIOSignal result Gtk.onMenuItemActivate $ return ((), v)
         return (result, event)


-- | Link an event with the appearance of a menu.
popupMenuOn :: Event Gtk.Menu -> MomentIO ()
popupMenuOn = reactimate . fmap (`Gtk.menuPopupAtPointer` Nothing)
