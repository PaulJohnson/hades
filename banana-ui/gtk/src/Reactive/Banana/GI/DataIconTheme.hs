{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.
-}


{- | This module is a kludge. It provides a single global theme for icons associated with
data types in the model.

The problem is that the icon theme is dictated by the application and then actually used at the
bottom concrete level. In between sits an abstract level that isn't supposed to know anything about
GTK. For the screen interaction this can be passed along with the rest of the Hades GTK parameters,
but diagram export and reports don't have that context.

The original solution was to mash the data icons in with the rest of the hicolor theme. That won't
work when GTK is being used properly, so in addition to the "Gtk.iconThemeGetDefault" global theme
we also need a separate theme.

The only other way around this would be to write modules to do abstract and concrete icons and
bypass the GTK functionality, which would be a lot of work to do properly. For now, lets just
use the GTK Icon Theme type.
-}
module Reactive.Banana.GI.DataIconTheme (
  setDataIconTheme,
  getDataIconTheme
) where

import Control.Monad.IO.Class
import Data.IORef
import qualified GI.Gtk as Gtk
import System.IO.Unsafe


setDataIconTheme :: (MonadIO m) => Gtk.IconTheme -> m ()
setDataIconTheme thm = liftIO $ writeIORef theDataIconTheme thm


-- | This is initially an empty icon theme.
getDataIconTheme :: (MonadIO m) => m Gtk.IconTheme
getDataIconTheme = liftIO $ readIORef theDataIconTheme


-- This is based on similar code in the implementation of "System.Random", where it is used
-- to implement the global random number generator.
{-# NOINLINE theDataIconTheme #-}
theDataIconTheme :: IORef Gtk.IconTheme
theDataIconTheme = unsafePerformIO $ do
  thm <- Gtk.iconThemeNew
  newIORef thm
