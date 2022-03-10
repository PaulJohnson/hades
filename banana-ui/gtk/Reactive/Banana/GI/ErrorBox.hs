{-# LANGUAGE OverloadedLabels #-}

{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the banana-ui-gtk library.  The banana-ui-gtk library is
proprietary and confidential. Copying is prohibited 
-}

{- |
-}

module Reactive.Banana.GI.ErrorBox where

import Control.Monad
import Control.Monad.IO.Class
import Data.Text
import GI.Gtk (AttrOp ((:=)))
import qualified GI.Gtk as Gtk
import System.IO.Unsafe


{-# NOINLINE cannotHappen #-}
-- | Displays an error box warning of an \"unexpected event\".
cannotHappen :: Text -> a -> a
cannotHappen err v = unsafePerformIO $ do
   errorBox (Nothing :: Maybe Gtk.Widget) $ msg <> err
   return v
   where
      msg = "An unexpected event has occured. \
         \Please save your work in a new file and report this bug.\n\n"


-- | A standard error dialog.
errorBox :: (MonadIO m, Gtk.IsWidget parent) => Maybe parent -> Text -> m ()
errorBox parent msg = liftIO $ do  -- Error dialog box.
   box <- Gtk.new Gtk.MessageDialog [
         #messageType := Gtk.MessageTypeError,
         #buttons := Gtk.ButtonsTypeClose,
         #text := msg,
         #modal := True
      ]
   forM_ parent $ \p -> do
      parentWin <- Gtk.castTo Gtk.Window =<< Gtk.widgetGetToplevel p
      forM_ parentWin $ \w -> Gtk.set box [#transientFor := w]
   void $ Gtk.dialogRun box
   Gtk.widgetDestroy box
