{-# LANGUAGE OverloadedLabels #-}

{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

-- |


module Hades.GI.Toolbar (makeGtkDeltaToolbar) where

import Control.Monad
import Data.List
import GI.Gtk( AttrOp( (:=) ) )
import qualified GI.Gtk as Gtk
import Hades.Abstract
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.Banana.GI.Menu


-- | Create a GTK toolbar from the Hades tool specification. If @v@ has a view control dialog
-- then the control button is automatically inserted at the far end.
makeGtkDeltaToolbar :: (Viewable d) => DeltaToolbar d -> MomentIO (Gtk.Toolbar, Event [Action d])
makeGtkDeltaToolbar (DeltaToolbar l r) = do
    bar <- liftIO Gtk.toolbarNew
    evs1 <- sequence $ intersperse (addSep bar) $ map (addTools bar) l
    bigSep <- Gtk.new Gtk.SeparatorToolItem [#draw := False]
    Gtk.toolbarInsert bar bigSep (-1)
    trueValue <- liftIO $ Gtk.toGValue True
    Gtk.containerChildSetProperty bar bigSep "expand" trueValue  -- See Gtk.Toolbar docs
    evs2 <- sequence $ intersperse (addSep bar) $ map (addTools bar) r
    return (bar, foldr (unionWith (++)) never (evs1 ++ evs2))
  where
    addSep bar = liftIO $ do
      sep <- Gtk.separatorToolItemNew
      Gtk.toolbarInsert bar sep (-1)
      return never
    addTools bar ts = do
      evs <- forM ts $ \t -> do
        (btn, ev) <- makeToolItem t
        Gtk.toolbarInsert bar btn (-1)
        return $ return <$> ev  -- Turn Event Action into Event [Action]
      return $ foldr (unionWith (++)) never evs


makeToolItem :: (Viewable d) => DeltaTool d -> MomentIO (Gtk.ToolButton, Event (Action d))
makeToolItem t = do
  btn <- Gtk.toolButtonNew (Nothing :: Maybe Gtk.Widget) $ Just $ toolTip t
  Gtk.toolButtonSetIconName btn $ Just $ toolIcon t
  Gtk.toolItemSetTooltipText btn $ toolTip t
  case toolAction t of
    DeltaToolAction act -> do
      (ev, handler) <- newEvent
      void $ Gtk.onToolButtonClicked btn $ handler act
      return (btn, ev)
    DeltaToolMenu menu -> do
      (gMenu, result) <- mkGtkMenu menu
      (ev, handler) <- newEvent
      void $ Gtk.onToolButtonClicked btn $ handler gMenu
      popupMenuOn ev
      return (btn, result)
