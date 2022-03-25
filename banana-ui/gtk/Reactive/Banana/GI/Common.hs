{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecursiveDo #-}

{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the banana-ui-gtk library. The banana-ui-gtk library is
proprietary and confidential. Copying is prohibited
-}

-- |
--
-- Common GTK-based functions for tables and dialogs.
module Reactive.Banana.GI.Common where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Coerce
import qualified Data.Colour as C
import qualified Data.Colour.CIE as C
import qualified Data.Colour.SRGB as C
import Data.Colour.Names (white)
import qualified Data.GI.Gtk as MV  -- for ModelView
import Data.List
import Data.Maybe
import Data.IORef
import Data.Text (Text, unpack, pack)
import Data.Time
import Data.Tree
import qualified GI.Gdk as Gdk
import qualified GI.GdkPixbuf as Gdk
import GI.Gtk (AttrOp ((:=)))
import qualified GI.Cairo as Cairo
import qualified GI.Cairo.Render as Cairo
import qualified GI.Cairo.Render.Connector as Cairo
import qualified GI.Gtk as Gtk
import Paths_banana_ui_gtk
import Reactive.Banana.Combinators
import Reactive.Banana.Common
import Reactive.Banana.Frameworks
import Reactive.Banana.GI.Connect
import System.IO


-- | Put a calendar icon at the end of the field and create a Calendar widget in a popover. The
-- popover is attached to the Entry widget.
createDatePopover :: (Gtk.IsWidget entry, Gtk.IsEntry entry, MonadIO m) =>
   Prism' Text Day -> entry -> m (Gtk.Popover, Gtk.Calendar)
createDatePopover prsm entry = do
      Gtk.entrySetIconFromIconName
         entry
         Gtk.EntryIconPositionSecondary
         $ Just "x-office-calendar-symbolic"
      calendar <- Gtk.calendarNew
      overlay <- Gtk.popoverNew $ Just entry
      Gtk.containerAdd overlay calendar
      Gtk.widgetShow calendar
      today <- liftIO $ utctDay <$> getCurrentTime
      str <- Gtk.entryGetText entry
      let (year, month, day) = toGregorian $ fromMaybe today $ str ^? prsm
      Gtk.calendarSelectMonth calendar (fromIntegral month - 1) (fromIntegral year)
      Gtk.calendarSelectDay calendar $ fromIntegral day
      void $ Gtk.onEntryIconPress entry $ \_ _ -> Gtk.popoverPopup overlay
      return (overlay, calendar)


-- | Put a colour icon at the end of the field and create a colour chooser widget in a popover. The
-- popover is attached to the Entry widget.
createColorDialog :: (Gtk.IsWidget entry, Gtk.IsEntry entry, MonadIO m) =>
   entry -> m Gtk.ColorChooserDialog
createColorDialog entry = do
      Gtk.entrySetIconFromIconName
         entry
         Gtk.EntryIconPositionSecondary
         $ Just "applications-science"
      parent <- widgetWindow entry
      chooser <- Gtk.colorChooserDialogNew Nothing parent
      str <- Gtk.entryGetText entry
      let colour = fromMaybe (Colour white) $ str ^? colourPrism
      bg <- colourToRGBA colour
      Gtk.colorChooserSetRgba chooser bg
      return chooser


-- | White for dark colours, black for bright or pale colours.
contrastText :: Colour -> Colour
contrastText c = Colour $ if C.luminance (getColour c) > 0.5 then C.black else white


-- | Opaque representation of a colour for GTK.
colourToRGBA :: (MonadIO m) => Colour -> m Gdk.RGBA
colourToRGBA colour = do
   let C.RGB r g b = C.toSRGB $ getColour colour
   Gtk.new Gdk.RGBA [#alpha := 1.0, #red := r, #green := g, #blue := b]


-- | Extract the Colour component of an RGBA (i.e. ignoring transparency).
rgbaToColour :: (MonadIO m) => Gdk.RGBA -> m Colour
rgbaToColour rgba =
   Colour <$> (C.sRGB <$> Gdk.getRGBARed rgba <*> Gdk.getRGBAGreen rgba <*> Gdk.getRGBABlue rgba)


-- | Data about an icon.
data IconData = IconData {
      iconName :: Text,  -- ^ The name of the icon.
      iconPixbuf :: Gdk.Pixbuf  -- ^ The image for the icon.
   }


-- | The icon called \"no-icon\" is a special case. Where the icon is optional this is used
-- for the @Nothing@ value. It contains the text "No Icon".
noIconName :: IconName
noIconName = "no-icon"

-- | The icon called \"blank-icon\" is a similar special case. It contains nothing.
blankIconName :: IconName
blankIconName = "blank-icon"


-- | Extract a tree of icon names from the theme. The result is a list of pairs, one for
-- each context, with the context name and a list of the icons in that context.
iconThemeContents :: (MonadIO m, Gtk.IsIconTheme theme) =>
   theme
   -> Bool  -- ^ If true, then include \"no-icon\" icon at start of each context.
   -> (Text -> Bool)  -- ^ Filter for context names.
   -> m [(Text, [IconData])]
iconThemeContents theme flag predicate = do
   themeContexts <- Gtk.iconThemeListContexts theme
   liftIO $ putStrLn $ "iconThemeContents: contexts = " <> show themeContexts
   ctxs <- sort . filter predicate <$> Gtk.iconThemeListContexts theme
   forM ctxs $ \ctx -> do
      iconNames <- sort <$> Gtk.iconThemeListIcons theme (Just ctx)
      let iconNames1 = if flag then noIconName : iconNames else iconNames
      icons <- forM iconNames1 $ \nm ->
         Gtk.iconThemeLoadIcon theme nm 48 [Gtk.IconLookupFlagsForceSize] >>= \case
            Nothing -> return Nothing
            Just pb -> return $ Just $ IconData nm pb
      return (ctx, catMaybes icons)


-- | A table of icons for the user to pick one. The event carries the selected icon name.
iconListWidget ::
   [IconData] -> MomentIO (Gtk.ScrolledWindow, Event Text)
iconListWidget icons = do
   store <- MV.seqStoreNew icons
   MV.customStoreSetColumn store (MV.makeColumnIdString 0) iconName
   MV.customStoreSetColumn store (MV.makeColumnIdPixbuf 1) iconPixbuf
   vw <- Gtk.new Gtk.IconView
         [#model := store, #pixbufColumn := 1, #selectionMode := Gtk.SelectionModeBrowse]
   scrolled <- Gtk.new Gtk.ScrolledWindow [#minContentHeight := 800]
   Gtk.containerAdd scrolled vw
   (ev, handleE) <- newEvent
   void $ Gtk.onIconViewItemActivated vw $
      Gtk.treePathGetIndices >=> \case
         Just (n:_) -> do
            v <- MV.seqStoreGetValue store n
            handleE $ iconName v
         _ -> return ()
   return (scrolled, ev)


-- A notebook with one tab for each icon context. The event carries the selected icon name.
iconTabsWidget :: [(Text, [IconData])] -> MomentIO (Gtk.Notebook, Event Text)
iconTabsWidget ctxs = do
   book <- Gtk.notebookNew
   Gtk.notebookSetScrollable book True
   Gtk.notebookPopupEnable book
   events <- forM ctxs $ \(ctx, icons) -> do
      (page, event) <- iconListWidget icons
      void $ Gtk.notebookAppendPage book page (Nothing :: Maybe Gtk.Widget)
      void $ Gtk.notebookSetTabLabelText book page ctx
      return event
   return (book, foldr (unionWith const) never events)


-- | A popup dialog which returns when the first icon is selected. Last argument is an IO
-- continuation to process the result. This is used to avoid race conditions.
iconDialog :: (MonadIO m, Gtk.IsWidget widget) =>
   widget -> [(Text, [IconData])] -> (Text -> IO ()) -> m ()
iconDialog parent ctxs cont = liftIO $ mdo
   dialog <- Gtk.new Gtk.Window [#title := "Icon Selection", #modal := True]
   Gtk.windowSetTransientFor dialog =<< Gtk.castTo Gtk.Window =<< Gtk.widgetGetToplevel parent
   Gtk.windowSetDefaultSize dialog 800 (-1)
   net <- compile $ withWaitCursor parent $ do
         (widget, event) <- iconTabsWidget ctxs
         Gtk.widgetShowAll widget
         Gtk.containerAdd dialog widget
         reactimate $ event <&> (\str -> do  -- Within local net, so no need to cancel it.
            cont str
            Gtk.widgetDestroy dialog
            pause net)
         Gtk.widgetShowAll dialog
   actuate net


-- | Execute an IO action with a \"wait\" cursor.
withWaitCursor :: (MonadIO m, Gtk.IsWidget w) => w -> m a -> m a
withWaitCursor widget action = do
   topWidget <- Gtk.widgetGetToplevel widget
   mGdkWin <- Gtk.widgetGetWindow topWidget
   case mGdkWin of
      Nothing -> return ()
      Just gdkWin -> do
         display <- Gdk.windowGetDisplay gdkWin
         busy <- Gdk.cursorNewForDisplay display Gdk.CursorTypeWatch
         Gdk.windowSetCursor gdkWin busy
   result <- action
   case mGdkWin of
      Nothing -> return ()
      Just gdkWin -> Gdk.windowSetCursor gdkWin (Nothing :: Maybe Gdk.Cursor)
   return result


-- | Try to get the window that contains the widget.
widgetWindow :: (MonadIO m, Gtk.IsWidget w) => w -> m (Maybe Gtk.Window)
widgetWindow w = do
   topWidget <- Gtk.widgetGetToplevel w
   liftIO $ Gtk.castTo Gtk.Window topWidget


-- | Are these two references actually pointing to the same widget?
sameWidget :: Gtk.Widget -> Gtk.Widget -> Bool
sameWidget (Gtk.Widget r1) (Gtk.Widget r2) =
      Gtk.managedForeignPtr r1 == Gtk.managedForeignPtr r2


-- | The tree of widgets underneath the argument.
widgetTree :: (MonadIO m, Gtk.IsWidget w) => w -> m (Tree Gtk.Widget)
widgetTree widget1 = do
   widget2 <- Gtk.toWidget widget1
   Node widget2 <$> widgetForest widget2


-- | The children of the argument as a forest of widgets.
widgetForest :: (MonadIO m, Gtk.IsWidget w) => w -> m (Forest Gtk.Widget)
widgetForest widget1 =
   liftIO (Gtk.castTo Gtk.Container widget1) >>= \case
      Nothing -> return []
      Just c -> do
         childs <- Gtk.containerGetChildren c
         mapM widgetTree childs


-- | Have the widget background colour follow a behavior. Foreground is either white or black
-- for contrast. If the behavior is @Nothing@ then the widget is drawn normally.
--
-- The colour is implemented using transparency. The style file should include
-- @hades-transparent-dark@ and @hades-transparent-light@ classes that set the background to
-- transparent.
widgetLinkColour :: (Gtk.IsWidget w) =>
   w -> Maybe Colour -> Event (Maybe Colour) -> Behavior (Maybe Colour) -> MomentIO ()
widgetLinkColour widget colourInit colourE colourB = do
      colourE1 <- plainChanges (Just widget) colourB
      colourB1 <- stepper colourInit $ unionWith const colourE colourE1
      drawContext <- registerIOSignal1 widget Gtk.onWidgetDraw $ \ctx -> return (False, ctx)
      stop1 <- reactimate1 $ customPaint <$> colourB1 <@> drawContext
      colourChanges <- changes $ Gtk.widgetQueueDraw widget <$ colourB1
      stop2 <- reactimate1' colourChanges
      void $ Gtk.onWidgetDestroy widget $ stop1 >> stop2
   where
      customPaint :: Maybe Colour -> Cairo.Context -> IO ()
      customPaint Nothing _ = do
         style <- Gtk.widgetGetStyleContext widget
         mapM_ (Gtk.styleContextRemoveClass style) [lightClass, darkClass]
      customPaint (Just c) ctx = do
         w <- Gtk.widgetGetAllocatedWidth widget
         h <- Gtk.widgetGetAllocatedHeight widget
         style <- Gtk.widgetGetStyleContext widget
         mapM_ (Gtk.styleContextRemoveClass style) [lightClass, darkClass]
         Gtk.styleContextAddClass style $
            if C.luminance (getColour c) > 0.5 then lightClass else darkClass
         flip Cairo.renderWithContext ctx $ do
            let
               C.RGB r1 g1 b1 = C.toSRGB $ getColour c
            Cairo.setSourceRGB r1 g1 b1
            Cairo.rectangle 0 0 (fromIntegral w) (fromIntegral h)
            Cairo.fill
      lightClass = "hades-transparent-light"
      darkClass = "hades-transparent-dark"


-- | Debugging routine.
objectDescription :: (Gtk.IsWidget a, MonadIO m) => a -> m String
objectDescription w = do
   typ <- unpack <$> Gtk.widgetGetName w
   let ptr = show $ Gtk.managedForeignPtr $ coerce w
   return $ typ <> "(" <> ptr <> ")"


-- | Do the action when the input changes unless the widget has focus or it is already in
-- the middle of an update.
unlessFocused :: (Gtk.IsWidget w) => w -> Changes a -> (a -> IO ()) -> MomentIO ()
unlessFocused w value action = do
   nugatory <- Gtk.widgetInDestruction w
   liftIO $ when nugatory $ hPutStrLn stderr "Warning: still updating a widget in destruction."
   blocker <- liftIO $ newIORef False
   stop <- reactimate1 $ changesE value <&> \v -> do
      focused <- Gtk.widgetHasFocus w
      b <- readIORef blocker
      -- Block value changes during "setter" because these are probably being triggered by the
      -- setter. GTK widgets can trigger arbitrary numbers of events during updates, and these can
      -- go infinite if re-entrant calls are permitted.
      unless (b || focused) $ do
         writeIORef blocker True
         action v
         writeIORef blocker False
   void $ Gtk.onWidgetDestroy w stop


-- | This is based on the function in the Reactive Banana documentation, but also takes a widget
-- argument. When the widget is destroyed the output event will stop.
plainChanges :: (Gtk.IsWidget w) => Maybe w -> Behavior a -> MomentIO (Event a)
plainChanges w b = do
   (e, handle) <- newEvent
   eb <- changes b
   stop <- reactimate1' $ fmap handle <$> eb
   forM_ w $ \w1 -> Gtk.onWidgetDestroy w1 stop
   return e


-- | The Banana UI GTK CSS file. Pass this to "Gtk.styleContextAddproviderForScreen" with a
-- priority of "Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION".
bananaCss :: IO Gtk.CssProvider
bananaCss = do
   r <- Gtk.cssProviderNew
   p <- getDataFileName "banana-gtk.css"
   Gtk.cssProviderLoadFromPath r $ pack p
   return r
