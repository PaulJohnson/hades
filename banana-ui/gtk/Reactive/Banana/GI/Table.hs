{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the banana-ui-gtk library.
-}

-- |


module Reactive.Banana.GI.Table (
   mkTableView,
   mkTableColumn,
   mkTreeTableView
) where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Colour.Names (white)
import qualified Data.GI.Gtk as MV  -- for ModelView
import Data.Int
import Data.IORef
import Data.Maybe
import Data.Time.Calendar
import Data.Tree
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import GI.Gtk (AttrOp ((:=)))
import qualified GI.Pango as Pango
import Reactive.Banana hiding ((<>))
import Reactive.Banana.Common
import Reactive.Banana.Frameworks
import Reactive.Banana.GI.Common
import Reactive.Banana.GI.Connect
import Reactive.Banana.GI.ErrorBox
import Reactive.Banana.Table


-- | A table allowing you to edit a list of items. Returns a widget containing the list, the
-- data store, an event for row activation, and an event for an edit to the list.
mkTableView :: (Eq row) =>
   [TableEditing row]  -- ^ Flags for how this table may be edited.
   -> Bool   -- ^ If True then send row activation events (see return value).
   -> Table row   -- ^ Definition of table columns.
   -> [row]  -- ^ Initial content for table.
   -> Event [row]  -- ^ Updates for table content.
   -> Behavior [row]
   -> MomentIO (Gtk.Box, MV.SeqStore row, Event (Int32, row), Event [row])
mkTableView flags activationFlag cols initial e b = do
      store <- MV.seqStoreNew initial
      scroll <- Gtk.scrolledWindowNew noAdjustment noAdjustment
      style <- Gtk.widgetGetStyleContext scroll
      Gtk.styleContextAddClass style "hades-table"
      Gtk.setScrolledWindowShadowType scroll Gtk.ShadowTypeIn
      Gtk.set scroll [
            #propagateNaturalHeight := True,
            #propagateNaturalWidth := True
         ]
      vw <- Gtk.new Gtk.TreeView [
            #model := store,
            #enableGridLines := Gtk.TreeViewGridLinesBoth,
            #hexpand := True,
            #vexpand := True,
            #heightRequest := 400]  -- Hard coded, but a reasonable value. The default is tiny.
      Gtk.containerAdd scroll vw
      -- Tracking changes.
      blockSignals <- liftIO $ newIORef False
      stop1 <- reactimate1 $ updateStore store <$> e
      ch <- changes $ b <&> (\v -> do
            writeIORef blockSignals True
            updateStore store v
            writeIORef blockSignals False
         )
      stop2 <- reactimate1' ch
      let
         storeChangeHandler = do
            block <- readIORef blockSignals
            if block
               then return ((), Nothing)
               else ((), ) . Just <$> MV.seqStoreToList store
      changed <- filterJust <$>
         registerIOSignal2 store Gtk.afterTreeModelRowChanged (const $ const storeChangeHandler)
      deleted <- filterJust <$>
         registerIOSignal1 store Gtk.afterTreeModelRowDeleted (const storeChangeHandler)
      inserted <- filterJust <$>
         registerIOSignal2 store Gtk.afterTreeModelRowInserted (const $ const storeChangeHandler)
      void $ Gtk.onWidgetDestroy vw $ stop1 >> stop2
      -- Selection plumbing.
      sel <- Gtk.treeViewGetSelection vw
      Gtk.treeSelectionSetMode sel Gtk.SelectionModeSingle
      forM_ cols $ mkTableColumn (modifyStore store) vw store >=> Gtk.treeViewAppendColumn vw
      buttons <- Gtk.buttonBoxNew Gtk.OrientationHorizontal
      Gtk.buttonBoxSetLayout buttons Gtk.ButtonBoxStyleCenter
      editEvents <- forM flags $ \case
         TableAdd str v -> do
            (btn, addEvent) <- addButton store str v
            void $ Gtk.boxPackStart buttons btn False False 0
            return addEvent
         TableDelete -> do
            btn <- delButton store sel
            void $ Gtk.boxPackStart buttons btn False False 0
            return never
         TableShuffle -> do
            Gtk.set vw [#reorderable := True]
            -- #reorderable isn't working. Don't know why. Using buttons instead.
            b1 <- upButton store sel
            b2 <- downButton store sel
            void $ Gtk.boxPackStart buttons b1 False False 0
            void $ Gtk.boxPackStart buttons b2 False False 0
            return never
      activationEvent <- filterJust <$> if activationFlag
         then registerIOSignal2 vw Gtk.onTreeViewRowActivated $ \path _ ->
               -- (n:_) <- fromMaybe [] <$> Gtk.treePathGetIndices path
               Gtk.treePathGetIndices path >>= \case
                  Just (n:_) -> do
                     row <- MV.seqStoreGetValue store n
                     return ((), Just (n, row))
                  _ -> return ((), Nothing)
         else return never
      box <- Gtk.boxNew Gtk.OrientationVertical 1
      Gtk.boxPackStart box scroll True True 0
      Gtk.boxPackStart box buttons False False 0
      return (
            box,
            store,
            foldr1 (unionWith const) $ activationEvent : editEvents,
            foldr1 (unionWith const) [changed, deleted, inserted]
               -- foldr1 safe because lists are never empty.
         )
   where
      modifyStore model path f =
         Gtk.treePathGetIndices path >>= \case
            Just (n:_) -> do
               row <- MV.seqStoreGetValue model n
               MV.seqStoreSetValue model n $ f row
            _ -> return ()
      updateStore store newVal = do
         oldVal <- MV.seqStoreToList store
         let
            oldLen = length oldVal
            newLen = length newVal
         forM_ (zip3 [0..] oldVal newVal) $ \(n, v1, v2) ->
            when (v1 /= v2) $ MV.seqStoreSetValue store n v2
         forM_ [oldLen-1, oldLen-2 .. newLen] $ MV.seqStoreRemove store . fromIntegral
         forM_ (zip [oldLen..] $ drop oldLen newVal) $ \(n, v) ->
            MV.seqStoreInsert store (fromIntegral n) v
      addButton store str v = do
         btn <- Gtk.new Gtk.Button [#label := "+ " <> str]
         addEvent <- filterJust <$> registerIOSignal btn Gtk.onButtonClicked ( do
               n <- MV.seqStoreAppend store v
               return ((), if activationFlag then Just (n, v) else Nothing)
            )
         return (btn, addEvent)
      delButton store sel = do
         btn <- Gtk.new Gtk.Button [#label := "Delete"]
         void $ Gtk.onButtonClicked btn $ do
            (isSel, _, iter) <- Gtk.treeSelectionGetSelected sel
            n <- MV.seqStoreIterToIndex iter
            when isSel $ MV.seqStoreRemove store n
         return btn
      upButton store sel = do
         btn <- Gtk.new Gtk.Button [#label := "Up"]
         void $ Gtk.onButtonClicked btn $ do
            (isSel, _, iter) <- Gtk.treeSelectionGetSelected sel
            n <- MV.seqStoreIterToIndex iter
            when (isSel && n > 0) $ do
               v <- MV.seqStoreGetValue store n
               MV.seqStoreRemove store n
               MV.seqStoreInsert store (n-1) v
               void $ Gtk.treeModelIterPrevious store iter
               Gtk.treeSelectionSelectIter sel iter
         return btn
      downButton store sel = do
         btn <- Gtk.new Gtk.Button [#label := "Down"]
         void $ Gtk.onButtonClicked btn $ do
            (isSel, _, iter) <- Gtk.treeSelectionGetSelected sel
            n <- MV.seqStoreIterToIndex iter
            size <- MV.seqStoreGetSize store
            when (isSel && n < (size-1)) $ do
               v <- MV.seqStoreGetValue store n
               MV.seqStoreRemove store n
               MV.seqStoreInsert store (n+1) v
               void $ Gtk.treeModelIterNext store iter
               Gtk.treeSelectionSelectIter sel iter
         return btn


-- | Create a column within a table.
mkTableColumn :: (MonadIO m, MV.IsTypedTreeModel s, Gtk.IsTreeModel (s row)) =>
   (Gtk.TreePath -> (row -> row) -> IO ())
   -> Gtk.TreeView
   -> s row
   -> TableColumn row
   -> m Gtk.TreeViewColumn
mkTableColumn modifyModel vw model (TableColumn (col :: TableField row a)) = do
      tvCol <- Gtk.new Gtk.TreeViewColumn [
               #reorderable := False,
               #resizable := True,
               #title := columnName col
            ]
      case fieldIcon col of
         Nothing -> return ()
         Just iconF -> do
            icon <- Gtk.cellRendererPixbufNew
            Gtk.cellLayoutPackStart tvCol icon False
            MV.cellLayoutSetAttributes tvCol icon model $ \item ->
                  [#iconName := item ^. fieldValue col . to iconF]
      void $ makeRenderer modifyModel model vw col tvCol
      return tvCol



-- | A table allowing you to edit a tree of items. Each individual row can be edited, but
-- rows cannot be added, deleted or moved.
mkTreeTableView :: (Eq row) =>
   Bool   -- ^ If True then send row activation events (see return value).
   -> [Table row]   -- ^ Each sublist is used for one level of the tree.
   -> Forest row  -- ^ Initial content for table.
   -> Changes (Forest row)  -- ^ Updates for table content.
   -> MomentIO (Gtk.Box, MV.ForestStore (Int, row), Event (Gtk.TreePath, row))
mkTreeTableView activationFlag groups initial value = do
      store <- MV.forestStoreNew $ mungForest 0 initial
      scroll <- Gtk.scrolledWindowNew noAdjustment noAdjustment
      Gtk.set scroll [
            #propagateNaturalHeight := True,
            #propagateNaturalWidth := True
         ]
      Gtk.setScrolledWindowShadowType scroll Gtk.ShadowTypeIn
      vw <- Gtk.new Gtk.TreeView [
            #model := store,
            #enableGridLines := Gtk.TreeViewGridLinesBoth,
            #enableTreeLines := True,
            #hexpand := True,
            #vexpand := True]
      stop <- reactimate1 $ updateForest vw store . mungForest 0 <$> changesE value
      void $ Gtk.onWidgetDestroy vw stop
      Gtk.containerAdd scroll vw
      sel <- Gtk.treeViewGetSelection vw
      Gtk.treeSelectionSetMode sel Gtk.SelectionModeSingle
      forM_ (zip [0..] groups) $ \(n, cols) ->
         forM_ cols $ \(TableColumn field) -> do
            let field1 = mungTableField n field
            tc <- mkTableColumn (modifyStore store) vw store $ TableColumn field1
            Gtk.treeViewAppendColumn vw tc
      buttons <- Gtk.buttonBoxNew Gtk.OrientationHorizontal
      Gtk.buttonBoxSetLayout buttons Gtk.ButtonBoxStyleCenter
      box <- Gtk.boxNew Gtk.OrientationVertical 1
      activationEvent <- if activationFlag
         then registerIOSignal2 vw Gtk.onTreeViewRowActivated $ \path _ -> do
               row <- MV.forestStoreGetValue store path
               return ((), (path, snd row))
         else return never
      Gtk.boxPackStart box scroll True True 0
      Gtk.boxPackStart box buttons False False 0
      Gtk.treeViewExpandAll vw
      return (box, store, activationEvent)
   where
      modifyStore :: (MonadIO m) => MV.ForestStore a -> Gtk.TreePath -> (a -> a) -> m ()
      modifyStore store path f = void $ MV.forestStoreChange store path f
      updateForest :: (Eq a) => Gtk.TreeView -> MV.ForestStore a -> Forest a -> IO ()
      updateForest vw store newVal = do
         MV.forestStoreClear store
         treePath <- Gtk.treePathNew  -- Null path for root.
         MV.forestStoreInsertForest store treePath 0 newVal
         Gtk.treeViewExpandAll vw


{- Design Note:

The tree entries have to be decorated with a depth number, and other types need to be
modified in a corresponding way. This is so that the display distinguishes between an
entity name in level 1 and level 2. The family of functions needed to do this are called
"mung".
-}

mungTree :: Int -> Tree a -> Tree (Int, a)
mungTree level (Node v cs) = Node (level, v) $ mungForest (level+1) cs

mungForest :: Int -> Forest a -> Forest (Int, a)
mungForest level = map (mungTree level)

mungTableField :: Int -> TableField row a -> TableField (Int, row) a
mungTableField level field = field {fieldValue = mung . fieldValue field}
   where
      mung :: Traversal' (Int, row) row
      mung f (n, v) =
         if n == level
            then (n,) <$> f v
            else pure (n, v)


-- | Low level function to create the appropriate renderer and attach it to the
-- "Gtk.TreeViewColumn".
makeRenderer :: (MonadIO m, MV.IsTypedTreeModel s, Gtk.IsTreeModel (s row)) =>
   (Gtk.TreePath -> (row -> row) -> IO ())  -- ^ Modify a value in the store.
   -> s row                    -- ^ Data store that this renderer accesses.
   -> Gtk.TreeView             -- ^ The widget where the renderer will be displayed.
   -> TableField row a         -- ^ Abstract description of the table column.
   -> Gtk.TreeViewColumn       -- ^ The GTK column that this renderer must be appended to.
   -> m ()
makeRenderer modifyStore store vw col tvCol = renderer $ fieldEditor col
   where
      renderer = \case
         EditBool -> do
            tick <- Gtk.new Gtk.CellRendererText [#editable := True, #sensitive := True]
               -- Not using CellRendererToggle because it cannot be coloured.
            Gtk.cellLayoutPackEnd tvCol tick True
            MV.cellLayoutSetDataFunction tvCol tick store $ \item -> do
               colour <- case fieldColour col of
                  Just f -> setColour $ fromMaybe (Colour white) $ item ^? fieldValue col . to f
                  Nothing -> setColour $ Colour white
               let b = item ^. fieldValue col . to (\b1 -> if b1 then "🗹" else "☐")
                  -- Unicode U+1F5F9 and U+2610
               Gtk.set tick $ colour ++ [#text := b]
            void $ Gtk.onCellRendererEditingStarted tick $ \_ pathStr -> do
               -- Toggle the stored value and cancel the edit.
               p <- Gtk.treePathNewFromString pathStr
               modifyStore p $ fieldValue col %~ not
               Gtk.cellRendererStopEditing tick True
         EditEntry prsm -> void $ createTextRenderer prsm tvCol
         EditNote -> do
            c <- createTextRenderer id tvCol
            Gtk.set c [#wrapMode := Pango.WrapModeWordChar, #wrapWidth := 200]
            -- ToDo: Figure out how to implement
            -- https://github.com/otsaloma/gaupol/blob/master/gaupol/renderers/multiline.py
            -- in Haskell.
         EditEnum textF vs -> void $ createComboRenderer textF vs tvCol
         EditDate fmt -> do
            txt <- createTextRenderer (datePrism fmt) tvCol
            void $ Gtk.onCellRendererEditingStarted txt $ \editable pathStr ->
               Gtk.castTo Gtk.Entry editable >>= \case
                  Nothing -> return ()
                  Just entry -> do
                     -- The popover grabs the focus, which causes "entry" to be umapped and
                     -- unparented. Hence we have to stick the popover to its rectangle on
                     -- the parent widget instead.
                     (overlay, calendar) <- createDatePopover (datePrism fmt) entry
                     void $ Gtk.onWidgetSizeAllocate entry $ \posn -> do
                        y <- Gdk.getRectangleY posn
                        x <- Gdk.getRectangleX posn
                        (x1, y1) <- Gtk.treeViewConvertTreeToWidgetCoords vw x y
                        Gtk.set posn [#x := x1, #y := y1]
                        parent <- Gtk.widgetGetParent entry
                        Gtk.popoverSetRelativeTo overlay parent
                        Gtk.popoverSetPointingTo overlay posn
                     void $ Gtk.onCalendarDaySelectedDoubleClick calendar $ do
                        p <- Gtk.treePathNewFromString pathStr
                        (year, month, day) <- Gtk.calendarGetDate calendar
                        let date = fromGregorian
                              (fromIntegral year)
                              (fromIntegral month + 1)
                              (fromIntegral day)
                        modifyStore p $ fieldValue col .~ date
                        Gtk.popoverPopdown overlay
         EditColour -> do
            txt <- Gtk.new Gtk.CellRendererText [#editable := True, #widthChars := 12]
            Gtk.cellLayoutPackEnd tvCol txt True
            MV.cellLayoutSetDataFunction tvCol txt store $ \item -> do
               colour <- setColour $ fromMaybe (Colour white) $ item ^? fieldValue col
               Gtk.set txt $ colour ++ [#text := item ^. fieldValue col . re colourPrism]
            void $ Gtk.onCellRendererTextEdited txt $ \pathStr newText ->
               case newText ^? colourPrism of
                  Nothing ->
                     return ()
                  Just v -> do
                     p <- Gtk.treePathNewFromString pathStr
                     modifyStore p $ fieldValue col .~ v
            void $ Gtk.onCellRendererEditingStarted txt $ \editable pathStr ->
               Gtk.castTo Gtk.Entry editable >>= \case
                  Nothing -> return ()
                  Just entry -> do
                     chooser <- createColorDialog entry
                     void $ Gtk.onEntryIconPress entry $ \_ _ -> do
                        r <- toEnum . fromIntegral <$> Gtk.dialogRun chooser
                        case r of
                           Gtk.ResponseTypeOk -> do
                              colour <- rgbaToColour =<< Gtk.colorChooserGetRgba chooser
                              p <- Gtk.treePathNewFromString pathStr
                              modifyStore p $ fieldValue col .~ colour
                           _ -> return ()
                        Gtk.widgetDestroy chooser
         EditIcon predicate -> do
            pic <- Gtk.new Gtk.CellRendererPixbuf [#sensitive := True]
            Gtk.cellLayoutPackEnd tvCol pic True
            MV.cellLayoutSetAttributes tvCol pic store $ \item ->
                  [#iconName := item ^. fieldValue col]
            void $ Gtk.onTreeViewRowActivated vw $ \path activeCol -> do
               path1 <- Gtk.treePathCopy path  -- path is no longer valid once the callback exits.
               let
                  Gtk.TreeViewColumn p1 = activeCol
                  Gtk.TreeViewColumn p2 = tvCol
               when (Gtk.managedForeignPtr p1 == Gtk.managedForeignPtr p2) $ do
                  theme <- withWaitCursor vw Gtk.iconThemeGetDefault
                  icons <- withWaitCursor vw $ iconThemeContents theme False predicate
                  iconDialog vw icons $ \newIcon ->
                     modifyStore path1 $ fieldValue col .~ newIcon
         EditCombo vs -> void $ createComboRenderer id vs tvCol
         EditFixed f -> do
            txt <- createTextRenderer (prism' f $ const Nothing) tvCol
            Gtk.set txt [#editable := False]
         Opt EditBool -> void $ createComboRenderer
                  (\case
                     Nothing -> ""
                     Just True -> "🗹"
                     Just False -> "☐")
                  [Nothing, Just True, Just False]
                  tvCol
         Opt (EditEntry prsm) -> renderer $ EditEntry $ prismToMaybe prsm
         Opt EditNote -> do
            c <- createTextRenderer (prismToMaybe id) tvCol
            Gtk.set c [#wrapMode := Pango.WrapModeWordChar, #wrapWidth := 200]
            -- ToDo: Figure out how to implement
            -- https://github.com/otsaloma/gaupol/blob/master/gaupol/renderers/multiline.py
            -- in Haskell.
         Opt (EditEnum textF vs) -> void $ createComboRenderer
                  (\case
                     Nothing -> ""
                     Just v -> textF v)
                  (Nothing : map Just vs)
                  tvCol
         Opt (EditDate fmt) -> do
            -- There is a lot of overlap with the plain EditDate version. This is not good, but
            -- factoring out the commonality is non-trivial
            txt <- createTextRenderer (prismToMaybe $ datePrism fmt) tvCol
            void $ Gtk.onCellRendererEditingStarted txt $ \editable pathStr ->
               Gtk.castTo Gtk.Entry editable >>= \case
                  Nothing -> return ()
                  Just entry -> do
                     -- The popover grabs the focus, which causes "entry" to be umapped and
                     -- unparented. Hence we have to stick the popover to its rectangle on
                     -- the parent widget instead.
                     (overlay, calendar) <- createDatePopover (datePrism fmt) entry
                     void $ Gtk.onWidgetSizeAllocate entry $ \posn -> do
                        parent <- Gtk.widgetGetParent entry
                        x <- Gdk.getRectangleX posn
                        y <- Gdk.getRectangleY posn
                        (x1, y1) <- Gtk.treeViewConvertTreeToWidgetCoords vw x y
                        Gtk.set posn [#x := x1, #y := y1]
                        Gtk.popoverSetRelativeTo overlay parent
                        Gtk.popoverSetPointingTo overlay posn
                     void $ Gtk.onCalendarDaySelectedDoubleClick calendar $ do
                        (year, month, day) <- Gtk.calendarGetDate calendar
                        let date = fromGregorian
                              (fromIntegral year)
                              (fromIntegral month + 1)
                              (fromIntegral day)
                        p <- Gtk.treePathNewFromString pathStr
                        modifyStore p $ fieldValue col ?~ date
                        Gtk.popoverPopdown overlay
         Opt EditColour -> do
            txt <- Gtk.new Gtk.CellRendererText [#editable := True, #widthChars := 12]
            Gtk.cellLayoutPackEnd tvCol txt True
            MV.cellLayoutSetDataFunction tvCol txt store $ \item ->
               case item ^. fieldValue col of
                  Nothing -> do
                     colour <- setColour $ Colour white
                     Gtk.set txt $ colour ++ [#text := ""]
                  Just c -> do
                     colour <- setColour c
                     Gtk.set txt $ colour ++ [#text := c ^. re colourPrism]
            void $ Gtk.onCellRendererTextEdited txt $ \ pathStr newText ->
               case newText ^? prismToMaybe colourPrism of
                  Nothing -> return ()
                  Just v -> do
                     p <- Gtk.treePathNewFromString pathStr
                     modifyStore p $ fieldValue col .~ v
            void $ Gtk.onCellRendererEditingStarted txt $ \editable pathStr ->
               Gtk.castTo Gtk.Entry editable >>= \case
                  Nothing -> return ()
                  Just entry -> do
                     chooser <- createColorDialog entry
                     void $ Gtk.onEntryIconPress entry $ \_ _ -> do
                        r <- toEnum . fromIntegral <$> Gtk.dialogRun chooser
                        case r of
                           Gtk.ResponseTypeOk -> do
                              colour <- rgbaToColour =<< Gtk.colorChooserGetRgba chooser
                              p <- Gtk.treePathNewFromString pathStr
                              modifyStore p $ fieldValue col ?~ colour
                           _ -> return ()
                        Gtk.widgetDestroy chooser
         Opt (EditIcon predicate) -> do
            pic <- Gtk.new Gtk.CellRendererPixbuf [#sensitive := True]
            Gtk.cellLayoutPackEnd tvCol pic True
            MV.cellLayoutSetAttributes tvCol pic store $ \item ->
                  [#iconName := fromMaybe "no-icon" (item ^. fieldValue col)]
            void $ Gtk.onTreeViewRowActivated vw $ \path activeCol -> do
               path1 <- Gtk.treePathCopy path  -- path is not valid after the callback exits.
               let
                  Gtk.TreeViewColumn p1 = activeCol
                  Gtk.TreeViewColumn p2 = tvCol
               when (Gtk.managedForeignPtr p1 == Gtk.managedForeignPtr p2) $ do
                  theme <- Gtk.iconThemeGetDefault   -- ToDo: create a custom theme for application.
                  icons <- withWaitCursor vw $ iconThemeContents theme True predicate
                  iconDialog vw icons $ \newIcon -> do
                     let v = if newIcon == "no-icon" then Nothing else Just newIcon
                     modifyStore path1 $ fieldValue col .~ v
         Opt (EditCombo vs) ->
            void $ createComboRenderer (fromMaybe "") (Nothing : map Just vs) tvCol
         Opt (EditFixed f) -> do
            let
               f2 Nothing = ""
               f2 (Just v) = f v
            renderer $ EditFixed f2
         Opt (Opt _) -> cannotHappen
               "makeRenderer: Nested Opt not supported in FieldEditor type."
               $ return ()
      -- createTextRenderer :: (MonadIO m) =>
      --   Prism' Text a -> Gtk.TreeViewColumn -> m Gtk.CellRendererText
      createTextRenderer prsm tvCol1 = do
         txt <- Gtk.new Gtk.CellRendererText [#editable := True]
         Gtk.cellLayoutPackEnd tvCol1 txt True
         MV.cellLayoutSetDataFunction tvCol1 txt store $ \item -> do
            let v = item ^? fieldValue col
            colour <- case fieldColour col of
               Just f -> setColour $ maybe (Colour white) f v
               Nothing -> setColour (Colour white)
            Gtk.set txt $ colour ++ [#text := fromMaybe "" (v ^? _Just . re (clonePrism prsm))]
         -- By default loss of focus merely cancels the edit. This commits the edit instead.
         void $ Gtk.onCellRendererEditingStarted txt $ \editable _ ->
            void $ Gtk.onWidgetFocusOutEvent editable $ \_ -> do
               Gtk.cellEditableEditingDone editable
               return False
         void $ Gtk.onCellRendererTextEdited txt $ \ pathStr newText ->
            case newText ^? clonePrism prsm of
               Nothing ->
                  return ()
               Just v -> do
                  p <- Gtk.treePathNewFromString pathStr
                  modifyStore p $ fieldValue col .~ v
         return txt
      -- createComboRenderer :: (MonadIO m) =>
      --    (a -> Text) -> [a] -> Gtk.TreeViewColumn -> m Gtk.CellRenderer
      createComboRenderer textFunc vs tvCol1 = do
         options <- MV.seqStoreNew vs
         MV.customStoreSetColumn   -- Text representation is column zero.
            options
            (MV.ColumnId (fmap (fromMaybe "") . Gtk.fromGValue) MV.CAString 0)
            textFunc
         combo <- Gtk.new Gtk.CellRendererCombo [
               #editable := True,
               #model := options,
               #textColumn := 0,
               #hasEntry := False
            ]
         Gtk.cellLayoutPackEnd tvCol1 combo True
         MV.cellLayoutSetDataFunction tvCol1 combo store $ \item -> do
            colour <- case fieldColour col of
               Just f -> setColour $ fromMaybe (Colour white) $ item ^? fieldValue col . to f
               Nothing -> setColour (Colour white)
            Gtk.set combo $ colour ++ [#text := item ^. fieldValue col . to textFunc]
         void $ Gtk.onCellRendererEditingStarted combo $ \editable _ ->
            Gtk.castTo Gtk.ComboBox editable >>= \case
               Nothing -> return ()
               Just e -> do
                  case fieldIcon col of
                     Nothing -> return ()
                     Just iconF -> do
                        icon <- Gtk.new Gtk.CellRendererPixbuf []
                        Gtk.cellLayoutPackStart e icon False
                        MV.cellLayoutSetAttributes e icon options $
                              \item -> [#iconName := iconF item]
                  case fieldColour col of
                     Nothing -> return ()
                     Just colourF ->
                        Gtk.cellLayoutGetCells e >>= mapM_ (
                              Gtk.castTo Gtk.CellRendererText >=> \case
                                 Nothing -> return ()
                                 Just txt ->
                                    MV.cellLayoutSetDataFunction e txt options $ \item -> do
                                       c <- setColour $ colourF item
                                       Gtk.set txt c )
         void $ Gtk.onCellRendererComboChanged combo $ \pathStr iter -> do
            v <- MV.customStoreGetRow options iter
            p <- Gtk.treePathNewFromString pathStr
            modifyStore p $ fieldValue col .~ v
         Gtk.toCellRenderer combo
      -- If the @TableColumn@ has a "fieldColour" function then this returns the corresponding
      -- attribute setter.
      -- setColour :: (MonadIO m) => Colour -> m [Gtk.AttrOp o t]
      setColour c = do
            bg <- colourToRGBA c
            fg <- colourToRGBA $ contrastText c
            return [#cellBackgroundRgba := bg, #foregroundRgba := fg]

-- | Nothing specialised to a type.
noAdjustment :: Maybe Gtk.Adjustment
noAdjustment = Nothing
