{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

-- |
--
-- Manage a binary tree of Gtk Paned widgets with Notebooks at the leaves.

module Model.GI.Notebooks (
  DiagramExport,
  ActiveEntity (..),
  BookManager (..),
  newBookManager,
  allBooks,
  sameWidget
) where

import Control.Monad
import Control.Monad.Except
import Data.IORef
import Data.List
import Data.Text (Text)
import GI.Gtk (AttrOp ((:=)))
import qualified GI.Gtk as Gtk
import qualified GI.Cairo.Render as Cairo
import Hades.Abstract
import Hades.GI.BasicShapes
import Model.Abstract.DiagramType
import Reactive.Banana.Combinators hiding ((<>))
import Reactive.Banana.Frameworks
import Reactive.Banana.GI.Common


-- | Diagrams are exported by rendering them on a suitable Cairo "Surface". This carries
-- the render operation and the required surface size.
type DiagramExport = (Cairo.Render (), BoundBox)


-- | Data for each entity currently being edited.
data ActiveEntity p v = ActiveEntity {
      activeEntityWidget :: Gtk.Widget,
        -- ^ The top level widget for the whole diagram, to be inserted into the application.
      activeEntityCanvas :: Gtk.Widget,
        -- ^ The widget where the diagram is actually drawn. Make this the keyboard focus when
        -- you want this to become the primary diagram. May or may not be the same as the
        -- @activeEntityWidget@.
      activeEntityRefresh :: IO (),
        -- ^ Action to trigger a refresh of the view of the entity. An action is used instead
        -- of just piping an event in order to ensure that only live views are refreshed.
      activeEntityUpdates :: Event [ModelScript p v v (Maybe Text)],
        -- ^ Updates to the model.
      activeEntityZoom :: Behavior Scale,
        -- ^ The zoom level of this diagram.
      activeEntityExport :: Behavior (Maybe DiagramExport),
        -- ^ The current state of the diagram prepared for export.
      activeWrapper :: EntityWrapper HadesRender v
        -- ^ The model ID of the diagram.
    }



{- | GTK Notebooks can have pages added, deleted and reordered by the user. The application needs
to keep track of which diagrams are open, and when a diagram is opened for a second time it
should move to that diagram. Hence the need for a mapping from Diagram UUID to GTK Widget.

IORefs are used rather than Behaviors because they are simpler to manage and there is no need for
actions to be taken when they change.
-}
data BookManager p v = BookManager {
    bookManagerDisplay :: IORef (Maybe Gtk.Widget),
      -- ^ Root of a binary tree of Paned widgets with Notebooks at the leaves.
    bookManagerContents :: IORef [ActiveEntity p v],
      -- ^ A table of the diagrams in the notebook, indexed by widget and UUID. For each diagram
      -- we have the associated display widget and an event for edits to the diagram that might
      -- also lead to updates to the model. Update this via "bookManagerModelHandler" only.
    bookManagerActiveEvent :: Event (ActiveEntity p v),
      -- ^ When the active tab is changed the associated event stream will
      -- be emitted as an event. Use "switchE" to tune into the stream of update events from the
      -- currently selected diagram.
    bookManagerActiveHandler :: Handler (ActiveEntity p v),
      -- ^ Callback for when a diagram becomes active.
    bookManagerModelUpdate :: Event [ActiveEntity p v],
      -- ^ The contents have changed.
    bookManagerModelHandler :: Handler [ActiveEntity p v],
      -- ^ Callback for when the contents change. Also updates the "bookManagerContents".
    bookManagerZoom :: Event Scale,
      -- ^ The current scale for diagrams to be drawn, and any changes to that scale.
    bookManagerRefresh :: Event ()
      -- ^ Events on this cause the contents of the notebooks to refresh.
  }


-- | Common name for all Notebooks in the BookManager to enable them to exchange tabs.
hadesGroup :: Text
hadesGroup = "hades-diagram-tabs"


-- | Extract the list of all GTK notebooks currently underneath the "BookManager".
allBooks :: (MonadIO m) => BookManager p v -> m [Gtk.Notebook]
allBooks manager = do
    w <- liftIO $ readIORef $ bookManagerDisplay manager
    traverseWidget w
  where
    traverseWidget Nothing = return []
    traverseWidget (Just w) =
      liftIO (Gtk.castTo Gtk.Paned w) >>= \case
        Just paned -> do
          results1 <- Gtk.panedGetChild1 paned >>= traverseWidget
          results2 <- Gtk.panedGetChild2 paned >>= traverseWidget
          return $ results1 ++ results2
        Nothing -> liftIO (Gtk.castTo Gtk.Notebook w) >>= \case
          Just nb -> return [nb]
          Nothing -> return []


-- | Split the nominated notebook in two. If the original notebook was the current display widget
-- then a new widget is passed to the Handler.
splitBook :: Handler Gtk.Widget -> Gtk.Notebook -> BookManager p v -> IO ()
splitBook handler book manager = do
    display <- readIORef $ bookManagerDisplay manager
    w1 <- Gtk.toWidget book
    Gtk.getWidgetParent book >>= \case
      Nothing -> return ()  -- Should never happen
      Just parentContainer ->
        if maybe False (sameWidget w1) display
          then do
            Gtk.containerRemove parentContainer book
            newWidget <- replace Gtk.OrientationHorizontal
            Gtk.containerAdd parentContainer newWidget  -- Assume its a simple container.
            newWidget1 <- Gtk.toWidget newWidget
            liftIO $ handler newWidget1
          else liftIO (Gtk.castTo Gtk.Paned parentContainer) >>= \case
            Nothing -> return ()  -- Should never happen.
            Just parent -> do  -- parent == parentContainer, but with different types.
              orient <- Gtk.orientableGetOrientation parent
              let
                orient2 = if orient == Gtk.OrientationHorizontal
                  then Gtk.OrientationVertical
                  else Gtk.OrientationHorizontal
              child1 <- Gtk.panedGetChild1 parent
              child2 <- Gtk.panedGetChild2 parent
              if | maybe False (sameWidget w1) child1 -> do
                  Gtk.containerRemove parent w1
                  newWidget <- replace orient2
                  Gtk.panedPack1 parent newWidget True True
                | maybe False (sameWidget w1) child2 -> do
                  Gtk.containerRemove parent w1
                  newWidget <- replace orient2
                  Gtk.panedPack2 parent newWidget True True
                | otherwise -> return ()
  where
    replace orient = do
      splitWidget <- Gtk.panedNew orient
      blank <- newHadesBook handler manager
      Gtk.panedPack1 splitWidget book True True
      Gtk.panedPack2 splitWidget blank True True
      Gtk.widgetShow splitWidget
      Gtk.widgetShow blank
      return splitWidget


-- | Merge a notebook at a leaf of Paned widgets with its sibling. If the argument is already
-- the root display widget then do nothing. If the argument is a direct child of the current
-- display widget then call the handler with the merged widget.
mergeBook :: Handler Gtk.Widget -> Gtk.Notebook -> BookManager p v -> IO ()
mergeBook handler book manager = do
    display <- liftIO $ readIORef $ bookManagerDisplay manager
    w1 <- Gtk.toWidget book
    if maybe False (sameWidget w1) display
      then
        return ()  -- Nothing to merge with.
      else
        Gtk.getWidgetParent book >>= \case
          Nothing -> return ()   -- Should never happen.
          Just p1 -> do
            parent <- Gtk.toWidget p1
            newWidget <- flatten parent
            -- Replace the parent with newWidget.
            Gtk.getWidgetParent parent >>= \case
              Nothing -> return ()  -- Should never happen.
              Just gp1 -> do
                Gtk.containerRemove gp1 parent
                Gtk.containerAdd gp1 newWidget
                when (maybe False (sameWidget parent) display) $
                  Gtk.toWidget newWidget >>= handler
  where
    -- Transfer all w2 tabs into w1 and return w1.
    merge :: Maybe Gtk.Widget -> Maybe Gtk.Widget -> IO Gtk.Notebook
    merge (Just w1) (Just w2) = do
      book1 <- flatten w1
      book2 <- flatten w2
      n2 <- Gtk.notebookGetNPages book2
      forM_ [1..n2] $ \_ ->
        Gtk.notebookGetNthPage book2 0 >>= \case
          Nothing -> return ()
          Just page -> do
            label <- Gtk.notebookGetTabLabel book2 page
            menu <- Gtk.notebookGetMenuLabel book2 page
            Gtk.containerRemove book2 page  -- Should also remove label and menu items.
            void $ Gtk.notebookAppendPageMenu book1 page label menu
            Gtk.notebookSetTabDetachable book1 page True
      return book1
    merge (Just w) Nothing = flatten w
    merge Nothing (Just w) = flatten w
    merge Nothing Nothing = newHadesBook handler manager
    -- If w is a Paned then merge its contents. If it is a notebook then just return it.
    -- If neither then create a new notebook and return that.
    flatten :: (Gtk.IsWidget w) => w -> IO Gtk.Notebook
    flatten w =
      liftIO (Gtk.castTo Gtk.Paned w) >>= \case
        Just paned -> do
          child1 <- Gtk.panedGetChild1 paned
          child2 <- Gtk.panedGetChild2 paned
          maybe (return ()) (Gtk.containerRemove paned) child1
          maybe (return ()) (Gtk.containerRemove paned) child2
          merge child1 child2
        Nothing -> liftIO (Gtk.castTo Gtk.Notebook w) >>= \case
          Just nb -> return nb
          Nothing -> newHadesBook handler manager  -- Should never happen.


-- | Hades Notebooks exist as the leaves of a tree of Paned widgets. Each has action buttons for
-- splitting and joining.
newHadesBook :: Handler Gtk.Widget -> BookManager p v -> IO Gtk.Notebook
newHadesBook handler manager = do
    book <- Gtk.notebookNew
    Gtk.set book [
        Gtk.notebookScrollable := True,
        Gtk.notebookEnablePopup := True
      ]
    Gtk.notebookSetGroupName book $ Just hadesGroup
    buttons <- Gtk.boxNew Gtk.OrientationHorizontal 1
    splitButton <- Gtk.buttonNewWithLabel "Split"
    void $ Gtk.onButtonPressed splitButton $ splitBook handler book manager
    Gtk.boxPackStart buttons splitButton True False 0
    mergeButton <- Gtk.buttonNewWithLabel "Merge"
    void $ Gtk.onButtonPressed mergeButton $ mergeBook handler book manager
    Gtk.boxPackStart buttons mergeButton True False 0
    void $ Gtk.onNotebookSwitchPage book focusOnPage
    Gtk.widgetShowAll buttons
    Gtk.notebookSetActionWidget book buttons Gtk.PackTypeEnd
    Gtk.widgetShowAll book
    -- Work around issue in notebooks where nothing is shown until there is a tab.
    dummy <- Gtk.labelNew $ Just "You should never see this text."
    Gtk.widgetShow dummy
    void $ Gtk.notebookAppendPage book dummy (Nothing :: Maybe Gtk.Widget)
    Gtk.containerRemove book dummy
    return book
  where
    focusOnPage :: Gtk.Widget -> a -> IO ()
    focusOnPage widget _ = do
      contents <- readIORef $ bookManagerContents manager
      case find (sameWidget widget . activeEntityWidget) contents of
        Just selected ->
          Gtk.widgetGrabFocus $ activeEntityCanvas selected
        Nothing ->     -- Should never happen.
          return ()   -- A diagram is shown but not in the contents.


-- | Create a new BookManager. Returns the initial widget along with the BookManager, which
-- includes a "Behavior" for future changes to the widget as it is split and merged.
newBookManager ::
  Event Scale    -- ^ Changes to the diagram scale.
  -> Event ()    -- ^ Refresh button clicked.
  -> MomentIO (Gtk.Widget, BookManager p v)
newBookManager scaleE refreshE = do
    contents <- {-# SCC "newBookManager-contents" #-} liftIO $ newIORef []
    widgetRef <- {-# SCC "newBookManager-widget" #-} liftIO $ newIORef Nothing
    (activeE, activeH) <- {-# SCC "newBookManager-active" #-} newEvent
    (updateE, updateH) <- {-# SCC "newBookManager-update" #-} newEvent
    let
      updateH1 v = do
        writeIORef contents v
        updateH v
      replaceWidgetH = writeIORef widgetRef . Just
      manager = BookManager widgetRef contents activeE activeH updateE updateH1 scaleE refreshE
    book <- liftIO $ newHadesBook replaceWidgetH manager
    widget <- Gtk.toWidget book
    liftIO $ replaceWidgetH widget
    return (widget, manager)
