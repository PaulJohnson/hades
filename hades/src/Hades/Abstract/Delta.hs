{-# LANGUAGE AllowAmbiguousTypes #-}
  -- Required for functions in Viewable that don't mention type "m".
{-# LANGUAGE UndecidableInstances #-}
  -- Required for the rather arcane MonadError instance for Delta.

{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

{- |

The Delta monad transformer wraps an inner monad in a stateful stream processor.

Because it is a stream processor the top-most
monadic action can never return (hence the use of "Void" in "applyInputT"). Instead the monadic
action can yield an output using either "yieldViews" or "yieldCheckpoint". These actions cause
the "applyInputT" function to return the yielded value together with a continuation function that
takes the next input action. Values are yielded in the inner monad, so applications can combine
diagram editing actions with actions in an application-specific inner monad.

The output of the stream processor is a stream of "ActionOutput" values. Each contains a set of
updates for the picture on the screen, represented as "ViewSet". It can also optionally include
an undo checkpoint.

The stream processor is held in "DeltaState". Part of this state is the accumulated "ViewSet"
which will be placed in the output stream on the next yield. When a diagram element needs to be
redrawn then "tellViews" is called to add it to this ViewSet.

This means that the "Delta" monad can act in a stateful way without using an explicit state
machine. Instead an action will call "yieldViews" to get an input,
update the diagram accordingly, and then use another "yieldViews" to get the next input.
-}


module Hades.Abstract.Delta (
  -- ** Hades State
  DeltaState (deltaViews, deltaNames),
  deltaDiagram,
  deltaZoom,
  deltaSelection,
  deltaContext,
  mkDiagramState,
  nextDiagramId,
  -- ** Hades Inputs
  Command (..),
  Modifiers (..),
  noMod,
  shiftMod,
  ctrlMod,
  altMod,
  Action (..),
  mkScriptAction,
  nullAction,
  -- ** Hades Output
  ActionOutput (..),
  -- ** Diagram Editing Action
  Delta (..),
  DeltaNext,
  applyInputT,
  -- ** Generating Viewable Output
  Viewable (..),
  diagramBounds,
  liftBase,
  diagramAt,
  View (..),
  ViewSet (..),
  updateViewSet,
  viewSetBoundBox,
  tellViews,
  tellTransients,
  tellItem,
  tellAll,
  tellCheckpoint,
  yieldViews,
  yieldProperties,
  -- ** Selection
  getSelection,
  changeSelection,
  toggleSelectItem,
  selectedItems,
  itemsDescription
) where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Class
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text, pack)
import Data.UUID.Generate
import Data.Void
import Hades.Abstract.AutoMonad
import Hades.Abstract.Diagram
import Hades.Abstract.Geometry
import Reactive.Banana.Menu


-- | The state for the "Delta" monad.
--
-- Care must be taken when updating individual fields. The Selection must only contain elements
-- in the Diagram. If the Diagram, Zoom or Selection is changed then the Views must also be
-- updated, as this will not be done by the Lenses.
data DeltaState v = DeltaState {
    _deltaDiagram :: Diagram v,
    _deltaZoom :: Scale,
      -- ^ Zoom factor of the diagram. For the most part diagram elements should
      -- ignore this as painting the diagram on the screen is done by the outer
      -- application. However some visual elements (notably editing handles) need
      -- a constant screen size regardless of zoom, so they need to scale inversely
      -- with the zoom factor.
    _deltaSelection :: Set DiagramId,
      -- ^ Diagram items that are currently selected.
    _deltaContext :: ViewContext v,
      -- ^ Application data that travels with the edited diagram.
    deltaViews :: ViewSet v,
      -- ^ Views that have been changed since the last yield.
      -- The views are accumulated using "tellViews" and automatically cleared after
      -- being yielded.
    deltaTransients :: ViewSet v,
      -- ^ Simlar to deltaViews, but holds views used during editing commands, such as handles
      -- and rubber bands. Transients are always shown in front of everything else and do not
      -- need to be in the diagram order list.
    deltaNames :: [DiagramId],
      -- ^ Infinite list of fresh DiagramIds.
    deltaCheckpoint :: Maybe Text
      -- ^ If the diagram has been changed then this will be a description of the latest change.
  }


-- | Lens for diagram within "DeltaState".
deltaDiagram :: Lens' (DeltaState v) (Diagram v)
deltaDiagram = lens _deltaDiagram $ \s d -> s {_deltaDiagram = d}

-- | Lens for the zoom factor.
deltaZoom :: Lens' (DeltaState v) Scale
deltaZoom = lens _deltaZoom $ \s z -> s {_deltaZoom = z}

-- | Lens for selection.
deltaSelection :: Lens' (DeltaState v) (Set DiagramId)
deltaSelection = lens _deltaSelection $ \s t -> s {_deltaSelection = t}

-- | User-editable description of how the diagram elements should be shown to the user.
-- When it changes the "deltaViewDef" should be updated to match.
deltaContext :: Lens' (DeltaState v) (ViewContext v)
deltaContext = lens _deltaContext $ \s v -> s {_deltaContext = v}


-- | Create an initial DeltaState from a Diagram. This is in the IO monad because it contains
-- a random generator for new DiagramIds.
mkDiagramState :: (Viewable v) =>
  ViewContext v -> Scale -> Diagram v -> IO (DeltaState v)
mkDiagramState ctx z d = do
  names <- uuidStreamIO
  return DeltaState {
      _deltaDiagram = d,
      _deltaZoom = z,
      _deltaSelection = S.empty,
      _deltaContext = ctx,
      deltaViews = mempty,
      deltaTransients = mempty,
      deltaNames = names,
      deltaCheckpoint = Nothing
    }


-- | Commands which the application can issue to Hades. A "MoveDrag" must be
-- followed by another "MoveDrag" or an "EndDrag", and an "EndDrag" may only follow a
-- "MoveDrag". Apart from that commands can occur in any order.
--
-- "Drag" and "EndDrag" event positions are the start of the drag. The pair of doubles gives the
-- offset to the current position during and after the drag.
data Command v =
  Select  -- ^ Typically a left click.
  | Activate  -- ^ Typically a double click.
  | Drag (Double, Double) -- ^ Cumulative delta x and y from start position.
  | EndDrag (Double, Double)
  | Key Char
  | Script (Delta v (Action v))
      -- ^ Run a Delta action to modify the diagram in an arbitrary way.

instance Show (Command v)  where
  show Select = "Select"
  show Activate = "Activate"
  show (Drag d) = "Drag " ++ show d
  show (EndDrag d) = "EndDrag " ++ show d
  show (Key c) = "Key " ++ show c
  show (Script _) = "Script"


-- | Command modifiers. Monoid instance is logical OR for each field.
data Modifiers = Modifiers {
    modShift, modCtrl, modAlt :: Bool
  } deriving (Eq, Ord)


instance Semigroup Modifiers where
  Modifiers s1 c1 a1 <> Modifiers s2 c2 a2 = Modifiers (s1 || s2) (c1 || c2) (a1 || a2)

instance Monoid Modifiers where
  mempty = noMod

instance Show Modifiers where
  show (Modifiers s c a) = concat [ "{ ",
    if s then "Shift " else "",
    if c then "Ctrl " else "",
    if a then "Alt " else "",
    "}" ]

noMod :: Modifiers
noMod = Modifiers False False False

shiftMod :: Modifiers
shiftMod = Modifiers True False False

ctrlMod :: Modifiers
ctrlMod = Modifiers False True False

altMod :: Modifiers
altMod = Modifiers False False True



-- | An action is a command, the point where the mouse was when it was issued, and any modifier
-- keys pressed at the time.
--
-- Note: this was originally called \"Event\". However having GTK events, Hades events and
-- Reactive Banana events meant there was too much scope for muddle, so Hades events are now
-- known as actions.
data Action v = Action {
    actionCommand :: Command v,
    actionLocation :: Point,
      -- ^ In the case of drags, this is the point the drag started.
    actionModifiers :: Modifiers,
    actionTarget :: Maybe (DiagramId, View v)
      -- ^ The View of the item at the @actionLocation@, if any.
  }

instance Show (Action v) where
  show ev = concat ["Action ", show $ actionCommand ev, " ",
    show $ actionLocation ev, " ", show $ actionModifiers ev, " ",
    maybe " " (show . fst) (actionTarget ev)]


-- | Convert a Delta into an Action. The action will occur at (0,0) and target Nothing.
mkScriptAction :: Delta v (Action v) -> Action v
mkScriptAction d = Action {
    actionCommand = Script d,
    actionLocation = Point 0 0,
    actionModifiers = Modifiers False False False,
    actionTarget = Nothing
  }


-- | The response to an action contains data for the application GUI.
data ActionOutput v = ActionOutput {
    outViews :: ViewSet v,   -- ^ Visual updates.
    outTransients :: ViewSet v,  -- ^ Transient visual updates.
    outCheckpoint :: Maybe Text,
      -- ^ If the diagram has been modified then this is the new version. The string describes
      -- the change.
    outProperties :: [v]
  }


-- | A no-operation action.
nullAction :: (Viewable v) => Action v
nullAction = mkScriptAction yieldViews


-- | The Delta monad describes edits to a diagram.  @v@ is the type
-- of diagram element.
--
-- The monad is an instance of "MonadState", so "put", "get" and "modify" are available.
newtype Delta v a =
  Delta {stepDelta :: StateStreamT (Action v) (ActionOutput v) Void (DeltaState v) (Base v) a}

instance (Viewable v) => Functor (Delta v) where
  fmap f (Delta s) = Delta $ fmap f s

instance (Viewable v) => Applicative (Delta v) where
  pure v = Delta $ pure v
  Delta f <*> Delta v = Delta $ f <*> v

instance (Viewable v) => Monad (Delta v) where
  m >>= f = Delta $ do
    v <- stepDelta m
    stepDelta $ f v

instance (Viewable v) => MonadState (DeltaState v) (Delta v) where
  get = Delta get
  put = Delta . put

instance (Viewable v) => AutoClass (Action v) (ActionOutput v) (Delta v) where
  yield = Delta . yield


-- | The continuation returned from "applyInputT".
type DeltaNext v = StreamNextT (Action v) (ActionOutput v) Void (DeltaState v) (Base v)


-- | Return a fresh name.
nextDiagramId :: (Viewable v) => Delta v DiagramId
nextDiagramId = do
  s <- get
  let (uuid : ns) = deltaNames s -- Safe because deltaNames is infinite.
  put s {deltaNames = ns}
  return uuid


-- | Things of type @v@ have a representation drawn in monad @Paint v@. This also includes
-- common behaviour for all Viewable objects.
--
-- The geometric properties default to those of the item shape. However they can be overridden. The
-- only exception is for the handles. The shape provides a list of handle locations and functions
-- for updating the shape when the handle is moved. However it is for the Viewable to interpret
-- this as a set of Views.
class (Monad (Base v), HasId v, Monad (Paint v)) => Viewable v where
  -- | Base monad for "Delta" actions.
  type Base v :: * -> *
  -- | Monad for drawing objects of type @v@
  type Paint v :: * -> *
  -- | Application data that runs with the diagram.
  data ViewContext v :: *
  -- | Transient handles for the shape, allowing it to be edited.
  itemHandles :: v -> Delta v [View v]
  -- | True if the point is on the Viewable. Usually the same as the "viewTouched"
  -- function for the @itemView@ when not selected. Takes "Scale" because lines need some
  -- scale-invariant slop around them.
  itemTouched :: v -> Scale -> Point -> Bool
  -- | Any child connection points within this item. The second item in the pair is a predicate
  -- for its area.
  itemChildren :: v -> Delta v [(DiagramId, Point -> Bool)]
  -- | True if the Viewable is contained in the BoundBox.
  itemInBox :: v -> BoundBox -> Bool
  -- | Bound box for this item. Should match the one in the "itemView".
  itemBounds :: v -> BoundBox
  -- | Attempt to set the width and height of @itemBounds@. May not succeed.
  -- In particular lines are expected to ignore this.
  itemResize :: (Double, Double) -> v -> Delta v v
  -- | Move the Viewable by (x,y) relative, updating the diagram, but not changing any connections.
  itemMove :: (Double, Double) -> v -> Delta v v
  -- | Move the Viewable by (x,y) relative, updating the diagram, making or breaking connections,
  -- snapping to other items as appropriate, and returning the new value.
  itemDrag :: (Double, Double) -> v -> Delta v v
  -- | A user-level description of the Viewable, such as \"rect023\" or \"Fuel pump\".
  itemDescription :: v -> Delta v Text
  -- | Current View (taking into account zoom and selection) of the Viewable.
  itemView :: v -> Delta v (View v)
  -- | Type name for the item, used to distinguish different types of diagram. The
  -- implementation MUST NOT inspect its argument.
  itemType :: v -> Text


-- | The bound box of a diagram.
diagramBounds :: (Viewable v) => Diagram v -> BoundBox
diagramBounds = mconcat . map itemBounds . diagramToList


-- | Lift a computation in the Base monad up into the Delta monad.
liftBase :: (Viewable v) => Base v a -> Delta v a
liftBase = Delta . lift


-- | The child item, if any, that the point is over.
itemChild :: (Viewable v) => v -> Point -> Delta v (Maybe DiagramId)
itemChild item pt = do
  candidates <- itemChildren item
  return $ fst <$> find (\(_, p) -> p pt) candidates


-- | The top-most element (if any) at the point in the diagram that complies with the predicate
-- and the "DiagramId" of the child connection at that point.
diagramAt :: (Viewable v) =>
  Scale
  -> Point
  -> (v -> Maybe DiagramId -> Delta v Bool)
    -- ^ The predicate arguments are an item and optionally a child of that item.
  -> Diagram v
  -> Delta v (Maybe (v, Maybe DiagramId))
diagramAt s pt predicate diagram = do
  candidates <- forM (diagramToList diagram) $ \v ->
    if itemTouched v s pt
      then do
        child <- itemChild v pt
        r <- predicate v child
        return $ if r then Just (v, child) else Nothing
      else return Nothing
  return $ listToMaybe $ catMaybes candidates


-- | A View of an object knows what points it occupies and how to respond to actions.
--
-- The Monoid instance puts the first View in front of the second.
data View v = View {
    viewDraw :: Paint v (),
    viewTouched :: Point -> Bool,
      -- ^ True if the point touches the object.
    viewBox :: BoundBox,
      -- ^ The object is entirely contained within this box.
    viewMenu :: Point -> Menu (Action v),
      -- ^ The action for right-clicks at this point. The @Point@ is available for objects that
      -- have different menus in different places; if the menu is always the same then ignore it.
    viewAction :: Action v -> Delta v (Action v)
      -- ^ What the View will do with a user "Action". This will include yielding the updates
      -- and returning the next action.
  }

instance (Viewable v) => Semigroup (View v) where
  v1 <> v2 = View {
      viewDraw = viewDraw v2 >> viewDraw v1,
      viewTouched = \p -> viewTouched v1 p || viewTouched v2 p,
      viewBox = viewBox v1 <> viewBox v2,
      viewMenu = \p -> if viewTouched v1 p then viewMenu v1 p else viewMenu v2 p,
      viewAction = \action ->
        if viewTouched v1 $ actionLocation action
          then viewAction v1 action
          else viewAction v2 action
    }

instance (Viewable v) => Monoid (View v) where
  mempty = View {
      viewDraw = return (),
      viewTouched = const False,
      viewBox = mempty,
      viewMenu = const $ Menu [],
      viewAction = const yieldViews
    }


-- | A set of views related to specific drawing items.
-- The Monoid instance aggregates the views by key.
newtype ViewSet v = ViewSet (Map DiagramId (View v))

instance (Viewable v) => Semigroup (ViewSet v) where
  ViewSet m1 <> ViewSet m2 = ViewSet $ M.unionWith mappend m2 m1

instance (Viewable v) => Monoid (ViewSet v) where
  mempty = ViewSet M.empty


-- | Similar to "mappend", but replaces views with the same key rather than appending to them.
updateViewSet :: ViewSet v -> ViewSet v -> ViewSet v
updateViewSet (ViewSet m1) (ViewSet m2) = ViewSet $ M.union m2 m1


-- | The bounding box around an entire ViewSet
viewSetBoundBox :: ViewSet v -> BoundBox
viewSetBoundBox (ViewSet vs) = foldr (mappend . viewBox . snd) NoBox $ M.toList vs


-- | Record that the entire diagram needs to be redrawn.
tellAll :: (Viewable v) => Delta v ()
tellAll = do
  -- Views
  contents <- use (deltaDiagram . diagramContents)
  vs <- forM contents itemView
  tellViews $ ViewSet vs
  -- Selection handles
  sel <- mapMaybe (`M.lookup` contents) . S.toList <$> use deltaSelection
  ts <- forM sel $ \i -> do
    t <- mconcat <$> itemHandles i
    return (identifier i, t)
  tellTransients $ ViewSet $ M.fromList ts


-- | Record updates to the diagram view.
tellViews :: (Viewable v) => ViewSet v -> Delta v ()
tellViews vs = modify $ \s -> s {deltaViews = deltaViews s `updateViewSet` vs}


-- | Record update to a transient view.
tellTransients :: (Viewable v) => ViewSet v -> Delta v ()
tellTransients vs = modify $ \s -> s {deltaTransients = deltaTransients s `updateViewSet` vs}


-- | Record that a single element in the diagram has changed.
tellItem :: (Viewable v) => v -> Delta v ()
tellItem item = do
  v <- itemView item
  tellViews $ ViewSet $ M.singleton (identifier item) v
  sel <- use deltaSelection
  if S.member (identifier item) sel
    then do
      ts <- itemHandles item
      tellTransients $ ViewSet $ M.singleton (identifier item) $ mconcat ts
    else
      tellTransients $ ViewSet $ M.singleton (identifier item) mempty


-- | Record an undo point with a description of the last change.
tellCheckpoint :: (Viewable v) => Text -> Delta v ()
tellCheckpoint str =
  modify $ \s -> s {deltaCheckpoint = Just str}


-- | Private function to yield changes to the diagram view. The parameter is 0 or more items to
-- be opened with property dialogs.
yieldViews1 :: (Viewable v) => [v] -> Delta v (Action v)
yieldViews1 items = do
  s <- get
  put s {
      deltaViews = mempty,
      deltaTransients = mempty,
      deltaCheckpoint = Nothing
    }
  yield ActionOutput {
      outViews = deltaViews s,
      outTransients = deltaTransients s,
      outCheckpoint = deltaCheckpoint s,
      outProperties = items
    }

-- Yield changes to the diagram view and get the next input action.
yieldViews :: (Viewable v) => Delta v (Action v)
yieldViews = yieldViews1 []


-- | Request a properties dialog for the argument. Also yields the changes and gets the next
-- input action. The next action is not necessarily the response to the dialog.
yieldProperties :: (Viewable v) => v -> Delta v (Action v)
yieldProperties item = yieldViews1 [item]


-- ---------------------------
-- Selection
-- ---------------------------

getSelection :: (Viewable v) => Delta v (Set DiagramId)
getSelection = use deltaSelection


-- | Replace the current selection with a new one. Tells the changed views.
changeSelection :: (Viewable v) => Set DiagramId -> Delta v ()
changeSelection newSet = do
    oldSet <- use deltaSelection
    deltaSelection .= newSet
    contents <- use $ deltaDiagram . diagramContents
    let
      unchanged = S.intersection oldSet newSet
      newlySelected = S.toList $ S.difference newSet unchanged
      deselected = S.toList $ S.difference oldSet unchanged
    vs <- catMaybes <$> forM newlySelected (\uuid ->
      case M.lookup uuid contents of
        Just i -> Just . (uuid, ) . mconcat <$> itemHandles i
        Nothing -> return Nothing)
    let ds = map (, mempty) deselected
    tellTransients $ ViewSet $ M.fromList (vs ++ ds)


-- | Toggle the selection of the identified item and tell the changed view.
toggleSelectItem :: (Viewable v) => DiagramId -> Delta v ()
toggleSelectItem uuid = do
  contents <- use $ deltaDiagram . diagramContents
  case M.lookup uuid contents of
    Nothing -> return ()
    Just item -> do
      oldSet <- use deltaSelection
      deltaSelection .=
        if S.member uuid oldSet
          then S.delete uuid oldSet
          else S.insert uuid oldSet
      tellItem item


-- | Items currently selected.
selectedItems :: (Viewable v) => Delta v [(DiagramId, v)]
selectedItems = do
  sel <- S.toList <$> use deltaSelection
  contents <- use $ deltaDiagram . diagramContents
  return $ mapMaybe (\u -> (u,) <$> M.lookup u contents) sel


-- | For a single item, give its description. For multiple items just say \"n items\"
itemsDescription :: (Viewable v) => [v] -> Delta v Text
itemsDescription [] = return "nothing"  -- Should never happen.
itemsDescription [x] = itemDescription x
itemsDescription xs = return $ pack (show $ length xs) <> " items"
