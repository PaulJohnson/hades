{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

{- |

Commands are represented as sequences of actions which will typically be created by the
user with mouse and keyboard. Each type of command is represented here as a function.

In most cases the first user action that initiates the command also carries information required
to implement the command, so this action is passed as a parameter to the command function.
Likewise the end of a command sequence is generally the first action in a new command, so these
functions return this action for processing by the caller.
-}

module Hades.Abstract.Commands (
   -- * Action Processing
   processAction,
   KeyCommands,
   processSingleAction,
   applyToSelection,
   processDrag,
   processDrag1,
   -- * Higher Level Commands
   moveItems,
   findAffected,
   ignoreDrag,
   handleDelegatedAction,
   rubberBand,
   areaSelect,
   -- * Alignment Operations
   alignGeneric,
   alignTop,
   alignBottom,
   alignLeft,
   alignRight,
   alignVertical,
   alignHorizontal,
   -- * Sizing operations
   sizeGeneric,
   sizeWidest,
   sizeNarrowest,
   sizeTallest,
   sizeShortest,
   -- * Distribute operations
   distributeGeneric,
   distributeH,
   distributeV,
   spaceH,
   spaceV
) where

import Control.Lens
import Control.Monad
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text, pack)
import Data.UUID
import Data.Void
import Hades.Abstract.Connections
import Hades.Abstract.Delta
import Hades.Abstract.Diagram
import Hades.Abstract.Geometry
import Hades.Abstract.Primitives


-- | Top level function for processing actions. Deals with selection and moving, and passes
-- anything else to the target item. Rather than returning it waits for the next action
-- and calls itself recursively.
processAction :: (Connectable v) =>
   (Point -> Point -> Paint v ())   -- ^ Draw an area select box. Usually uses thin dashed lines.
   -> KeyCommands v                  -- ^ How to handle keys pressed.
   -> Action v                       -- ^ The next action to process.
   -> Delta v Void
processAction areaBox keyCommands act1 =
   processSingleAction areaBox keyCommands act1 >>= processAction areaBox keyCommands


-- | Table of actions for keyboard commands. The @Set DiagramId@ argument is the selection to which
-- the command is to be applied.
--
-- The "SemiGroup" instance for "Map" means that these
-- can be conveniently combined.
type KeyCommands v = Map (Modifiers, Char) (Delta v ())

{- Design Note

GTK can handle keys that do not directly translate into unicode characters (e.g. "Insert") via
its own encoding system. If you need this then Hades.GI.MouseMachine will have to be modified to
emit a suitable encoding instead of Char. The native GTK one *could* be used, except that at this
level we are trying not to have any direct GTK dependencies.
-}


-- | Process a single action. Yields the changes and returns the next action.
processSingleAction :: (Connectable v) =>
   (Point -> Point -> Paint v ())   -- ^ Draw an area select box. Usually uses thin dashed lines.
   -> KeyCommands v                  -- ^ How to handle keys pressed.
   -> Action v                       -- ^ The next action to process.
   -> Delta v (Action v)
processSingleAction areaBox keyCommands ev =
   case actionCommand ev of
      Select -> do
         case actionTarget ev of
            Just (uuid, _) ->
               if modShift $ actionModifiers ev
                  then toggleSelectItem uuid
                  else changeSelection $ S.singleton uuid
            Nothing -> changeSelection S.empty
         yieldViews
      Drag _ -> do
         s <- use deltaSelection
         items <- selectedItems
         case actionTarget ev of
            Nothing ->
               areaSelect areaBox (actionLocation ev) (actionModifiers ev)  -- Cases 1 & 2.
            Just (uuid, v) ->
               if S.member uuid s
                  then case items of
                     []  -> viewAction v ev
                     [_] -> viewAction v ev  -- Case 4.
                     _   -> moveItems (map snd items) ev  -- Cases 6 & 7.
                  else
                     viewAction v ev  -- Cases 3 & 5.
      EndDrag _ ->
         yieldViews
      Script action ->
         action
      Key c -> do
         case M.lookup (actionModifiers ev, c) keyCommands of
            Nothing -> return ()
            Just h -> h
         yieldViews
      _ ->   -- Anything else is delegated to the element.
         case actionTarget ev of      -- Not a select or a move, so ...
            Just (_, v) -> viewAction v ev   -- delegate to the View of an object in the diagram,
            Nothing     -> yieldViews       -- or if there isn't one, then no-op.





-- | Apply the action to the selection. Ignore the target location. This is suitable for key
-- commands as they do not have a location.
applyToSelection :: (Connectable v) =>
   ([v] -> Delta v Text)
      -- ^ Message for the undo log, based on the items the action is applied to.
   -> (DiagramId -> Delta v ())
      -- ^ Action for each item.
   -> Delta v ()
applyToSelection strF action = do
   sel <- use deltaSelection
   unless (S.null sel) $ do
      contents <- use $ deltaDiagram . diagramContents
      msg <- strF $ mapMaybe (`M.lookup` contents) $ S.toList sel
      mapM_ action sel
      tellCheckpoint msg


{-
MoveDrag handling has a lot of sub-cases, so here they are in tabular format:

Case | Selection  |  Target   |  Modifiers  |  Process
-----+------------+-----------+-------------+------------------------------------------------
  1  |   -        | None      | None        | Box selection.
  2  |   -        | None      | Shift       | Box selection toggle.
  3  | Empty      | Item      |   -         | Pass actions to target.
  4  | Single     | Selected  |   -         | Pass actions to target.
  5  |   -        | Unselected|   -         | Pass actions to target.
  6  | Multiple   | Selected  | None        | Unconstrained drag of selected items.
  7  | Multiple   | Selected  | Shift       | Constrained drag of selected items

-}


-- | Generic function for handling a sequence of "Drag" actions. It returns the "EndDrag" action
-- that terminates this drag command.
processDrag1 :: (Connectable v) =>
   Delta v s
      -- ^ Any read-only state required by the drag functions. This is evaluated once and
      -- then passed to each call to the movement function.
   -> (s -> Action v -> (Double, Double) -> Delta v ())
      -- ^ The movement function. This is called for each "Drag" action that follows, and then
      -- again for the "EndDrag".
      -- The first argument is the read-only state, the second is the latest "Action" and the
      -- third is the cumulative offset from the location of the first action.
   -> Action v
      -- ^ The first action.
   -> Delta v (Action v)
processDrag1 moveState moveAction ev1 = do
   s <- moveState
   let
      process1 d ev2 =
         case actionCommand ev2 of
            Drag dPos -> do
               moveAction s ev2 dPos
               yieldViews >>= process1 dPos
            EndDrag dPos -> do
               moveAction s ev2 dPos
               return ev2
            Script act ->
               act >>= process1 d
            _ -> do
               moveAction s ev2 d
               return ev2
   process1 (0,0) ev1


-- | As for "processDrag1", but also creates an undo checkpoint at the end of the drag.
processDrag :: (Connectable v) =>
   Text
      -- ^ Checkpoint message for this drag.
   -> Delta v s
      -- ^ Any read-only state required by the drag functions. This is evaluated once and
      -- then passed to each call to the movement function.
   -> (s -> Action v -> (Double, Double) -> Delta v ())
      -- ^ The movement function. This is called for each "Drag" action that follows, and then
      -- again for the "EndDrag".
      -- The first argument is the read-only state, the second is the latest "Action" and the
      -- third is the cumulative offset from the location of the first action.
   -> Action v
      -- ^ The first action.
   -> Delta v (Action v)
processDrag msg moveState moveAction ev1 = do
   ev2 <- processDrag1 moveState moveAction ev1
   tellCheckpoint msg
   return ev2


{-
Design note: Line behaviour during selection moves:

   Unconnected ends move if the line is selected.
   Connected ends move (or not) with the item they are connected to.

However if a line is both connected and selected then it needs to be handled once, because
trying to move it and update it in two separate actions will cause two different versions to be
emitted to the renderer. Hence this function identifies elements that are "targets" of the move,
elements that are affected by the targets, and elements that are both.

Pegged boxes are handled similarly.
-}


-- | For a move on a set of diagram items, compute the items that are:
--
-- * Both targetted and possibly affected by the move. These should be moved and then adjusted.
--
-- * Not targetted, but affected by a targetted item. These should be adjusted after everything
-- else.
moveTargetSets :: (Connectable v) => Set DiagramId -> Delta v (Set DiagramId, Set DiagramId)
moveTargetSets targetSet = do
   targetAffectedSet <- findAffected targetSet
   let onlyAffectedSet = targetAffectedSet `S.difference` targetSet
   return (targetAffectedSet, onlyAffectedSet)


-- | Move several items as a group. Returns the "EndDrag" action that terminates this command.
moveItems :: (Connectable v) => [v] -> Action v -> Delta v (Action v)
moveItems targets ev = do
      let targetSet = S.fromList $ map identifier targets
      (_, onlyAffectedSet) <- moveTargetSets targetSet
      desc <- itemsDescription targets
      processDrag
         ("Move " <> desc)
         (return ())
         (\_ _ dPos -> do
            contents <- use $ deltaDiagram . diagramContents
            dragged <- mapM (constrainedDrag dPos) targets
            let onlyAffected = mapMaybe (`M.lookup` contents) $ S.toList onlyAffectedSet
            mapM_ itemAdjust $ dragged ++ onlyAffected)
         ev
   where
      constrainedDrag dPos@(dx, dy) item = do
         newView <- itemDrag dPos item >>= itemView
         let
            -- Constrain the item not to go off the top or left of the diagram.
            adjustment = case viewBox newView of
               NoBox -> dPos
               BoundBox p _ -> (dx - min (p ^. pX) 0, dy - min (p ^. pY) 0)
         itemDrag adjustment item

-- ToDo: add shift modifier for constrained moves.


-- | Follow connections and pegs to find all the other elements on the diagram that must
-- be adjusted after something is moved.
findAffected :: (Connectable v) => Set DiagramId -> Delta v (Set DiagramId)
findAffected s = do
   let xs = S.toList s
   add1 <- concatMap (map identifier) <$> mapM getAllConnected xs
   add2 <- concatMap (map identifier) <$> mapM getAllPegged xs
   let rs = S.fromList (add1 ++ add2) `S.union` s
   if S.size rs == S.size s then return s else findAffected rs


-- | An attempt to drag something is to be ignored.
ignoreDrag :: (Connectable v) => Action v -> Delta v (Action v)
ignoreDrag ev =
   case actionCommand ev of
      Drag _ -> yieldViews >>= ignoreDrag
      _ -> return ev


-- | Generic function for handling actions delegated to Connectable.
handleDelegatedAction :: (Connectable v) =>
   v  -- ^ The Connectable item that has been delegated the action.
   -> Delta v (Action v)
      -- ^ What to do when the current item is activated (double-clicked). Typically will either
      -- call "yieldProperties" or else open up some other dialog and then call "yieldViews"
   -> (Char -> Delta v (Action v))
      -- ^ Function for handling a key press. Delete is already handled. Note that this should
      -- finish with a call to "yieldViews" to update the screen and get the next action.
   -> Action v  -- ^ The action.
   -> Delta v (Action v)
handleDelegatedAction item activation charFunc act =
   case actionCommand act of
      Select -> yieldViews  -- Not delegated to shapes. Should never happen.
      Activate -> activation
      Drag _ -> moveItems [item] act
      EndDrag _ -> yieldViews  -- Not delegated to shapes. Should never happen.
      Key '\DEL' -> deleteItem (identifier item) >> yieldViews
      Key c -> charFunc c
      Script f -> f >> yieldViews



-- | Rubber band drawing. Returns the start and end location as well as the next action.
-- Should be called when the left button is already down as it returns at the first non-drag
-- action.
rubberBand :: (Viewable v) =>
   (Point -> Point -> Paint v ())  -- ^ For drawing the rubber band from start to end.
   -> Point                     -- ^ Start point. Usually the "actionLocation" of the last action.
   -> Delta v (Point, Point, Action v)
rubberBand drawBand start = do
   modifying (deltaDiagram . diagramOrder) (uuid :)
   let
      next end1 action =
         case actionCommand action of
            Drag dPos -> do
               let end2 = start `movePoint` dPos
               tellTransients $ ViewSet $ M.singleton uuid $ rubberView start end2
               yieldViews >>= next end2
            _ -> do
               tellTransients $ ViewSet $ M.singleton uuid mempty
               return (start, end1, action)
   ev <- yieldViews
   result <- next start ev
   modifying (deltaDiagram . diagramOrder) $ delete uuid
   return result
   where
      rubberView p1 p2 = mempty {viewDraw = drawBand p1 p2, viewBox = mkBoundBox p1 p2}
      uuid = fromWords 2369725315 95306541 2660896237 3031441912
         -- Arbitrary constant UUID used only for rubber bands.


-- | Select by dragging a box from one corner to the other. Items that are enclosed by the box
-- are included. The "Paint" action should draw a rectangular box in thin dashed lines.
areaSelect :: (Viewable v) =>
   (Point -> Point -> Paint v ()) -> Point -> Modifiers -> Delta v (Action v)
areaSelect drawBox start modifiers = do
   (p1, p2, action) <- rubberBand drawBox start
   let box = mkBoundBox p1 p2
   items <- M.toList <$> use (deltaDiagram . diagramContents)
   selection <- use deltaSelection
   let targets = S.fromList $ map fst $ filter (\(_, v) -> itemInBox v box) items
      -- Items in the selected area.
   changeSelection $ if modShift modifiers then S.union selection targets else targets
   return action


-- | Align the selected items.
alignGeneric :: (Connectable v) =>
   Text  -- ^ Action description prefix. E.g. \"Aligned\".
   -> (v -> BoundBox -> (Double, Double))  -- ^ Offset for @Viewable@ to move for alignment.
   -> Action v
alignGeneric desc offsetF = mkScriptAction $ do
   sel <- selectedItems
   when (length sel >= 2) $ do  -- Alignment of 0 or 1 items is a no-op.
      (targetAffectedSet, onlyAffectedSet) <- moveTargetSets $ S.fromList $ map fst sel
      contentsBefore <- use $ deltaDiagram . diagramContents
      let
         targets = mapMaybe ((`M.lookup` contentsBefore) . fst) sel
         outer = mconcat $ map (itemBounds . snd) sel
      forM_ targets $ \item -> itemMove (offsetF item outer) item
      contentsAfter <- use $ deltaDiagram . diagramContents
      let
         targetAffected = mapMaybe (`M.lookup` contentsAfter) $ S.toList targetAffectedSet
         onlyAffected = mapMaybe (`M.lookup` contentsAfter) $ S.toList onlyAffectedSet
      forM_ (targetAffected ++ onlyAffected) $ \item -> itemAdjust item
      tellCheckpoint $ desc <> " " <> pack (show (length sel)) <> " items."
   yieldViews


-- | Align to the top of the selection bound box.
alignTop :: (Connectable v) => Action v
alignTop = alignGeneric "Top aligned" topFunc
   where
      topFunc (itemBounds -> BoundBox p1 _) (BoundBox p2 _) = (0, p2 ^. pY - p1 ^. pY)
      topFunc _ _ = (0,0)


-- | Align to the top of the selection bound box.
alignBottom :: (Connectable v) => Action v
alignBottom = alignGeneric "Bottom aligned" bottomFunc
   where
      bottomFunc (itemBounds -> BoundBox _ p1) (BoundBox _ p2) = (0, p2 ^. pY - p1 ^. pY)
      bottomFunc _ _ = (0,0)


-- | Align to left of selection bound box.
alignLeft :: (Connectable v) => Action v
alignLeft = alignGeneric "Left aligned" leftFunc
   where
      leftFunc (itemBounds -> BoundBox p1 _) (BoundBox p2 _) = (p2 ^. pX - p1 ^. pX, 0)
      leftFunc _ _ = (0,0)


-- | Align to left of selection bound box.
alignRight :: (Connectable v) => Action v
alignRight = alignGeneric "Right aligned" rightFunc
   where
      rightFunc (itemBounds -> BoundBox _ p1) (BoundBox _ p2) = (p2 ^. pX - p1 ^. pX, 0)
      rightFunc _ _ = (0,0)


alignVertical :: (Connectable v) => Action v
alignVertical = alignGeneric "Vertically aligned" midFunc
   where
      midFunc (itemBounds -> b@BoundBox {}) outer@BoundBox {} =
         (outer ^. boxCentre . pX - b ^. boxCentre . pX, 0)
      midFunc _ _ = (0,0)


alignHorizontal :: (Connectable v) => Action v
alignHorizontal = alignGeneric "Horizontally aligned" midFunc
   where
      midFunc (itemBounds -> b@BoundBox {}) outer@BoundBox {} =
         (0, outer ^. boxCentre . pY - b ^. boxCentre . pY)
      midFunc _ _ = (0,0)


sizeGeneric :: (Connectable v) =>
   ([v] -> Double)  -- ^ Function to extract update from list of items.
   -> (Double -> v -> Delta v v)  -- ^ Function to update an item.
   -> Action v
sizeGeneric sizeF resizeF = mkScriptAction $ do
   sel <- selectedItems
   when (length sel >= 2) $ do  -- Size adjustment of 0 or 1 items is a no-op.
      contentsBefore <- use $ deltaDiagram . diagramContents
      let
         p = sizeF $ map snd sel
         targets = mapMaybe ((`M.lookup` contentsBefore) . fst) sel
      forM_ targets $ resizeF p
      contentsAfter <- use $ deltaDiagram . diagramContents
      (targetAffectedSet, onlyAffectedSet) <- moveTargetSets $ S.fromList $ map fst sel
      let
         targetAffected = mapMaybe (`M.lookup` contentsAfter) $ S.toList targetAffectedSet
         onlyAffected = mapMaybe (`M.lookup` contentsAfter) $ S.toList onlyAffectedSet
      forM_ (targetAffected ++ onlyAffected) itemAdjust
      tellCheckpoint $ "Resized " <> pack (show (length sel)) <> " items."
   yieldViews


sizeWidest :: (Connectable v, Viewable v) => Action v
sizeWidest = sizeGeneric widest setWidth
   where
      widest vs = maximum $ map (boxWidth . itemBounds) vs
      -- Maximum is safe because this is never called with a list of less than 2 items.


sizeNarrowest :: (Connectable v, Viewable v) => Action v
sizeNarrowest = sizeGeneric narrowest setWidth
   where
      narrowest vs = minimum $ map (boxWidth . itemBounds) vs


sizeTallest :: (Connectable v, Viewable v) => Action v
sizeTallest = sizeGeneric tallest setHeight
   where
      tallest vs = maximum $ map (boxHeight . itemBounds) vs
      -- Maximum is safe because this is never called with a list of less than 2 items.


sizeShortest :: (Connectable v, Viewable v) => Action v
sizeShortest = sizeGeneric shortest setHeight
   where
      shortest vs = minimum $ map (boxHeight . itemBounds) vs


setWidth :: (Viewable v) => Double -> v -> Delta v v
setWidth w item = itemResize (w, boxHeight $ itemBounds item) item


setHeight :: (Viewable v) => Double -> v -> Delta v v
setHeight h item = itemResize (boxWidth $ itemBounds item, h) item


-- | A function which computes the distributions of items along an axis. Items are represented
-- by an unsorted list of of @(position, size)@ pairs, and the result is a list of the same
-- length containing adjusted positions for the corresponding items.
type DistributionFunc = [(Double, Double)] -> [Double]


-- | Calculate a distribution which places items at regular intervals, ignoring spaces.
distributeEven :: DistributionFunc
distributeEven [] = []
distributeEven [(p, _)] = [p]
distributeEven pairs = map fst $ sortOn snd $ zipWith newPos indexes [0..]
   where
      n = length pairs
      start = minimum $ map fst pairs
      end = maximum $ map fst pairs
      sep = (end - start) / fromIntegral (n-1)
      indexes = map snd $ sortOn (fst . fst) $ zip pairs [0..]
      newPos :: Int -> Int -> (Double, Int)
      newPos i l = (fromIntegral l * sep + start, i)


-- | Calculate a distribution which spaces the items evenly. If the residual space is
-- negative then they will overlap evenly.
distributeSpace :: DistributionFunc
distributeSpace [] = []
distributeSpace [(p, _)] = [p]
distributeSpace pairs = map (fst . fst) $ sortOn snd $ scanl1 newPair pairs1
   where
      n = length pairs
      pairs1 = sortOn (fst . fst) $ zip pairs [0..]
      start = let (x, w) = fst $ head pairs1 in (x - w/2)
      end = let (x, w) = fst $ last pairs1 in (x + w/2)
      totalSpace = end - start
      used = sum $ map snd pairs
      gap = (totalSpace - used) / fromIntegral (n - 1)
      newPair :: ((Double, Double), Int) -> ((Double, Double), Int) -> ((Double, Double), Int)
      newPair ((p1, w1), _) ((_, w0), i) = ((p1 + (w0+w1)/2 + gap, w0), i)


-- | Horizontal position and width of the item. Assumes the item bounds are not "NoBox".
itemHPos :: (Viewable v) => v -> (Double, Double)
itemHPos v = (b ^. boxCentre . pX, boxWidth b) where b = itemBounds v


-- | Vertical position and height of the item. Assumes the item bounds are not "NoBox".
itemVPos :: (Viewable v) => v -> (Double, Double)
itemVPos v = (b ^. boxCentre . pY, boxHeight b) where b = itemBounds v

-- | Move the item to a horizontal position.
itemHMove :: (Viewable v) => Double -> v -> Delta v ()
itemHMove x v = void $ itemMove (x - v ^. to itemBounds . boxCentre . pX, 0) v


-- | Move the item to a horizontal position.
itemVMove :: (Viewable v) => Double -> v -> Delta v ()
itemVMove y v = void $ itemMove (0, y - v ^. to itemBounds . boxCentre . pY) v


-- | Distribute the current selection according to the rule functions given.
distributeGeneric :: (Connectable v) =>
   Text  -- ^ Description of operation. E.g. @\"Distribute\"@ or @\"Space\"@.
   -> DistributionFunc
   -> (v -> (Double, Double))  -- ^ Current position and width or height.
   -> (Double -> v -> Delta v ())  -- ^ Move the item to this position.
   -> Action v
distributeGeneric desc distFunc posFunc moveFunc = mkScriptAction $ do
      sel <- filter (validBox . snd) <$> selectedItems
      when (length sel >= 3) $ do    -- Distributing less than 3 items is a no-op.
         let
            newPositions = distFunc $ map (posFunc . snd) sel
         zipWithM_ moveFunc newPositions $ map snd sel
         contentsAfter <- use $ deltaDiagram . diagramContents
         (targetAffectedSet, onlyAffectedSet) <- moveTargetSets $ S.fromList $ map fst sel
         let
            targetAffected = mapMaybe (`M.lookup` contentsAfter) $ S.toList targetAffectedSet
            onlyAffected = mapMaybe (`M.lookup` contentsAfter) $ S.toList onlyAffectedSet
         forM_ (targetAffected ++ onlyAffected) $ \item -> itemAdjust item
         tellCheckpoint $ desc <> " " <> pack (show (length sel)) <> " items."
      yieldViews
   where
      validBox v = itemBounds v /= NoBox && isNothing (v ^? itemConnectors)

-- | Distribute the selection horizontally at regular intervals.
distributeH :: (Connectable v) => Action v
distributeH = distributeGeneric "Horizontally distribute" distributeEven itemHPos itemHMove


-- | Distribute the selection vertically at regular intervals.
distributeV :: (Connectable v) => Action v
distributeV = distributeGeneric "Vertically distribute" distributeEven itemVPos itemVMove


-- | Distribute the selection with regular horizontal spaces between them.
spaceH :: (Connectable v) => Action v
spaceH = distributeGeneric "Horizontally space" distributeSpace itemHPos itemHMove


-- | Distribute the selection with regular vertical spaces between them.
spaceV :: (Connectable v) => Action v
spaceV = distributeGeneric "Vertically space" distributeSpace itemVPos itemVMove
