{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

-- |
--
-- Primitive editing actions for abstract diagrams.
module Hades.Abstract.Primitives (
  setDiagram,
  mergeDiagram,
  addItem,
  deleteItem,
  withItem,
  withItem1,
  modifyItem,
  updateItem,
  moveUp,
  moveTop,
  moveDown,
  moveBottom
) where

import Control.Lens
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import Hades.Abstract.Connections
import Hades.Abstract.Delta
import Hades.Abstract.Diagram


-- | Set the Diagram to be the argument, and update the Views accordingly.
setDiagram :: (Viewable v) => Diagram v -> Delta v ()
setDiagram d = do
  oldItems <- use (deltaDiagram . diagramList)
  let v = ViewSet $ M.fromList $ map (\i -> (identifier i, mempty)) oldItems
  tellViews v   -- Clear old diagram views.
  tellTransients v
  deltaDiagram `assign` d   -- Lens (.=) is hidden, so use `assign` synonym instead.
  deltaSelection `assign` S.empty
  tellAll


-- | Merge an existing diagram into the current one. The new items are placed at the front and
-- selected.
mergeDiagram :: (Viewable v) => Diagram v -> Delta v ()
mergeDiagram d = do
  deltaDiagram %= mappend d
  changeSelection $ S.fromList $ map identifier $ diagramToList d
  tellAll


-- | Add a new item to the Diagram. The new item is assigned a "DiagramId"
-- and put at the front. Returns the new item.
addItem :: (Viewable v) => (DiagramId -> v) -> Delta v v
addItem f = do
  uuid <- nextDiagramId
  let item = f uuid
  deltaDiagram %= diagramAdd item
  tellItem item
  return item


-- | Remove an item from the Diagram. If any other entities are connected to it then disconnect
-- them.
deleteItem :: (Connectable v) => DiagramId -> Delta v ()
deleteItem uuid = do
    sel <- getSelection
    changeSelection $ S.delete uuid sel
    connected <- getAllConnected uuid
    forM_ connected $ updateItem . (itemConnectors %~ disconnect)
    deltaDiagram %= diagramDelete uuid
    let v = ViewSet $ M.singleton uuid mempty
    tellViews v
    tellTransients v
  where
    disconnect c = if c `isConnectedTo` uuid then Unconnected $ connectorPoint c else c


-- | Execute an action with the item corresponding to a UUID. If the UUID is not found
-- then @Nothing@ is returned.
withItem :: (Viewable v) => DiagramId -> (v -> Delta v a) -> Delta v (Maybe a)
withItem uuid actionF = do
  cs <- use $ deltaDiagram . diagramContents
  case M.lookup uuid cs of
    Nothing -> return Nothing
    Just item -> Just <$> actionF item


-- | Similar to "withItem", except that if the item is not found then the action is ignored.
withItem1 :: (Viewable v) => DiagramId -> (v -> Delta v ()) -> Delta v ()
withItem1 uuid act = void $ withItem uuid act


-- | Modify an item in the Diagram if it exists, and return the new version. The modification
-- is a monadic action so that the modified item can have side effects elsewhere in the diagram,
-- such as moving connectors to follow a box.
modifyItem :: (Viewable v) => DiagramId -> (v -> Delta v v) -> Delta v (Maybe v)
modifyItem uuid actionF = do
  cs <- use $ deltaDiagram . diagramContents
  case M.lookup uuid cs of
    Nothing -> return Nothing
    Just item -> do
      newItem <- actionF item
      deltaDiagram %= diagramModify (const newItem) uuid
      tellItem newItem
      return $ Just newItem


-- | If there is an element in the diagram with the same UUID then this will replace it.
-- Otherwise it will be added.
updateItem :: (Viewable v) => v -> Delta v ()
updateItem item = do
  cs <- use $ deltaDiagram . diagramContents
  let uuid = identifier item
  case M.lookup uuid cs of
    Nothing -> deltaDiagram %= diagramAdd item
    Just _ -> deltaDiagram %= diagramModify (const item) uuid
  tellItem item


-- | Private utility function. If the UUID exists then modify the diagram and "tell" that the
-- item in question must be redrawn.
alterDiagram :: (Viewable v) =>
  (Diagram v -> Diagram v) -> DiagramId -> Delta v ()
alterDiagram f uuid = do
  cs <- use $ deltaDiagram . diagramContents
  case M.lookup uuid cs of
    Nothing -> return ()
    Just item -> do
      deltaDiagram %= f
      tellItem item


-- | Move an element up one layer in the diagram.
moveUp :: (Viewable v) => DiagramId -> Delta v ()
moveUp uuid = alterDiagram (diagramMoveUp uuid) uuid


-- | Move an element to the top of the diagram.
moveTop :: (Viewable v) => DiagramId -> Delta v ()
moveTop uuid = alterDiagram (diagramMoveTop uuid) uuid


-- | Move an element down one layer in the diagram.
moveDown :: (Viewable v) => DiagramId -> Delta v ()
moveDown uuid = alterDiagram (diagramMoveDown uuid) uuid

-- | Move an element to the bottom of the diagram.
moveBottom :: (Viewable v) => DiagramId -> Delta v ()
moveBottom uuid = alterDiagram (diagramMoveBottom uuid) uuid
