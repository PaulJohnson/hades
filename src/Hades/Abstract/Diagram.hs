{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

-- |
--
-- This module handles diagrams in their most abstract form.
module Hades.Abstract.Diagram (
   DiagramId,
   HasId (..),
   Diagram,
   diagramContents,
   diagramOrder,
   emptyDiagram,
   diagramToList,
   diagramFromList,
   diagramList,
   diagramAdd,
   diagramDelete,
   diagramModify,
   diagramMoveUp,
   diagramMoveDown,
   diagramMoveTop,
   diagramMoveBottom
) where


import Control.Lens
import Data.Aeson
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.UUID (UUID)


type DiagramId = UUID

-- | Things that carry a unique ID with them.
class HasId a where
   identifier :: a -> DiagramId


-- | The type of diagrams. The "Monoid" instance places the first diagram in front of the second.
-- If the two diagrams share DiagramIds then those from the first diagram will be used.
data Diagram v = Diagram {
      _diagramContents :: Map DiagramId v,
         -- ^ Each element has a "DiagramId", which is used as a form of weak pointer.
      _diagramOrder :: [DiagramId]
         -- ^ Order of the contents from front to back.
   } deriving Eq

instance Semigroup (Diagram v) where
   d1 <> d2 = Diagram {
         _diagramContents = _diagramContents d1 `mappend` _diagramContents d2,
         _diagramOrder = nub $ _diagramOrder d1 ++ _diagramOrder d2
      }

instance Monoid (Diagram v) where
   mempty = emptyDiagram

instance (ToJSON v) => ToJSON (Diagram v) where
   toJSON = toJSON . diagramToList

instance (FromJSON v, HasId v) => FromJSON (Diagram v) where
   parseJSON v = diagramFromList <$> parseJSON v


-- Template Haskell is not used for lens definitions because of painful problems
-- with recursive datatypes.

-- | The elements in the diagram. Each element has a "DiagramId",
-- which is used as form of weak pointer.
diagramContents :: Lens' (Diagram v) (Map DiagramId v)
diagramContents = lens _diagramContents $ \s c -> s {_diagramContents = c}


-- | Order of the contents from front to back.
diagramOrder :: Lens' (Diagram v) [DiagramId]
diagramOrder = lens _diagramOrder $ \s o -> s {_diagramOrder = o}


-- | Diagram with nothing in it.
emptyDiagram :: Diagram v
emptyDiagram = Diagram M.empty []


-- | Diagram contents in order from front to back.
diagramToList :: Diagram v -> [v]
diagramToList (Diagram c o) = mapMaybe (`M.lookup` c) o


-- | Inverse of "diagramToList". If two elements have the same DiagramId then
-- the first (uppermost) will be used.
diagramFromList :: (HasId v) => [v] -> Diagram v
diagramFromList = foldr diagramAdd emptyDiagram


-- | Lens for the list of elements in the diagram
diagramList :: (HasId v) => Lens' (Diagram v) [v]
diagramList = lens diagramToList $ const diagramFromList


-- | Add a new element to the front of the diagram. If the "DiagramId" already exists then
-- it replaces the old one.
diagramAdd :: (HasId v) => v -> Diagram v -> Diagram v
diagramAdd item (Diagram contents order) = Diagram newContents newOrder
   where
      newContents = M.insert (identifier item) item contents
      newOrder = uuid : if M.member uuid contents then delete uuid order else order
         where uuid = identifier item


-- | Remove an element from the diagram by its "DiagramId".
-- If the "DiagramId" is not found then no change.
diagramDelete :: DiagramId -> Diagram v -> Diagram v
diagramDelete uuid (Diagram contents order) = Diagram (M.delete uuid contents) (delete uuid order)


-- | Modify an element within the diagram. Assumes that the "DiagramId" does not change.
diagramModify :: (v -> v) -> DiagramId -> Diagram v -> Diagram v
diagramModify f uuid (Diagram contents order) = Diagram (M.adjust f uuid contents) order


-- | Move a diagram element up one in the order, unless its already at the top.
diagramMoveUp :: DiagramId -> Diagram v -> Diagram v
diagramMoveUp uuid (Diagram contents order) =
      Diagram contents $ if null ids1 then order else concat [ids1a, [uuid], ids1b, ids2]
   where
      (ids1, ids2) = contentsSplit uuid order
      (ids1a, ids1b) = splitAt (length ids1 - 1) ids1


-- Move a diagram element down one in the order, unless its already at the bottom.
diagramMoveDown :: DiagramId -> Diagram v -> Diagram v
diagramMoveDown uuid (Diagram contents order) =
      Diagram contents $ if null ids2 then order else concat [ids1, [ids2a, uuid], ids2b]
   where
      (ids1, ids2) = contentsSplit uuid order
      (ids2a : ids2b) = ids2  -- Safe because they are only used when ids2 is not null.


-- | Move a diagram element to the top.
diagramMoveTop :: DiagramId -> Diagram v -> Diagram v
diagramMoveTop uuid (Diagram contents order) =
      Diagram contents $ if null ids1 then order else uuid : ids1 ++ ids2
   where
      (ids1, ids2) = contentsSplit uuid order


-- | Move a diagram element to the bottom.
diagramMoveBottom :: DiagramId -> Diagram v -> Diagram v
diagramMoveBottom uuid (Diagram contents order) =
      Diagram contents $ if null ids2 then order else concat [ids1, ids2, [uuid]]
   where
      (ids1, ids2) = contentsSplit uuid order


-- | Private utility function. Splits the list at the "DiagramId"
-- and returns those before and those after.
contentsSplit :: DiagramId -> [DiagramId] -> ([DiagramId], [DiagramId])
contentsSplit uuid ids = (ids1, drop 1 ids2)
   where
      (ids1, ids2) = break (== uuid) ids
