{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

-- |
module Hades.Abstract.Connections (
  ConnectorEnd (..),
  moveEnd,
  Terminators,
  ConnectionDirection (..),
  Peg,
  Connectable (..),
  isConnectedTo,
  getAllConnected,
  getAllPegged,
  moveEnds,
  mkEndPoint,
  optimiseEnds
) where


import Control.Lens hiding ((.=))
import Data.Aeson
import qualified Data.Map as M
import Data.Text (Text, unpack)
import Hades.Abstract.Delta
import Hades.Abstract.Diagram
import Hades.Abstract.Geometry

-- | A connector end point may be a point on the diagram or on a diagram element.
-- The "Eq" instance ignores the location.
data ConnectorEnd =
  Unconnected {connectorPoint :: Point}
  | Connected {
      connectorPoint :: Point,
      connectorTarget :: DiagramId,
      connectorTargetChild :: Maybe DiagramId
        -- ^ Diagram elements may optionally have a list of child entities that can be
        -- connected to independently. If so then this may contain the child ID.
    }
  deriving (Show)

instance Eq ConnectorEnd where
  Unconnected pt1 == Unconnected pt2  =  pt1 == pt2
  Connected _ u1 u1c == Connected _ u2 u2c  = u1 == u2 && u1c == u2c
  _ == _  = False


instance ToJSON ConnectorEnd where
  toJSON (Unconnected pt) =
    object ["state" .= ("unconnected" :: Text), "point" .= pt]
  toJSON (Connected pt target child) =
    let
      base = [
        "state" .= ("connected" :: Text),
        "point" .= pt,
        "target" .= target]
    in case child of
      Nothing -> object base
      Just c -> object $ ("child" .= c) : base

instance FromJSON ConnectorEnd where
  parseJSON = withObject "connector end" $ \v ->
    v .: "state" >>= \case
      "unconnected" -> Unconnected <$> v .: "point"
      "connected" -> Connected <$> v .: "point" <*> v .: "target" <*> v .:? "child"
      s -> fail $ "Unknown connector end state: " ++ unpack s


-- | The start and ends of a connector as a pair. Note that in the Lens library the "both"
-- traversal is a handy way of doing something to both ends at once.
type Terminators = (ConnectorEnd, ConnectorEnd)


-- | For elements that have directed connections with other elements, this distinguishes the
-- direction of the relationship.
data ConnectionDirection = ConnectionFrom | ConnectionTo deriving (Eq, Ord, Enum, Show)


-- | A peg represents a possible attachment of a box shape to a point part-way along a line.
type Peg = (DiagramId, Double)


-- | Diagram types where some lines can be connected to boxes, and some boxes can be
-- pegged on to lines.
--
-- The default implementations are suitable for types where no such associations are possible.
class (Viewable v) => Connectable v where
  -- | Can the specified end of the first element connect to the second element?
  itemCanConnect :: ConnectionDirection -> v -> v -> Maybe DiagramId -> Delta v Bool
  itemCanConnect _ _ _ _ = return False
  -- | Each item may have zero or more connectors, which may be modified.
  itemConnectors :: Traversal' v ConnectorEnd
  itemConnectors = ignored
  -- | Given a line from the point to the element, where should the line terminate?
  -- The "DiagramId" argument specifies which child (if any) the connector will attach to.
  itemConnectionPoint :: v -> Maybe DiagramId -> Delta v (Point -> Point)
  itemConnectionPoint _ _  = return id
  -- | Attempt to peg the item to the argument. Returns the new value if it succeeds.
  -- Does not write the new value back to the diagram.
  itemPegAt :: v -> DiagramId -> Double -> Delta v (Maybe v)
  itemPegAt _ _ _ = return Nothing
  -- | Attempt to unpeg the item. No-op by default.
  itemUnpeg :: v -> Delta v v
  itemUnpeg = return
  -- | The point where this item is pegged, if any.
  itemPeg :: Traversal' v Peg
  itemPeg = ignored
  -- | The point where the item is pegged to, assuming the peg is valid.
  itemPegPoint :: Peg -> Delta v (Maybe Point)
  itemPegPoint = const $ return Nothing
  -- | Used when the item is connected in some way to other items, and those items have been
  -- changed. The item will adjust itself in the diagram and update its views in response.
  itemAdjust :: v -> Delta v ()


-- | If you try to move a "ConnectorEnd" it will only move if it is not connected to something.
-- Hence the connector position cannot be a lens.
moveEnd :: ConnectorEnd -> (Double, Double) -> ConnectorEnd
moveEnd (Unconnected pt) dPos = Unconnected $ movePoint pt dPos
moveEnd cnctr _ = cnctr


-- | Predicate: is this connected to the item referenced by the UUID?
isConnectedTo :: ConnectorEnd -> DiagramId -> Bool
isConnectedTo Unconnected {} _ = False
isConnectedTo (Connected _ t _) uuid = t == uuid


-- | Find all the diagram elements with connectors linking to the specified item.
getAllConnected :: (Connectable v) => DiagramId -> Delta v [v]
getAllConnected uuid =
    filter check . M.elems <$> use (deltaDiagram . diagramContents)
  where
    check = any (`isConnectedTo` uuid) . toListOf itemConnectors


-- | Find all the diagram elements which are pegged to the argument.
getAllPegged :: (Connectable v) => DiagramId -> Delta v [v]
getAllPegged uuid = filter check . M.elems <$> use (deltaDiagram . diagramContents)
  where
    check d = d ^? itemPeg . _1 == Just uuid


-- | Apply a vector to both ends.
moveEnds :: Terminators -> (Double, Double) -> Terminators
moveEnds (t1, t2) dPos = (moveEnd t1 dPos, moveEnd t2 dPos)


-- | If the Point is over an element satisfying the "itemCanConnect" predicate then generate a
-- connector to it. Otherwise generate an Unconnected end point.
mkEndPoint :: (Connectable v) =>
  ConnectionDirection -- ^ The end we are trying to connect.
  -> v                -- ^ The diagram element with an end point.
  -> Point            -- ^ Where the end point is currently located.
  -> Delta v ConnectorEnd
mkEndPoint end line pt = do
    scale <- use deltaZoom
    diagram <- use deltaDiagram
    let predicate = itemCanConnect end line
    diagramAt scale pt predicate diagram >>= \case
      Nothing -> return $ Unconnected pt
      Just (v, child) -> do
        pt1 <- ($ pt) <$> itemConnectionPoint v child
        return $ Connected pt1 (identifier v) child


-- | Under some circumstances (especially when connected at both ends) the correct
-- end points for a line can require iteration. This function
-- will return end points that are close enough. The first argument is the iteration limit.
optimisePoints ::
  Int    -- ^ Iteration limit.
  -> Point   -- ^ Starting point for end 1.
  -> (Point -> Point)  -- ^ Line connection point at end 1 for a line starting at end 2.
  -> Point   -- ^ Starting point for end 2.
  -> (Point -> Point) -- ^ Line connection point at end 2 for a line starting at end 1.
  -> (Point, Point)
optimisePoints n p1 f1 p2 f2 =
  if n <= 0 || d p1 p1a + d p2 p2a < 0.1
    then (p1, p2)
    else optimisePoints (n-1) p1a f1 p2a f2
  where
    p1a = f1 p2
    p2a = f2 p1
    d p q = let (dx,dy) = pointDiff p q in abs dx + abs dy
      -- Use rectangular distance because its faster to compute.


-- | Like optimisePoints, but applied to Terminators. If one of the ends is connected to
-- an element that is not in the diagram then the function returns an
-- "Unconnected" instead.
optimiseEnds :: (Connectable v) => Terminators -> Delta v Terminators
optimiseEnds (c1@Unconnected {}, c2@Unconnected {}) = return (c1, c2)
optimiseEnds (c1@(Unconnected p1), Connected p2 uuid child) = do
  cs <- use $ deltaDiagram . diagramContents
  case M.lookup uuid cs of
    Nothing -> return (c1, Unconnected p2)
    Just target -> do
      p2a <- ($ p1) <$> itemConnectionPoint target child
      return (c1, Connected p2a uuid child)
optimiseEnds (c1@Connected {}, c2@Unconnected {}) = do
  (c2a, c1a) <- optimiseEnds (c2, c1)
  return (c1a, c2a)
optimiseEnds (Connected p1 uuid1 child1, c2@(Connected p2 uuid2 child2)) = do
  cs <- use $ deltaDiagram . diagramContents
  case M.lookup uuid1 cs of
    Nothing -> optimiseEnds (Unconnected p1, c2)
    Just target1 ->
      case M.lookup uuid2 cs of
        Nothing -> do
          p1a <- ($ p2) <$> itemConnectionPoint target1 child1
          return (Connected p1a uuid1 child1, Unconnected p2)
        Just target2 -> do
          f1 <- itemConnectionPoint target1 child1
          f2 <- itemConnectionPoint target2 child2
          let (p1a, p2a) = optimisePoints 10 p1 f1 p2 f2
          return (Connected p1a uuid1 child1, Connected p2a uuid2 child2)
