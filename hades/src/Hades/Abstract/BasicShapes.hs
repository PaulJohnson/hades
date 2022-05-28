{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}


{- |

__The set of abstract shapes supported by Hades.__

Each geometrical shape has
properties and behaviours that will be the same regardless of what it represents
in the diagram. These are encoded here to avoid duplication.

The one commonality that is not included here is the drawing method. This is
dependent on the underlying GUI library, and hence is kept in a parallel module
elsewhere.

Shapes covered here are:

* Ellipse

* Diamond: a parallelogram with corners top, bottom, left and right.

* A parallelogram with horizontal edges. This is defined by its
inner box (the rectangle where text can be drawn) rather than the outer bounding box.
This avoids a bunch of tricky edge cases with narrow parallelograms, including the
non-existent inner box and the negative base.

* Oval: a rectangle with rounded ends. Like the parallelogram this is defined by its inner box.

* Lines that connect boxes together.
-}

module Hades.Abstract.BasicShapes (
  -- * Handles
  Handle,
  boxHandles,
  simpleHandle,
  -- * Basic shape definitions
  ShapeClass (..),
  BoxShapeClass (..),
  LineShapeClass (..),
  -- * Functions for specific shapes
  -- ** Lines
  LineShape (LineShape),
  lineEnds,
  lineMiddle,
  lineFractionSegment,
  lineFractionPoint,
  linePoints,
  lineSegments,
  lineHandles,
  lineTouched,
  lineSnap,
  lineMenu,
  lineUpdate,
  -- ** Rectangles
  Rectangle (Rectangle),
  rectangleShape,
  intersectWithRectangle,
  rectangleCorners,
  rectangleEdges,
  -- ** Ellipses
  Ellipse (Ellipse),
  ellipseShape,
  ellipseTouched,
  -- ** Diamonds
  Diamond (Diamond),
  diamondShape,
  diamondTouched,
  diamondCorners,
  diamondInnerBox,
  diamondConnectionPoint,
  -- ** Parallelograms
  Parallelogram (Parallelogram),
  parallelogramShape,
  parallelogramSlant,
  parallelogramTouched,
  parallelogramCorners,
  parallelogramOuterBox,
  parallelogramConnectionPoint,
  -- ** Ovals
  Oval (Oval),
  ovalShape,
  ovalAspect,
  ovalBoxTouched,
  ovalBoxOuter,
  ovalBoxConnectionPoint,
  -- ** Comment boxes
  CommentBox (CommentBox),
  commentBoxShape,
  commentBoxCorners,
  commentBoxOuter,
  commentBoxConnectionPoint,
  commentBoxFoldSize
) where

import Control.Lens hiding ((.=))
import Control.Monad
import Data.Aeson
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import qualified Data.Set as S
import Data.Text (Text)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Hades.Abstract.Commands
import Hades.Abstract.Connections
import Hades.Abstract.Diagram
import Hades.Abstract.Delta
import Hades.Abstract.Geometry
import Hades.Abstract.Primitives
import Reactive.Banana.Menu


-- | A Handle allows a diagram element to be moved and resized. It takes a starting point
-- and a Delta to execute in response to user Actions. The Delta should finish by calling
-- "yieldViews" to update the screen and get the next user Action.
--
--  The returned View should be passed to "tellTransients" to keep it on top of everything else.
type Handle v = Menu (Action v) -> Point -> (Action v -> Delta v (Action v)) -> Delta v (View v)


-- | Handles for a shape defined by a "BoundBox".
--
-- Each handle is defined by its location and a function for updating
-- the shape from a new point if the handle is dragged.
boxHandles :: Lens' a BoundBox -> a -> [(Point, Point -> a)]
boxHandles boxLens v = case v ^. boxLens of
  NoBox -> []
  BoundBox (Point x1 y1) (Point x2 y2) ->
    let
      topLeft = (
        Point x1 y1,
        \p -> (set boxLens $ mkBoundBox (Point x2 y2) $ constrainQ2 (Point (x2-1) (y2-1)) p) v)
      top = (
        Point midX y1,
        \p -> (set boxLens $ mkBoundBox (Point x2 y2) $ pX .~ x1 $ pY %~ min (y2-1) $ p) v)
      topRight = (
        Point x2 y1,
        \p -> (set boxLens $ mkBoundBox (Point x1 y2) $ constrainQ1 (Point (x1+1) (y2-1)) p) v)
      right = (
        Point x2 midY,
        \p -> (set boxLens $ mkBoundBox (Point x1 y1) $ pX %~ max (x1+1) $ pY .~ y2 $ p) v)
      botRight = (
        Point x2 y2,
        \p -> (set boxLens $ mkBoundBox (Point x1 y1) $ constrainQ4 (Point (x1+1) (y1+1)) p) v)
      bottom = (
        Point midX y2,
        \p -> (set boxLens $ mkBoundBox (Point x1 y1) $ pX .~ x2 $ pY %~ max (y1+1) $ p) v)
      botLeft = (
        Point x1 y2,
        \p -> (set boxLens $ mkBoundBox (Point x2 y1) $ constrainQ3 (Point (x2-1) (y1+1)) p) v)
      left = (
        Point x1 midY,
        \p -> (set boxLens $ mkBoundBox (Point x2 y2) $ pX %~ min (x2-1) $ pY .~ y1 $ p) v)
      midX = (x1+x2)/2
      midY = (y1+y2)/2
    in [botRight, botLeft, bottom, topRight, right, topLeft, top, left]
      -- bottom and right first because they need to be on top when handles overlap.


-- | The simplest case for a handle is that moving it just causes the diagram element to be
-- redrawn with a different shape, and no special semantics are associated with this.
simpleHandle :: (Connectable v) =>
  Text
    -- ^ Description of what moving this handle does. E.g. \"Resize \" or \"Move bend in \".
    -- Note the trailing space on the string: the object description will be appended.
  -> Menu (Action v)
    -- ^ Menu for this handle. "mempty" for nothing.
  -> Handle v
    -- ^ The type of handle to draw.
  -> (Point, Point -> v)
    -- ^ The starting point for the handle, and how the item should be updated.
  -> Delta v (View v)
simpleHandle msg menu handle (pt, updateF) = do
  contents <- use $ deltaDiagram . diagramContents
  let
    item = updateF pt
    uuid = identifier item
  affectedList <- S.toList . S.delete uuid <$> findAffected (S.singleton uuid)
  let
    dragState = mapMaybe (\u -> contents ^? ix u) affectedList
    dragUpdate connected newItem = do
      updateItem newItem
      forM_ connected itemAdjust
      -- dragState & dragUpdate executed by processDrag later.
  desc <- itemDescription item
  handle menu pt $ processDrag (msg <> desc) (return dragState) $ \s ev d ->
    dragUpdate s $ updateF $ movePoint (actionLocation ev) d


-- | A Shape tells a Viewable where it is, what size it is and so on. Based on this certain
-- geometrical properties can be derived.
class (ToJSON s, FromJSON s) => ShapeClass s where
  -- | The boundbox for the shape. Note that an actual Viewable may extend past this box,
  -- depending on how it is drawn.
  shapeBounds :: s -> BoundBox
  -- | True if the point is on the shape. Takes "Scale" because lines need some scale-invariant
  -- slop around them.
  shapeTouched :: s -> Scale -> Point -> Bool
  -- | True if the shape is entirely contained in the BoundBox.
  shapeInBox :: s -> BoundBox -> Bool
  -- | Move the shape by (x,y) relative.
  shapeDrag :: (Double, Double) -> s -> s
  -- | When drawing a line from the point to the shape, where should the line terminate?
  shapeConnectionPoint :: s -> Point -> Point


-- | Enclosed shapes such as boxes, ellipses and diamonds.
class (ShapeClass s) => BoxShapeClass s where
  -- | A box suitable for drawing text within the shape.
  shapeInnerBox :: s -> BoundBox
  -- | Attempt to set the width and height.
  -- Boxes may impose constraints to avoid impossible results.
  shapeResize :: (Double, Double) -> s -> s
  -- | Handles for resizing this box.
  shapeHandles :: s -> [(Point, Point -> s)]
  -- | The boundbox that defines the shape.
  shapeDefinition :: Lens' s BoundBox


-- | Non-enclosed shapes, especially lines.
class (ShapeClass s) => LineShapeClass s where
  -- | A point where annotation text (e.g. the line name) should be centered.
  shapeAnnotationPoint :: s -> Point


-- | A shape for a line connecting a sequence of points.
data LineShape = LineShape {
    _lineEnds :: Terminators,
    _lineMiddle :: Vector Point
  } deriving (Eq, Show)

instance ToJSON LineShape where
  toJSON shape = object [
      "ends" .= (shape ^. lineEnds),
      "middle" .= (shape ^. lineMiddle)
    ]

instance FromJSON LineShape where
  parseJSON = withObject "line position" $ \v -> LineShape <$>
      v .: "ends" <*>
      v .: "middle"

instance ShapeClass LineShape where
  shapeBounds = polygonBox . linePoints
  shapeTouched line scale testPoint = any (lineTouched scale testPoint) $ lineSegments line
  shapeInBox line box = all (`inBox` box) $ linePoints line
  shapeDrag d = (lineEnds %~ (`moveEnds` d)) . (lineMiddle %~ fmap (`movePoint` d))
  shapeConnectionPoint line fromPoint = snd $ minimumBy (comparing fst) candidates
    where  -- At present this functionality is not used for lines.
      candidates = map (\pt -> (distance $ fromPoint `pointDiff` pt, pt)) points
      points = linePoints line

instance LineShapeClass LineShape where
  shapeAnnotationPoint = lineMidPoint


lineEnds :: Lens' LineShape Terminators
lineEnds = lens _lineEnds $ \s v -> s{_lineEnds = v}


lineMiddle :: Lens' LineShape (Vector Point)
lineMiddle = lens _lineMiddle $ \s v -> s{_lineMiddle = v}


-- | Segment on the line a fraction of the way along the whole line.
-- @0@ means the start of the line, @1@ means the end of the line, and @0.5@ means the mid point.
-- Values outside this range will give the start or end of the line as appropriate.
lineFractionSegment ::
  Double -> LineShape -> (Line, Double)
lineFractionSegment f shape = case dropWhile ((< dist) . snd) segEndDists of
    [] ->
      (uncurry lineFromPoints $ last segments, 1)  -- Numeric edge case.
    ((p1, p2), d) : _ ->
      (lineFromPoints p1 p2, 1 - (d - dist) / distance (p1 `pointDiff` p2))
  where
    f' = 0 `max` (f `min` 1)  -- Limit f' to range 0-1.
    segments = zip allPoints (tail allPoints)  -- allPoints contains at least 2 points.
    allPoints = linePoints shape
    lengths = map (distance . uncurry pointDiff) segments
    segEndDists = zip segments $ drop 1 $ scanl (+) 0 lengths  -- Length up to end of each seg.
    dist = sum lengths * f'


-- | The point a fraction along the total length of a line. @0@ means the start of the line, @1@
-- means the end of the line, and @0.5@ means the mid point.
lineFractionPoint :: Double -> LineShape -> Point
lineFractionPoint f shape = uncurry linePoint $ lineFractionSegment f shape


-- | The point half way along the length of the line.
lineMidPoint :: LineShape -> Point
lineMidPoint = lineFractionPoint 0.5


-- | If the line is connected to other shapes then update the end-points with the
-- appropriate connection points for those shapes.
lineUpdate :: (Connectable v) => LineShape -> Delta v LineShape
lineUpdate line =
    if V.null $ line ^. lineMiddle
      then do
        newEnds <- optimiseEnds $ line ^. lineEnds
        return $ lineEnds .~ newEnds $ line
      else do  -- Partial functions on lineMiddle are safe because line is not null.
        let (start, end) = line ^. lineEnds
        newStart <- updateConnection start $ V.head $ line ^. lineMiddle
        newEnd <- updateConnection end $ V.last $ line ^. lineMiddle
        return $ lineEnds .~ (newStart, newEnd) $ line
  where
    updateConnection c@Unconnected {} _ = return c
    updateConnection (Connected pt diagId child) sourcePt = do
      cs <- use $ deltaDiagram . diagramContents
      case M.lookup diagId cs of
        Nothing -> return $ Unconnected pt
        Just target -> do
          newPt <- ($ sourcePt) <$> itemConnectionPoint target child
          return $ Connected newPt diagId child


-- | Connectors must connect and disconnect with other shapes as they are dragged around.
-- Making and breaking connections can have semantic effects on the model.
--
-- If the line has points in the middle then the corresponding handles have a menu option
-- to delete themselves.
lineHandles :: (Connectable v) =>
  LineShape   -- ^ The shape to provide the handles for.
  -> Handle v   -- ^ The type of handle to draw.
  -> (LineShape -> v)   -- ^ Function for a copy of the item updated with a new shape.
  -> (v -> Delta v ())
    -- ^ The action to take when the drag is completed. Typically this implements the semantics
    -- of this type of line.
  -> Delta v [View v]
lineHandles line handle updateF updateFinal = do
    let
      item = updateF line
      uuid = identifier item
    contents <- use $ deltaDiagram . diagramContents
    affectedList <- S.toList . S.delete uuid <$> findAffected (S.singleton uuid)
    let
      dragState = return $ mapMaybe (\u -> contents ^? ix u) affectedList
      dragUpdate affected newShape = do
        shape2 <- lineUpdate newShape  -- Move the endpoints as necessary.
        updateItem $ updateF shape2
        forM_ affected itemAdjust
    desc <- itemDescription item
    startH <- handle mempty (connectorPoint $ line ^. lineEnds . _1) $ \act -> do
      next <- processDrag ("Move " <> desc) dragState
        (\s ev d -> do
          newStart <- mkEndPoint ConnectionFrom item $ movePoint (actionLocation ev) d
          dragUpdate s $ lineEnds . _1 .~ newStart $ line
        ) act
      item1 <- M.lookup uuid <$> use (deltaDiagram . diagramContents)
      mapM_ updateFinal item1
      return next
    endH <- handle mempty (connectorPoint $ line ^. lineEnds . _2) $ \act -> do
      next <- processDrag ("Move " <> desc) dragState
        (\s ev d -> do
          newEnd <- mkEndPoint ConnectionTo item $ movePoint (actionLocation ev) d
          dragUpdate s $ lineEnds . _2 .~ newEnd $ line
        ) act
      item1 <- M.lookup uuid <$> use (deltaDiagram . diagramContents)
      mapM_ updateFinal item1
      return next
    bendHandles <- forM [0 .. V.length (line ^. lineMiddle)-1] $ \n -> do
        let
          deleteAction = mkScriptAction $ do
            newShape <- lineUpdate $ lineMiddle %~ deleteItemN n $ line
            updateItem $ updateF newShape
            tellCheckpoint $ "Delete bend in " <> desc
            yieldViews
          menu = Menu [[menuItem "Delete bend" deleteAction]]
        handle menu ((line ^. lineMiddle) ! n) $
          processDrag ("Move bend in " <> desc) dragState
            (\s _ d -> dragUpdate s $ (lineMiddle . ix n) %~ flip movePoint d $ line)
    return $ startH : endH : bendHandles
  where
    -- Delete the n'th item from the vector (counting from zero), leaving the vector 1 item
    -- shorter.
    deleteItemN n vec = V.take n vec V.++ V.drop (n+1) vec


-- | The control points for the line, in order, including the start and end points. Guaranteed to
-- contain at least 2 points
linePoints :: LineShape -> [Point]
linePoints line =
    connectorPoint start : V.toList (line ^. lineMiddle) ++ [connectorPoint end]
  where
    (start, end) = line ^. lineEnds

-- | The line segments for the line, in order. Guaranteed to contain at least one line.
lineSegments :: LineShape -> [Line]
lineSegments line = zipWith lineFromPoints points $ tail points  -- points contains >= 2 items
  where points = linePoints line


-- | Predicate to determine if the Point is close enough to the line to be considered as
-- touching it. This means being within @4/scale@.
lineTouched :: Scale -> Point -> Line -> Bool
lineTouched scale p line@(Line p1 _ _) =
    near p1 || near p2 || nearLine
  where
    tolerance = 4/scale
    p2 = linePoint line 1.0 -- End of line.
    near q = distance (p `pointDiff` q) <= tolerance
    nearLine  = maybe False (<= tolerance) $ lineDistance line p


-- | Function to find if a point is within the given distance of a line, and if so to snap the point
-- to the line. If the snap occurs it returns the snap distance, the fraction along the line,
-- and the new point.
lineSnap :: Double -> LineShape -> Point -> Maybe (Double, Double, Point)
lineSnap dist shape pt =
    case foldr (joinSnaps . segSnap) Nothing segStartDists of
      Nothing -> Nothing
      Just (d, p, pt2) -> Just (d, p / shapeLength, pt2)
  where
    allPoints = linePoints shape
    segments = zip allPoints (tail allPoints)
    lengths = map (distance . uncurry pointDiff) segments
    shapeLength = sum lengths
    segStartDists = zip segments $ scanl (+) 0 lengths  -- Length up to start of each seg.
    segSnap ((p1, p2), d) =
      let
        line@(Line _ dx dy) = lineFromPoints p1 p2
        normal = Line pt (-dy) dx
        endSnaps = joinSnaps (snapPoint d p1) (snapPoint (d + distance (pointDiff p1 p2)) p2)
      in case lineIntersection line normal of
        Nothing -> endSnaps  -- Can't happen because lines are at 90 degrees.
        Just (f1, _) ->
          let
            snap = linePoint line f1
            snapDist = distance $ pointDiff pt snap
          in if 0 <= f1 && f1 <= 1 && snapDist <= dist
            then Just (snapDist, d + distance (pointDiff p1 snap), snap)
            else endSnaps
    snapPoint d p =
      let d1 = distance (pointDiff pt p)
      in if d1 < dist then Just (d1, d, p) else Nothing
    joinSnaps Nothing Nothing = Nothing
    joinSnaps (Just s) Nothing = Just s
    joinSnaps Nothing (Just s) = Just s
    joinSnaps s1@(Just (d1, _, _)) s2@(Just (d2, _, _)) =
      if d1 < d2 then s1 else s2


-- | Menu for lines. Containes a single option to create a new bend in the line.
lineMenu :: (Viewable v) => (LineShape -> v) -> LineShape -> Point -> Menu (Action v)
lineMenu updateF shape pt = Menu [[menuItem "Add bend" action]]
  where
    segments = zip [0..] $ lineSegments shape
    candidates = mapMaybe (\(n,ln) -> (n,) <$> lineDistance ln pt) segments
    touchedSeg = fst $ minimumBy (comparing snd) candidates
    action =
      if null candidates
        then nullAction  -- Missed all the segments - no change. Maybe clicked near end point
        else mkScriptAction $ do
          let newItem = updateF $ lineMiddle %~ insertItemN touchedSeg pt $ shape
          updateItem newItem
          desc <- itemDescription newItem
          sel <- getSelection
          changeSelection $ S.insert (identifier newItem) sel
          tellCheckpoint $ "Add bend to " <> desc
          yieldViews
    -- Insert a new item "n" in the vector with value x. The old item n and all following it
    -- are bumped up one place.
    insertItemN n x vec = snoc before x V.++ after
      where (before, after) = V.splitAt n vec


newtype Rectangle = Rectangle {_rectangleShape :: BoundBox} deriving (Eq)

instance ToJSON Rectangle where
  toJSON (Rectangle box) = toJSON box

instance FromJSON Rectangle where
  parseJSON v = Rectangle <$> parseJSON v

instance ShapeClass Rectangle where
  shapeBounds = view rectangleShape
  shapeTouched (Rectangle box) _ pt = pt `inBox` box
  shapeInBox (Rectangle box) = (`containsBox` box)
  shapeDrag d = rectangleShape %~ (`moveBox` d)
  shapeConnectionPoint box p0 =
      fromMaybe p0 $ intersectWithRectangle box $ lineFromPoints p0 p1
        -- p1 is inside the rectangle, so p0 should never be returned.
    where p1 = ellipseNormalPoint (shapeBounds box) p0  -- On ellipse inside rectangle.

instance BoxShapeClass Rectangle where
  shapeInnerBox = view $ rectangleShape . to (shrinkBox 3)
  shapeResize _ (Rectangle NoBox) = Rectangle NoBox
  shapeResize d (Rectangle oldShape) = Rectangle $ boxResize d oldShape
  shapeHandles = boxHandles rectangleShape
  shapeDefinition = rectangleShape


-- | A rectangle is defined by its bounding box.
rectangleShape :: Iso' Rectangle BoundBox
rectangleShape = iso _rectangleShape Rectangle


-- | Given a rectangle and a line, compute the first intersection.
intersectWithRectangle :: Rectangle -> Line -> Maybe Point
intersectWithRectangle rect line = linePoint line <$> intersectWith (rectangleEdges rect) line


-- | The four corners of a rectangle, clockwise from top left.
rectangleCorners :: Rectangle  -> [Point]
rectangleCorners (Rectangle NoBox) = []
rectangleCorners (Rectangle (BoundBox (Point x1 y1) (Point x2 y2))) =
  [Point x1 y1, Point x2 y1, Point x2 y2, Point x1 y2]


-- | The four edges of a rectangle as lines, clockwise from the top.
rectangleEdges :: Rectangle -> [Line]
rectangleEdges = polygonLines . rectangleCorners


newtype Ellipse = Ellipse {_ellipseShape :: BoundBox} deriving (Eq)

instance ToJSON Ellipse where toJSON (Ellipse box) = toJSON box

instance FromJSON Ellipse where parseJSON v = Ellipse <$> parseJSON v

instance ShapeClass Ellipse where
  shapeBounds = view ellipseShape
  shapeTouched shape _ = ellipseTouched $ shapeBounds shape
  shapeInBox (Ellipse e) = (`containsBox` e)
  shapeDrag d = ellipseShape %~ (`moveBox` d)
  shapeConnectionPoint = ellipseNormalPoint . shapeBounds

instance BoxShapeClass Ellipse where
  shapeInnerBox = boxInsideEllipse . shapeBounds
  shapeResize d (Ellipse oldShape) = Ellipse $ boxResize d oldShape
  shapeHandles = boxHandles ellipseShape
  shapeDefinition = ellipseShape


ellipseShape :: Iso' Ellipse BoundBox
ellipseShape = iso _ellipseShape Ellipse


-- | Test if an ellipse is touched by the point.
ellipseTouched :: BoundBox -> Point -> Bool
ellipseTouched NoBox _ = False
ellipseTouched box pt@(Point x y) =
    pt `inBox` box && sq ((x - cX) * rY) + sq ((y - cY) * rX) <= sq rX * sq rY
  where
    rX = boxWidth box / 2
    rY = boxHeight box / 2
    Point cX cY = box ^. boxCentre  -- Safe because box is not NoBox

-- | A diamond looks like this: ◇
newtype Diamond = Diamond {_diamondShape :: BoundBox} deriving (Eq)

instance ToJSON Diamond where
  toJSON (Diamond b) = toJSON b

instance FromJSON Diamond where
  parseJSON v = Diamond <$> parseJSON v

instance ShapeClass Diamond where
  shapeBounds = view diamondShape
  shapeTouched p _ = diamondTouched $ shapeBounds p
  shapeInBox (Diamond d) = (`containsBox` d)
  shapeDrag d = diamondShape %~ (`moveBox` d)
  shapeConnectionPoint = diamondConnectionPoint . shapeBounds

instance BoxShapeClass Diamond where
  shapeInnerBox = diamondInnerBox . shapeBounds
  shapeResize _ (Diamond NoBox) = Diamond NoBox
  shapeResize (w, h) (Diamond oldShape) =
      Diamond $ BoundBox (c `movePoint` (-w1, h1)) (c `movePoint` (w1,h1))
    where
      c = oldShape ^. boxCentre
      w1 = max 1 w / 2
      h1 = max 1 h / 2
  shapeHandles = boxHandles diamondShape
  shapeDefinition = diamondShape

diamondShape :: Iso' Diamond BoundBox
diamondShape = iso _diamondShape Diamond


-- | Test if a diamond shape with corners at the midpoints of the bounding box is touched by
-- the point.
diamondTouched :: BoundBox -> Point -> Bool
diamondTouched box pt =
  pt `inBox` box && insidePolygon (diamondCorners box) pt


-- | The corner points of a diamond, starting from the left corner going clockwise.
diamondCorners :: BoundBox -> [Point]
diamondCorners NoBox = []
diamondCorners (BoundBox (Point x1 y1) (Point x2 y2)) =
    [Point x1 midY, Point midX y1, Point x2 midY, Point midX y2]
  where
    midX = (x1+x2)/2
    midY = (y1+y2)/2


-- | A BoundBox which is contained entirely within the diamond inscribed in the argument.
diamondInnerBox :: BoundBox -> BoundBox
diamondInnerBox NoBox = NoBox
diamondInnerBox box = mkBoundBox (linePoint side1 0.5) (linePoint side3 0.5)
  where side1 : _ : side3 : _ = polygonLines $ diamondCorners box
  -- Hopefully an inner box half the height and width of the outer one is the maximum area.
  -- Pattern match cannot fail because we have a BoundBox, so polygonLines must return 4 items.


-- | The connection point to a diamond.
diamondConnectionPoint :: BoundBox -> Point -> Point
diamondConnectionPoint box = polygonConnectionPoint (diamondCorners box) (diamondInnerBox box)





-- | A parallelogram has the top and bottom sides horizontal and the left and right sides slanted.
-- It is defined by its *inner* box (i.e. the rectangular area between the two
-- horizontal edges) and the slant ratio, where zero is vertical and a positive slant leans to
-- the right.
newtype Parallelogram = Parallelogram {_parallelogramShape :: BoundBox} deriving (Eq)

instance ToJSON Parallelogram where
  toJSON (Parallelogram b) = toJSON b

instance FromJSON Parallelogram where
  parseJSON v = Parallelogram <$> parseJSON v

instance ShapeClass Parallelogram where
  shapeBounds = parallelogramOuterBox
  shapeTouched p _ = parallelogramTouched p
  shapeInBox p = (`containsBox` shapeBounds p)
  shapeDrag d = parallelogramShape %~ (`moveBox` d)
  shapeConnectionPoint = parallelogramConnectionPoint

instance BoxShapeClass Parallelogram where
  shapeInnerBox = view parallelogramShape
  shapeResize (w,h) (Parallelogram oldShape) =
      Parallelogram $ boxResize (w - 2*widening, h) oldShape
    where
      widening = h * parallelogramSlant
  shapeHandles = boxHandles parallelogramShape
  shapeDefinition = parallelogramShape


parallelogramShape :: Iso' Parallelogram BoundBox
parallelogramShape = iso _parallelogramShape Parallelogram


parallelogramTouched :: Parallelogram -> Point -> Bool
parallelogramTouched = insidePolygon . parallelogramCorners


-- | Slant ratio of parallelograms.
parallelogramSlant :: Double
parallelogramSlant = 1/3


-- | The four corners of a parallelogram.
parallelogramCorners :: Parallelogram -> [Point]
parallelogramCorners (Parallelogram NoBox) = []
parallelogramCorners p@(Parallelogram (BoundBox (Point x1 y1) (Point x2 y2)))
  | parallelogramSlant < 0  = [Point x1a y1, Point x1 y2, Point x2a y2, Point x2 y1]
  | parallelogramSlant > 0  = [Point x1 y1, Point x1a y2, Point x2 y2, Point x2a y1]
  | otherwise  = [Point x1 y1, Point x1 y2, Point x2 y2, Point x2 y1]
  where
    BoundBox (Point x1a _) (Point x2a _) = parallelogramOuterBox p


-- | The outer box of a parallelogram, defined as the inner box plus the two triangular wings.
-- This is the actual boundbox of the entire shape.
parallelogramOuterBox :: Parallelogram -> BoundBox
parallelogramOuterBox (Parallelogram NoBox) = NoBox
parallelogramOuterBox (Parallelogram box@(BoundBox (Point x1 y1) (Point x2 y2))) =
    BoundBox (Point (x1-widening) y1) (Point (x2+widening) y2)
  where
    widening = boxHeight box * parallelogramSlant


parallelogramConnectionPoint :: Parallelogram -> Point -> Point
parallelogramConnectionPoint p =
  polygonConnectionPoint (parallelogramCorners p) (p ^. parallelogramShape)


-- | An oval box consists of a rectangle with rounded ends. The width of the ends is specified
-- by the aspect ratio. Zero gives a normal rectangle while one gives semicircular ends.
newtype Oval = Oval {_ovalShape :: BoundBox} deriving (Eq)

instance ToJSON Oval where
  toJSON (Oval b) = toJSON b

instance FromJSON Oval where
  parseJSON v = Oval <$> parseJSON v

instance ShapeClass Oval where
  shapeBounds = ovalBoxOuter
  shapeTouched oval _ = ovalBoxTouched oval
  shapeInBox oval = (`containsBox` ovalBoxOuter oval)
  shapeDrag d = ovalShape %~ (`moveBox` d)
  shapeConnectionPoint = ovalBoxConnectionPoint

instance BoxShapeClass Oval where
  shapeInnerBox = view ovalShape
  shapeResize (w,h) (Oval oldShape) = Oval $ boxResize (w-widening, h) oldShape
    where
      widening = h * ovalAspect
  shapeHandles = boxHandles ovalShape
  shapeDefinition = ovalShape

ovalShape :: Iso' Oval BoundBox
ovalShape = iso _ovalShape Oval


-- | Proportion of height by which the inner rectangle is widened by the elliptical sides.
ovalAspect :: Double
ovalAspect = 1/3


-- | Private function that returns the extra overlapping shapes that comprise an oval box;
-- the bound box for the left hand ellipse, and the bound box for the right
-- hand ellipse.
ovalBoxSides :: Oval -> (BoundBox, BoundBox)
ovalBoxSides (Oval NoBox) = (NoBox, NoBox)
ovalBoxSides (Oval box@(BoundBox p1 p2)) = (leftBox, rightBox)
  where
    extra = boxHeight box * ovalAspect / 2
    leftBox = BoundBox (p1 `movePoint` (-extra, 0)) (p1 `movePoint` (extra, boxHeight box))
    rightBox = BoundBox (p2 `movePoint` (-extra, -(boxHeight box))) (p2 `movePoint` (extra, 0))



ovalBoxTouched :: Oval -> Point -> Bool
ovalBoxTouched oval pt =
    pt `inBox` (oval ^. ovalShape) ||
      ellipseTouched leftBox pt ||
      ellipseTouched rightBox pt
  where
    (leftBox, rightBox) = ovalBoxSides oval


-- | The outer box of an oval, defined as the inner box plus the two half-ellipses on each end.
ovalBoxOuter :: Oval -> BoundBox
ovalBoxOuter oval = leftBox <> rightBox
  where
    (leftBox, rightBox) = ovalBoxSides oval


ovalBoxConnectionPoint :: Oval -> Point -> Point
ovalBoxConnectionPoint oval p0 =
    case allIntersects of
      [] -> p0
      _ -> linePoint line $ minimum allIntersects
  where
    outer = ovalBoxOuter oval
    (leftSide, rightSide) = ovalBoxSides oval
    p1 = ellipseNormalPoint outer p0
    line = lineFromPoints p0 p1
    leftIntersect = ellipseIntersection1 leftSide line
    boxIntersect = intersectWith (rectangleEdges $ Rectangle $ oval ^. ovalShape) line
    rightIntersect = ellipseIntersection1 rightSide line
    allIntersects = catMaybes [leftIntersect, boxIntersect, rightIntersect]


-- | A comment box is a rectangular box with the top left hand corner turned over. It is defined
-- by the inner box, which is the area suitable for text.
newtype CommentBox = CommentBox {_commentBoxShape :: BoundBox} deriving Eq

instance ToJSON CommentBox where toJSON (CommentBox b) = toJSON b

instance FromJSON CommentBox where parseJSON v = CommentBox <$> parseJSON v

instance ShapeClass CommentBox where
  shapeBounds = commentBoxOuter
  shapeTouched c _ = insidePolygon $ commentBoxCorners c
  shapeInBox c = (`containsBox` shapeBounds c)
  shapeDrag d = commentBoxShape %~ (`moveBox` d)
  shapeConnectionPoint = commentBoxConnectionPoint

instance BoxShapeClass CommentBox where
  shapeInnerBox = view commentBoxShape
  shapeResize (w, h) (CommentBox oldShape) =
    CommentBox $ boxResize (w - 2*commentBoxFoldSize, h - 2*commentBoxFoldSize) oldShape
  shapeHandles = boxHandles commentBoxShape
  shapeDefinition = commentBoxShape

commentBoxShape :: Iso' CommentBox BoundBox
commentBoxShape = iso _commentBoxShape CommentBox


-- | The first two corners are the diagonal fold line.
commentBoxCorners :: CommentBox -> [Point]
commentBoxCorners (CommentBox NoBox) = []
commentBoxCorners box = [
    Point (p1 ^. pX) (p1 ^. pY + commentBoxFoldSize),
    Point (p1 ^. pX + commentBoxFoldSize) (p1 ^. pY),
    Point (p2 ^. pX) (p1 ^.pY),
    p2,
    Point (p1 ^. pX) (p2 ^. pY)
  ]
  where
    (BoundBox p1 p2) = commentBoxOuter box  -- Safe because "box" is not "NoBox"


-- | A comment box is defined by its inner box. This gives the bounding box for the entire comment.
commentBoxOuter :: CommentBox -> BoundBox
commentBoxOuter (CommentBox NoBox) = NoBox
commentBoxOuter (CommentBox s) = growBox commentBoxFoldSize s


-- | Comment boxes are generally free standing and don't have lines connecting to them. But
-- the "Viewable" instance will require one anyway, so here it is.
commentBoxConnectionPoint :: CommentBox -> Point -> Point
commentBoxConnectionPoint box = polygonConnectionPoint (commentBoxCorners box) $ shapeBounds box


-- | Size of the fold in the top left corner of a comment box.
commentBoxFoldSize :: Double
commentBoxFoldSize = 10


-- | Connection point for a polygon, given a suitable BoundBox within the polygon
polygonConnectionPoint :: [Point] -> BoundBox -> Point -> Point
polygonConnectionPoint pts ellipse p0 =
    fromMaybe p0 $ intersectPoint edges p0 p1
  where
    p1 = ellipseNormalPoint ellipse p0
    edges = polygonLines pts


-- | Square the argument.
sq :: (Num a) => a -> a
sq x = x * x
