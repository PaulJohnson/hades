{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

-- |
module Hades.Abstract.Geometry (
   -- * Basic Geometry
   Point (..),
   pX,
   pY,
   movePoint,
   pointDiff,
   distance,
   Scale,
   -- * Bounding Boxes
   BoundBox (..),
   mkBoundBox,
   mkConstrainedBox,
   boxCentre,
   boxWidth,
   boxHeight,
   boxResize,
   growBox,
   shrinkBox,
   moveBox,
   inBox,
   overlapBox,
   containsBox,
   fitIntoBox,
   -- * Lines
   Line (..),
   linePoint,
   lineFromPoints,
   lineIntersection,
   lineNormal,
   lineDistance,
   polygonLines,
   polygonBox,
   insidePolygon,
   -- * Intersects
   Intersect (..),
   intersectPoint,
   -- * Ellipses
   ellipseIntersection,
   ellipseIntersection1,
   ellipseNormalPoint,
   boxInsideEllipse,
   -- * Constrained Points
   -- $quadrants
   Constraint,
   constrainLine,
   constrainLineLeft,
   constrainBox,
   constrainQ1,
   constrainQ2,
   constrainQ3,
   constrainQ4
) where

import Control.Lens hiding ((.=))
import Control.Monad
import Data.Aeson
import Data.Function
import Data.Maybe


-- | Position on a diagram
data Point = Point {_pX, _pY :: Double} deriving (Eq, Show, Read)

instance ToJSON Point where
   toJSON (Point x y) = object ["x" .= x, "y" .= y]

instance FromJSON Point where
   parseJSON = withObject "Point" $ \v -> Point <$> v .: "x" <*> v .: "y"

pX, pY :: Lens' Point Double
pX = lens _pX $ \p v -> p{_pX = v}
pY = lens _pY $ \p v -> p{_pY = v}


-- | Move a point by (x,y) relative.
movePoint :: Point -> (Double, Double) -> Point
movePoint (Point x y) (dx, dy) = Point (x + dx) (y + dy)

-- | Vector from p1 to p2.
pointDiff :: Point -> Point -> (Double, Double)
pointDiff (Point x1 y1) (Point x2 y2) = (x2-x1, y2-y1)

-- | Distance represented by an (x,y) relative pair.
distance :: (Double, Double) -> Double
distance (dx, dy) = sqrt (dx*dx + dy*dy)


-- | Diagram scaling factor.
type Scale = Double


-- | Bounding box in diagram space. Monoid instance is union.
data BoundBox =
   BoundBox Point Point
      -- ^ First point <= second point in both pX and pY.
   | NoBox
   deriving (Eq, Show, Read)

instance Semigroup BoundBox where
   NoBox <> b = b
   b <> NoBox = b
   (BoundBox p1a p2a) <> (BoundBox p1b p2b) =
      BoundBox
         (Point ((min `on` _pX) p1a p1b) ((min `on` _pY) p1a p1b))
         (Point ((max `on` _pX) p2a p2b) ((max `on` _pY) p2a p2b))

instance Monoid BoundBox where
   mempty = NoBox

instance ToJSON BoundBox where
   toJSON NoBox = String "NoBox"
   toJSON (BoundBox p1 p2) = object ["p1" .= p1, "p2" .= p2]

instance FromJSON BoundBox where
   parseJSON (String "NoBox") = return NoBox
   parseJSON v = withObject "BoundBox" (\v1 -> BoundBox <$> v1 .: "p1" <*> v1 .: "p2") v


-- | Safely make a bounding box in which @p1 <= p2@ in both @x@ and @y@.
mkBoundBox :: Point -> Point -> BoundBox
mkBoundBox p1 p2 = BoundBox (Point x1 y1) (Point x2 y2)
   where
      (x1,x2) = sort2 (_pX p1, _pX p2)
      (y1,y2) = sort2 (_pY p1, _pY p2)


-- | Make a boundbox if @p1 < p2@ for both @x@ and @y@, or else return @NoBox@.
mkConstrainedBox :: Point -> Point -> BoundBox
mkConstrainedBox p1 p2 =
   if p1 ^. pX < p2 ^. pX && p1 ^. pY < p2 ^. pY then BoundBox p1 p2 else NoBox

-- | Lens for the center of a bounding box. This is a partial function: if the box is "NoBox" then
-- it will fail.
boxCentre :: Lens' BoundBox Point
boxCentre = lens getCenter setCenter
   where
      getCenter (BoundBox p1 p2) = Point ((av `on` _pX) p1 p2) ((av `on` _pY) p1 p2)
      getCenter NoBox = error "boxCenter does not exist for NoBox."
      setCenter box@(BoundBox p1 p2) p =
         let
            (Point x y) = getCenter box
            v = (_pX p - x, _pY p - y)
         in BoundBox (movePoint p1 v) (movePoint p2 v)
      setCenter NoBox _ = error "boxCenter cannot be set for NoBox."
      av v1 v2 = (v1 + v2) / 2.0


-- | Get the width of the box.
--
-- This is not a lens because a negative width box is not possible.
boxWidth :: BoundBox -> Double
boxWidth NoBox = 0
boxWidth (BoundBox (Point px1 _) (Point px2 _)) = px2 - px1


-- | Get the height of the box.
boxHeight :: BoundBox -> Double
boxHeight NoBox = 0
boxHeight (BoundBox (Point _ py1) (Point _ py2)) = py2 - py1


-- | Change the width and height without moving the centre. May result in a zero size box.
-- No-operation on a "NoBox".
boxResize :: (Double, Double) -> BoundBox -> BoundBox
boxResize _ NoBox = NoBox
boxResize (w, h) box = BoundBox (c `movePoint` (-w1, -h1)) (c `movePoint` (w1,h1))
   where
      c = box ^. boxCentre
      w1 = max 0 w / 2
      h1 = max 0 h / 2



-- | Move a box by (x,y) relative.
moveBox :: BoundBox -> (Double, Double) -> BoundBox
moveBox NoBox _ = NoBox
moveBox (BoundBox p1 p2) v = BoundBox (movePoint p1 v) (movePoint p2 v)


-- | Add an extra border to a BoundBox to allow for additional decoration around the perimeter,
-- such as thicker lines or shadows.
growBox :: Double -> BoundBox -> BoundBox
growBox _ NoBox = NoBox
growBox k (BoundBox p1 p2) = BoundBox (movePoint p1 (-k',-k')) (movePoint p2 (k',k'))
   where k' = abs k


-- | Add an extra border inside a BoundBox. Note that the result may be NoBox if the new width
-- or height would be less than zero.
shrinkBox :: Double -> BoundBox -> BoundBox
shrinkBox _ NoBox = NoBox
shrinkBox k (BoundBox (Point x1 y1) (Point x2 y2)) =
      if nx1 <= nx2 && ny1 <= ny2
         then BoundBox (Point nx1 ny1) (Point nx2 ny2)
         else NoBox
   where
      nx1 = x1 + k
      ny1 = y1 + k
      nx2 = x2 - k
      ny2 = y2 - k


-- | Point in bounding box?
inBox :: Point -> BoundBox -> Bool
inBox _ NoBox = False
inBox p (BoundBox p1 p2) =
   _pX p1 <= _pX p && _pX p <= _pX p2 &&
   _pY p1 <= _pY p && _pY p <= _pY p2


-- | Boxes overlap?
overlapBox :: BoundBox -> BoundBox -> Bool
overlapBox NoBox _ = False
overlapBox _ NoBox = False
overlapBox (BoundBox p1a p1b) (BoundBox p2a p2b) =
   overlap (_pX p1a) (_pX p1b) (_pX p2a) (_pX p2b) &&
   overlap (_pY p1a) (_pY p1b) (_pY p2a) (_pY p2b)
   where
      overlap v1a v1b v2a v2b =
         v1b >= v2a && v2b >= v1a

-- | Box1 entirely contains Box2
containsBox :: BoundBox -> BoundBox -> Bool
containsBox NoBox _ = False
containsBox _ NoBox = False
containsBox box1 (BoundBox p2a p2b) = p2a `inBox` box1 && p2b `inBox` box1


-- | The scaling factor required to fit the first box into the width and height of the second.
-- Result will always be less than or equal to 1. The absolute positions are ignored.
fitIntoBox :: BoundBox -> BoundBox -> Double
fitIntoBox box1 box2 =
   let
      wScale = min 1 $ boxWidth box2 / boxWidth box1
      hScale = min 1 $ boxHeight box2 / boxHeight box1
   in min wScale hScale


-- | A line is represented as a parametric equation with a start point and a vector.
data Line = Line Point Double Double


-- | A point on the line.
linePoint :: Line -> Double -> Point
linePoint (Line pt vx vy) d = movePoint pt (vx*d, vy*d)


-- | Line between pt1 and pt2.
--
-- If @l = lineFromPoints p1 p2@ then:
--
-- > linePoint l 0 == p1
-- > linePoint l 1 == p2
lineFromPoints :: Point -> Point -> Line
lineFromPoints p1 p2 = Line p1 vx vy
   where
      (vx,vy) = pointDiff p1 p2


-- | The intersection of two lines as the parametric argument for each line.
-- Returns @Nothing@ if the lines are parallel or the same line.
lineIntersection :: Line -> Line -> Maybe (Double, Double)
lineIntersection (Line p1 vx1 vy1) (Line p2 vx2 vy2) =
   if d == 0.0
      then Nothing
      else Just ((vx2 * sy - sx * vy2) / d, (vx1 * sy - sx * vy1) / d)
   where
      d = vx2 * vy1 - vx1 * vy2
      (sx,sy) = pointDiff p1 p2


-- | The intersection point of two lines.
lineIntersectionPoint :: Line -> Line -> Maybe Point
lineIntersectionPoint line1 line2 =
   linePoint line1 . fst <$> lineIntersection line1 line2


-- | A line 90 degrees anti-clockwise to the argument starting at the given point.
lineNormal :: Line -> Point -> Line
lineNormal (Line _ dx dy) pt = Line pt (-dy) dx

-- | The distance of a point from a line, provided the closest point is within a parametric
-- argument between 0 and 1.
lineDistance :: Line -> Point -> Maybe Double
lineDistance line point =
      case lineIntersection line (lineNormal line point) of
         Nothing -> Nothing   -- Line must be zero length.
         Just (t1, _) ->
            if t1 >= 0.0 && t1 <= 1.0
               then Just $ distance $ point `pointDiff` linePoint line t1
               else Nothing


-- | Is the point inside the polygon defined by the list of vertices? Uses the
-- \"winding number inclusion\" algorithm of http://geomalgorithms.com/a03-_inclusion.html
insidePolygon :: [Point] -> Point -> Bool
insidePolygon [] _ = False
insidePolygon [_] _ = False
insidePolygon [_, _] _ = False
insidePolygon vertices testPoint@(Point _ ty) =
      sum (map winding edges) /= 0
   where   -- List functions can assume vertices contains 3 or more points.
      edges = (last vertices, head vertices) : zip vertices (tail vertices)
      winding l@(Point _ y0, Point _ y1)
         | y0 <= ty  = if y1 > ty && isLeft l testPoint > 0 then 1 else 0 :: Int
         | y1 <= ty && isLeft l testPoint < 0  = -1
         | otherwise = 0
      isLeft (Point x0 y0, Point x1 y1) (Point x2 y2) =
         (x1 - x0) * (y2 - y0) - (x2 -  x0) * (y1 - y0)


-- | The lines that make up the polygon defined by the list of vertices.
polygonLines :: [Point] -> [Line]
polygonLines [] = []
polygonLines [_] = []
polygonLines pts = zipWith lineFromPoints pts1 $ tail pts1
   where pts1 = last pts : pts


-- The boundbox of a list of vertices.
polygonBox :: [Point] -> BoundBox
polygonBox pts = BoundBox (Point (minimum xs) (minimum ys)) (Point (maximum xs) (maximum ys))
   where
      xs = map (^. pX) pts
      ys = map (^. pY) pts

-- ----------------------------------
-- Intersects
-- ----------------------------------

-- | Intersects are geometric elements that can compute the intersection of a line with the
-- element. The list instance finds the intersection with the lowest parameter.
class Intersect a where
   intersectWith :: a -> Line -> Maybe Double
      -- ^ Return the parameter for the argument line such that "linePoint" gives the point
      -- of intersection. If there are multiple intersects then return the lowest parameter.
      -- If there are no intersects then return "Nothing".


instance Intersect Line where
   intersectWith line1 line2 = do -- Maybe monad
      (i1,i2) <- lineIntersection line1 line2
      guard $ 0.0 <= i1 && i1 <= 1.0
      return i2


instance (Intersect a) => Intersect [a] where
   intersectWith xs line =
      case mapMaybe (`intersectWith` line) xs of
         [] -> Nothing
         vs -> Just $ minimum vs



-- | Given an Intersect and two points, this returns the point of intersection on the line
-- between them.
intersectPoint :: (Intersect a) => a -> Point -> Point -> Maybe Point
intersectPoint shape p1 p2 = linePoint line <$> intersectWith shape line
   where
      line = lineFromPoints p1 p2


-- --------------------------------
-- Constraints
-- --------------------------------


-- | A constraint forces its argument to stay within a certain area or on a line. A Horizontal
-- constraint is @set pY y@. Unfortunately constraints do not compose well.
type Constraint = Point -> Point


-- | Constrain all values to lie on the line with a positive parameter.
constrainLine :: Line -> Constraint
constrainLine line1@(Line _ vx vy) pt = linePoint line1 (max s 0)
   where
      Just (s, _) = lineIntersection line1 line2
      line2 = Line pt (-vy) vx  -- Normal to line1 through pt.


-- | Constrain all values to lie on the left of a line.
constrainLineLeft :: Line -> Constraint
constrainLineLeft line1@(Line _ vx vy) pt = if s2 >= 0 then pt else linePoint line1 s1
   where
      Just (s1, s2) = lineIntersection line1 line2
      line2 = Line pt (-vy) vx  -- Normal to line1 through pt.



-- | Constrain values to lie within a "BoundBox".
constrainBox :: BoundBox -> Constraint
constrainBox NoBox pt = pt
constrainBox (BoundBox (Point x1 y1) (Point x2 y2)) (Point x y) =
      Point (limit x1 x2 x) (limit y1 y2 y)
   where
      limit v1 v2 v = max v1 (min v2 v)


{- $quadrants

The diagram space and quadrants are defined as follows:

> (0,0)               x ->
>    +----------------------------------
>    |
>    |
>  y |
>    |
>  | |
>  V |                    |
>    |               Q2   |   Q1
>    |                    |
>    |                    |(x,y)
>    |               -----+-----
>    |                    |
>    |                    |
>    |               Q3   |   Q4
>    |                    |
>    |
>    |

-}




-- | Constrain to quadrant above right of argument.
constrainQ1 :: Point -> Constraint
constrainQ1 (Point x0 y0) (Point x y) = Point (max x x0) (min y y0)

-- | Constrain to quadrant above left of argument.
constrainQ2 :: Point -> Constraint
constrainQ2 (Point x0 y0) (Point x y) = Point (min x x0) (min y y0)

-- | Constrain to quadrant below left of argument.
constrainQ3 :: Point -> Constraint
constrainQ3 (Point x0 y0) (Point x y) = Point (min x x0) (max y y0)

-- | Constrain to quadrant below right of argument.
constrainQ4 :: Point -> Constraint
constrainQ4 (Point x0 y0) (Point x y) = Point (max x x0) (max y y0)


--------------------------------------------------------------------
-- Connector intersections.
--------------------------------------------------------------------

-- | Utility function for sorting a pair.
sort2 :: (Ord a) => (a,a) -> (a,a)
sort2 (v1, v2) = if v1 <= v2 then (v1, v2) else (v2, v1)


-- | Points of intersection between an ellipse (defined by the BoundBox) and a Line.
-- Returns @Nothing@ if the line does not intersect the ellipse, otherwise it returns
-- the two line parameters of the intersection points.
ellipseIntersection :: BoundBox -> Line -> Maybe (Double, Double)
ellipseIntersection NoBox _ = Nothing
ellipseIntersection box@(BoundBox p1 p2) (Line pt vx vy) =
   if det < 0
      then Nothing
      else Just (((-bigB) + sdet) / (2 * bigA), ((-bigB) - sdet) / (2 * bigA))
   where
      -- Analytical solution from
      -- http://csharphelper.com/blog/2012/09/calculate-where-a-line-segment-and-an-ellipse-intersect-in-c/
      a = (p2 ^. pX - p1 ^. pX)/2
      b = (p2 ^. pY - p1 ^. pY)/2
      (x1, y1) = pointDiff (box ^. boxCentre) pt   -- Point relative to centre of ellipse
         -- Partial function boxCentre is safe because box is not NoBox.
      bigA = vx * vx / (a * a) + vy * vy / (b * b)
      bigB = 2 * x1 * vx / (a * a) + 2 * y1 * vy / (b * b)
      bigC = x1 * x1 / (a * a) + y1 * y1 / (b * b) - 1
      det = bigB * bigB - 4 * bigA * bigC
      sdet = sqrt det


-- | As for "ellipseIntersection", but returns the smaller of the two line parameters that is
-- positive.
ellipseIntersection1 :: BoundBox -> Line -> Maybe Double
ellipseIntersection1 box line = do
      (i1, i2) <- sort2 <$> ellipseIntersection box line
      guard $ i2 >= 0.0
      if i1 >= 0.0 then return i1 else return i2



-- | The analytic solution to a line normal to an ellipse through a point is a quartic
-- equation which may have up to 3 roots on the other side of the ellipse as well as the obvious
-- one on the near side. This is a simpler approximation for the nearest point that gives
-- results good enough for graphical work.
--
-- If the point is inside the ellipse then this just uses a simple radial from the centre.
ellipseNormalPoint :: BoundBox -> Point -> Point
ellipseNormalPoint NoBox pt = pt
ellipseNormalPoint box@(BoundBox p1 p2) pt =
   maybe center (linePoint line) (ellipseIntersection1 box line)
         -- If the ellipseIntersection1 fails then either we have a zero-size ellipse or
         -- something has gone wrong. Either way the centre is a good fall-back.
   where
      inside1 = sq (pt^.pX - center^.pX) / sq a + sq (pt^.pY - center^.pY) / sq b <= 1.0
      center = box ^. boxCentre  -- Safe because box is not NoBox.
      a = (p2 ^. pX - p1 ^. pX)/2
      b = (p2 ^. pY - p1 ^. pY)/2
      c = sqrt $ abs $ sq a - sq b
      (f1, f2) = if a < b  -- Foci of ellipse
         then ((pY -~ c) center, (pY +~ c) center)  -- Vertical ellipse
         else ((pX -~ c) center, (pX +~ c) center)  -- Horizontal ellipse
      pf1 = lineFromPoints pt f1
      pf2 = lineFromPoints pt f2
      normal = do  -- Maybe monad. Returns Nothing if pt is co-linear with f1 and f2.
         guard $ not inside1
         ptA <- linePoint pf1 <$> ellipseIntersection1 box pf1
         ptB <- linePoint pf2 <$> ellipseIntersection1 box pf2
         let
            af2 = lineFromPoints ptA f2
            bf1 = lineFromPoints ptB f1
         ptC <- lineIntersectionPoint af2 bf1
         return $ lineFromPoints pt ptC
      line = fromJust $ normal `mplus` Just (lineFromPoints pt center)
             -- If "normal" fails then a line to the centre is the Right Thing.
{-
The algorithm for ellipseNormalPoint is based on
http://www.mathpages.com/home/kmath505/kmath505.htm

Quote:

Incidentally, there's a simple way of determining a very close estimate of the location of
the "near side" normal point. For any external point P, let the lines from the two foci
F1 - P and F2 - P strike the ellipse at the points A and B respectively, and then let
C denote the intersection of the lines F1 - B and F2 - A. The line C - P is very nearly
normal to the ellipse. This construction can be performed with just quadratics, and gives a
line so close to the normal line that it could easily be mistaken for it.
-}



-- | If an ellipse is inscribed inside the argument box then this returns the largest area box that
-- will fit inside the ellipse.
boxInsideEllipse :: BoundBox -> BoundBox
boxInsideEllipse NoBox = NoBox
boxInsideEllipse (BoundBox pt1 pt2) = BoundBox pt1' pt2'
   where
      diagonal = lineFromPoints pt1 pt2
      pt1' = linePoint diagonal $ 0.5 - 0.5 / sqrt 2
      pt2' = linePoint diagonal $ 0.5 + 0.5 / sqrt 2

-- Proof: for a circle the largest rectangle that fits inside is a square, which is a smaller
-- version of the square that bounds the circle. If the outer square has unit diagonal then the
-- circle has radius @0.5/sqrt 2@. Stretching preserves the ratios between areas, so the rectangle
-- inside an ellipse can be constructed in the same way.

-- | Square the argument.
sq :: (Num a) => a -> a
sq x = x * x
