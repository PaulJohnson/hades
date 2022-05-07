{-# LANGUAGE OverloadedLabels #-}
{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

-- |
--
-- Cairo renderings of the basic shapes supported by Hades.
module Hades.GI.BasicShapes (
   -- * Diagram style
   RGB,
   HadesStyle (..),
   HadesRender,
   renderWithHades,
   withLineColour,
   hadesFontSize,
   hadesConfig,
   stroke,
   strokePreserve,
   fillBox,
   basicLine,
   basicBox,
   -- * Handles for editing diagrams
   squareHandle,
   -- * Box-like shapes
   drawPolygon,
   drawEllipse,
   drawOval,
   drawCommentBox,
   -- * Line shapes
   ArrowHead (..),
   noArrowHead,
   openArrowHead,
   closedArrowHead,
   circleArrowHead,
   multiArrowHead,
   drawPolyLine,
   ArrowDecoration (..),
   drawArrowDecoration,
   -- * Text layout
   HadesLayout,
   runLayout,
   runLayout_,
   pangoCreateDiagramLayout,
   layoutRegion,
   layoutRegion_,
   layoutBox,
   layoutLine,
   layoutConsume,
   layoutParallel,
   layoutPixbuf,
   layoutSvg,
   layoutHLine,
   layoutDrawPango,
   layoutParagraphGap,
   layoutTitle,
   layoutText,
   layoutParagraphs,
   textBlock,
   drawCornerLetter
) where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import qualified Data.Colour.Names as C
import qualified Data.Colour.SRGB as C
import qualified Data.GI.Base as GI
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Cairo as Cairo (Context)
import qualified GI.Cairo.Render as Cairo
import qualified GI.Cairo.Render.Connector as Cairo
import GI.Pango (AttrOp ((:=)))
import qualified GI.Gdk.Functions as Gdk
import qualified GI.GdkPixbuf as Gdk
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango
import qualified GI.PangoCairo as Pango
import qualified GI.Rsvg as SVG
import Hades.Abstract
import Reactive.Banana.Common


{- Design Note:

Rather than try to anticipate every possible future diagram type this module merely provides the
facilities required so far. As more diagrams are added it is likely that this module will increase
in capability.
-}

type RGB = (Double, Double, Double)


-- | Key style attributes for Hades diagrams. Shapes should use these unless they have a
-- specific reason to override them (such as conditional formatting). Each component consists
-- of @Render@ actions to set the Cairo context appropriately.
data HadesStyle = HadesStyle {
   boxBorderStyle :: HadesRender (),
   boxFillStyle :: HadesRender (),
   lineStyle :: HadesRender (),
   textFont :: Pango.FontDescription,
   textStyle :: HadesRender (),
   textGap :: Double,  -- ^ Inter-paragraph separation as fractions of a line.
   dataIcons :: Gtk.IconTheme
}


-- | Size of the font on Hades diagrams.
hadesFontSize :: Double
hadesFontSize = 9


-- | Assumed DPI for drawing Hades diagrams. By keeping this constant we can ensure that things
-- always get arranged the same way, and scaling is done later.
hadesDPI :: Double
hadesDPI = 90


-- | Convert HADES diagram units to Pango.
hadesToPango :: (Integral a) => Double -> a
hadesToPango x = round $ x * fromIntegral Pango.SCALE


-- | Convert Pango units to HADES.
pangoToHades :: (Integral a) => a -> Double
pangoToHades x = fromIntegral x / fromIntegral Pango.SCALE


-- | Placeholder with hard-coded style. Colours chosen by staring at a mock-up.
hadesConfig :: Gtk.IconTheme -> IO HadesStyle
hadesConfig thm =
   do
      font <- Pango.fontDescriptionNew
      Pango.fontDescriptionSetFamily font "Roboto,Symbola,STIX Two Text"
      Pango.fontDescriptionSetSize font $ round $ hadesFontSize * fromIntegral Pango.SCALE
      return $ HadesStyle {
         boxBorderStyle = border,
         boxFillStyle = fill,
         lineStyle = basicLine 2 black,
         textFont = font,
         textStyle = basicText black,
         textGap = 0.5,
         dataIcons = thm
      }
   where
      boxColor = Colour $ C.sRGB24 206 198 190
      black = Colour C.black
      (border, fill) = basicBox 2 black boxColor



-- | Monad for Cairo operations with a consistent drawing style.
type HadesRender = ReaderT HadesStyle Cairo.Render


-- | Local modification to the line drawing colour.
withLineColour :: Colour -> HadesRender a -> HadesRender a
withLineColour c act = do
      let C.RGB r g b = C.toSRGB $ getColour c
      local (addToLines $ lift $ Cairo.setSourceRGB r g b) act
   where
      addToLines c1 s = s {
            boxBorderStyle = boxBorderStyle s >> c1,
            lineStyle = lineStyle s >> c1
         }



-- | Bridge between gi-cairo and the hand-written cairo
-- package. It takes a `GI.Cairo.Context` (as it appears in gi-cairo),
-- and a `Render` action (as in the cairo lib), and renders the
-- `Render` action into the given context.
--
-- Copied from
-- https://github.com/haskell-gi/haskell-gi/wiki/Using-Cairo-with-haskell-gi-generated-bindings
renderWithHades :: Cairo.Context -> HadesStyle -> HadesRender () -> IO ()
renderWithHades ctx style r = Cairo.renderWithContext (runReaderT r style) ctx


-- | Stroke the current path using the Hades line style.
stroke :: HadesRender ()
stroke = do
   join $ asks lineStyle
   lift Cairo.stroke


-- | Stroke the current path using the Hades line style, and preserve it.
strokePreserve :: HadesRender ()
strokePreserve = do
   join $ asks lineStyle
   lift Cairo.strokePreserve


-- | Stroke and fill the current path using the Hades box styles.
fillBox :: HadesRender ()
fillBox = do
   join $ asks boxFillStyle
   lift Cairo.fillPreserve
   join $ asks boxBorderStyle
   lift Cairo.stroke


-- | Sets the Cairo context for drawing lines of a given thickness and colour.
basicLine ::
   Double   -- ^ Thickness
   -> Colour
   -> HadesRender ()
basicLine width c = lift $ do
   let (r,g,b) = colourToRGB c
   Cairo.setLineJoin Cairo.LineJoinRound
   Cairo.setDash [] 0
   Cairo.setLineWidth width
   Cairo.setLineCap Cairo.LineCapRound
   Cairo.setSourceRGB r g b


-- | Provides the Cairo contexts for boxes. The first is the edge style, the second the fill style.
basicBox ::
   Double    -- ^ Border thickness
   -> Colour    -- ^ Border colour
   -> Colour    -- ^ Fill colour
   -> (HadesRender (), HadesRender ())
basicBox width border fill = (edgeStyle, fillStyle)
   where
      edgeStyle = lift $ do
         let (r,g,b) = colourToRGB border
         Cairo.setLineWidth width
         Cairo.setLineJoin Cairo.LineJoinRound
         Cairo.setLineCap Cairo.LineCapRound
         Cairo.setDash [] 0
         Cairo.setSourceRGB r g b
      fillStyle = lift $ do
         let (r,g,b) = colourToRGB fill
         Cairo.setFillRule Cairo.FillRuleWinding  -- Matches Hades.Abstract.Geometry.insidePolygon
         Cairo.setSourceRGB r g b


-- | Provides the Cairo context for rendering text in the given colour.
basicText :: Colour -> HadesRender ()
basicText c =
   let (r,g,b) = colourToRGB c in lift $ Cairo.setSourceRGB r g b


colourToRGB :: Colour -> RGB
colourToRGB = (\(C.RGB r g b) -> (r,g,b)) . C.toSRGB . getColour


-- | A handle with a small black square as a glyph. The handle does not scale with the rest of the
-- diagram.
squareHandle :: (Viewable v, Paint v ~ HadesRender) => Handle v
squareHandle menu p0 evh = do
      zm <- use deltaZoom
      let
         handleSize = 10 -- pixels on a side.
         d2 = handleSize / zm
         d = d2 / 2
         b = BoundBox (Point (_pX p0 - d) (_pY p0 - d)) (Point (_pX p0 + d) (_pY p0 + d))
      return View {
            viewDraw = lift $ do
               Cairo.setSourceRGB 0 0 0
               Cairo.rectangle (_pX p0 - d) (_pY p0 - d) d2 d2
               Cairo.fill,
            viewTouched = (`inBox` b),
            viewBox = b,
            viewMenu = const menu,
            viewAction = evh
         }


-- | A closed polygon represented by its vertices.
drawPolygon :: [Point] -> HadesRender ()
drawPolygon [] = return ()
drawPolygon [_] = return ()
drawPolygon (pt : pts) = do
   lift $ do
      Cairo.moveTo (pt ^. pX) (pt ^. pY)
      forM_ pts $ \p -> Cairo.lineTo (p ^. pX) (p ^. pY)
      Cairo.closePath
   fillBox


-- | Draw an ellipse within the bounding box.
drawEllipse :: Ellipse -> HadesRender ()
drawEllipse (Ellipse NoBox) = return ()
drawEllipse (Ellipse (BoundBox p1 p2)) = do
   let
      (dx, dy) = pointDiff p1 p2
   lift $ do
      Cairo.save  -- This code adapted from the example given in the Haskell Cairo library.
      Cairo.translate (p1 ^. pX + dx / 2) (p1 ^. pY + dy / 2)
      Cairo.scale (dx / 2) (dy / 2)
      Cairo.moveTo 1 0
      Cairo.arc 0 0 1 0 (2 * pi)
      Cairo.restore
   fillBox


-- | Draw a box with rounded ends.
drawOval :: Oval -> HadesRender ()
drawOval (Oval NoBox) = return ()
drawOval (Oval (BoundBox p1 p2)) = do
      lift $ do
         Cairo.moveTo (p1 ^. pX) (p2 ^. pY)  -- Top left of inner box.
         Cairo.save
         Cairo.translate (p2 ^. pX) (p1 ^. pY + height / 2)
         Cairo.scale (ovalAspect * height / 2) (height / 2)
            -- Top border is implicit line to start of arc
         Cairo.arcNegative 0 0 1 (pi/2) (3*pi/2)
         Cairo.restore
         Cairo.save
         Cairo.translate (p1 ^. pX) (p1 ^. pY + height / 2)
         Cairo.scale (ovalAspect * height / 2) (height / 2)
            -- Bottom border is implicit line to start of arc
         Cairo.arcNegative 0 0 1 (3*pi/2) (pi/2)
         Cairo.restore
         Cairo.closePath
      fillBox
   where
      height = snd $ pointDiff p1 p2


-- | Draw a comment box. This is a rectangle with the top left hand corner folded over.
drawCommentBox :: CommentBox -> HadesRender ()
drawCommentBox box =
      case commentBoxCorners box of
         [] -> return ()
         [_] -> return ()
         points@(p1:p2:_) -> do  -- commentBoxCorners returns the fold ends as the first two points.
            let (border, fill) = basicBox 2 (Colour C.black) commentBlue
            local (\e -> e {boxBorderStyle = border, boxFillStyle = fill}) $ do
               drawPolygon points
               lift $ do
                  Cairo.moveTo (p1 ^. pX) (p1 ^. pY)
                  Cairo.lineTo (p2 ^. pX) (p1 ^. pY)
                  Cairo.lineTo (p2 ^. pX) (p2 ^. pY)
                  Cairo.stroke
   where
      commentBlue = Colour $ C.sRGB24 183 187 210


-- | Describes how to draw the end of an arrow.
data ArrowHead = ArrowHead {
      arrowHeadLength :: Double,  -- ^ Distance from origin that the incoming line should stop.
      arrowHeadWidth :: Double,  -- ^ Width of the arrow head.
      arrowHeadRender :: HadesRender ()
         -- ^ Draw an arrowhead pointing from right to left ending at @(0,0)@
   }

noArrowHead :: ArrowHead
noArrowHead = ArrowHead 0 0 $ return ()

-- | Triangular arrow head, either filled or not.
closedArrowHead ::
   Double  -- ^ Angle of arrow head in degrees. 90 is a fat arrow, 45 is a narrow arrow.
   -> Double -- ^ Length of the arrow head.
   -> Bool  -- ^ If true then fill the arrow black, otherwise fill white.
   -> ArrowHead
closedArrowHead angle len solid = ArrowHead len width $ do
      lift $ do
         Cairo.save
         Cairo.moveTo 0 0
         Cairo.lineTo len (width/2)
         Cairo.lineTo len ((-width)/2)
         Cairo.closePath
         Cairo.setLineJoin Cairo.LineJoinMiter  -- Arrows have sharp corners.
         if solid then Cairo.setSourceRGB 0 0 0 else Cairo.setSourceRGB 1 1 1
         Cairo.fillPreserve
      join $ asks lineStyle
      lift $ do
         Cairo.stroke  -- Stroke will expand arrow size by line thickness even when filled.
         Cairo.restore
   where
      width = 2 * len * tan (angle * pi/360)  -- Angle is the arrow head angle.


-- | Open arrow head without the line across the back, and with the incoming line going all
-- the way to the point.
openArrowHead :: Double -> Double -> ArrowHead
openArrowHead angle len = ArrowHead 0 width $ do
      lift $ do
         Cairo.save
         Cairo.moveTo len (width/2)
         Cairo.lineTo 0 0
         Cairo.lineTo len ((-width)/2)
         Cairo.setLineJoin Cairo.LineJoinMiter  -- Arrows have sharp corners.
      join $ asks lineStyle
      lift $ do
         Cairo.stroke  -- Stroke will expand arrow size by line thickness even when filled.
         Cairo.restore
   where
      width = 2 * len * tan (angle * pi/360)


-- | Draw a small circle as an arrow head.
circleArrowHead ::
   Double   -- ^ Diameter of circle.
   -> Bool  -- ^ Fill black if true, white if false.
   -> ArrowHead
circleArrowHead diam solid = ArrowHead diam diam $ do
      lift $ do
         Cairo.save
         Cairo.moveTo 0 0
         Cairo.arc r 0 r (-pi) pi
         Cairo.closePath
         if solid then Cairo.setSourceRGB 0 0 0 else Cairo.setSourceRGB 1 1 1
         Cairo.fillPreserve
      join $ asks lineStyle
      lift $ do
         Cairo.stroke  -- Stroke will expand arrow size by line thickness even when filled.
         Cairo.restore
   where
      r = diam / 2


-- | An arrow head composed of two or more arrow heads. Useful for @->>@ and @->o@ arrows.
--
-- The arrow heads are drawn with the first one on top, and the last one in the list placed at
-- the tip.
multiArrowHead ::
   Double  -- ^ Compression factor. The length of all arrow heads except the first will be
           -- multiplied by this factor, squashing them together if it is less than 1.
   -> [ArrowHead]  -- ^ List of arrow heads drawn in this order.
   -> ArrowHead
multiArrowHead _ [] = noArrowHead
multiArrowHead compress allHeads@(firstHead : otherHeads) = ArrowHead len width $ do
      lift Cairo.save
      forM_ (reverse allHeads) $ \h -> do
         arrowHeadRender h
         lift $ Cairo.translate (arrowHeadLength h * compress) 0
      lift Cairo.restore
   where
      len = arrowHeadLength firstHead + compress * sum (map arrowHeadLength otherHeads)
      width = maximum $ map arrowHeadWidth allHeads


-- | Draw a line from the current point to Point 2 ending in an arrow head.
drawArrowLine :: ArrowHead -> Point -> HadesRender ()
drawArrowLine arrow p2 = do
      p1 <- uncurry Point <$> lift Cairo.getCurrentPoint
      let
         headLength = arrowHeadLength arrow
         (dx, dy) = p1 `pointDiff` p2
         angle = atan2 dy dx
         len = sqrt (dx*dx + dy*dy)
         unitX = fixNaN 1 $ dx / len
         unitY = fixNaN 0 $ dy / len
         back = p2 `movePoint` ((-unitX) * headLength, (-unitY) * headLength)
      -- Draw the line to the back of the arrowhead.
      lift $ Cairo.lineTo (back ^. pX) (back ^. pY)
      lift Cairo.stroke
      lift $ Cairo.setDash [] 0
      -- Draw the arrowhead pointing right and rotate it
      -- into place.
      lift $ do
         Cairo.save
         Cairo.translate (p2 ^. pX) (p2 ^. pY)
         Cairo.rotate (angle + pi)
         Cairo.moveTo 0 0
      arrowHeadRender arrow
      lift $ do
         Cairo.stroke  -- Stroke will expand arrow size by line thickness even when filled.
         Cairo.restore
   where
      fixNaN d v = if isNaN v then d else v  -- Kludge for zero length lines.


-- | Draw a polyline with a specified arrowhead at the end. Due to the non-composable nature of
-- Cairo elements this cannot be represented as a path.
--
-- ToDo: Figure out how to stop connected lines from gribbling over the boxes they are
-- connected to.
drawPolyLine ::
   ArrowHead
   -> [Double]  -- ^ Line dash pattern. @[]@ means a solid line.
   -> Point     -- ^ Start point of arrow.
   -> [Point]   -- ^ Points where the line bends.
   -> Point     -- ^ End point of arrow.
   -> Maybe Text  -- ^ Optional label
   -> HadesRender ()
drawPolyLine arrow dashes startPoint midPoints endPoint label = do
      join $ asks lineStyle
      lift $ do
         Cairo.setDash dashes 0
         Cairo.moveTo (startPoint ^. pX) (startPoint ^. pY)
         forM_ midPoints $ \pt -> Cairo.lineTo (pt ^. pX) (pt ^. pY)
      drawArrowLine arrow endPoint
      case label of
         Nothing -> return ()
         Just txt -> do
            join $ asks textStyle
            font <- asks textFont
            lift $ do
               Cairo.save
               labelLayout <- pangoCreateDiagramLayout txt
               Pango.layoutSetFontDescription labelLayout $ Just font
               rect <- fst <$> Pango.layoutGetExtents labelLayout
               x <- pangoToHades <$> Pango.get rect #x
               y <- pangoToHades <$> Pango.get rect #y
               w <- pangoToHades <$> Pango.get rect #width
               h <- pangoToHades <$> Pango.get rect #height
               Cairo.moveTo (middle ^. pX - x - w/2) (middle ^. pY - y - h/2)
               Cairo.toRender (`Pango.showLayout` labelLayout)
               Cairo.restore
   where
      segMiddle f p1 p2 = p1 `movePoint` (both *~ f $ p1 `pointDiff` p2)
      allPoints = startPoint : midPoints ++ [endPoint]
      segments = zip allPoints (tail allPoints)
      lengths = map (distance . uncurry pointDiff) segments
      half = sum lengths / 2
      segEndDists = zip segments $ drop 1 $ scanl (+) 0 lengths
      middle = case dropWhile ((< half) . snd) segEndDists of
         [] -> segMiddle 0.5 startPoint endPoint  -- Something wierd happened in the numerics.
         ((p1, p2), d) : _ -> segMiddle ((d - half) / distance (p1 `pointDiff` p2)) p2 p1


-- | Descriptor for an annotation to go on an arrow
data ArrowDecoration = ArrowDecoration {
      arrowDecPosition :: Double,   -- ^ 0 = tail, 1 = head, 0.5 = half way along.
      arrowDecOnLine :: HadesRender (),  -- ^ Render a decoration on top of the arrow line.
      arrowDecOffLine :: Maybe ((Double, Double), HadesRender ())
         -- ^ Optional annotation to one side of the arrow, containing the @(width, height)@ of
         -- the annotation, and the render action to start from (0,0).
         -- Width and height must not be negative.
   }


-- | Draw the decoration at the appropriate point, figuring out the best place to put the
-- off-line component.
drawArrowDecoration :: LineShape -> ArrowDecoration -> HadesRender ()
drawArrowDecoration shape deco = do
      let
         (seg@(Line _ dx dy), f) = lineFractionSegment (arrowDecPosition deco) shape
         pt0 = linePoint seg f
         len = sqrt (dx*dx + dy*dy)
         unitX = fixNaN 1 $ dx / len
         unitY = fixNaN 0 $ dy / len
         angle = atan2 dy dx
      -- Render on-line component.
      lift $ do
         Cairo.save
         Cairo.translate (pt0 ^. pX) (pt0 ^. pY)
         Cairo.rotate (angle + pi)
         Cairo.moveTo 0 0
      arrowDecOnLine deco
      lift Cairo.restore
      -- Render off-line component.
      case arrowDecOffLine deco of
         Nothing -> return ()
         Just ((w, h), offLineAct) -> do
            let
               topLeft = case (dx > 0, dy > 0) of
                  (True, True) ->  -- Box touches seg at top right.
                     let
                        d0 = dotProduct (w/2, -h/2) (unitX, unitY)
                        pt1 = pt0 `movePoint` (unitX * d0, unitY * d0)
                     in pt1 `movePoint` (-w, 0)
                  (True, False) ->   -- Box touches seg at top left.
                     let
                        d0 = dotProduct (-w/2, -h/2) (unitX, unitY)
                     in pt0 `movePoint` (unitX * d0, unitY * d0)
                  (False, True) ->   -- Box touches seg at bottom right.
                     let
                        d0 = dotProduct (w/2, h/2) (unitX, unitY)
                        pt1 = pt0 `movePoint` (unitX * d0, unitY * d0)
                     in pt1 `movePoint` (-w, -h)
                  (False, False) ->   -- Box touches seg at bottom left.
                     let
                        d0 = dotProduct (-w/2, h/2) (unitX, unitY)
                        pt1 = pt0 `movePoint` (unitX * d0, unitY * d0)
                     in pt1 `movePoint` (0, -h)
            lift $ do
               Cairo.save
               Cairo.translate (topLeft ^. pX) (topLeft ^. pY)
               Cairo.moveTo 0 0
            offLineAct
            lift Cairo.restore
   where
      fixNaN d v = if isNaN v then d else v  -- Kludge for zero length lines.
      dotProduct :: (Double, Double) -> (Double, Double) -> Double
      dotProduct (x1, y1) (x2, y2) = x1*x2 + y1*y2


-- | Hades Layout tracks the remaing BoundBox into which sub-elements can be inserted. As each
-- sub-element is addded it consumes space from the top of the box. The "Double" is the
-- line height.
type HadesLayout a = StateT (BoundBox, Double) HadesRender a


-- | Execute a Hades Layout action within the HadesRender monad. If there is no box then
-- it returns @Nothing@.
runLayout :: BoundBox -> HadesLayout a -> HadesRender (Maybe a)
runLayout NoBox _ = return Nothing
runLayout box@(BoundBox p1 p2) act = do
   let (dx, dy) = pointDiff p1 p2
   -- Establish a clip region for this text.
   join $ asks textStyle
   font <- asks textFont
   ctx <- lift $ do
      -- This is like using assembler. Oh for a declarative API...
      Cairo.save
      Cairo.rectangle (p1 ^. pX) (p1 ^. pY) dx dy
      Cairo.clip
      Cairo.toRender Pango.createContext
   metrics <- Pango.contextGetMetrics ctx (Just font) Nothing
   lineHeight <- pangoToHades <$> Pango.fontMetricsGetHeight metrics
   r <- evalStateT act (box, lineHeight)
   lift Cairo.restore
   return $ Just r


-- | Execute a Hades Layout action, ignoring the result.
runLayout_ :: BoundBox -> HadesLayout a -> HadesRender ()
runLayout_ box act = void $ runLayout box act


-- | Run two layouts in parallel and consume the space of the longest. Results are returned
-- unless there is no space for a layout.
layoutParallel ::
   Double  -- ^ The Width of the left layout.
   -> Double -- ^ The separation between the left and right layouts.
   -> HadesLayout a -> HadesLayout b -> HadesLayout (Maybe a, Maybe b)
layoutParallel dx sep a b = do
      (outerBox, lineH) <- get
      case outerBox of
         NoBox -> return (Nothing, Nothing)
         (BoundBox p0 p1) -> do
            let
               leftBox = mkConstrainedBox p0 $ p0 `movePoint` (dx, boxHeight outerBox)
               rightBox = mkConstrainedBox (p0 `movePoint` (dx+sep, 0)) p1
            (leftUsed, leftResult) <- lift $ evalStateT (layoutRegion a) (leftBox, lineH)
            (rightUsed, rightResult) <- lift $ evalStateT (layoutRegion b) (rightBox, lineH)
            layoutConsume $ max (boxHeight leftUsed) (boxHeight rightUsed)
            return (leftResult, rightResult)


-- | Create a Pango layout with a fixed resolution of 96dpi. This will override any OS font
-- scale settings to ensure that text is the same size relative to the other diagram elements.
pangoCreateDiagramLayout :: Text -> Cairo.Render Pango.Layout
pangoCreateDiagramLayout str = do
   ctx <- Cairo.toRender Pango.createContext
   layout <- Pango.layoutNew ctx
   Pango.layoutSetText layout str (-1)
   return layout


-- | Perform the layout action and return the box that it consumed. If there is no space left
-- then the result will be @(NoBox, Nothing)@.
layoutRegion :: HadesLayout a -> HadesLayout (BoundBox, Maybe a)
layoutRegion act =
   layoutBox >>= \case
      NoBox -> return (NoBox, Nothing)
      before@(BoundBox p1 p2) -> do
         r <- act
         layoutBox >>= \case
            NoBox ->  return (before, Just r)  -- Run out of space within action.
            BoundBox p3 _ -> return (mkBoundBox p1 (Point (p2 ^. pX) (p3 ^. pY)), Just r)


-- | Perform the layout action but ignore the result and return only the box that it consumed.
layoutRegion_ :: HadesLayout a -> HadesLayout BoundBox
layoutRegion_ act = fst <$> layoutRegion act


-- | Remaining available space for a new element to be rendered. If all the space has been
-- consumed then it returns "NoBox".
layoutBox :: HadesLayout BoundBox
layoutBox = gets fst


-- | The height of a line.
layoutLine :: HadesLayout Double
layoutLine = gets snd


-- | Moves the top of the layout down.
layoutConsume :: Double -> HadesLayout ()
layoutConsume h = layoutBox >>= \case
   NoBox -> return ()
   BoundBox p1 p2 -> do
      let b = BoundBox (p1 `movePoint` (0, h)) p2  -- Deliberately unsafe. See next line.
      _1 .= (if boxHeight b <= 0 then NoBox else b)  -- Only use b if it is valid.


-- | Leave a gap as a paragraph break.
layoutParagraphGap :: HadesLayout ()
layoutParagraphGap = do
   lh <- layoutLine
   gap <- lift $ asks textGap
   layoutConsume $ lh * gap


-- | Draw a thin horizontal line at the top of the current layout box.
layoutHLine :: HadesLayout ()
layoutHLine = layoutBox >>= \case
   NoBox -> return ()
   BoundBox p1 p2 -> do
      layoutConsume 1
      lift $ lift $ do  -- Cairo Render monad
         Cairo.save
         Cairo.setLineWidth 1
         Cairo.setSourceRGB 0 0 0   -- Black line.
         Cairo.moveTo (p1 ^. pX) (p1 ^. pY)
         Cairo.lineTo (p2 ^. pX) (p1 ^. pY)
         Cairo.stroke
         Cairo.restore


-- | Draws a Pango text layout in the Hades layout box.
layoutDrawPango :: Pango.Layout -> HadesLayout ()
layoutDrawPango layout = layoutBox >>= \case
   NoBox -> return ()
   BoundBox p1 _ -> do
      lift $ lift $ do
         Cairo.moveTo (p1 ^. pX) (p1 ^. pY)
         Cairo.toRender (`Pango.showLayout` layout)
      rect <- snd <$> Pango.layoutGetExtents layout
      layoutConsume . pangoToHades =<< Pango.get rect #height


-- | Draws bold wrapped text in the layout box, possibly to the right of an image.
layoutTitle :: Maybe Gdk.Pixbuf -> Text -> HadesLayout ()
layoutTitle m txt = case m of
      Nothing -> textPart
      Just icon -> do
         iconWidth <- fromIntegral <$> Gdk.pixbufGetWidth icon
         void $ layoutParallel iconWidth 3
            (layoutPixbuf False Nothing icon)
            textPart
   where
      textPart = do
         weight <- Pango.attrWeightNew Pango.WeightBold
         attrs <- Pango.attrListNew
         Pango.attrListInsert attrs weight
         layoutText [
               (`Pango.layoutSetAttributes` Just attrs),
               (`Pango.layoutSetAlignment` Pango.AlignmentCenter)
            ] txt


-- | Draw the pixbuf at the top left hand of the box
-- and consume vertical space equal to its height.
-- If the pixbuf is bigger than the current box then it will be scaled down to fit.
layoutPixbuf ::
   Bool   -- ^ If the picture is wider than the box then @True@ = scale, @False@ = clip.
   -> Maybe Colour  -- ^ Background colour, if any.
   -> Gdk.Pixbuf -> HadesLayout ()
layoutPixbuf scaleFlag bg pic =
   layoutBox >>= \case
      NoBox ->
         return ()
      BoundBox (Point x0 y0) _ -> do
         availableWidth <- boxWidth <$> layoutBox
         w <- fromIntegral <$> Gdk.pixbufGetWidth pic
         h <- fromIntegral <$> Gdk.pixbufGetHeight pic
         let
            scale = if scaleFlag then min 1 $ availableWidth / w else 1
            scaledHeight = h * scale
         layoutConsume scaledHeight
         lift $ lift $ do
            Cairo.save
            Cairo.translate x0 y0
            Cairo.scale scale scale
            forM_ bg $ \c -> do
               let C.RGB r g b = C.toSRGB $ getColour c
               Cairo.setSourceRGB r g b
               Cairo.rectangle 0 0 w h
               Cairo.fill
            Cairo.toRender $ \ctx -> Gdk.cairoSetSourcePixbuf ctx pic 0 0
            Cairo.rectangle 0 0 w h
            Cairo.fill
            Cairo.restore


-- | Draw the SVG at the top left hand of the box and consume its height.
-- If the SVG is wider than the box then it will be scaled down to fit.
--
-- The SVG handle must be fully loaded.
layoutSvg ::
   Maybe Colour   -- ^ Background colour, if any.
   -> SVG.Handle
   -> HadesLayout ()
layoutSvg bg pic =
   layoutBox >>= \case
      NoBox ->
         return ()
      BoundBox (Point x0 y0) _ -> do
         availableWidth <- boxWidth <$> layoutBox
         SVG.handleSetDpi pic 90
         (widthFlag, width, heightFlag, height, _, _) <-
            SVG.handleGetIntrinsicDimensions pic
         -- If height not given then assume it is square.
         w <- computeSize availableWidth widthFlag width
         h <- computeSize availableWidth heightFlag height
         let
            scale = if w > availableWidth then availableWidth / w else 1
            scaledHeight = h * scale
         viewport <- GI.new SVG.Rectangle [#width := w, #height := h]
         lift $ lift $ do
            Cairo.save
            Cairo.translate x0 y0
            Cairo.scale scale scale
            forM_ bg $ \c -> do
               let C.RGB r g b = C.toSRGB $ getColour c
               Cairo.setSourceRGB r g b
               Cairo.rectangle 0 0 w h
               Cairo.fill
            void $ Cairo.toRender $ \ctx -> SVG.handleRenderDocument pic ctx viewport
            Cairo.restore
         layoutConsume scaledHeight
   where
      computeSize available False _ = return available
      computeSize available True v = do
         l <- SVG.getLengthLength v
         SVG.getLengthUnit v >>= \case
            SVG.UnitPercent -> return $ l * available
            SVG.UnitPx -> return l
            SVG.UnitEm -> return $ l * hadesFontSize
            SVG.UnitEx -> return $ l * hadesFontSize * 0.48
               -- Assume an ex is 48% of an em. This is font dependent, but never mind.
            SVG.UnitIn -> return $ l * hadesDPI
            SVG.UnitCm -> return $ l * hadesDPI / 2.54  -- cm per inch.
            SVG.UnitMm -> return $ l * hadesDPI / 25.4  -- mm per inch.
            SVG.UnitPt -> return $ l * hadesDPI / 70    -- Points per inch.
            SVG.UnitPc -> return $ l * hadesDPI / 6     -- Picas per inch.
            SVG.AnotherUnit _ -> return available


-- | Draws a block of text in the layout box. By default the text is in the "HadesRender" font,
-- left aligned and wrapped to the width of the layout box.
layoutText ::
   [Pango.Layout -> HadesLayout ()]  -- ^ List of layout modifiers to be applied to the text.
   -> Text   -- ^ The text to be displayed.
   -> HadesLayout ()
layoutText mods txt = do
   box <- layoutBox
   font <- lift $ asks textFont
   layout <- lift $ lift $ pangoCreateDiagramLayout txt
   Pango.layoutSetFontDescription layout $ Just font
   Pango.layoutSetWidth layout $ hadesToPango $ boxWidth box
   Pango.layoutSetAlignment layout Pango.AlignmentLeft
   forM_ mods ($ layout)
   layoutDrawPango layout


-- | Draws a block of text, separating paragraphs with a paragraph gap. Paragraphs in the text are
-- denoted by a blank line.
layoutParagraphs ::
   [Pango.Layout -> HadesLayout ()]  -- ^ List of layout modifiers to be applied to the text.
   -> Text   -- ^ The text to be displayed.
   -> HadesLayout ()
layoutParagraphs mods txt =
   sequence_ $ intersperse layoutParagraphGap $ map (layoutText mods) $ T.splitOn "\n\n" txt



-- | Draw the first text as a title (bold, centred) and the second as a sequence of paragraphs,
-- all within the BoundBox. Ellipsize as necessary.
--
-- It's worth noting that @Data.Text.splitOn "\n\n"@ will conveniently split text into a
-- list of paragraphs.
textBlock ::
   Text    -- ^ Title text. Bold, centered, on a single line and ellipsized as necessary.
   -> [Text]   -- ^ Paragraphs of body text.
   -> BoundBox  -- ^ Box the text will fit into.
   -> HadesRender ()
textBlock _ _ NoBox = return ()
textBlock title descriptions box = runLayout_ box $ do
      layoutTitle Nothing title
      layoutParagraphGap
      forM_ descriptions $ \txt -> do
         layoutText [] txt
         layoutParagraphGap


-- | Draw a letter inside the bottom right hand corner of the BoundBox.
drawCornerLetter :: BoundBox -> Char -> HadesRender ()
drawCornerLetter NoBox _ = return ()
drawCornerLetter (BoundBox _ (Point x y)) c = do
   lift Cairo.save
   font <- fromJust <$> (Pango.fontDescriptionCopy =<< asks textFont)
      -- fontDescriptionCopy result wrongly tagged as nullable.
   layout <- lift $ pangoCreateDiagramLayout $ T.singleton c
   fontSize <- Pango.fontDescriptionGetSize font
   Pango.fontDescriptionSetSize font $
      round $ 1.25 * if fontSize == 0 then hadesFontSize else fromIntegral fontSize -- A bit bigger.
   Pango.layoutSetFontDescription layout $ Just font
   attr <- Pango.attrWeightNew Pango.WeightBold
   Pango.setAttributeEndIndex attr 2
   attrs <- Pango.attrListNew
   Pango.attrListInsert attrs attr
   rect <- fst <$> Pango.layoutGetExtents layout
   w <- pangoToHades <$> Pango.getRectangleWidth rect
   h <- pangoToHades <$> Pango.getRectangleHeight rect
   join $ asks textStyle
   lift $ do
      Cairo.moveTo (x - w) (y - h)
      ctx <- Cairo.getContext
      Pango.showLayout ctx layout
      Cairo.restore
