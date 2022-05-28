{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}


{- |

DrawingML combinators.
-}

module Model.Report.Docx.Drawing (
  EMU,
  centimeters,
  inches,
  points,
  wpName,
  pName,
  aName,
  wpAttr,
  inlinePicture
) where

import Data.Text (Text)
import qualified Data.Text as T
import Model.Report.Base
import Model.Report.Docx.Document
import qualified Text.XML.Light as X


-- | EMU = English Metric Unit. Unit of length designed to divide both centimeters and inches into
-- a whole number of units. See ECMA-376 Part 1 Section 20.1.2.
type EMU = Integer


-- | Measurement in centimters converted into EMUs
centimeters :: Double -> EMU
centimeters x = round $ x * 360000


-- | Measurement in US inches converted into EMUs.
inches :: Double -> EMU
inches x = round $ x * 914400


-- | Measurement in Points converted into EMUs
points  :: Double -> EMU
points x = round $ x * 12700  -- 914400 / 72


-- | Qualified name for a Drawing XML node.
wpName :: Text -> X.QName
wpName n = qual "wp" $ T.unpack n


-- | Qualified name for a Drawing XML node in \"@p:@\".
pName :: Text -> X.QName
pName n = qual "pic" $ T.unpack n


-- | Qualified name for a Drawing XML node in \"@a:@\".
aName :: Text -> X.QName
aName n = qual "a" $ T.unpack n


-- | Attribute with qualified name for Drawing XML
wpAttr :: Text -> Text -> X.Attr
wpAttr nm = X.Attr (wpName nm) . T.unpack


-- | Wrap the picture with @<w:drawing>@ and @<wp:inline>@.
inlinePicture ::
  (Double, Double)   -- ^ Width and height of the drawing in points.
  -> ImageRef        -- ^ Image to include.
  -> Text            -- ^ Image name.
  -> DocMonad X.Element
inlinePicture (w, h) ref title = do
  picId <- T.unpack <$> newId
  picture <- mkPicture picId
  return $ X.node (wName "drawing") $ X.node (wpName "inline") (
    [
      X.Attr (X.unqual "distT") "0",
      X.Attr (X.unqual "distB") "0",
      X.Attr (X.unqual "distL") "0",
      X.Attr (X.unqual "distR") "0"
    ], [
      X.node (wpName "extent") [
        X.Attr (X.unqual "cx") $ show $ points w,
        X.Attr (X.unqual "cy") $ show $ points h],
      X.node (wpName "effectExtent") [  -- Don't know what this does, but Word adds it.
        X.Attr (X.unqual "l") "0",
        X.Attr (X.unqual "t") "0",
        X.Attr (X.unqual "r") "0",
        X.Attr (X.unqual "b") "0" ],
      X.node (wpName "docPr") [
        X.Attr (X.unqual "id") picId,
        X.Attr (X.unqual "name") $ T.unpack title ],
      X.node (wpName "cNvGraphicFramePr") (),
      X.node (aName "graphic") (
        nameSpace "a" aUri,
        X.node (aName "graphicData") (X.Attr (X.unqual "uri") picUri, picture))
    ])
  where
    mkPicture picId = do
      b <- blipFill ref
      let
        nv = nonVisualProperties picId title True
        sp = shapeProperties 0 0 w h
      return $ X.node (pName "pic") ([nameSpace "pic" picUri], [nv, b, sp])

picUri :: String
picUri = "http://schemas.openxmlformats.org/drawingml/2006/picture"

aUri :: String
aUri = "http://schemas.openxmlformats.org/drawingml/2006/main"

svgUri :: String
svgUri = "http://schemas.microsoft.com/office/drawing/2016/SVG/main"

-- | Non-visual picture properties.
nonVisualProperties ::
  String    -- ^ Picture ID.
  -> Text    -- ^ Picture name.
  -> Bool  -- ^ Lock aspect ratio.
  -> X.Element
nonVisualProperties picId title lock = X.node (pName "nvPicPr") [
    X.node (pName "cNvPr") [
        X.Attr (X.unqual "id") picId,
        X.Attr (X.unqual "name") $ T.unpack title
      ],
    X.node
      (pName "cNvPicPr")
      ([X.node (aName "picLocks") $ X.Attr (X.unqual "noChangeAspect") "1" | lock])
  ]


-- | A reference to an embedded image.
blipFill :: ImageRef -> DocMonad X.Element
blipFill ref =
  -- This is gnarly. Generate a blip node with the PNG in its attribute.
  -- If there is an SVG then wrap it in an extLst in the blip node content.
  getMediaRef ref PngTarget >>= \case
    Nothing -> return $ wText "Missing image"
    Just pngRel -> do
      let pngAttr = X.Attr (qual "r" "embed") $ T.unpack $ relId pngRel
      svgM <- getMediaRef ref SvgTarget
      return $ X.node (pName "blipFill") [
          case svgM of
            Nothing -> X.node (aName "blip") pngAttr
                -- No SVG, so blip contains bare attribute and no content.
            Just svgRel -> X.node (aName "blip") (pngAttr,
                -- blip node content: SVG incantations.
                qNode "a" "extLst" $ qNode "a" "ext" (
                    X.Attr (X.unqual "uri") "{96DAC541-7B7A-43D3-8B79-37D633B846F1}",
                    qNode "asvg" "svgBlip" [
                        X.Attr (qual "xmlns" "asvg") svgUri,
                        X.Attr (qual "r" "embed") $ T.unpack $ relId svgRel
              ])),
          X.node (aName "stretch") $ X.node (aName "fillRect") ()
        ]


-- | Shape properties: rectangle with the given position and width and height.
shapeProperties :: Double -> Double -> Double -> Double -> X.Element
shapeProperties x y w h =
  X.node (pName "spPr") [
      X.node (aName "xfrm") [
          X.node (aName "off") [
              X.Attr (X.unqual "x") $ show $ points x,
              X.Attr (X.unqual "y") $ show $ points y],
          X.node (aName "ext") [
              X.Attr (X.unqual "cx") $ show $ points w,
              X.Attr (X.unqual "cy") $ show $ points h]
        ],
      X.node (aName "prstGeom") (
          X.Attr (X.unqual "prst") "rect",
          X.node (aName "avLst") ()
        ),
      X.node (aName "noFill") (),
      X.node (aName "ln") $ X.node (aName "noFill") ()
    ]


{- Design note:

The contents of the picture combinators are largely incantations taken
from ECMA-376 Part 1 Appendix L.4.10.2, which includes the following "simple" example:
  <p:pic>
    <p:nvPicPr>
      <p:cNvPr id="4" name="St_Patrick's_Day.jpg"/>
      <p:cNvPicPr>
        <a:picLocks noChangeAspect="1"/>
      </p:cNvPicPr>
      <p:nvPr/>
    </p:nvPicPr>
    <p:blipFill>
      <a:blip r:embed="rId2"/>
      <a:stretch>
        <a:fillRect/>
      </a:stretch>
    </p:blipFill>
    <p:spPr>
      <a:xfrm>
        <a:off x="1346200" y="914400"/>
        <a:ext cx="3657600" cy="2743200"/>
      </a:xfrm>
      <a:prstGeom prst="rect">
        <a:avLst/>
      </a:prstGeom>
      <a:noFill/>
      <a:ln>
        <a:noFill/>
      </a:ln>
    </p:spPr>
  </p:pic>

This has then been enhanced using cargo-cult cut-and-paste from
a Word document with embedded SVG. Nodes were added until Word stopped
complaining.
-}
