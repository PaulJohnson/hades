{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}


{- |

Convert Documents into HTML.
-}

module Model.Report.Html (
   mkHtmlDocument
) where

import qualified Data.Colour.CIE as C
import qualified Data.Colour.SRGB as C
import Data.Text (Text)
import Data.Tree
import Model.Report.Base
import Model.Report.Document
import Reactive.Banana.Common
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA


-- | The HTML \"type\" code for ordered lists.
listNumber :: ListNumber -> H.Attribute
listNumber ListDecimal = HA.type_ "1"
listNumber ListAlphaUpper = HA.type_ "A"
listNumber ListAlphaLower = HA.type_ "a"
listNumber ListRomanUpper = HA.type_ "I"
listNumber ListRomanLower = HA.type_ "i"


-- | Top level conversion. Puts HTML head and body tags around the blocks.
mkHtmlDocument ::
   (ImageRef -> FilePath)  -- ^ Paths where images will be stored.
   -> Html        -- ^ Contents for the head section.
   -> [Block]  -- ^ Contents for the body section.
   -> Html
mkHtmlDocument pathFunc headStuff blks = H.docTypeHtml $
      H.head (H.meta ! HA.charset (H.toValue ("UTF-8" :: Text)) <> headStuff) <>
      H.body (blocksToHtml pathFunc blks)


-- | Generate the HTML for a block.
blockToHtml :: (ImageRef -> FilePath) -> Block -> Html
blockToHtml pathFunc (Plain inlines) = inlinesToHtml pathFunc inlines
blockToHtml _ (Para []) = mempty
blockToHtml pathFunc (Para inlines) = H.p $ inlinesToHtml pathFunc inlines
blockToHtml pathFunc (OrderedList numType blockss) =
   H.ol ! listNumber numType $ mconcat $ map (H.li . blocksToHtml pathFunc) blockss
blockToHtml pathFunc (UnorderedList blockss) =
   H.ul $ mconcat $ map (H.li . blocksToHtml pathFunc) blockss
blockToHtml pathFunc (DefinitionList pairs) =
      H.dl $ mconcat $ map ddPair pairs
   where
      ddPair (term, def) = H.dt (inlinesToHtml pathFunc term) <> H.dd (blocksToHtml pathFunc def)
blockToHtml pathFunc (Heading level attr inlines) =
      h level ! attrToHtml attr $ inlinesToHtml pathFunc inlines
   where
      h 1 = H.h1
      h 2 = H.h2
      h 3 = H.h3
      h 4 = H.h4
      h 5 = H.h5
      h 6 = H.h6
      h _ = H.p . H.b
blockToHtml pathFunc (Matrix attr caption colHeaderss cells) =
   matrixToHtml pathFunc attr caption colHeaderss cells
blockToHtml pathFunc (Div attr blocks) = H.div ! attrToHtml attr $ blocksToHtml pathFunc blocks
blockToHtml pathFunc (Picture attr caption alt _ diagramId) = H.figure ! attrToHtml attr $
      H.img ! HA.src (H.toValue $ pathFunc $ DiagramRef diagramId) ! HA.alt (H.toValue alt) <>
      H.figcaption (H.span ! HA.class_ "diagramCaption" $ inlinesToHtml pathFunc caption)
blockToHtml _ Null = mempty


blocksToHtml :: (ImageRef -> FilePath) -> [Block] -> Html
blocksToHtml pathFunc = mconcat . map (blockToHtml pathFunc)


-- | Generate the HTML for an Inline.
inlineToHtml :: (ImageRef -> FilePath) -> Inline -> Html
inlineToHtml _ (Str txt) = H.text txt
inlineToHtml pathFunc (Emph inlines) = H.em $ inlinesToHtml pathFunc inlines
inlineToHtml pathFunc (Strong inlines) = H.strong $ inlinesToHtml pathFunc inlines
inlineToHtml _ Space = H.text " "
inlineToHtml pathFunc (Link target inlines) =
   H.a ! HA.href (H.toValue $ '#' : show target) $ inlinesToHtml pathFunc inlines
inlineToHtml pathFunc (LinkOut url inlines) =
   H.a ! HA.href (H.toValue url) $ inlinesToHtml pathFunc inlines
inlineToHtml pathFunc (Icon nm) =
   H.img ! HA.src (H.toValue $ pathFunc $ IconRef nm) !
   HA.alt "icon" ! HA.height "16" ! HA.width "16"
inlineToHtml pathFunc (Highlight colour inlines) =
      H.span ! HA.style (H.toValue colourSet) $ inlinesToHtml pathFunc inlines
   where
      colourSet =
            "background-color:" <> C.sRGB24show (getColour colour) <>
            ";color:" <> if C.luminance (getColour colour) < 0.5 then "white" else "black"
inlineToHtml pathFunc (Span attr inlines) = H.span ! attrToHtml attr $ inlinesToHtml pathFunc inlines


inlinesToHtml :: (ImageRef -> FilePath) -> [Inline] -> Html
inlinesToHtml pathFunc = mconcat . map (inlineToHtml pathFunc)


attrToHtml :: Attr -> H.Attribute
attrToHtml (Attr ident classes) = identAttr <> classesAttr
   where
      identAttr = case ident of
         Nothing -> mempty
         Just (uuid, _) -> HA.id $ H.toValue $ show uuid
      classesAttr = case classes of
         Nothing -> mempty
         Just txt -> HA.class_ $ H.toValue txt


-- | Convert a matrix to HTML. The matrix cells are represented as a forest of values. Each value
-- is itself a list of cells. Each value is shown as a row of table cells which vertically spans
-- the rows of the next level.
--
-- It is up to the caller to make sure that the cell layout agrees with the header layout.
matrixToHtml ::
   (ImageRef -> FilePath)  -- ^ Paths for image files.
   -> Attr  -- ^ Identifier and style classes for this matrix.
   -> [Inline]  -- ^ Caption for this matrix.
   -> [MatrixHeader]
      -- ^ The outer list matches tree levels. The inner lists are colums within each level.
   -> Forest [MatrixCell]
   -> Html
matrixToHtml pathFunc attr caption headers cells =
      H.table ! attrToHtml attr $ captionHtml <> headerRows <> mconcat (map H.tr $ dataRows cells)
   where
      captionHtml = if null caption then mempty else H.caption $ inlinesToHtml pathFunc caption
      headerRows = case headers of
         [] -> mempty
         [grp] -> H.tr $ snd $ groupHeader grp
         _ -> let (r1, r2) = complexHeader in H.tr r1 <> H.tr r2
      groupHeader (MatrixHeader nm cols) = (
            H.th ! HA.colspan (H.toValue $ length cols) $ inlinesToHtml pathFunc nm,
            mconcat $ map (H.th . inlinesToHtml pathFunc) cols
         )
      complexHeader =  -- Only used when headers is not empty, so "head" and ":" are safe.
         let
            (grp1 : grps) = map groupHeader headers
            n = length $ matrixHeaderColumns $ head headers
         in (snd grp1, H.th ! HA.colspan (H.toValue n) $ mempty) <> mconcat grps
      dataRows = concatMap dataRow
      dataRow node@(Node cells1 childs) =
         case childRows of
            [] -> [thisLevel]
            (row1 : rows) -> (thisLevel <> row1) : rows
         where
            thisLevel = mconcat $ map htmlCell cells1
            childRows = dataRows childs
            htmlCell (attr1, blks) = H.td ! spread ! attrToHtml attr1 $ blocksToHtml pathFunc blks
            spread = case leafCount node of
               1 -> mempty
               n -> HA.rowspan $ H.toValue n
      leafCount (Node _ []) = 1 :: Int
      leafCount (Node _ cs) = sum $ map leafCount cs
