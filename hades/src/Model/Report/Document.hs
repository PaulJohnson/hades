{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}


{- |

This module is based on the ideas in PanDoc. It has been created for the following reasons:

1. PanDoc is licensed under the GPL, and therefore cannot be incorporated into proprietary
software.

2. PanDoc tables do not have the flexibility required for DSM matrices.

3. Colour markup is not supported by PanDoc.

At the same time, this is much less ambitious than PanDoc: no import or serialisation, and the
number of formats supported is smaller.
-}

module Model.Report.Document (
  -- * Main Types
  Block (..),
  MatrixHeader (..),
  MatrixCell,
  ListNumber (..),
  Ident,
  Attr (..),
  noAttr,
  Inline (..),
  trimSpaces
) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree
import Data.UUID (UUID)
import Reactive.Banana.Common


data Block =
  Plain [Inline]  -- ^ Plain text.
  | Para [Inline]  -- ^ A paragraph.
  | OrderedList ListNumber [[Block]]  -- ^ First argument is the list index. Ignored for DocX.
  | UnorderedList [[Block]]  -- ^ Bulleted list. First argument is the bullet.
  | DefinitionList [([Inline], [Block])]
  | Heading Int Attr [Inline]
  | Matrix Attr [Inline] [MatrixHeader] (Forest [MatrixCell])
    -- ^ Caption. List column header lists, one per level of forest. Forest of cell contents.
  | Div Attr [Block]
  | Picture Attr [Inline] Text (Double, Double) UUID
    -- ^ Model diagram displayed as a block with a caption, alt-text and size in points.
  | Null
  deriving (Show)


-- | Type of numbering for ordered lists.
data ListNumber = ListDecimal | ListAlphaUpper | ListAlphaLower | ListRomanUpper | ListRomanLower
  deriving (Eq, Show)

-- | Matrix headers consist of groups of columns.
data MatrixHeader = MatrixHeader {
    matrixHeaderName :: [Inline],
      -- ^ Title for this entire group. Ignored for the first group.
    matrixHeaderColumns :: [[Inline]]
      -- ^ Columns within the group. Each column has a title made of a list of Inlines.
  } deriving (Show)


-- | A matrix cell is a list of blocks with an attribute. This allows different types of cells
-- to be formatted using CSS.
type MatrixCell = (Attr, [Block])


-- | Identifiers for cross references and diagrams.
type Ident = UUID


-- | Identifiers are used for internal cross references.
data Attr = Attr {
    attrIdentifier :: Maybe (Ident, Int),  -- ^ Optional cross reference identifier and priority.
    attrClass :: Maybe Text              -- ^ Format class for the text.
  } deriving (Show)


-- | Empty attribute
noAttr :: Attr
noAttr = Attr Nothing Nothing


data Inline =
  Str Text   -- ^ Plain text.
  | Emph [Inline]  -- ^ Emphasized text.
  | Strong [Inline]  -- ^ Strongly emphasized text.
  | Space   -- ^ Interword space.
  | Link Ident [Inline]  -- ^ Internal cross reference
  | LinkOut Text [Inline]  -- ^ External reference. Text contains a URL.
  | Icon Text  -- ^ In-line image.
  | Highlight Colour [Inline]
    -- ^ Highlight the text with the colour. Foreground is black or white for contrast.
  | Span Attr [Inline] -- ^ Span this stretch of text with the attribute.
  deriving (Show)


-- | Trim leading and trailing spaces from the list of Inlines.
-- Works on both "Space" items and text with leading spaces.
trimSpaces :: [Inline] -> [Inline]
trimSpaces = trimLeading . reverse . trimTrailing . reverse
    -- Double-reverse so trimTrailing can work from the start of the list.
  where
    trimLeading [] = []
    trimLeading (Str txt : ys) = case T.stripStart txt of
      "" -> trimLeading ys
      txt2 -> Str txt2 : ys
    trimLeading (Emph xs : ys) = trimLeading1 Emph xs ys
    trimLeading (Strong xs : ys) = trimLeading1 Strong xs ys
    trimLeading (Space : ys) = trimLeading ys
    trimLeading (Link ident xs : ys) = trimLeading1 (Link ident) xs ys
    trimLeading (LinkOut url xs : ys) = trimLeading1 (LinkOut url) xs ys
    trimLeading (Icon txt : ys) = Icon txt : ys
    trimLeading (Highlight c xs : ys) = trimLeading1 (Highlight c) xs ys
    trimLeading (Span a xs : ys) = trimLeading1 (Span a) xs ys
    trimLeading1 f xs ys = case trimLeading xs of
        [] -> trimLeading ys
        xs1 -> f xs1 : ys
    trimTrailing [] = []
    trimTrailing (Str txt : ys) = case T.stripEnd txt of
      "" -> trimTrailing ys
      txt2 -> Str txt2 : ys
    trimTrailing (Emph xs : ys) = trimTrailing1 Emph xs ys
    trimTrailing (Strong xs : ys) = trimTrailing1 Strong xs ys
    trimTrailing (Space : ys) = trimTrailing ys
    trimTrailing (Link ident xs : ys) = trimTrailing1 (Link ident) xs ys
    trimTrailing (LinkOut url xs : ys) = trimTrailing1 (LinkOut url) xs ys
    trimTrailing (Icon txt : ys) = Icon txt : ys
    trimTrailing (Highlight c xs : ys) = trimTrailing1 (Highlight c) xs ys
    trimTrailing (Span a xs : ys) = trimTrailing1 (Span a) xs ys
    trimTrailing1 f xs ys = case reverse $ trimTrailing $ reverse xs of
        [] -> trimTrailing ys
        xs1 -> f xs1 : ys
