{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

-- |
--
-- Natural order sorting for Text. Separates out numbers from the rest of the text, and uses
-- the Unicode sort order for the text and numerical order for the numbers.

module Text.NaturalOrder where

import Data.Char
import Data.List
import Data.Text (Text)
import qualified Data.Text as T

-- | A chunk of text suitable for sorting as an element, in collation order.
data NaturalChunk =
  SpaceChunk   -- ^ Whitespace of any kind.
  | OtherChunk ! Text  -- ^ All characters not otherwise recognised.
  | NumberChunk ! Integer
  | LetterChunk ! Text   -- ^ Must have been converted to folded case using "T.toCaseFold"
  deriving (Show, Eq, Ord)


-- | Functions to classify characters. First match wins.
chunkClasses :: [(Char -> Bool, Text -> NaturalChunk)]
chunkClasses = [
    (isSpace, const SpaceChunk),
    (\c -> isLetter c || isMark c, LetterChunk . T.toCaseFold),
    (isDigit, NumberChunk . read . T.unpack)  -- "read" is safe because argument is all digits.
  ]


-- | True if character is not one of the "chunkClasses"
isOther :: Char -> Bool
isOther c = all (($ c) . (not .) . fst) chunkClasses


-- | Split the text into runs of the same type.
chunkText :: Text -> [NaturalChunk]
chunkText = go . T.strip
  where
    go txt = case T.uncons txt of
      Nothing -> []
      Just (c1, _) ->
        case find (($ c1) . fst) chunkClasses of
          Just (predicate, typ) ->
            let (t1, t2) = T.span predicate txt in typ t1 : go t2
          Nothing ->
            let (t1, t2) = T.span isOther txt in OtherChunk (T.toCaseFold t1) : go t2


-- | Compare the two strings using natural order sorting.
naturalOrder :: Text -> Text -> Ordering
naturalOrder t1 t2 =
  if t1 == t2 then EQ else case compare (chunkText t1) (chunkText t2) of
    EQ -> compare t1 t2
    o -> o
