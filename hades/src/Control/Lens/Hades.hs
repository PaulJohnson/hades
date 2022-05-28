{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

-- |
--
-- Some minor lens utilities used in the Hades library.


module Control.Lens.Hades where

import Control.Lens
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S


-- | Create an "Iso'" betweeen a @Maybe@ value and a plain value by using a
-- default that equates to @Nothing@. Useful for things like
--
-- > optionalText = defaultIso ""
--
-- which will equate an empty string with @Nothing@.
--
-- This is not quite a true Iso because @Just d@ will round-trip to @Nothing@ when
-- @d@ is the default value.
defaultIso :: (Eq a) => a -> Iso' (Maybe a) a
defaultIso d = iso f b
  where
    f Nothing = d
    f (Just v) = v
    b v = if v == d then Nothing else Just v


-- | Make a prism into a lens by using a default value.
prismLens :: (Eq a) => a -> Prism' s a -> Lens' s a
prismLens d prsm = lens
  (\s -> fromMaybe d $ s ^? prsm)
  (\s v -> if v == d && isNothing (s ^? prsm) then s else review prsm v)


-- | Make a prism into a lens that passes through a Maybe.
maybeLens :: Prism' s a -> Lens' (Maybe s) (Maybe a)
maybeLens prsm = lens extract1 inject
  where
    extract1 Nothing = Nothing
    extract1 (Just v) = v ^? prsm
    inject _ Nothing = Nothing
    inject _ (Just b) = Just $ b ^. re prsm


-- Pseudo-ISO for converting between a "Maybe" and a "Set". If the set contains multiple items
-- then the lowest is taken.
setMaybeIso :: Iso' (Set a) (Maybe a)
setMaybeIso = iso forwards1 backwards1
  where
    forwards1 s = case S.toList s of
      [] -> Nothing
      x:_ -> Just x
    backwards1 Nothing = S.empty
    backwards1 (Just x) = S.singleton x
