{-
Copyright © Paul Johnson 2019. See LICENSE file for details.
-}

-- |
--
-- Statically defined menus.

module Reactive.Banana.Menu (
   Menu (..),
   MenuItem (..),
   menuItem,
   subMenu
) where

import Data.Text (Text)

-- | Menu items are displayed in groups with separators in between. Each sublist is a group.
newtype Menu a = Menu [[MenuItem a]] deriving (Eq)

instance Functor Menu where
   fmap f (Menu items) = Menu $ map (map (fmap f)) items

instance Semigroup (Menu a) where
   Menu m1 <> Menu m2  = Menu $ m1 <> m2

instance Monoid (Menu a) where
   mempty = Menu []


-- | The selection of a menu item triggers an @Event a@.
--
-- Use @pure True@ if the item is always enabled.
data MenuItem a = MenuItem {
      menuLabel :: Text,
      menuValue :: Either (Menu a) a  -- ^ @Left@ for a sub-menu, @Right@ for an item.
   } deriving (Eq)

instance Functor MenuItem where
   fmap f (MenuItem str v) = MenuItem str $ either (Left . fmap f) (Right . f) v


-- | A single menu item which is always enabled.
menuItem :: Text -> a -> MenuItem a
menuItem label v = MenuItem label $ Right v

-- | A sub-menu which is always enabled.
subMenu :: Text -> Menu a -> MenuItem a
subMenu label sub = MenuItem label $ Left sub
