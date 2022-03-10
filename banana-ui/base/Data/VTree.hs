{-# LANGUAGE TypeFamilies #-}

{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.
-}


{- |
Vector-based rose trees. Similar to "Data.Tree", but using vectors to store children rather
than lists. This makes for more efficient accessing.

-}

module Data.VTree (
   -- * Types
   VTree (VNode),
   VForest,
   Address,
   -- * Lenses
   root,
   branches,
   treeIso,
   forestIso,
   -- * Accessors
   getNode,
   getValue,
   setValue,
   toTree,
   fromTree,
   -- * Traversal
   goDown,
   goUp,
   goNext
) where

import Control.Lens
import Control.Monad
import Data.Tree
import Data.Vector (Vector, (!?))
import qualified Data.Vector as V
import qualified Data.Vector.Lens as V

data VTree a = VNode {_root :: a,  _branches :: VForest a}

instance Functor VTree where
   fmap f (VNode v forest) = VNode (f v) $ fmap f <$> forest


type VForest a = Vector (VTree a)


-- | A node in a VTree can be referenced by an Address.
-- This is a list of indices within the vectors, one for each level. The empty address refers to
-- the root of the tree.
type Address = [Int]

type instance Index (VTree a) = Address

type instance IxValue (VTree a) = a

-- Cribbed from the Ixed Tree instance.
instance Ixed (VTree a) where
   ix xs0 f = go xs0 where
      go [] (VNode a as) = f a <&> \a' -> VNode a' as
      go (i:is) t@(VNode a as)
         | i < 0 || i >= V.length as  = pure t
         | otherwise                  = VNode a <$> ix i (go is) as
   {-# INLINE ix #-}


-- | Lens focusing on the value at the root of the tree.
root :: Lens' (VTree a) a
root = lens _root $ \s v -> s {_root = v}


-- | Lens focusing on the immediate children of the tree.
branches :: Lens' (VTree a) (VForest a)
branches = lens _branches $ \s b -> s {_branches = b}


-- | ISO with list-based rose tree.
treeIso :: Iso' (Tree a) (VTree a)
treeIso = iso fromTree toTree


forestIso :: Iso' (Forest a) (VForest a)
forestIso = V.vector . mapping treeIso


-- | Retrieve the node referenced by the address. Returns @Nothing@ if the address does not exist
-- in the tree.
getNode :: Address -> VTree a -> Maybe (VTree a)
getNode [] node = Just node
getNode (n:ns) (VNode _ forest) = forest !? n >>=  getNode ns


-- | Get the value referenced by the address. Returns @Nothing@ if the address does not exist.
getValue :: Address -> VTree a -> Maybe a
getValue addr tree = do
   (VNode v _) <- getNode addr tree
   return v


-- | Set the node value at the address. Returns Nothing if the address does not exist.
setValue :: Address -> a -> VTree a -> Maybe (VTree a)
setValue [] v (VNode _ forest) = Just $ VNode v forest
setValue (n:ns) v (VNode _ forest) = do
   child <- forest !? n
   setValue ns v child


toTree :: VTree a -> Tree a
toTree (VNode v forest) = Node v $ map toTree $ V.toList forest


fromTree :: Tree a -> VTree a
fromTree (Node v forest) = VNode v $ V.fromList $ map fromTree forest


-- | Return the first child of the current node, if it exists.
goDown :: VTree a -> Address -> Maybe Address
goDown tree addr = do
   VNode _ xs <- getNode addr tree
   if V.null xs then Nothing else return $ addr ++ [0]


-- | Return the parent of the current node. @Nothing@ if already at root.
goUp :: Address -> Maybe Address
goUp [] = Nothing
goUp addr = Just $ init addr


-- | Return the right sibling of the current node. @Nothing@ if there is none.
goNext :: VTree a -> Address -> Maybe Address
goNext tree addr = do
   parent <- goUp addr
   n <- if null addr then Nothing else Just $ last addr
   let r = parent ++ [n+1]
   void $ getValue r tree   -- Verify that the sibling exists.
   return r
