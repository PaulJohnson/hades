
{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}


{- |

Walk a document tree.
-}

module Model.Report.Walkable (
  Walkable (..),
  unifyIdents
) where

import Control.Monad.Identity
import Control.Monad.State
import qualified Data.Foldable as DF
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Traversable as DT
import Model.Report.Document


-- | Walk structures of type @b@ applying a function to every @a@ found within them.
class Walkable a b where
  walk :: (a -> a) -> b -> b
  walk f = runIdentity . walkM (return . f)
  walkM :: (Monad m) => (a -> m a) -> b -> m b
  query :: (Monoid c) => (a -> c) -> b -> c

instance {-# OVERLAPPABLE #-} (Foldable t, Traversable t, Walkable a b) => Walkable a (t b) where
 walk f  = DT.fmapDefault (walk f)
 walkM f = DT.mapM (walkM f)
 query f = DF.foldMap (query f)

instance Walkable Block Block where
  walkM f blk = walkBlock f blk >>= f
  query f blk = f blk <> queryBlock f blk

instance Walkable Inline Block where
  walkM = walkBlock
  query = queryBlock

instance {-# OVERLAPPING  #-} Walkable [Inline] Block where
  walkM = walkBlock
  query = queryBlock

instance Walkable Attr Block where
  walkM = walkBlock
  query = queryBlock

instance Walkable MatrixHeader Block where
  walkM = walkBlock
  query = queryBlock

instance Walkable Block Inline where
  walkM _ = return  -- No blocks inside inlines.
  query _ = mempty

instance Walkable Inline Inline where
  walkM f inline = walkInline f inline >>= f
  query f inline = f inline <> queryInline f inline

instance {-# OVERLAPPING  #-} Walkable [Inline] [Inline] where
  walkM f inlines = mapM (walkM f) inlines >>= f
  query f inlines = f inlines <> mconcat (map (queryInline f) inlines)

instance {-# OVERLAPPING  #-} Walkable [Inline] Inline where
  walkM = walkInline
  query = queryInline

instance Walkable Attr Inline where
  walkM = walkInline
  query = queryInline

instance Walkable MatrixHeader Inline where
  walkM = walkInline
  query = queryInline

instance Walkable Block Attr where
  walkM _ = return  -- No blocks inside attributes.
  query _ = mempty

instance Walkable Inline Attr where
  walkM _ = return  -- No inlines inside attributes.
  query _ = mempty

instance {-# OVERLAPPING  #-} Walkable [Inline] Attr where
  walkM _ = return
  query _ = mempty

instance Walkable Attr Attr where
  walkM f = f
  query f = f

instance Walkable MatrixHeader Attr where
  walkM _ = return  -- No matrix headers inside attributes.
  query _ = mempty

instance Walkable Block MatrixHeader where
  walkM _ = return  -- No blocks inside matrix headers.
  query _ = mempty

instance Walkable Inline MatrixHeader where
  walkM f (MatrixHeader nm cols) = MatrixHeader <$> walkM f nm <*> walkM f cols
  query f (MatrixHeader nm cols) = query f nm <> query f cols

instance {-# OVERLAPPING  #-} Walkable [Inline] MatrixHeader where
  walkM f (MatrixHeader nm cols) = MatrixHeader <$> walkM f nm <*> walkM f cols
  query f (MatrixHeader nm cols) = query f nm <> query f cols

instance Walkable Attr MatrixHeader where
  walkM f (MatrixHeader nm cols) = MatrixHeader <$> walkM f nm <*> walkM f cols
  query f (MatrixHeader nm cols) = query f nm <> query f cols

instance Walkable MatrixHeader MatrixHeader where
  walkM f = f
  query f = f


-- | Apply the function to each item in the block.
walkBlock :: (Monad m, Walkable a MatrixHeader, Walkable a Attr,
    Walkable a [Inline], Walkable a Block) =>
  (a -> m a) -> Block -> m Block
walkBlock f (Plain inlines) =
  Plain <$> walkM f inlines
walkBlock f (Para inlines) =
  Para <$> walkM f inlines
walkBlock f (OrderedList lbl blockss) =
  OrderedList lbl <$> walkM f blockss
walkBlock f (UnorderedList blockss) =
  UnorderedList <$> walkM f blockss
walkBlock f (DefinitionList pairs) =
  DefinitionList <$> forM pairs
    (\(inlines, blocks) -> (,) <$> walkM f inlines <*> walkM f blocks)
walkBlock f (Heading level attr inlines) =
  Heading level <$> walkM f attr <*> walkM f inlines
walkBlock f (Matrix attr caption headerss cells) =
  Matrix <$> walkM f attr <*> walkM f caption <*> walkM f headerss <*> walkM f cells
walkBlock f (Div attr blocks) =
  Div <$> walkM f attr <*> walkM f blocks
walkBlock f (Picture attr inlines alt size path) =
  Picture <$> walkM f attr <*> walkM f inlines <*> pure alt <*> pure size <*> pure path
walkBlock _ Null = return Null


-- | Apply the function to each item in the inline.
walkInline :: (Monad m, Walkable a [Inline], Walkable a Attr) => (a -> m a) -> Inline -> m Inline
walkInline _ x@Str {} = return x
walkInline f (Emph inlines) = Emph <$> walkM f inlines
walkInline f (Strong inlines) = Strong <$> walkM f inlines
walkInline _ Space = return Space
walkInline f (Link ident inlines) = Link ident <$> walkM f inlines
walkInline f (LinkOut url inlines) = LinkOut url <$> walkM f inlines
walkInline _ x@Icon {} = return x
walkInline f (Highlight clr inlines) = Highlight clr <$> walkM f inlines
walkInline f (Span attr inlines) = Span <$> walkM f attr <*> walkM f inlines


-- | Apply the query function to each item in the block.
queryBlock :: (Monoid c, Walkable a Block, Walkable a [Inline], Walkable a Attr,
    Walkable a MatrixHeader) =>
  (a -> c) -> Block -> c
queryBlock f (Plain inlines) = query f inlines
queryBlock f (Para inlines) = query f inlines
queryBlock f (OrderedList _ blocks) = query f blocks
queryBlock f (UnorderedList blocks) = query f blocks
queryBlock f (DefinitionList pairs) = DF.foldMap
    (\(inlines, blocks) -> query f inlines <> query f blocks)
    pairs
queryBlock f (Heading _ attr inlines) = query f attr <> query f inlines
queryBlock f (Matrix attr caption headers cells) =
  query f attr <> query f caption <> query f headers <> query f cells
queryBlock f (Div attr blocks) = query f attr <> query f blocks
queryBlock f (Picture attr caption _ _ _) = query f attr <> query f caption
queryBlock _ Null = mempty


-- | Apply the query function to each item in the inline.
queryInline :: (Monoid c, Walkable a [Inline], Walkable a Attr) => (a -> c) -> Inline -> c
queryInline _ Str {} = mempty
queryInline f (Emph inlines) = query f inlines
queryInline f (Strong inlines) = query f inlines
queryInline _ Space = mempty
queryInline f (Link _ inlines) = query f inlines
queryInline f (LinkOut _ inlines) = query f inlines
queryInline _ (Icon _) = mempty
queryInline f (Highlight _ inlines) = query f inlines
queryInline f (Span attr inlines) = query f attr <> query f inlines


-- | Find the highest priority (i.e. lowest score) of each cross reference identifier using
-- the monoid instance.
newtype PriorityIdents = PriorityIdents (Map Ident Int)

instance Semigroup PriorityIdents where
  PriorityIdents m1 <> PriorityIdents m2 = PriorityIdents $ M.unionWith min m1 m2

instance Monoid PriorityIdents where
  mempty = PriorityIdents mempty


-- | Remove duplicate labels from the blocks and return the blocks with the list of known labels.
-- If the same identifier appears in multiple places then the one with the lowest numerical
-- priority is used. In the case of a tie the earliest one is used.
--
-- The blocks are also scanned for internal links that do not have targets. These are removed.
unifyIdents :: [Block] -> ([Block], [Ident])
unifyIdents blocks = (walk doLinks $ evalState (walkM doAttrs blocks) labels, M.keys labels)
  where
    doLinks x@(Link ident inlines) = if M.member ident labels then x else Span noAttr inlines
    doLinks x = x
    doAttrs x@(Attr (Just (ident, priority1)) classes) =
      gets (M.lookup ident) >>= \case
        Nothing -> return $ Attr Nothing classes  -- Must have already seen this one.
        Just priority2 -> if priority1 == priority2
          then do
            modify $ clearLabel ident
            return x
          else return $ Attr Nothing classes  -- Higher priority somewhere else.
    doAttrs x@(Attr Nothing _) = return x
    PriorityIdents labels = query findLabels blocks
    findLabels = PriorityIdents . maybe mempty (uncurry M.singleton ) . attrIdentifier
    clearLabel = M.delete
