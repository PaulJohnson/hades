{-
Copyright © Paul Johnson 2021. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

{- |
A TreeModel is a way to represent a mutable forest of data. Nodes can be created, edited, moved and
deleted by reference to unique IDs.

This module is designed to be imported qualified.
-}

module Data.ForestModel (
   ModelIndex,
   rootIndex,
   Model,
   emptyModel,
   makeModel,
   modelIso,
   Edit,
   runEdit,
   evalEdit,
   execEdit,
   mapWithIndex,
   checkIndex,
   isDescendantOf,
   isStrictDescendantOf,
   getItem,
   getItem1,
   getTree,
   getTree1,
   getForest,
   getTreeIndices,
   getTreeIndices1,
   getForestIndices,
   Insertion (..),
   Data.ForestModel.insert,
   Data.ForestModel.delete,
   move,
   stableUpdate
) where

import Control.Lens hiding (children)
import Control.Monad.State
import Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Tree as T
import Data.Maybe


-- | The model is indexed by integers.
type ModelIndex = Int

-- | A flag value for root. All other indices are @> rootIndex@.
rootIndex :: Int
rootIndex = -1


-- | A tree node within the model.
data Node a = Node {
      _parent :: ModelIndex,
      _children :: [ModelIndex],
      _value :: a
   } deriving (Functor, Eq)


parent :: Lens' (Node a) ModelIndex
parent = lens _parent $ \s x -> s{_parent = x}

children :: Lens' (Node a) [ModelIndex]
children = lens _children $ \s x -> s{_children = x}

value :: Lens (Node a) (Node b) a b
value = lens _value $ \s x -> s{_value = x}

-- | A tree represented as a graph. Two models are considered equal if they contain the same
-- underlying forest, even if the indices to the two forests are different.
data Model a = Model {
      _modelTop :: ModelIndex,  -- ^ Source of new indices.
      _modelRoots :: [ModelIndex],
      _modelStore :: Map ModelIndex (Node a)
   } deriving (Functor)

instance (Eq a) => Eq (Model a) where
   m1 == m2 = evalEdit getForest m1 == evalEdit getForest m2

type instance Index (Model a) = ModelIndex
type instance IxValue (Model a) = a

instance Ixed (Model a) where
   ix k f m = case m ^? modelStore . ix k . value of
      Just v -> f v <&> \v' -> modelStore . ix k . value .~ v' $ m
      Nothing -> pure m

modelTop :: Lens' (Model a) ModelIndex
modelTop = lens _modelTop $ \s x -> s{_modelTop = x}

modelRoots :: Lens' (Model a) [ModelIndex]
modelRoots = lens _modelRoots $ \s x -> s{_modelRoots = x}

modelStore :: Lens (Model a) (Model b) (Map ModelIndex (Node a)) (Map ModelIndex (Node b))
modelStore = lens _modelStore $ \s x -> s{_modelStore = x}


-- | Equivalence of models and forests.
modelIso :: Iso (T.Forest a) (T.Forest b) (Model a) (Model b)
modelIso = iso makeModel $ evalEdit getForest

-- | A model containing no nodes.
emptyModel :: Model a
emptyModel = Model 0 [] mempty

newtype Edit a b = Edit {getEdit :: State (Model a) b}
   deriving (Functor, Applicative, Monad)

instance MonadState (Model a) (Edit a) where
   get = Edit get
   put v = Edit $ put v
   state f = Edit $ state f


runEdit :: Edit a b -> Model a -> (b, Model a)
runEdit = runState . getEdit

evalEdit :: Edit a b -> Model a -> b
evalEdit = evalState . getEdit

execEdit :: Edit a b -> Model a -> Model a
execEdit = execState . getEdit


-- | Get a new locally unique index for an item.
newIndex :: Edit a ModelIndex
newIndex = Edit $ do
   modelTop += 1
   gets _modelTop


-- | Convert a traditional Forest into a Model.
makeModel :: T.Forest a -> Model a
makeModel f = flip execEdit emptyModel $ do
      roots <- mapM (goTree rootIndex) f
      modelRoots .= roots
   where
      goTree p (T.Node v cs) = do
         idx <- newIndex
         cs1 <- mapM (goTree idx) cs
         modelStore %= M.insert idx (Node p cs1 v)
         return idx


-- | Map contents with the node indices included.
mapWithIndex :: (ModelIndex -> a -> b) -> Model a -> Model b
mapWithIndex f = modelStore %~ M.mapWithKey (\i n -> value %~ f i $ n)


-- | Return true iff the index is valid in the model.
checkIndex :: ModelIndex -> Edit a Bool
checkIndex i = M.member i <$> use modelStore


-- | Test if the first argument is a descendant (i.e. direct or indirect child) of the second.
--
-- @False@ if either argument is not a valid index.
--
-- If and only if @x@ is a valid index then @isDescendantOf x x@ returns @True@.
isDescendantOf :: ModelIndex -> ModelIndex -> Edit a Bool
isDescendantOf x1 x2 = do
   cs <- if x2 == rootIndex
      then use $ modelStore . to M.keys . to Just
      else fmap T.flatten <$> getTreeIndices1 x2
   return $ x1 `elem` fromMaybe [] cs


-- | Test if the second argument is a strict descendant of the first.
--
-- @isStrictDescendantOf x x@ returns @False@.
isStrictDescendantOf :: ModelIndex -> ModelIndex -> Edit a Bool
isStrictDescendantOf x1 x2 = if x1 == x2 then return False else isDescendantOf x1 x2


-- | Extract a value from the tree. This is a partial function: if the @ModelIndex@ is not found
-- then an error will be thrown.
getItem :: ModelIndex -> Edit a a
getItem i = use (modelStore . at i) >>= \case
   Just n -> return $ n ^. value
   Nothing -> error $ "ForestModel.getItem: index " <> show i <> " not found."


-- | Safe version of 'getItem'
getItem1 :: ModelIndex -> Edit a (Maybe a)
getItem1 i = do
   fmap (view value) <$> use (modelStore . at i)


-- | Extract a tree from the Model. This is a partial function: if the @ModelIndex@ is not found
-- then an error will be thrown.
getTree :: ModelIndex -> Edit a (T.Tree a)
getTree i = getTree1 i >>= \case
   Just n -> return n
   Nothing -> error $ "ForestModel.getTree: index " <> show i <> " not found."


-- | Safe version of 'getTree'.
getTree1 :: ModelIndex -> Edit a (Maybe (T.Tree a))
getTree1 i = do
   use (modelStore . at i) >>= \case
      Just n -> do
         cs <- catMaybes <$> mapM getTree1 (n ^. children)
         return $ Just $ T.Node (n ^. value) cs
      Nothing -> return Nothing


-- | Extract the entire forest from the Model.
getForest :: Edit a (T.Forest a)
getForest = do
   rs <- use modelRoots
   mapM getTree rs


-- | Extract a tree of indices from the model. This is a partial function: if the @ModelIndex@ is
-- not found then an error will be thrown. Also fails for @rootIndex@.
getTreeIndices :: ModelIndex -> Edit a (T.Tree ModelIndex)
getTreeIndices i = getTreeIndices1 i >>= \case
   Just n -> return n
   Nothing -> error $ "ForestModel.getTreeIndices: index " <> show i <> " not found."

-- | Safe version of 'getTreeIndices'
getTreeIndices1 :: ModelIndex -> Edit a (Maybe (T.Tree ModelIndex))
getTreeIndices1 i = do
   use (modelStore . at i) >>= \case
      Just n -> do
         cs <- catMaybes <$> mapM getTreeIndices1 (n ^. children)
         return $ Just $ T.Node i cs
      Nothing -> return Nothing

-- | Extract the forest of indexes from the model.
getForestIndices :: Edit a (T.Forest ModelIndex)
getForestIndices = do
      rs <- use modelRoots
      mapM go rs
   where
      go i = use (modelStore . at i) >>= \case
         Just n -> do
            cs <- mapM go $ n ^. children
            return $ T.Node i cs
         Nothing -> error $ "ForestModel.getForestIndicies: index " <> show i <> " not found."


-- | Relative positions for the targets of tree insertion and move operations.
--
-- @InsertChild@ and @AppendChild@ can be targetted at the 'rootIndex'. The others will fail.
data Insertion =
   InsertChild     -- ^ Create a new node as the first child of the index.
   | AppendChild   -- ^ Create a new node as the last child of the index.
   | InsertBefore  -- ^ Create a new node as a sibling of the given index immediately before it.
   | InsertAfter   -- ^ Create a new node as a sibling of the given index immediately after it.
   deriving (Eq, Ord, Read, Show, Enum, Bounded)


-- | Internal function for modifying a model by inserting a node. The 'Edit' action needs
-- access to the internals of the model; hence this is not exported. If the insertion succeeds
-- then the it returns the index of the adopted node. The 'Edit' action will only be run if
-- the adoption is successful.
privateAdopt ::
   Insertion
   -> ModelIndex
   -> Edit a ModelIndex
      -- ^ This must place the new node in the model and return its index, but the node must not
      -- be spliced into the tree structure (i.e. it must not appear in a 'children' list).
      -- Its 'parent' value will be overwritten.
   -> Edit a (Maybe ModelIndex)
privateAdopt ins idx src
   | idx == rootIndex  = do
      newIdx <- src2 idx
      case ins of
         InsertChild -> modelRoots %= (newIdx :)
         InsertBefore -> modelRoots %= (newIdx :)
         AppendChild -> modelRoots %= (++ [newIdx])
         InsertAfter -> modelRoots %= (++ [newIdx])
      return $ Just newIdx
   | otherwise  = do
      st <- use modelStore
      case (ins, st ^? ix idx) of
         (InsertChild, Just _) -> do
            newIdx <- src2 idx
            modelStore . ix idx . children %= (newIdx :)
            return $ Just newIdx
         (AppendChild, Just _) -> do
            newIdx <- src2 idx
            modelStore . ix idx . children %= (++[newIdx])
            return $ Just newIdx
         (InsertBefore, Just node) ->
            if M.member (node ^. parent) st
               then do
                  newIdx <- src2 $ node ^. parent
                  modelStore . ix (node ^. parent) . children %= \xs ->
                        let (xs1, xs2) = break (== idx) xs
                        in xs1 ++ newIdx : xs2
                  return $ Just newIdx
               else if node ^. parent == rootIndex
                  then do
                     newIdx <- src2 $ node ^. parent
                     modelRoots %= \xs ->
                        let (xs1, xs2) = break (== idx) xs
                        in xs1 ++ newIdx : xs2
                     return $ Just newIdx
                  else return Nothing
         (InsertAfter, Just node) -> do
            if M.member (node ^. parent) st
               then do
                  newIdx <- src2 $ node ^. parent
                  modelStore . ix (node ^. parent) . children %= \xs ->
                     case break (== idx) xs of
                        (xs1, x : xs2) -> xs1 ++ x : newIdx : xs2
                        (xs1, []) -> xs1 ++ [newIdx]
                  return $ Just newIdx
               else if node ^. parent == rootIndex
                  then do
                     newIdx <- src2 $ node ^. parent
                     modelRoots %= \xs ->
                        case break (== idx) xs of
                           (xs1, x : xs2) -> xs1 ++ x : newIdx : xs2
                           (xs1, []) -> xs1 ++ [newIdx]
                     return $ Just newIdx
                  else return Nothing
         _ -> return Nothing
   where
      src2 p = do
         newIdx <- src
         modelStore . ix newIdx . parent .= p  -- Set parent of the new node.
         return newIdx


-- | Internal function for modifying a model by unlinking a node from the
-- tree while leaving it in the model store.
--
-- If it succeeds then it returns @True@, otherwise @False@.
privateOrphan :: ModelIndex -> Edit a Bool
privateOrphan idx = do
   st <- use modelStore
   use (modelStore . at idx) >>= \case
      Nothing -> return False
      Just node -> if node ^. parent == rootIndex
         then do
            modelRoots %= L.delete idx
            return True
         else if M.member (node ^. parent) st
            then do
               modelStore . ix (node ^. parent) . children %= L.delete idx
               return True
            else
               return False


-- | Insert a new value into the tree. Returns the index of the new element if it is successful.
--
-- As a special case, where the @ModelIndex@ equals @rootIndex@ the @InsertBefore@ and
-- @InsertAfter@ are equivalent to @InsertChild@ and @AppendChild@ respectively.
insert :: Insertion -> ModelIndex -> a -> Edit a (Maybe ModelIndex)
insert ins idx v = privateAdopt ins idx $ do
         i <- newIndex
         modelStore . at i .= Just (Node 0 [] v)
         return i


-- | Delete a node from the tree, including all its children.
delete :: ModelIndex -> Edit a Bool
delete idx = do
      ok <- privateOrphan idx
      when ok $ do
         ns <- getTreeIndices idx  -- Safe because privateOrphan already checked it exists.
         delTree ns
      return ok
   where
      delTree (T.Node i cs) = do
         modelStore . at i .= Nothing
         mapM_ delTree cs


-- | Move a node from one place in the tree to another. Returns @True@ if the move was successful.
--
-- A node may not be moved to become a child of itself. The @Insertion@ follows the same logic
-- as 'insert'.
move ::
   ModelIndex  -- ^ The node to be moved.
   -> Insertion
   -> ModelIndex  -- ^ The new location.
   -> Edit a Bool
move i ins target =
   target `isDescendantOf` i >>= \case
      True -> return False
      False -> do
         oldState <- get
         step1 <- privateOrphan i
         if step1
            then do
               step2 <- privateAdopt ins target $ return i
               if step1 && isJust step2
                  then return True
                  else do
                     put oldState
                     return False
            else do
               put oldState
               return False


-- | Update the model from a forest of the existing indices. Any branch with indices that are not
-- found in the model will be silently pruned. Any indices not used in the new tree will be
-- deleted.
stableUpdate :: T.Forest ModelIndex -> Model a -> Model a
stableUpdate frst old = Model (old ^. modelTop) newRoots newStore
   where
      (newRoots, newStore) = walkForest rootIndex frst
      walkForest p idxs =
         let (idxs1, tbls) = unzip $ mapMaybe (walkTree p) idxs
         in (idxs1, mconcat tbls)
      walkTree p (T.Node i cs) = (old ^. modelStore . at i) <&> \node ->
         let (cs1, tbl) = walkForest i cs
         in (i, M.insert i (Node p cs1 $ node ^. value) tbl)
