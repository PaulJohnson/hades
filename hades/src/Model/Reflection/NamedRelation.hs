{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}


{- |

Named Relations store named pair-wise associations between items. Internally they are stored
as a map of sets of (item, name) pairs. When a new association is inserted between two items the
association is inserted twice, once for each direction.

This module is intended to be imported qualified.
-}

module Model.Reflection.NamedRelation (
  Relation,
  edgeFromRelation,
  edgeToRelation,
  NamedRelation (),
  empty,
  insert,
  delete,
  deleteRelation,
  deleteAll,
  member,
  relation,
  relations,
  union,
  intersection,
  check,
  toList,
  fromList,
  renameRelation
) where

import Control.Monad.Except
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V


-- | Relationships are identified by name.
type Relation = Text


-- | Standard name for a relationship formed when an arrow tail is connected.
edgeFromRelation :: Relation
edgeFromRelation = "Arrow Tail"


-- | Standard name for a relationship formed when an arrow head is connected.
edgeToRelation :: Relation
edgeToRelation = "Arrow Head"


-- | A NamedRelation stores the existence of named relationships between pairs of entities.
-- Each relationship has a name, so two entities can have more than one relationship between them.
newtype NamedRelation a = NamedRelation (Map a (Set (a, Relation))) deriving Eq

instance (Ord a, ToJSON a) => ToJSON (NamedRelation a) where
  toJSON = toJSON . toList

instance (Ord a, FromJSON a) => FromJSON (NamedRelation a) where
  parseJSON = withArray "named relations" $ \v -> fromList <$> mapM parseJSON (V.toList v)


-- | Empty relation.
empty :: (Ord a) => NamedRelation a
empty = NamedRelation M.empty


-- | Record the association between these entities. If the association already exists
-- then no change is made.
insert :: (Ord a) => a -> a -> Relation -> NamedRelation a -> NamedRelation a
insert v1 v2 txt (NamedRelation er) = NamedRelation $ addItem v1 v2 $ addItem v2 v1 er
  where
    addItem v1' v2' = M.insertWith S.union v1' (S.singleton (v2', txt))


-- | Delete the association between these entities. No-op if it doesn't exist.
delete :: (Ord a) => a -> a -> Relation -> NamedRelation a -> NamedRelation a
delete v1 v2 txt (NamedRelation er) = NamedRelation $ remove v1 v2 $ remove v2 v1 er
  where
    remove v1' v2' = M.update (remove2 v2') v1'
    remove2 v s = let ns = S.delete (v, txt) s in if S.null ns then Nothing else Just ns


-- | Delete any associations this entity has via this relation.
deleteRelation :: (Ord a) => a -> Relation -> NamedRelation a -> NamedRelation a
deleteRelation v1 r nr = foldr (\v2 -> delete v1 v2 r) nr (relation v1 r nr)


-- | Delete all associations involving this entity. No-op if there are none.
deleteAll :: (Ord a) => a -> NamedRelation a -> NamedRelation a
deleteAll v1 r = foldr (uncurry $ delete v1) r (relations v1 r)


-- | Does the relation contain this association?
member :: (Ord a) => a -> a -> Relation -> NamedRelation a -> Bool
member v1 v2 txt = S.member (v2, txt) . relations v1


-- | The set of entities that the argument is linked to by the specified relationship.
relation :: (Ord a) => a -> Relation -> NamedRelation a -> Set a
relation v r (NamedRelation nr) =
  S.map fst $ S.filter ((== r) . snd) $ M.findWithDefault S.empty v nr


-- | The set of all relations that the argument has.
relations :: (Ord a) => a -> NamedRelation a -> Set (a, Relation)
relations v (NamedRelation nr) = M.findWithDefault S.empty v nr


-- | The union of the relations.
union :: (Ord a) => NamedRelation a -> NamedRelation a -> NamedRelation a
union (NamedRelation r1) (NamedRelation r2) = NamedRelation $ M.unionWith S.union r1 r2


-- | The intersection of the relations.
intersection :: (Ord a) => NamedRelation a -> NamedRelation a -> NamedRelation a
intersection (NamedRelation r1) (NamedRelation r2) =
  NamedRelation $ clean $ M.intersectionWith S.intersection r1 r2


-- | Verify that the NamedRelation is properly symmetric, so for every relation @a -> (b, name)@
-- there is an equivalent relation @b ->(a, name)@. Throws an error giving some clue if a
-- problem is found.
check :: (MonadError Text m, Ord a, Show a) => NamedRelation a -> m ()
check r = unless (null results) $ throwError $ T.intercalate "\n" results
  where
    results = mapMaybe checkPair prs
    prs = concatMap (\(v1, rs) -> map (\(v2, txt) -> (v1, v2, txt)) rs) $ toList r
    checkPair (v1, v2, txt) =
      if member v2 v1 txt r
        then Nothing
        else Just $ T.pack $ show v1 ++ " >-" ++ T.unpack txt ++ "->" ++ show v2 ++
            " has no converse."


-- | Internal utility to remove any empty sets.
clean :: (Ord a) => Map a (Set (a, Relation)) -> Map a (Set (a, Relation))
clean = M.filter (not . S.null)


-- | Convert the structure to a compact list. Symmetric relations are not included.
toList :: (Ord a) => NamedRelation a -> [(a, [(a, Relation)])]
toList (NamedRelation r) = M.toList $ M.mapWithKey (\k -> filter ((>= k) . fst) . S.toList) r


-- | Convert a list into a "NamedRelation". Convers of "toList".
fromList :: (Ord a) => [(a, [(a, Relation)])] -> NamedRelation a
fromList inputs = foldr (\(v1, v2, txt) r -> insert v1 v2 txt r) empty prs
  where
    prs = concatMap (\(v1, rs) -> map (\(v2, txt) -> (v1, v2, txt)) rs) inputs


-- | Change all instances of the first relation to the second.
renameRelation :: (Ord a) => Relation -> Relation -> NamedRelation a -> NamedRelation a
renameRelation oldRel newRel nr = fromList $ map changeName $ toList nr
  where
    changeName (v1, rels) = (v1, map changeName2 rels)
    changeName2 (v2, nm) = if nm == oldRel then (v2, newRel) else (v2, nm)
