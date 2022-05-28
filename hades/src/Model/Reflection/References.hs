{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}


{- |

References are associations between entities. A reference is not the same as an arrow. An
arrow is an entity just like a box. Instead an arrow has potentially two references; one to the
box it comes from, and the box it goes to.

References are bidirectional: if entity Foo has a reference to Bar then Bar will also have
a reference to Foo.

References are typed by the entity types that they can relate, and the multiplicity of those
relationships.
-}

module Model.Reflection.References (
  RefType (RefType),
  refTypeSets,
  refTypeSingles,
  refTypeCoerce,
  refTypeReverse,
  refTypeOtherSide,
  RefTypeTable (..),
  refTypeTableCoerce,
  refTypeTableToList,
  refTypeTableFromList,
  refTypeTableContents,
  mkOneToOne,
  mkOneToMany,
  mkManyToMany,
  arrowRelations
) where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Model.Reflection.NamedRelation (Relation)
import qualified Model.Reflection.NamedRelation as NR
import Model.Reflection.Values


-- | A basic reference type has two sides. Each side is a set of variant names which can be
-- on one side of the reference, and a flag. If the flag is True then the reference can only
-- be instantiated with any given value on this side once. For instance, an arrow can only
-- point to one entity at a time, so the corresponding relationship for arrow type \"MyArrow\" that
-- can point to box type \"MyBox\" would look like this:
--
-- > RefType
-- >    [(S.singleton "MyArrow", S.singleton "MyBox")]
-- >    (False, True)
--
-- This shows that, given a @MyArrow@ entity, the other side of the relationship can only have zero
-- or one @MyBox@. However given a @MyBox@ entity there might be several @MyArrow@ entities
-- that point to it.
--
-- It is prohibited for the same variant to appear on both sides of a one-many relationship.
-- If it did then it would have a one-many relationship with some variants and a many-one
-- relationship with others.
--
-- The monoid instance isn't really safe. It merges the single flags using @(&&)@, so if they
-- conflict then inconsistencies will arise, and it can't defend against variants being introduced
-- on both sides of a one-many relationship. However as long as you use it for merging arrow heads
-- and tails this won't be a problem.
data RefType a = RefType {
    _refTypeSets :: [(Set (Variant a), Set (Variant a))],
    _refTypeSingles :: (Bool, Bool)
  } deriving (Eq, Show)

instance ToJSON (RefType a) where
  toJSON (RefType variants singles) = object [
      "sets" .= variants,
      "singles" .= singles
    ]

instance FromJSON (RefType a) where
  parseJSON = withObject "RefType definition" $ \v ->
    RefType <$> v .: "sets" <*> v .: "singles"

instance Semigroup (RefType a) where
  (RefType l1 (s1a, s1b)) <> (RefType l2 (s2a, s2b)) =
    RefType (l1 ++ l2) (s1a && s2a, s1b && s2b)

instance Monoid (RefType a) where
  mempty = RefType [] (True, True)


refTypeSets :: Lens
    (RefType a)
    (RefType b)
    [(Set (Variant a), Set (Variant a))]
    [(Set (Variant b), Set (Variant b))]
refTypeSets = lens _refTypeSets $ \t v -> t {_refTypeSets = v}


refTypeSingles :: Lens' (RefType a) (Bool, Bool)
refTypeSingles = lens _refTypeSingles $ \t v -> t {_refTypeSingles = v}


-- | Reverses the A and B sides of the type.
refTypeReverse :: RefType a -> RefType a
refTypeReverse (RefType prs singleFlags) = RefType (map revPair prs) (revPair singleFlags)
  where
    revPair (a, b) = (b, a)


-- | The type parameter for "RefType" is actually a phantom. Hence it can be changed
-- without a function mapping one to another.
refTypeCoerce :: RefType a -> RefType b
refTypeCoerce = refTypeSets . each . each %~ S.map (Variant . view variantName)


-- | Given a entity type name and a reference type, this returns the entity names that
-- can be linked by a reference, and a flag for the cardinality. @True@ indicates that
-- this variant can only be linked to a single instance of the returned variants.
refTypeOtherSide :: Variant a -> RefType a -> (Bool, Set (Variant a))
refTypeOtherSide v t = merge sideA sideB
  where
    merge (a, setA) (b, setB)
      | a /= b && S.null setA  = (b, setB)
      | a /= b && S.null setB  = (a, setA)
      | a == b                 = (a, S.union setA setB)
      | otherwise              = (b, setB)
          -- Should never happen, but if it does then take the A->B direction and ignore B->A.
    (singleA, singleB) = t ^. refTypeSingles
    sideA = (singleA, foldr f S.empty $ t ^. to refTypeReverse . refTypeSets)
    sideB = (singleB, foldr f S.empty $ t ^. refTypeSets)
    f (setA, setB) accum = if S.member v setA then S.union setB accum else accum


-- Reference types have the namespace of Named References.
newtype RefTypeTable a = RefTypeTable (Map Relation (RefType a))
    deriving Eq

instance ToJSON (RefTypeTable a) where
  toJSON (RefTypeTable tbl) = toJSON tbl

instance FromJSON (RefTypeTable a) where
  parseJSON v = RefTypeTable <$> parseJSON v

instance Semigroup (RefTypeTable a) where
  (RefTypeTable tbl1) <> (RefTypeTable tbl2) = RefTypeTable $ M.unionWith mappend tbl1 tbl2

instance Monoid (RefTypeTable a) where
  mempty = RefTypeTable mempty


-- | Change the type of a RefTypeTable.
refTypeTableCoerce :: RefTypeTable a -> RefTypeTable b
refTypeTableCoerce (RefTypeTable v) = RefTypeTable $ fmap refTypeCoerce v


refTypeTableToList :: RefTypeTable a -> [(Relation, RefType a)]
refTypeTableToList (RefTypeTable v) = M.toList v

refTypeTableFromList :: [(Relation, RefType a)] -> RefTypeTable a
refTypeTableFromList = RefTypeTable . M.fromList

refTypeTableContents :: Iso' (RefTypeTable a) [(Relation, RefType a)]
refTypeTableContents = iso refTypeTableToList refTypeTableFromList


-- | Create a reference type which is constrained to be 1 to 1.
mkOneToOne :: Relation -> [Variant a] -> [Variant a] -> RefTypeTable a
mkOneToOne rel v1 v2 = RefTypeTable $ M.singleton rel $
  RefType [(S.fromList v1, S.fromList v2)] (True, True)


-- | Create a reference type which is constrained to be 1 to many.
mkOneToMany :: Relation -> [Variant a] -> [Variant a] -> RefTypeTable a
mkOneToMany rel v1 v2 = RefTypeTable $ M.singleton rel $
  RefType [(S.fromList v1, S.fromList v2)] (True, False)


mkManyToMany :: Relation -> [Variant a] -> [Variant a] -> RefTypeTable a
mkManyToMany rel v1 v2 = RefTypeTable $ M.singleton rel $
  RefType [(S.fromList v1, S.fromList v2)] (False, False)


-- | Construct a RefTypeTable for an arrow variant.
arrowRelations ::
  Variant a   -- ^ The arrow variant.
  -> [Variant a]  -- ^ The list of things the tail can connect to.
  -> [Variant a]  -- ^ The list of things the head can connect to.
  -> RefTypeTable a
arrowRelations arrVariant tailList headList =
  mkOneToMany NR.edgeFromRelation tailList [arrVariant] <>
  mkOneToMany NR.edgeToRelation headList [arrVariant]
