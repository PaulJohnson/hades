{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}



{- |

-}

module Model.Query.Base (
  -- * Core abstractions
  QueryId,
  QueryStep (..),
  noStep,
  inStep,
  Query,
  compileQuery,
  runQuery,
  subQuery,
  step,
  filterStep,
  -- * Built in query steps.
  parentStep,
  childrenStep,
  filterVariants,
  followArrows
) where

import Control.Lens
import Data.Either
import Data.Graph.Inductive as G
import qualified Data.IntMap as IM
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as U
import Model.Abstract.PackageTree
import Model.Reflection.NamedRelation (Relation)
import qualified Model.Reflection.NamedRelation as NR
import Model.Reflection.Reflective
import Model.Reflection.Values

-- | The UUID of a query diagram in the model.
type QueryId = ModelId


-- | A query step uses a model to convert a set of incoming UUIDs from previous steps
-- into a set of outgoing UUIDs to be passed to succeeding steps. The function can also
-- return a list of text strings containing warning messages about the execution
-- of the query step. The first argument is a list of model IDs from outer queries in a matrix
-- or report. These are used in formulae to determine the complete set of visible extension values.
--
-- The monoid instance allows two or more sequential steps to be joined together into a single one.
--
-- Law: there is no cross-talk between set members in the @runStep@ function. Formally
--
-- >    union (fst $ runStep q m s1) (fst $ runStep q m s2) == fst (runStep q m (union s1 s2))
newtype QueryStep v = QueryStep {runStep ::
  [ModelId] -> Model v -> Set ModelId -> (Set ModelId, [Text])}

instance Semigroup (QueryStep v) where
  qs1 <> qs2 = QueryStep $ \evs m i ->
    let
      (r1, msgs1) = runStep qs1 evs m i
      (r2, msgs2) = runStep qs2 evs m r1
    in (r2, msgs1 ++ msgs2)

instance Monoid (QueryStep v) where
  mempty = QueryStep $ const $ const (, [])


-- | A step with no output. This is distinct from "mempty" which copies its inputs to its outputs.
-- Hence this is the unit of parallelisation while "mempty" is the unit of sequencing.
noStep :: QueryStep v
noStep = QueryStep $ const $ const $ const (mempty, [])


-- | Run the two arguments in parallel and unify their outputs.
inStep :: QueryStep v -> QueryStep v -> QueryStep v
inStep qs1 qs2 = QueryStep $ \m i -> runStep qs1 m i `mappend` runStep qs2 m i


-- | A query consists of a graph of query steps. Edges denote the flow of results between steps.
type Query v = Gr (QueryStep v) ()


-- | Query steps in the model are themselves indexed by UUID. However the inductive graph library
-- indexes things by integers. Hence a function is needed to convert one to the other.
--
-- A flow from the null UUID represents the query taking an incoming set of UUIDs from its
-- caller, and a flow to the null UUID represents the output flow from the query. The input
-- side becomes node 1 in the graph, and the output becomes node 2.
compileQuery :: [(UUID, QueryStep v)] -> [(UUID, UUID)] -> Query v
compileQuery ns es = mkGraph ns1 es1
  where
    zs = zip [3..] ns
    ns1 = (1, mempty) : (2, mempty) : map (\(n, (_, qs)) -> (n, qs)) zs
    tbl = M.fromList $ map (\(n, (u, _)) -> (u, n)) zs
    es1 = mapMaybe edgeLookup es
    edgeLookup (u1, u2) = do
      n1 <- if U.null u1 then Just 1 else M.lookup u1 tbl
      n2 <- if U.null u2 then Just 2 else M.lookup u2 tbl
      return (n1, n2, ())


-- | Execute a query in the context of a model. Returns a set of ModelIds and a list of any warning
-- messages (such as type mismatch errors) generated during the execution.
runQuery ::
    Query v   -- ^ The query to run.
    -> [ModelId]   -- ^ Any entities from outer queries in a report or matrix/.
    -> Model v    -- ^ The model to run the query against.
    -> Set ModelId   -- ^ The inputs to the query.
    -> (Set ModelId, [Text])
runQuery query outers model input =
    (S.unions $ map (IM.findWithDefault S.empty 2 . fst) stepList1, nub $ concatMap snd stepList1)
  -- Notionally the result is the fixpoint of nodeOuts. However sets evaluate strictly so
  -- instead we have to scan for the point where changes stop happening. Also as an optimisation
  -- we only inject the input once rather than at every iteration, and then take the union
  -- of the output from all iterations. This depends on the no-crosstalk law for QueryStep.
  where
    -- Iterate steps, starting with the input set at node 1.
    stepList = iterate (nodeOuts query) (IM.singleton 1 input, [])
    -- Find the point where the node results stop changing.
    stepList1 = head stepList :
        map snd (takeWhile (uncurry (/=)) $ zip stepList $ tail stepList)
    -- Calculate the output for each node given the union of its incoming sets.
    nodeOuts gr (tbl, _) = (IM.fromList results, concat newErrs)
      where
        outputs = map nodeOut $ labNodes gr
        results = map (\(n, (r, _)) -> (n, r)) outputs
        newErrs = map (\(_, (_, es)) -> es) outputs
        nodeOut (n, qs) =
          (n,
          runStep qs outers model $ S.unions $
            map (\n1 -> IM.findWithDefault S.empty n1 tbl) $ G.pre gr n)


-- -- Convert a query into a query step so that it can be used within another query.
subQuery :: Query v -> QueryStep v
subQuery = QueryStep . runQuery


-- | Follow a set of relations in the model to a new set of entities.
step :: (EntityClass v) => Set Relation -> QueryStep v
step rels = QueryStep $ \_ model input ->
  let nexts (uuid, rel) = NR.relation uuid rel $ modelRelations model
  in (S.unions $ map nexts $ cartesian input rels, [])
  where
    cartesian sx sy = [(x, y) | x <- S.toList sx, y <- S.toList sy]


-- | Restrict the output to those entities that meet the predicate. Entities that produce
-- error messages will also be blocked.
filterStep :: (EntityClass v) => (ExtensionValues -> Entity v -> Either Text Bool) -> QueryStep v
filterStep p = QueryStep $ \outers model inputs ->
    let
      outerValues = mconcat $ map (view $ entityVariantProperties model) $
        mapMaybe (\uuid -> M.lookup uuid $ modelContents model) outers
      (msgs, results) = partitionEithers $ map (test outerValues model) $ S.toList inputs
    in (S.fromList $ catMaybes results, msgs)
  where
    test outers model uuid = case M.lookup uuid $ modelContents model of
      Just e -> case p outers e of
        Left msg -> Left $ e ^. entityName . nameText <> ": " <> msg
        Right True -> Right $ Just uuid
        Right False -> Right Nothing
      Nothing -> Left $ "Internal error: UUID " <> T.pack (show uuid) <> " not found in model."


-- | Move to the parents of the inputs.
parentStep :: (EntityClass v) => QueryStep v
parentStep = QueryStep $ \_ model inputs ->
    (S.fromList $ mapMaybe (p model) $ S.toList inputs, [])
  where
    p m uuid = view entityParent <$> M.lookup uuid (modelContents m)


-- | Move to the children of the inputs.
childrenStep :: (EntityClass v) => QueryStep v
childrenStep = QueryStep $ \_ model inputs ->
    (S.fromList $ concatMap (c model) $ S.toList inputs, [])
  where
    c m uuid = maybe [] (M.elems . view entityChildren) $ M.lookup uuid (modelContents m)


-- | Restrict the output to these variants.
filterVariants :: (EntityClass v, Reflective v) => Set (Variant v) -> QueryStep v
filterVariants vars = filterStep $ \_ e ->
  Right $ S.member (e ^. entityContents . to reflectiveName) vars


-- | Follow model arrows forwards. To follow them backwards reverse the head and tail connections.
--
-- If the set of variants includes boxes then this can also move from an arrow backwards
-- across the box to a previous arrow.
followArrows :: (EntityClass v, Reflective v) =>
  Set (Variant v)  -- ^ The arrow variants to follow.
  -> Relation   -- ^ Name of relation for tail connection of arrow.
  -> Relation   -- ^ Name of relation for head connection of arrow.
  -> QueryStep v
followArrows variants rel1 rel2 = mconcat [
    step (S.singleton rel1),
    filterVariants variants,
    step (S.singleton rel2)
  ]
