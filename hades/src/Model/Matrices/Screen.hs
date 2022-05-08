{-# LANGUAGE Arrows #-}  -- For the benefit of HLint

{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

{- |
-}

module Model.Matrices.Screen (
   MatrixGadget,
   getMatrixWrapper,
   matrixDisplay
) where

import Control.Arrow
import Control.Lens hiding ((.=))
import Control.Monad.Trans.Class
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree
import qualified Data.UUID as U
import Model.Abstract.DiagramType
import Model.Abstract.PackageTree
import Model.Abstract.Properties
import Model.Lookups.Base
import Model.Matrices.Base
import Model.Reflection.Dialogs
import Model.Reflection.Reflective
import Model.Reflection.Types
import Model.Reflection.Values
import Model.Query.Base
import Model.Query.Diagram
import Model.Reflection.Parser
import Reactive.Banana.ArrowDialog
import Reactive.Banana.Common
import qualified Text.Megaparsec as MP


-- | A Matrix takes inputs of type @i@, but does not output changes to something downstream.
-- Instead it emits model edit scripts via the side channel. The output value is ignored, and
-- is only a function because that is the type required by the "forestEdit" gadget.
type MatrixGadget p v i = GadgetF' (Model v) (ModelScript p v v (Maybe Text)) i


-- | If the entity is a matrix then return a wrapper for viewing the matrix.
getMatrixWrapper :: (Editable p v, EntityClass v, HasLookups v, HasMatrices v) =>
   Entity v -> ModelEdit v v (Maybe (EntityWrapper p v))
getMatrixWrapper ent =
   case ent ^? entityContents . _Matrix of
      Nothing -> return Nothing
      Just m -> return $ Just ViewWrapper {
            wrappedEntityId = entityId ent,
            viewTag = "run",
            viewInput = Left (),
            viewInitial = m ^. matrixDefaultInput,
            viewGadget = matrixDisplay $ entityId ent
         }


{- | Display the matrix results.

The @Set ModelId@ is the set of entities used as input to the matrix query. It is input and output
so that it can be preserved when the matrix is re-run. The output is a function which can be safely
applied to the model contents.
-}
matrixDisplay :: (Editable p v, EntityClass v, HasLookups v, HasMatrices v) =>
   MatrixId
   -> Gadget' (Model v) (ModelScript p v v (Maybe Text)) (Set ModelId, ())
matrixDisplay mId = getInitialEnv $ \model1 ->
   case model1 ^? to modelContents . ix mId . entityContents . _Matrix of
      Nothing -> second (arr $ const ()) <<< message1 "The matrix has been deleted."
      Just matrix ->
         let
            headings = matrixTable model1 matrix
         in case compileMatrix matrix model1 of
               Left errs -> second (arr (const ())) <<< errorDisplay errs
               Right queries -> proc (input1, _) -> do
                  input2 <- simpleFrame "Matrix inputs" $
                        modelSubsetSpecifier (variantOk matrix) (const Nothing) -< input1
                  _ <- exec (\input ->
                     let (forest, warns) = getForest queries (sorts matrix) input model1
                     in proc () -> do
                        _ <- cond [
                                 PrismaticGadget () _Empty $ arr id,
                                 PrismaticGadget [] id $
                                    readOnlyMemo MemoSmall False (const $ T.intercalate "\n")
                              ] -< warns
                        sendMap updateScript $
                              forestTable headings (const forest) <<< arr modelContents <<< getEnv
                           -< ()
                     ) -< (input2, ())
                  returnA -< (input2, ())
   where
      sorts matrix = map (view groupSort) $ matrix ^. matrixGroups
      variantOk matrix model =
         let variantSet = matrixInputSet matrix model
         in \v -> S.member (v ^. entityContents . to reflectiveName) variantSet
      errorDisplay errs = simpleFrame "Matrix Errors" $
               readOnlyMemo MemoMedium True $ const $ const $ T.intercalate "\n" errs
      updateScript (Left v) = Just v
      updateScript _ = Nothing  -- Individual widgets are responsible for their own edit events.


-- | Convert the matrix parameters into a table of entities and a forest of lenses to
-- access them. The lenses output a threeple containing:
--
-- * The ID of the current entity and all the entities to its left.
--
-- * The editable fields of the current entity.
--
-- * The union of all the fields (including stamps) for the current entity and its ancestors
-- in the tree. This is read-only and is ignored for updates.
getForest :: (EntityClass v) =>
   [Query v]
   -> [SortOrder]  -- ^ Sort criteria for each level in the forest.
   -> Set ModelId   -- ^ Input to the query list.
   -> Model v    -- ^ The static model used to derive the matrix.
   -> (Forest (
         ModelId,
         Lens1 (Map ModelId (Entity v)) ([ModelId], ExtensionValues, ExtensionValues)),
       [Text])
getForest queries sorts inputs model = (map (fmap makeLens) ancestors, warnings)
   where
      ancestors = ancestorize $ map (fmap entityId) entForest  -- sorted version
      -- entForest :: Forest (Entity v)
      entForest =
         sortForest sorts $
         forestCatMaybe $ fmap (fmap (\u -> M.lookup u $ modelContents model)) idForest
      -- makeLens :: [ModelId] ->
      --   (ModelId, Lens1 (Map ModelId (Entity v)) (ModelId, ExtensionValues, ExtensionValues))
      makeLens ids = (
            fromMaybe U.nil $ listToMaybe ids,
            Lens1 $ lens (extract1 ids) (update ids))
         where
            extract1 (i:_) store =
               let
                  f i1 evs =
                     M.union evs $
                     fromMaybe mempty $
                     store ^? ix i1 . entityVariantProperties model
                  anc = foldr f mempty ids
               in case store ^? ix i of
                     Just ent ->
                        (ids,
                         (ent ^. entityVariantProperties model) `M.union` entityStampsAsFields ent,
                         entityStampsAsFields ent `M.union` anc)
                     Nothing -> (ids, mempty, anc)
            extract1 [] _ = ([], mempty, mempty)
            update (i:_) store (_, vs, _) =
               ix i . entityVariantProperties model .~ vs $ store
            update _ store _ = store
      (idForest, warnings) = runMatrix queries model inputs

{- Design note:

makeLens.extract1 gets called for every cell every time the model is updated. On large matrices
this makes it slow. However I can't see an easy way of factoring this out.

Also makelens.update appears to be dead code, which is not surprising because updates are done
via the sent data side channel rather than the lens.
-}


-- | Replace every node value with a list of the value and its ancestors. The head of each list
-- is the origial value followed by the ancestors going up to the root as the last item.
ancestorize :: Forest a -> Forest [a]
ancestorize = ancestorizeForest []
   where
      ancestorizeTree :: [v] -> Tree v -> Tree [v]
      ancestorizeTree ancestors (Node v cs) =
         let allVals = v : ancestors in Node allVals $ ancestorizeForest allVals cs
      ancestorizeForest :: [v] -> Forest v -> Forest [v]
      ancestorizeForest ancestors = map $ ancestorizeTree ancestors


-- | Generate the table groups for displaying the matrix.
matrixTable :: (Editable p v, EntityClass v, HasLookups v, Reflective v, Queryable v) =>
   Model v
   -> Matrix
   -> [(Text, [(Text, MatrixGadget p v ([ModelId], ExtensionValues, ExtensionValues) )] )]
matrixTable model matrix = mapMaybe mkGroup $ matrix ^. matrixGroups
   where
      mkGroup grp =
         case modelContents model ^? ix (grp ^. groupQuery) . entityName . nameText of
            Just txt -> Just (txt, map (columnTableField model) $ grp ^. groupColumns)
            Nothing -> Nothing


-- | Generate a table column corresponding to a matrix column.
columnTableField :: (Editable p v, EntityClass v, HasLookups v, Reflective v, Queryable v) =>
   Model v
   -> MatrixColumn
   -> (Text, MatrixGadget p v ([ModelId], ExtensionValues, ExtensionValues))
columnTableField model col = (col ^. matrixColumnName, g)
   where
      allFields = withStampFields $ withBuiltIn $ modelFields model
      g = case col ^. matrixColumnData of
         FieldColumn (FieldColumnData dataField linkField) ->
            case M.lookup dataField allFields of
               Nothing -> arr (const id) <<< readOnlyMemo
                     MemoSmall
                     False
                     (const $ const "Unknown or deleted field.")
               Just fType -> proc (mIds, values, _) -> do
                  let mId = fromMaybe U.nil $ listToMaybe mIds
                  _ <- fieldColumn fType linkField -< (mId, values)
                  returnA -< id
         ExprColumn isMemo expr ->
            let
               inner = case MP.parse (topExpr entityFunctions allFields) "" expr of
                  Right parsed ->
                     let textFunc vs = either id displayValue $ evaluate parsed vs model
                     in if isMemo
                           then readOnlyMemo MemoSmall False $ const textFunc
                           else readOnlyText $ const textFunc
                  Left bundle -> readOnlyMemo MemoSmall False $ const $ const $
                        T.pack $ MP.parseErrorTextPretty $ NE.head $ MP.bundleErrors bundle
            in arr (const id) <<< inner <<< arr (view _3)
         QueryColumn queryId -> proc (mIds, _, _) -> do
            _ <- (getInitial $ \mIds1 -> case queryCell queryId mIds1 of
                     Left msgs -> readOnlyMemo MemoSmall False $
                           const $ const $ T.intercalate "\n" msgs
                     Right cell -> cell)
               -< mIds
            returnA -< id
         LookupColumn tableId ->
            let
               inner = case lookupCell tableId of
                     Just cell -> cell
                     Nothing -> focusing id $ readOnlyText $ const $ const "Invalid lookup"
            in arr (const id) <<< inner <<< arr (view _3)
      queryCell :: (Editable p v, EntityClass v) =>
         ModelId
         -> [ModelId]
         -> Either [Text] (Gadget' (Model v) (ModelScript p v v (Maybe Text)) [ModelId])
      queryCell queryId modelIds = do -- Either monad.
         queryEnt <-
            maybe (Left ["No such query"]) Right $ M.lookup queryId (modelContents model)
         qd <-
            maybe (Left [queryEnt ^. entityName . nameText <> " is not a query"]) Right $
                  queryEnt ^? entityContents . _QueryDiagram . queryDiagram
         query <- compileQueryDiagram model qd
         return $ getInitial $ \mIds ->
            let
               mId = maybe mempty S.singleton (listToMaybe mIds)
               resultSet = fst $ runQuery query modelIds model mId
            in proc mId1 -> do
               _ <- clickableEntitySet -< resultSet
               returnA -< mId1
      lookupCell :: ModelId -> Maybe (GadgetF' e w ExtensionValues)
      lookupCell tableId = do  -- Maybe monad
         tbl <- modelContents model ^? ix tableId . entityContents . _LookupTable
         output <- M.lookup (tbl ^. tableOutput) $ modelFields model
         return $
            focusing (lens (runLookup tbl) const) $  -- Read-only so lens setter unused.
            addFieldDecoration (output ^. fieldType) $
            readOnlyText $ const displayValue


-- | Create a table column for a field in an entity.
fieldColumn :: (EntityClass v) =>
   Field  -- ^ Extension field .
   -> Maybe FieldId  -- ^ Link field
   -> MatrixGadget p v (ModelId, ExtensionValues)
fieldColumn typ linkField = proc (mId, vs) -> do
      updateF <- fieldGadget -< vs
      _ <- send id <<< arr (uncurry updateScript) -< (mId, updateF)
      returnA -< id
   where
      fieldGadget = linkExt $ case typ of
         -- Special case for one-line text fields in a matrix to let them be shown folded.
         Field False fid _ multi (BuiltInDef ModelText) ->
            focusing (extValue fid) $ multiMod multi _ExtText displayText
         -- Special case for memo fields to avoid a model update every key press.
         Field False fid _ multi (BuiltInDef ModelNote) ->
            focusing (extValue fid) $ multiMod multi _ExtText displayMemo
         -- Normal case: display the same as any other dialog.
         _ -> snd $ fieldElement1 False typ
      linkExt = case linkField of
         Nothing -> id
         Just lfid -> linked $ preview $ ix lfid . _ExtText
      multiMod :: Multiplicity -> Prism' ExtValue Text -> Gadget' e w Text -> Gadget' e w ExtValue
      multiMod multi prsm = case multi of
         Optional -> prismatic Nothing (optionalExtension . below prsm) . optional ""
         One -> prismatic "" prsm
      optionalExtension :: Iso' ExtValue (Maybe ExtValue)
      optionalExtension = iso forward $ fromMaybe ExtNone
         where
            forward ExtNone = Nothing
            forward x = Just x
      updateScript mId f = Just $ lift $ do
         goToEntity1 "That entity has been deleted in the model." mId
         nm <- currentName
         oldDat <- getData
         change <- setData $ f oldDat
         return $ if change then Just $ "Edited " <> nm else Nothing
