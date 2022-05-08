{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}



{- |
-}

module Model.Abstract.Properties (
   -- * Property Dialog
   ReferenceValues,
   getReferenceValues,
   putReferenceValues,
   compareReferenceValues,
   reflectiveDialog,
   -- * Entity Selection Dialog
   refPicker,
   modelItemSpecifier,
   modelSubsetSpecifier,
   modelFilter,
   showEntitySet
) where

import Control.Arrow
import Control.Category
import Control.Lens
import Control.Monad
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Format
import Data.Tree
import Model.Abstract.PackageTree
import Model.Reflection.Dialogs
import Model.Reflection.NamedRelation (NamedRelation, Relation)
import qualified Model.Reflection.NamedRelation as NR
import Model.Reflection.Reflective
import Model.Reflection.Types
import Model.Reflection.Values
import Prelude hiding ((.), id)
import Reactive.Banana.ArrowDialog
import Reactive.Banana.Menu


-- | The ModelIds of entities linked to this one by references.
type ReferenceValues = Map Relation (Set ModelId)


-- | A map of the sets of entities cross-referenced with the argument, keyed by relationship name.
getReferenceValues :: ModelId -> NamedRelation ModelId -> ReferenceValues
getReferenceValues uuid rels =
      foldr (M.unionWith S.union . \(u, r) -> M.singleton r $ S.singleton u) M.empty $
      NR.relations uuid rels


-- | Set the cross-references for the ModelId in the model to the contents of the reference values.
putReferenceValues :: (EntityClass v) => Entity v -> ReferenceValues -> ModelEdit v w ()
putReferenceValues e refs = fromHere $ do
      modifyRelations $ NR.deleteAll $ entityId e
      forM_ (M.toList refs) $ \(rel, s) -> do
         es <- catMaybes <$> mapM (\u -> goToEntity u >> current) (S.toList s)
         mapM_ (addCheckedRelation rel e) es


-- | Compares the two sets of reference values and returns a list of those that have changed.
compareReferenceValues :: ReferenceValues -> ReferenceValues -> [ModelId]
compareReferenceValues refs1 refs2 = S.toList $ M.foldr S.union S.empty allChanges
   where
      added = M.unionWith S.difference refs2 refs1   -- In refs2 but not refs1
      deleted = M.unionWith S.difference refs1 refs2  -- In refs1 but not refs2
      allChanges = M.unionWith S.union added deleted


-- | A dialog for picking elements from a model, with the choice restricted by a predicate.
refPicker :: (EntityClass v) =>
   Bool    -- ^ If true then only one item may be picked.
   -> (Entity v -> Bool)   -- ^ Only entities that satisfy this can be picked.
   -> Dialog' (Model v) w (Set ModelId)
refPicker singleFlag predicate =
      Dialog title OkButton $
         validate (\s -> not singleFlag || S.size s <= 1) $
         frame (const $ Just label) $
         treeSelector $ modelFilter predicate
   where
      title = if singleFlag then "Single Selector" else "Multi Selector"
      label = if singleFlag then "Selected entity: " else "Selected entities: "


-- | Attempts to compile the reflection data within the model into a dialog selector.
reflectiveDialog :: (EntityClass v) =>
   (Entity v -> Maybe (Menu w))       -- ^ Menus for referenced entities.
   -> DialogSelector' (Model v) w (Maybe (Entity v, ReferenceValues))
reflectiveDialog _ _ Nothing =
   Just $ Dialog "Nothing selected" OkApplyButton id
reflectiveDialog menuF model (Just (ent, _)) = Just $ Dialog {
         dialogTitle =
               ent ^. entityContents . to reflectiveName . to variantUserLabel <>
               " properties",
         dialogButtons = OkApplyButton,
         dialogGadget =  accum $
            box Vertical [
               [fieldsFrame | not $ null fields] ++
               [referencesFrame | not $ null refs] ++
               [stampsMessage]
            ]
      }
   where
      fields = mapMaybe (`M.lookup` (withBuiltIn $ modelFields model)) $
            reflection extension $ ent ^. entityContents
      refs = valueReferences (modelRefTypes model) $ ent ^. entityContents
      extension v = M.findWithDefault [] v $ modelExtensions model
      refElement (nm, single, variants) =
         let
            vFilter e = S.member (reflectiveName $ e ^. entityContents) variants
            g = focusing (relLens nm) $
                  modelSubsetSpecifier1 single (const vFilter) menuF
               -- Use traversing because we don't create an entity if none exists.
         in (nm, g)
      -- Reference lookup as a lens with the empty set as the default value.
      relLens k = lens
            (M.findWithDefault mempty k)
            (\s v -> if S.null v then M.delete k s else M.insert k v s)
      fieldsFrame =
         frame (const $ Just "Fields") $
         traversingOver mempty (_Just . _1 . entityProperties) $
         form Vertical $ map (fieldElement1 True) fields
         -- Use traversingOver because we don't create an entity if none exists.
      referencesFrame =
         frame (const $ Just "References") .
         traversingOver mempty (_Just . _2) .
         form Vertical $ map refElement refs
      stampsMessage = arr (const id) <<< message (const $ \case
         Nothing -> "Entity deleted."
         Just (e, _) ->
            stampMessage "Created: " (entityCreated e) <> "\n" <>
            stampMessage "Modified: " (e ^. entityModified))
      stampMessage msg (user, day) =
         msg <> showDate day <> " by " <> user <> "."
      showDate = T.pack . formatTime defaultTimeLocale "%d %b %y"


-- | Extract the model tree, filtering out any contents that do not satisfy the predicate.
modelFilter :: (Reflective v, EntityClass v) =>
   (Entity v -> Bool) -> Model v -> Forest (Text, Maybe Text, Maybe ModelId)
modelFilter predicate model =
      case evalModelEdit id model modelPackageForest of
         Left _ -> [Node ("Internal error", Nothing, Nothing) []]   -- Should never happen.
         Right frst -> fmap (fmap getTuple) frst
            -- fromJust is safe because prism for evalModelEdit was id.
   where
      getTuple ent =
         let
            nm = ent ^. entityName . nameText
            desc = do -- Maybe monad
               field <- ent ^? entityContents . to reflectiveGet . ix descriptionField
               guard $ T.length (displayValue field) > 0
               listToMaybe $ T.splitOn "\n\n" $ displayValue field
         in if predicate ent then (nm, desc, Just $ entityId ent) else (nm, desc, Nothing)


-- | Displays a single model entity name. On left-click it opens up a dialog to change the
-- selection, limited to those entities that satisfy the predicate.
--
-- If the initial set contains multiple elements then these will be displayed. However any updates
-- are constrained to be 0 or 1 item.
--
-- The predicate is called with the model initially, and the result is then called with each
-- entity. Hence if the predicate takes just the model and returns a lambda for its second
-- argument then any evaluaiton which depends only on the model will be executed once.
modelItemSpecifier :: (EntityClass v) =>
   (Model v -> Entity v -> Bool)  -- ^ Predicate for allowing the entity in the selection.
   -> (Entity v -> Maybe (Menu w))       -- ^ Menu for displayed entities.
   -> Gadget' (Model v) w (Set ModelId)
modelItemSpecifier = modelSubsetSpecifier1 True


-- | Displays a set of model items as a comma-separated list of names. On double-click it opens
-- up a dialog to change the selection restricted to those entities that satisfy the predicate.
--
-- See note in "modelItemSpecifier" about optimising the predicate.
modelSubsetSpecifier :: (EntityClass v) =>
   (Model v -> Entity v -> Bool)   -- ^ Predicate for allowing the entity in the selection.
   -> (Entity v -> Maybe (Menu w))  -- ^ Menu for the displayed entities.
   -> Gadget' (Model v) w (Set ModelId)
modelSubsetSpecifier = modelSubsetSpecifier1 False


-- | General case for model subsets and items. The first argument is @False@ to pick a subset and
-- @True@ to pick a single item.
modelSubsetSpecifier1 :: (EntityClass v) =>
   Bool
   -> (Model v -> Entity v -> Bool)   -- ^ Predicate for allowing the entity in the selection.
   -> (Entity v -> Maybe (Menu w))  -- ^ Menu for the displayed entities.
   -> Gadget' (Model v) w (Set ModelId)
modelSubsetSpecifier1 singleFlag predicate menuF =
   intercept False picker $ proc inSet -> do
         _ <- send id <<< clickableList <<< getInitialEnv (arr . items) -< inSet
         returnA -< inSet
      -- memoPopup MemoSmall False showEntitySet picker
   where
      items model = map clickFunc . mapMaybe (`M.lookup` modelContents model) . S.toList
      clickFunc ent = ClickableItem {
            clickText = ent ^. entityName . nameText,
            clickSingle = Nothing,
            clickDouble = Nothing,
            clickMenu = menuF ent
         }
      picker model = const $ Just $ refPicker singleFlag $ predicate model


-- | Convert a set of Model IDs into a text string.
showEntitySet :: (EntityClass v) => Model v -> Set ModelId -> Text
showEntitySet model s =
   if S.null s then " - " else
      T.intercalate ", " $
      map (view nameText) $
      sort $
      mapMaybe (\mId -> model ^? to modelContents . at mId . _Just . entityName) $
      S.toList s
