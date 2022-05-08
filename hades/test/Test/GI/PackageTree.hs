{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedLabels #-}

{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

-- |
module Test.GI.PackageTree where

import Control.Lens hiding (elements)
import Control.Monad
import Crypto.Random
import Data.List hiding (delete)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Tree
import Data.UUID.Generate
import qualified Data.UUID as UUID
import GI.Gtk (AttrOp ((:=)))
import qualified GI.Gtk as Gtk
import Model.Abstract.PackageTree
import Model.GI.PackageTree
import Model.Reflection.Reflective
import Model.Reflection.Values
import Test.QuickCheck
import Test.QuickCheck.Monadic


newtype TestItem = TestPackage {getPackage :: Package TestItem} deriving Eq

testPackage :: Iso' TestItem (Package TestItem)
testPackage = iso getPackage TestPackage

instance Reflective TestItem where
   reflectiveName _ = Variant "Test"
   reflectiveDefaults = TestPackage <$> reflectiveDefaults
   reflectiveBuiltIn (TestPackage p) = reflectiveBuiltIn p
   reflectiveGet (TestPackage p) = reflectiveGet p
   reflectiveSet (TestPackage p) = TestPackage <$> reflectiveSet p
   reflectiveBuiltInRefs = mempty
   reflectiveArrows = mempty

instance EntityClass TestItem where
   type PackageMeta TestItem = ()
   name = testPackage . packageName
   _Package = testPackage
   entityCanHaveChild _ _ = True
   entityCanMove _ = True
   entityExports _ = return []
   entityClone _ = error "TestItem entityClone not implemented."
   entityFunctions = error "TestItem entityFunctions not implemented."

instance Arbitrary TestItem where
   arbitrary = TestPackage . flip Package () <$> arbName
      where
         arbName = Name . T.pack <$> vectorOf 10 (oneof [choose ('A','Z'), choose ('a','z')])


instance (Arbitrary a, EntityClass a) => Arbitrary (Model a) where
   arbitrary = do
         n <- abs <$> arbitrary
         go n $ modelStartState gen (emptyModel "Test")
      where
         Right (gen :: Generator) = newGen "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
         go :: Int -> ModelEditState a -> Gen (Model a)
         go i state
            | i <= 0     = return $ stateModel state
            | otherwise  = modelGen addArbitraryEntity state >>= go (i-1)
   shrink model = map delOne uuids
      where
         Right (gen :: Generator) = newGen "1aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
         uuids = M.keys $ modelContents model
         delOne uuid =
            case execModelEdit id (modelStartState gen model) $ goToEntity uuid >> delete of
               Left err -> error $ "Failed to delete an item during shrink: " <> show err
               Right (newModel, _) -> newModel



-- | Execute an arbitrary model edit on the model state.
modelStep :: (Arbitrary a, EntityClass a) =>
   (Model a -> Gen (ModelEdit a a ()))
   -> ModelEditState a
   -> Gen (ModelUpdate a (), ModelEditState a)
modelStep gen state = do
   act <- gen $ stateModel state
   case runModelEdit id state act of
      Left err -> error $ "modelGen: " <> show err
      Right x -> return x

-- | Like "modelStep", but throw away the results and keep only the mutated state.
modelGen :: (Arbitrary a, EntityClass a) =>
   (Model a -> Gen (ModelEdit a a ()))
   -> ModelEditState a
   -> Gen (ModelEditState a)
modelGen gen state = snd <$> modelStep gen state


addArbitraryEntity :: (Arbitrary a, EntityClass a) => Model a -> Gen (ModelEdit a a ())
addArbitraryEntity model = do
      v <- arbitrary
      parent <- pickPackage model
      return $ entity parent v
   where
      entity parent v = do
         goToEntity parent
         void $ mkEntity v


-- | Select one of the operations that change the visible tree.
arbitraryTreeOp :: (Arbitrary a, EntityClass a) =>
   Model a -> Gen (ModelEdit a a (), String)
arbitraryTreeOp model = do
      entityM <- pickEntity model
      package <- pickPackage model
      newItem <- arbitrary
      elements $
         (
            do  -- Add a new entity.
               goToEntity package
               void $ mkEntity newItem,
            "Add new package in " <> show package <> ": "
               <> show (newItem ^. name . nameText)
         ) : case entityM of
           Nothing -> []
           Just entity -> [
              (
                 do  -- Rename an entity.
                       goToEntity $ entityId entity
                       void $ renameEntity $ newItem ^. name,
                 "Rename " <> show (entity ^. entityName . nameText) <> " as "
                    <> show (newItem ^. name . nameText)
              ), (
                 do  -- Delete an entity.
                       goToEntity $ entityId entity
                       delete,
                 "Delete " <> show (entity ^. entityName . nameText) <> " "
                    <> show (entityId entity)
              ), (
                 do  -- Move an entity. No-op if the package is a child of the entity.
                       goToEntity package
                       p <- map entityId <$> currentPath
                       unless (entityId entity `elem` p) $ do
                          goToEntity $ entityId entity
                          moveEntity package,
                 "Move " <> show (entity ^. entityName . nameText) <> " to "
                    <> show package
              )
              ]


arbitraryTreeOpSequence :: (Arbitrary a, EntityClass a) =>
   Int -> ModelEditState a -> Gen [(ModelEdit a a (), String)]
arbitraryTreeOpSequence n state
   | n < 1  = return []
   | otherwise = do
      op1 <- arbitraryTreeOp (stateModel state)
      let Right (_, newState) = runModelEdit id state (fst op1)
      ops <- arbitraryTreeOpSequence (n-1) newState
      return $ op1 : ops


-- | Pick an arbitrary entity from the model. Returns @Nothing@ for an empty model.
pickEntity :: (EntityClass a) => Model a -> Gen (Maybe (Entity a))
pickEntity model = case M.elems $ modelContents model of
      [] -> return Nothing
      es -> Just <$> elements es


-- | Pick an arbitrary package, possibly the root.
pickPackage :: (EntityClass a) => Model a -> Gen ModelId
pickPackage model = elements $ UUID.nil : map entityId (M.elems $ modelContents model)


getGtkTree :: (EntityClass a, Monad m) => Model a -> m (Tree GtkEntity)
getGtkTree model = do
      forest <- Model.Abstract.PackageTree.monadic [] $ evalModelEdit id model gtkEntities
      return $ Node (GtkEntity (Name $ modelName model) "model-home" UUID.nil) forest
   where
      gtkEntities = map (fmap $ entityToGtk (const "")) <$> modelPackageForest


-- | Set up the GTK test data.
gtkTestTree :: (EntityClass a) => Model a -> IO (GtkTreeInfo a)
gtkTestTree model = do
      Node _ forest <- getGtkTree model
      store <-  Gtk.treeStoreNew entityColumnTypes
      sorted <- Gtk.new Gtk.TreeModelSort [#model := store]
      vw <- Gtk.treeViewNewWithModel sorted
      Gtk.treeSortableSetSortFunc sorted 0 treeSortF
      Gtk.treeSortableSetSortColumnId sorted 0 Gtk.SortTypeAscending
      iter <- clearTree store $ Name $ modelName model
      storeEntityForest store iter forest
      return $ GtkTreeInfo store sorted vw (const "")


sortTree :: (Ord a) => Tree a -> Tree a
sortTree (Node a subtrees) = Node a $ sortOn rootLabel $ map sortTree subtrees

prop_treeOp :: (Gtk.IsWidget window) =>
   window -> ModelEditState TestItem -> [(ModelEdit TestItem TestItem (), String)] -> Property
prop_treeOp window state acts = monadicIO $ do
      monitor $ counterexample $ "Acts = " <> show (map snd acts)
      let
         (update, newState) = case runModelEdit id state $ mapM fst acts of
               Left err -> error $ "prop_treeOp: " <> show err
               Right x -> x
      newModelTree <- getGtkTree $ stateModel newState
      monitor $ counterexample $ "EditLog = " <> show (updateEditLog update)
      newGtkTree <- run $ do
         treeInfo <- gtkTestTree $ stateModel state
         updateGtkTree window treeInfo $ updateEditLog update
         let GtkTreeInfo _ gtkSorted _ _ = treeInfo
         (_, iter) <- Gtk.treeModelGetIter gtkSorted =<< Gtk.treePathNewFromIndices [0]
         getEntityTree gtkSorted iter
      monitor $ counterexample $ "GTK tree = " <> drawTree (fmap show newGtkTree)
      assert $ newGtkTree == newModelTree



-- | Run many tests. Note that this calls Gtk.initGUI and creates a window.
runTests :: IO Bool
runTests = do
      void $ Gtk.init Nothing
      window <- Gtk.windowNew Gtk.WindowTypeToplevel
      r <- quickCheckWithResult stdArgs {maxSuccess = 1000} $ \model (Positive n) -> do
         let state = modelStartState gen model
         acts <- arbitraryTreeOpSequence n state
         return $ prop_treeOp window state acts
      case r of
         Success {} -> return True
         _ -> return False
   where
      Right (gen :: Generator) = newGen "3aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
