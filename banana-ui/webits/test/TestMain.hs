{-
Copyright © Paul Johnson 2021. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

module Main where

import Control.Lens hiding (elements)
import Data.Tree
import qualified Data.ForestModel as FM
import Test.Hspec
import Test.QuickCheck

import Debug.Trace

main :: IO ()
main = do
   putStrLn "Test Cases:"
   testCases
   putStrLn "\nProperty Tests:"
   testProperties


testCases :: IO ()
testCases = hspec $ do
   describe "DeleteRoot" $ it "Deletes node 1 in the model." $
         FM.evalEdit (FM.delete 1 >> FM.getForest) testModel `shouldBe` tail testForest
   describe "DeleteChild" $ it "Deletes node 2 in the model." $
         FM.evalEdit (FM.delete 2 >> FM.getForest) testModel `shouldBe`
            let
               (n1:ns1)=testForest
               n1' = n1 {subForest = tail $ subForest n1}
            in n1':ns1
   describe "InsertRoot1" $ it "Inserts a node value 99 at the start of the tree." $
         FM.evalEdit (FM.insert FM.InsertBefore 1 99 >> FM.getForest) testModel `shouldBe`
            Node 99 [] : testForest
   describe "InsertRoot2" $ it "Inserts a node value 99 after node 1." $
         FM.evalEdit (FM.insert FM.InsertAfter 1 99 >> FM.getForest) testModel `shouldBe`
            let (f1,f2) = splitAt 1 testForest in f1 ++ Node 99 [] : f2
   describe "InsertRoot3" $ it "Inserts a child of the rootIndex" $
         FM.evalEdit (FM.insert FM.InsertChild FM.rootIndex 99 >> FM.getForest) testModel `shouldBe`
            Node 99 [] : testForest
   describe "InsertRoot4" $ it "Appends a child of the rootIndex" $
         FM.evalEdit (FM.insert FM.AppendChild FM.rootIndex 99 >> FM.getForest) testModel `shouldBe`
            testForest ++ [Node 99 []]
   describe "InsertChild" $ it "Inserts a node value 99 at the start of the node 1 children." $
         FM.evalEdit (FM.insert FM.InsertChild 1 99 >> FM.getForest) testModel `shouldBe`
            let
               (n1:ns)=testForest
               n1' = n1 {subForest = Node 99 [] : subForest n1}
            in n1':ns
   describe "AppendChild" $ it "Appends a node value 99 at the end of the node 1 children." $
         FM.evalEdit (FM.insert FM.AppendChild 1 99 >> FM.getForest) testModel `shouldBe`
            let
               (n1:ns)=testForest
               n1' = n1 {subForest = subForest n1 ++ [Node 99 []]}
            in n1':ns
   describe "InsertAfterChild1" $ it "Inserts a node value 99 after node 2." $
         FM.evalEdit (FM.insert FM.InsertAfter 2 99 >> FM.getForest) testModel `shouldBe`
            let
               (n1:ns1)=testForest
               n1' = n1 {subForest = let (n2:ns2) = subForest n1 in n2 : Node 99 [] : ns2}
            in n1':ns1
   describe "InsertAfterChild2" $ it "Inserts a node value 99 after node 3." $
         FM.evalEdit (FM.insert FM.InsertAfter 3 99 >> FM.getForest) testModel `shouldBe`
            let
               (n1:ns1)=testForest
               n1' = n1 {subForest = subForest n1 ++ [Node 99 []]}
            in n1':ns1
   describe "InsertBeforeChild" $ it "Inserts a node value 99 before node 3." $
         FM.evalEdit (FM.insert FM.InsertBefore 3 99 >> FM.getForest) testModel `shouldBe`
            let
               (n1:ns1)=testForest
               n1' = n1 {subForest = let (n2:ns2) = subForest n1 in n2 : Node 99 [] : ns2}
            in n1':ns1
   describe "Move1" $ it "Moves node 1 to be a child of node 8." $
         FM.evalEdit (FM.move 1 FM.InsertChild 8 >> FM.getForest) testModel `shouldBe`
            let [n1, n4, n8] = testForest
            in [n4, n8 {subForest = [n1]}]
   describe "Move2" $ it "Fails to move node 1 to be a child of node 2." $
      FM.evalEdit (FM.move 1 FM.InsertChild 2 >> FM.getForest) testModel `shouldBe` testForest
   describe "Move3" $ it "Moves node 2 to be the first root node." $
         FM.evalEdit (FM.move 2 FM.InsertChild FM.rootIndex >> FM.getForest) testModel `shouldBe`
         let [Node v1 (n2:ns), n4, n8] = testForest
         in [n2, Node v1 ns, n4, n8]
   describe "Move4" $ it "Moves node 2 to be the last root node." $
         FM.evalEdit (FM.move 2 FM.AppendChild FM.rootIndex >> FM.getForest) testModel `shouldBe`
         let [Node v1 (n2:ns), n4, n8] = testForest
         in [Node v1 ns, n4, n8, n2]
   describe "Move5" $ it "Fails to move root to be a child of node 1." $
      FM.evalEdit (FM.move FM.rootIndex FM.InsertChild 1 >> FM.getForest) testModel `shouldBe`
         testForest
testProperties :: IO ()
testProperties = do
      quickCheck $ label "ModelIso" $ withMaxSuccess 1000 prop_ModelIso
      quickCheck $ label "InsertDelete1" $ withMaxSuccess 1000 prop_InsertDelete1
      quickCheck $ label "InsertDelete2" $ withMaxSuccess 1000 prop_InsertDelete2
      quickCheck $ label "StableUpdate" $ withMaxSuccess 1000 prop_StableUpdate
   where
      prop_ModelIso :: Forest Int -> Property
      prop_ModelIso x = x ^. FM.modelIso . from FM.modelIso === x
      prop_InsertDelete1 :: Forest Int -> Gen Property
      prop_InsertDelete1 x = do
         let
            mdl = x ^. FM.modelIso
            keys = concatMap flatten $ FM.evalEdit FM.getForestIndices mdl
         (n, ins) <- if null keys
            then (FM.rootIndex,) <$> elements [FM.InsertChild, FM.AppendChild]
            else frequency [
                  (1, (FM.rootIndex,) <$> elements [FM.InsertChild, FM.AppendChild]),
                  (10, (,) <$> elements keys <*> chooseEnum (FM.InsertChild, FM.InsertAfter))
               ]
         v <- arbitrary
         return $ x === FM.evalEdit (
               FM.insert ins n v >>= \case
                  Just k -> do
                     FM.delete k >>= \case
                        True -> return ()
                        False -> error "Delete failed."
                     FM.getForest
                  Nothing -> error "Invalid insert."
            ) mdl
      prop_InsertDelete2 :: Forest Int -> Gen Property
      prop_InsertDelete2 x = do
         let mdl = x ^. FM.modelIso
         n <- oneof [return FM.rootIndex, abs <$> arbitrary]
         ins <- chooseEnum (FM.InsertChild, FM.InsertAfter)
         v <- arbitrary
         return $ x === FM.evalEdit ( do
               FM.insert ins n v >>= \case
                  Just k -> do
                     FM.delete k >>= \case
                        True -> return ()
                        False -> error "Delete failed."
                  Nothing -> return ()
               FM.getForest
            ) mdl
      prop_StableUpdate :: NonEmptyList (Tree Int) -> Gen Property
      prop_StableUpdate (NonEmpty x) = do
         let
            mdl1 = x ^. FM.modelIso
            keys = concatMap flatten $ FM.evalEdit FM.getForestIndices mdl1
         traceM $ "keys = " <> show keys
         n1 <- elements keys
         ins <- chooseEnum (FM.InsertChild, FM.InsertAfter)
         n2 <- elements $ FM.rootIndex : keys
         let
            mdl2 = FM.execEdit (FM.move n1 ins n2) mdl1
            forest2 = FM.evalEdit FM.getForestIndices mdl2
            mdl3 = FM.stableUpdate forest2 mdl1
         return $ (mdl2 ^. from FM.modelIso) === (mdl3 ^. from FM.modelIso)


-- | Test forest. Node values chosen to be equal to model indices.
testForest :: Forest Int
testForest = [
      Node 1 [ Node 2 [], Node 3 [] ],
      Node 4 [ Node 5 [], Node 6 [], Node 7 [] ],
      Node 8 []
   ]

testModel :: FM.Model Int
testModel = FM.makeModel testForest
