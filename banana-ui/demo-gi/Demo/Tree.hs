{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the banana-ui-gtk library. The banana-ui-gtk library is
proprietary and confidential. Copying is prohibited 
-}

module Demo.Tree where

import Control.Lens
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree
import Reactive.Banana.Dialog
import Reactive.Banana.Frameworks

testForest :: Forest (Text, Maybe Text, Maybe Text)
testForest = [
      Node ("One", Just "Tooltip one!", Just "One") [
         Node ("One-A", Just "Tooltip one hay", Just "One-A") [],
         Node ("One-B", Just "Tooltip one bee", Just "One-B") []
      ],
      Node ("Two (not selectable)", Nothing, Nothing) [
         Node ("Two-A", Nothing, Just "Two-A") [],
         Node ("Two-B", Nothing, Just "Two-B") []
      ]
   ]


treePickerOne :: DialogSelector (Set Text)
treePickerOne = const $ Just $ DialogWrapper id $ return $
      Dialog "Single Item Picker" $ ValidityCheck (\s -> S.size s == 1) $ BigElement $
      LensElement (pure True) "Pick: " spec id mempty
   where
      spec = TreeSelectorSpec testForest


treePickerMany :: DialogSelector (Set Text)
treePickerMany = const $ Just $ DialogWrapper id $ return $
      Dialog "Multipe Item Picker" $ BigElement $
      LensElement (pure True) "Pick: " spec id mempty
   where
      spec = TreeSelectorSpec testForest


data TreeTestData = TreeTestData {
      _singleItem :: Set Text,
      _multiItem :: Set Text
   } deriving Show

singleItem :: Lens' TreeTestData (Set Text)
singleItem = lens _singleItem $ \s x -> s{_singleItem = x}


multiItem :: Lens' TreeTestData (Set Text)
multiItem = lens _multiItem $ \s x -> s{_multiItem = x}


nullTreeData :: TreeTestData
nullTreeData = TreeTestData S.empty S.empty

treeDialog :: Int -> MomentIO (Dialog TreeTestData)
treeDialog n = return Dialog {
      dialogTitle = "Tree Selector Test " <> T.pack (show n),
      dialogMain = Elements [
            LensElement (pure True) "Single selection: "
                  (FixedTextSpec showSet $ Just treePickerOne)
                  singleItem
                  mempty,
            LensElement (pure True) "Multiple selection: "
                  (FixedTextSpec showSet $ Just treePickerMany)
                  multiItem
                  mempty
         ]
   }

showSet :: Set Text -> Text
showSet = T.intercalate ", " . S.toList
