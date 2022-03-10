{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the banana-ui-gtk library. The banana-ui-gtk library is
proprietary and confidential. Copying is prohibited 
-}

module Demo.TreeTable where

import Control.Lens
import Data.Colour hiding (Colour)
import Data.Colour.Names as C
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Tree
import Demo.Table
import Reactive.Banana.Common
import Reactive.Banana.Dialog
import Reactive.Banana.Frameworks


treeTableDialog :: Int -> MomentIO (Dialog (Forest TableData))
treeTableDialog n = return $ Dialog {
      dialogTitle = "Tree Table " <> T.pack (show n),
      dialogMain = BigElement LensElement {
            elementEnable = pure True,
            elementLabel = "Tree Table",
            elementSpec = ForestTableSpec demoTreeTable,
            elementLens = id,
            elementChanged = mempty
         }
   }


demoTreeTable :: [(Text, [DialogElement TableData])]
demoTreeTable = [
      ("Level 1", [
            LensElement (const True) "Text" simpleTextBox tableText mempty,
            IconDecorated (T.pack . numIcon) $ ColourDecorated (Just . numColour) $
               LensElement (const True) "Number" typedTextBox tableNum mempty,
            LensElement (const True) "Enumeration" enumerationSpec tableEnum mempty,
            LensElement (const True) "Date" (DateSpec shortDate) tableDate mempty
      ]), ("Level 2", [
            LensElement (const True) "Text" simpleTextBox tableText mempty,
            LensElement (const True) "Colour" ColourSpec tableColour mempty,
            LensElement (const True) "Icon" (IconSpec isStandardContext) tableIcon mempty,
            LensElement (const True) "Combo" comboSpec tableCombo mempty
      ])
   ]
   where
      enumerationSpec = MenuSpec $
         map (\i -> (T.pack $ show i, Just $ enumIcon i, Just $ enumColour i, i)) [Foo .. Woo]
      comboSpec = MenuSpec $ map (\i -> (i, Nothing, Nothing, i)) comboOptions
      numIcon v
         | v ^. tableNum < 25 = "face-sad"
         | v ^. tableNum < 50 = "face-plain"
         | v ^. tableNum < 75 = "face-smile"
         | otherwise = "face-smile-big"
      numColour v
         | v ^. tableNum < 0 = Colour C.red
         | v ^. tableNum < 100 = Colour $ blend (fromIntegral (v ^. tableNum) / 100) C.green C.red
         | otherwise = Colour C.green


{- demoTreeTable :: [Table TableData]
demoTreeTable = [[
         mkField "Text" tableText Nothing (EditEntry id) Nothing,
         mkField "Number" tableNum (Just numIcon) (EditEntry textPrism) (Just numColour),
         mkField "Enumeration" tableEnum (Just enumIcon) (EditEnum allValues) (Just enumColour),
         mkField "Date" tableDate Nothing (EditDate shortDate) Nothing
      ],[
         mkField "Text" tableText Nothing (EditEntry id) Nothing,
         mkField "Colour" tableColour Nothing EditColour Nothing,
         mkField "Icon" tableIcon Nothing (EditIcon $ const True) Nothing,
         mkField "Combo" tableCombo Nothing (EditCombo comboOptions) Nothing
      ]]
   where
      numIcon n
         | n < 25 = "face-sad"
         | n < 50 = "face-plain"
         | n < 75 = "face-smile"
         | otherwise = "face-smile-big"
      numColour n
         | n < 0 = C.red
         | n < 100 = blend (fromIntegral n / 100) C.green C.red
         | otherwise = C.green
-}

testTreeTable :: Forest TableData
testTreeTable = [
      Node (TableData "One row" 23 Wibble (fromGregorian 2016 6 24) (Colour C.red) "go-home" "One") [
            Node (TableData "One A" 74 Baz (fromGregorian 2017 7 25) (Colour C.blue) "" "Two") [],
            Node (TableData "One B" 92 Bar (fromGregorian 2018 8 26) (Colour C.green) "" "") []
         ],
      Node (TableData "Two row" 56 Foo (fromGregorian 2018 3 4) (Colour C.white) "" "") []
   ]
