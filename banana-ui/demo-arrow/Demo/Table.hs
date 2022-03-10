{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the banana-ui-gtk library. The banana-ui-gtk library is
proprietary and confidential. Copying is prohibited 
-}

module Demo.Table where

import Control.Lens
import Data.Colour hiding (Colour)
import Data.Colour.Names as C
import Data.Text
import Data.Time
import Reactive.Banana.Common
import Reactive.Banana.Table


data DemoEnum = Foo | Bar | Baz | Wibble | Woo deriving (Eq, Ord, Bounded, Enum, Show, Read)

enumColour :: DemoEnum -> Colour
enumColour Foo = Colour coral
enumColour Bar = Colour gray
enumColour Baz = Colour indigo
enumColour Wibble = Colour mediumspringgreen
enumColour Woo = Colour salmon

enumIcon :: DemoEnum -> Text
enumIcon Foo = "weather-clear"
enumIcon Bar = "weather-clear-night"
enumIcon Baz = "weather-few-clouds"
enumIcon Wibble = "weather-showers"
enumIcon Woo = "weather-overcast"


data TableData = TableData {
   _tableText :: Text,
   _tableNum :: Int,
   _tableEnum :: DemoEnum,
   _tableDate :: Day,
   _tableColour :: Colour,
   _tableIcon :: Text,
   _tableCombo :: Text
} deriving (Eq, Show)

tableText :: Lens' TableData Text
tableText = lens _tableText $ \s t -> s{_tableText = t}

tableNum :: Lens' TableData Int
tableNum = lens _tableNum $ \s n -> s{_tableNum = n}

tableEnum :: Lens' TableData DemoEnum
tableEnum = lens _tableEnum $ \s e -> s{_tableEnum = e}

tableDate :: Lens' TableData Day
tableDate = lens _tableDate $ \s d -> s{_tableDate = d}

tableColour :: Lens' TableData Colour
tableColour = lens _tableColour $ \s c -> s{_tableColour = c}

tableIcon :: Lens' TableData Text
tableIcon = lens _tableIcon $ \s i -> s{_tableIcon = i}

tableCombo :: Lens' TableData Text
tableCombo = lens _tableCombo $ \s c -> s{_tableCombo = c}


blankTableRow :: TableData
blankTableRow = TableData "" 0 Foo (fromGregorian 2000 1 1) (Colour white) "" ""


comboOptions :: [Text]
comboOptions = ["One", "Two", "Three", "Four", "Five"]


demoTableSpec :: Table TableData
demoTableSpec = [
      mkField "Text" tableText Nothing (EditEntry id) Nothing,
      mkField "Number" tableNum (Just numIcon) (EditEntry textPrism) (Just numColour),
      mkField "Enumeration" tableEnum (Just enumIcon) (enumField allValues) (Just enumColour),
      mkField "Date" tableDate Nothing (EditDate shortDate) Nothing,
      mkField "Colour" tableColour Nothing EditColour Nothing,
      mkField "Icon" tableIcon Nothing (EditIcon $ const True) Nothing,
      mkField "Combo" tableCombo Nothing (EditCombo comboOptions) Nothing
   ]
   where
      numIcon n
         | n < 25 = "face-sad"
         | n < 50 = "face-plain"
         | n < 75 = "face-smile"
         | otherwise = "face-smile-big"
      numColour n
         | n < 0 = Colour C.red
         | n < 100 = Colour $ blend (fromIntegral n / 100) C.green C.red
         | otherwise = Colour C.green


testTable :: [TableData]
testTable = [
      TableData "One row" 23 Wibble (fromGregorian 2016 6 24) (Colour C.red) "go-home" "One",
      TableData "Second row" 74 Baz (fromGregorian 2017 7 25) (Colour C.blue) "" "Two",
      TableData "Third row" 92 Bar (fromGregorian 2018 8 26) (Colour C.green) "" ""
   ]
