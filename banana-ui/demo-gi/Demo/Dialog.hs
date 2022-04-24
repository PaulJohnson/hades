{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Copyright © Paul Johnson 2019. See LICENSE file for details.
-}

module Demo.Dialog where

import Control.Lens
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Demo.Table
import Reactive.Banana.Common
import Reactive.Banana.Dialog
import Reactive.Banana.Frameworks
import Reactive.Banana.Table


type MRange a = (Maybe a, Maybe a)


data DemoData = DemoData {
   _demoRangeEnable :: Bool,
   _demoTextRangeEnable :: Bool,
   _demoRange :: MRange Integer,
   _demoEnum :: DemoEnum,
   _demoTextRange :: (Text, Text),
   _demoDate :: Day,
   _demoTable :: [TableData],
   _demoComment :: Text
} deriving (Show)


demoRangeEnable :: Lens' DemoData Bool
demoRangeEnable = lens _demoRangeEnable $ \s x -> s{_demoRangeEnable = x}

demoTextRangeEnable :: Lens' DemoData Bool
demoTextRangeEnable = lens _demoTextRangeEnable $ \s x -> s{_demoTextRangeEnable = x}

demoRange :: Lens' DemoData (MRange Integer)
demoRange = lens _demoRange $ \s x -> s{_demoRange = x}

demoEnum :: Lens' DemoData DemoEnum
demoEnum = lens _demoEnum $ \s x -> s{_demoEnum = x}

demoTextRange :: Lens' DemoData (Text, Text)
demoTextRange = lens _demoTextRange $ \s x -> s{_demoTextRange = x}

demoDate :: Lens' DemoData Day
demoDate = lens _demoDate $ \s x -> s{_demoDate = x}

demoTable :: Lens' DemoData [TableData]
demoTable = lens _demoTable $ \s x -> s{_demoTable = x}

demoComment :: Lens' DemoData Text
demoComment = lens _demoComment $ \s x -> s{_demoComment = x}


validRange :: (Ord a, Show a) => (Maybe a, Maybe a) -> [Text]
validRange (Just v1, Just v2) = ["Invalid range" | v1 > v2]
validRange _ = []

-- An optional range. First must be less than or equal to second.
mRangeDialog :: (Num a, Ord a, Show a, Read a) => Text -> Dialog (MRange a)
mRangeDialog title =
   Dialog {
      dialogTitle = title,
      dialogMain = ValidityMessage validRange $ Elements [
         LensElement {
            elementEnable = pure True,
            elementLabel = "Lower bound:",
            elementSpec = maybeTextBox,
            elementLens = _1,
            elementChanged = mempty },
         LensElement {
            elementEnable = pure True,
            elementLabel = "Upper bound:",
            elementSpec = maybeTextBox,
            elementLens = _2,
            elementChanged = mempty },
         LensElement {
            elementEnable = pure True,
            elementLabel = "Range size:",
            elementSpec = FixedTextSpec
                  (\(mv1,mv2) ->
                     maybe "" (T.pack . show) $ subtract <$> mv1 <*> mv2)
                  Nothing,
            elementLens = id,
            elementChanged = mempty }
            ]
      }


demoDialog :: Int -> MomentIO (Dialog DemoData)
demoDialog n = do
   return Dialog {
      dialogTitle = "Demo Dialog " <> T.pack (show n),
      dialogMain = VBox [[Elements [
            LensElement {
               elementEnable = pure True,
               elementLabel = "Enable numerical range:",
               elementSpec = TickBox,
               elementLens = demoRangeEnable,
               elementChanged = mempty },
            LensElement {
               elementEnable = pure True,
               elementLabel = "Enable text range:",
               elementSpec = TickBox,
               elementLens = demoTextRangeEnable,
               elementChanged = mempty } ]
         ], [
            Frame (view demoRangeEnable) demoRange $
                  mRangeDialog "Numerical range with optional limits",
            Elements [LensElement {
                  elementEnable = pure True,
                  elementLabel = "Foo Selection:",
                  elementSpec = boundedMenu,
                  elementLens = demoEnum,
                  elementChanged = mempty },
               LensElement {
                  elementEnable = pure True,
                  elementLabel = "Date:",
                  elementSpec = DateSpec longDate,
                  elementLens = demoDate,
                  elementChanged = mempty } ],
            Frame (view demoTextRangeEnable) demoTextRange Dialog {
               dialogTitle = "Text range",
               dialogMain = ValidityMessage validTextRange $ Elements [
                     LensElement {
                        elementEnable = pure True,
                        elementLabel = "Lower text limit:",
                        elementSpec = simpleTextBox,
                        elementLens = _1,
                        elementChanged = mempty },
                     LensElement {
                        elementEnable = pure True,
                        elementLabel = "Upper text limit:",
                        elementSpec = simpleTextBox,
                        elementLens = _2,
                        elementChanged = mempty } ]
               },
            BigElement LensElement {
               elementEnable = pure True,
               elementLabel = "Table Demo",
               elementSpec = TableSpec
                     [TableAdd "row" blankTableRow, TableDelete, TableShuffle]
                     demoTableSpec
                     Nothing,
               elementLens = demoTable,
               elementChanged = mempty }
         ], [
            BigElement LensElement {
                  elementEnable = pure True,
                  elementLabel = "Comment",
                  elementSpec = MemoBoxSpec MemoMedium True,
                  elementLens = demoComment,
                  elementChanged = mempty } ]
         ]
      }
   where
      validTextRange (t1, t2) = ["Invalid text range." | T.toCaseFold t1 > T.toCaseFold t2]

initialValue :: DemoData
initialValue = DemoData
      False
      False
      (Nothing, Just 5)
      Wibble
      ("alpha", "Omega")
      (fromGregorian 2018 3 12)
      testTable
      "This is the default."

testValues :: [DemoData]
testValues = [
      DemoData
         True
         False
         (Just 2, Just 6)
         Foo
         ("", "ZZZ")
         (fromGregorian 2000 1 1)
         []
         "The Foo Value.",
      DemoData
         False
         True
         (Just 5, Nothing)
         Bar
         ("run", "Wimbledon")
         (fromGregorian 2010 1 1)
         testTable
         "The Bar Value."
   ]
