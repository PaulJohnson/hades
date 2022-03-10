{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the banana-ui-gtk library. The banana-ui-gtk library is
proprietary and confidential. Copying is prohibited 
-}

module Demo.Dialog where

import Control.Lens
import Data.Default
import Data.Default.Orphan ()
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Demo.Table
import Reactive.Banana.ArrowDialog
import Reactive.Banana.Common
import Reactive.Banana.Table
import System.IO.Unsafe


type MRange a = (Maybe a, Maybe a)


data DemoData = DemoData {
   _demoRange :: MRange Integer,
   _demoEnum :: DemoEnum,
   _demoTextRange :: Maybe (Text, Text),
   _demoDate :: Day,
   _demoTable :: [TableData],
   _demoComment :: Text
} deriving (Show)

instance Default DemoData where
   def = DemoData def Foo (Just def) today def def
      where today = unsafePerformIO $ utctDay <$> getCurrentTime


demoRange :: Lens' DemoData (MRange Integer)
demoRange = lens _demoRange $ \s x -> s{_demoRange = x}

demoEnum :: Lens' DemoData DemoEnum
demoEnum = lens _demoEnum $ \s x -> s{_demoEnum = x}

demoTextRange :: Lens' DemoData (Maybe (Text, Text))
demoTextRange = lens _demoTextRange $ \s x -> s{_demoTextRange = x}

demoDate :: Lens' DemoData Day
demoDate = lens _demoDate $ \s x -> s{_demoDate = x}

demoTable :: Lens' DemoData [TableData]
demoTable = lens _demoTable $ \s x -> s{_demoTable = x}

demoComment :: Lens' DemoData Text
demoComment = lens _demoComment $ \s x -> s{_demoComment = x}


validRange :: (Ord a, Show a) =>
   (Maybe a, Maybe a) -> Maybe Text
validRange (v1, v2) = do
   v1a <- v1
   v2a <- v2
   if v1a > v2a then return "Invalid range" else Nothing


-- An optional range. First must be less than or equal to second.
mRangeGadget :: (Num a, Ord a, Show a, Read a) => GadgetF' () () (MRange a)
mRangeGadget = validateTextOver validRange $ form Vertical [
      ("Lower bound:", focusing _1 maybeTextBox),
      ("Upper bound:", focusing _2 maybeTextBox),
      ("Range size:", focusing id $ readOnlyText $ \_ (mv1, mv2) ->
            maybe "" (T.pack . show) $ subtract <$> mv1 <*> mv2)
   ]


demoDialog ::  Dialog () () DemoData DemoData
demoDialog = Dialog "Demo Dialog" OkApplyButton $ accum $ box Vertical [[
         frame (pure $ Just "Numerical range with optional limits") $
            focusingOver demoRange mRangeGadget,
         form Vertical [
            ("Foo Selection:", focusing demoEnum boundedCombo),
            ("Date:", focusing demoDate $ dateBox longDate)
         ],
         simpleFrame "Optional text range" $
            focusingOver demoTextRange $
            optionalOver ("", "") $
            validateTextOver validTextRange $
            form Horizontal [
                  ("Lower bound:", focusing _1 simpleTextBox),
                  ("Upper bound:", focusing _2 simpleTextBox)
               ],
         focusing demoTable $
            table [TableAdd "row" blankTableRow, TableDelete, TableShuffle] demoTableSpec Nothing,
         focusing demoComment $ memoBox MemoMedium True
      ]]
   where
      validTextRange (t1, t2) =
         if T.toCaseFold t1 > T.toCaseFold t2 then Just "Invalid text range." else Nothing


initialValue :: DemoData
initialValue = DemoData
      (Nothing, Just 5)
      Wibble
      (Just ("alpha", "Omega"))
      (fromGregorian 2018 3 12)
      testTable
      "This is the default."

testValues :: [DemoData]
testValues = [
      DemoData
         (Just 2, Just 6)
         Foo
         (Just ("", "ZZZ"))
         (fromGregorian 2000 1 1)
         []
         "The Foo Value.",
      DemoData
         (Just 5, Nothing)
         Bar
         (Just ("run", "Wimbledon"))
         (fromGregorian 2010 1 1)
         testTable
         "The Bar Value."
   ]
