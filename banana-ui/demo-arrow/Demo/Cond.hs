{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows #-}

{- |
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the banana-ui-gtk library. The banana-ui-gtk library is
proprietary and confidential. Copying is prohibited 
-}

module Demo.Cond where

import Control.Lens
import Data.Default
import Data.Default.Orphan ()
import Data.Text (Text)
import Reactive.Banana.ArrowDialog


data Side = LeftSide | RightSide deriving (Eq, Show)

makeSelection :: (Default a, Default b) => Side -> Either a b -> Either a b
makeSelection LeftSide (Right _) = Left def
makeSelection RightSide (Left _) = Right def
makeSelection _ x = x

itemDialogs :: Gadget' () () (Either Integer Text)
itemDialogs = cond [
      PrismaticGadget 0 _Left $
            accum $ form Vertical [("Number:", focusing id typedTextBox)],
      PrismaticGadget "" _Right $
            accum $ form Vertical [("Text:", focusing id simpleTextBox)]
   ]

sideMenu :: Gadget' () () Side
sideMenu = comboBox $ const [
      ComboItem "Number" Nothing Nothing LeftSide,
      ComboItem "Text" Nothing Nothing RightSide
   ]

condDialog :: Dialog () () (Either Integer Text) (Either Integer Text)
condDialog = Dialog {
      dialogTitle = "Conditional Test",
      dialogButtons = OkApplyButton,
      dialogGadget = proc input -> do
         s <- sideMenu -< case input of {Left {} -> LeftSide; Right {} -> RightSide}
         itemDialogs -< makeSelection s input
   }
