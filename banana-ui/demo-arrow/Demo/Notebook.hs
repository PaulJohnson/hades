{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the banana-ui-gtk library. The banana-ui-gtk library is
proprietary and confidential. Copying is prohibited 
-}

module Demo.Notebook where

import Control.Lens
import Data.Default.Orphan ()
import Data.Text (Text)
import Reactive.Banana.ArrowDialog


itemDialog :: Gadget' () () (Either Integer Text)
itemDialog = unionTab [
      ("Left", PrismaticGadget 0 _Left $
            accum $ form Vertical [("Number:", focusing id typedTextBox)]),
      ("Right", PrismaticGadget "" _Right $
            accum $ form Vertical [("Text:", focusing id simpleTextBox)])
   ]

notebookDialog :: Dialog () () (Either Integer Text) (Either Integer Text)
notebookDialog = Dialog {
      dialogTitle = "Notebook Test",
      dialogButtons = OkApplyButton,
      dialogGadget = itemDialog
   }
