{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Copyright © Paul Johnson 2019. See LICENSE file for details.
-}

module Demo.Notebook where

import Control.Lens
import Data.Text (Text)
import qualified Data.Text as T
import Reactive.Banana.Dialog
import Reactive.Banana.Frameworks


itemDialog :: SubDialog (Either Integer Text)
itemDialog = UnionTab id [
      UnionTabData {
            tabLabel = "Left",
            tabPrism = _Left,
            tabDefault = 0,
            tabSubDialog = Elements [LensElement (pure True) "Number: " typedTextBox id mempty]
         },
      UnionTabData {
            tabLabel = "Right",
            tabPrism = _Right,
            tabDefault = "",
            tabSubDialog = Elements [LensElement (pure True) "Text: " simpleTextBox id mempty]
         }
   ]

notebookDialog :: Integer -> MomentIO (Dialog (Either Integer Text))
notebookDialog n = return $ Dialog {
      dialogTitle = "Notebook Test " <> T.pack (show n),
      dialogMain = itemDialog
   }
