{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Copyright © Paul Johnson 2019. See LICENSE file for details.
-}

module Main where

import Control.Lens
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Demo.Dialog
import Demo.Notebook
import Demo.Tree
import Demo.TreeTable
import qualified GI.Gtk as Gtk
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.GI
import System.Environment



main :: IO ()
main = do
   args <- getArgs
   void $ Gtk.init $ Just $ map T.pack args
   window <- Gtk.windowNew Gtk.WindowTypeToplevel
   void $ Gtk.onWidgetDestroy window Gtk.mainQuit
   buttons <- Gtk.boxNew Gtk.OrientationVertical 5
   showDialogButton <- Gtk.buttonNewWithLabel "Show test dialog"
   showTreeDialogButton <- Gtk.buttonNewWithLabel "Show tree dialog"
   showNotebookDialogButton <- Gtk.buttonNewWithLabel "Show notebook dialog"
   showTreeTableButton <- Gtk.buttonNewWithLabel "Show tree table dialog"
   Gtk.containerAdd buttons showDialogButton
   Gtk.containerAdd buttons showTreeDialogButton
   Gtk.containerAdd buttons showNotebookDialogButton
   Gtk.containerAdd buttons showTreeTableButton
   Gtk.containerAdd window buttons
   Gtk.widgetShowAll window
   net <- compile $ do -- MomentIO
      -- Connect the test dialog button.
      testCount <- registerIOSignal showDialogButton Gtk.onButtonClicked $ return ((), (+1))
      activateDialog <- accumE 0 testCount <&> fmap popupParameters
      resultE <- mkGtkPopup window activateDialog
      reactimate $ resultE <&> \(n, mSetter) ->
         case mSetter of
            Nothing -> putStrLn "Dialog cancelled."
            Just setter -> print (n, map setter testValues)
      -- Connect the tree dialog button.
      treeCount <- registerIOSignal showTreeDialogButton Gtk.onButtonClicked $ return ((), (+1))
      treeActivate <- accumE 0 treeCount <&> fmap treeParameters
      resultTree <- mkGtkPopup window treeActivate
      reactimate $ resultTree <&> \(n, mSetter) ->
         case mSetter of
            Nothing -> putStrLn "Tree dialog cancelled."
            Just setter -> print (n, setter nullTreeData)
      -- Connect the notebook dialog button.
      notebookCount <- registerIOSignal showNotebookDialogButton Gtk.onButtonClicked $
            return ((), (+1))
      notebookActivate <- accumE 0 notebookCount <&> fmap notebookParameters
      resultUnion <- mkGtkPopup window notebookActivate
      reactimate $ resultUnion <&> \(n, mSetter) ->
         case mSetter of
            Nothing -> putStrLn "Notebook dialog cancelled."
            Just setter -> print (n, setter (Left 4) :: Either Integer Text)
      -- Connect the tree table dialog button.
      treeTableCount <- registerIOSignal showTreeTableButton Gtk.onButtonClicked $
            return ((), (+1))
      treeTableActivate <- accumE 0 treeTableCount <&> fmap treeTableParameters
      resultForest <- mkGtkPopup window treeTableActivate
      reactimate $ resultForest <&> \(n, mSetter) ->
         case mSetter of
            Nothing -> putStrLn "Tree Table cancelled."
            Just setter -> print (n, setter testTreeTable)
   actuate net
   Gtk.main
   where
      popupParameters n = (True, n, initialValue, demoDialog n)
      treeParameters n = (True, n, nullTreeData, treeDialog n)
      notebookParameters n = (True, n, Right "Foo!", notebookDialog n)
      treeTableParameters n = (True, n, testTreeTable, treeTableDialog n)
