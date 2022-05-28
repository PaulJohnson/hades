{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{- |
Copyright © Paul Johnson 2019. See LICENSE file for details.
-}

module Main where

import Control.Arrow
import Control.Lens
import Control.Monad
import qualified Data.Text as T
import Data.Tree
import Demo.Cond
import Demo.Dialog
import Demo.Notebook
-- import Demo.Tree
-- import Demo.TreeTable
import qualified GI.Gtk as Gtk
import Reactive.Banana
import Reactive.Banana.ArrowDialog
import Reactive.Banana.Frameworks
import Reactive.Banana.GI
import Reactive.Banana.GI.ArrowDialog as AD
import System.Environment


main :: IO ()
main = do
  args <- getArgs
  void $ Gtk.init $ Just $ map T.pack args
  window <- Gtk.windowNew Gtk.WindowTypeToplevel
  void $ Gtk.onWidgetDestroy window Gtk.mainQuit
  buttons <- Gtk.boxNew Gtk.OrientationVertical 5
  showDialogButton <- Gtk.buttonNewWithLabel "Show test dialog"
  -- showTreeDialogButton <- Gtk.buttonNewWithLabel "Show tree dialog"
  showNotebookDialogButton <- Gtk.buttonNewWithLabel "Show notebook dialog"
  showCondDialogButton <- Gtk.buttonNewWithLabel "Show Conditional dialog"
  showLoopDialogButton <- Gtk.buttonNewWithLabel "Show Loop dialog"
  -- showTreeTableButton <- Gtk.buttonNewWithLabel "Show tree table dialog"
  Gtk.containerAdd buttons showDialogButton
  -- Gtk.containerAdd buttons showTreeDialogButton
  Gtk.containerAdd buttons showNotebookDialogButton
  Gtk.containerAdd buttons showCondDialogButton
  Gtk.containerAdd buttons showLoopDialogButton
  -- Gtk.containerAdd buttons showTreeTableButton
  Gtk.containerAdd window buttons
  Gtk.widgetShowAll window
  iconTheme <- Gtk.iconThemeGetDefault
  net <- compile $ do -- MomentIO
    -- Connect the test dialog button.
    testCount <- registerIOSignal showDialogButton Gtk.onButtonClicked $ return ((), (+1))
    activateDialog <- accumE (0 :: Int) testCount <&> fmap popupParameters
    resultE <- AD.mkGtkPopupSelect
        window
        iconTheme
        (pure ())
        (constantDialog demoDialog)
        activateDialog
    reactimate $ resultE <&> \(n, mValue) ->
      case mValue of
        Nothing -> putStrLn "Dialog cancelled."
        Just v -> print (n, v)
    {-
    -- Connect the tree dialog button.
    treeCount <- registerIOSignal showTreeDialogButton Gtk.onButtonClicked $ return ((), (+1))
    treeActivate <- accumE 0 treeCount <&> fmap treeParameters
    resultTree <- mkGtkPopup window treeActivate
    reactimate $ resultTree <&> \(n, mSetter) ->
      case mSetter of
        Nothing -> putStrLn "Tree dialog cancelled."
        Just setter -> print (n, setter nullTreeData)
    -}
    -- Connect the notebook dialog button.
    notebookCount <- registerIOSignal showNotebookDialogButton Gtk.onButtonClicked $
        return ((), (+1))
    notebookActivate <- accumE 0 notebookCount <&> fmap notebookParameters
    resultUnion <- AD.mkGtkPopupSelect
        window
        iconTheme
        (pure ())
        (constantDialog notebookDialog)
        notebookActivate
    reactimate $ resultUnion <&> \(n :: Int, mVal) ->
      case mVal of
        Nothing -> putStrLn "Notebook dialog cancelled."
        Just v -> print (n, v)
    -- Connect the Cond dialog button
    condCount <- registerIOSignal showCondDialogButton Gtk.onButtonClicked $
        return ((), (+1))
    condActivate <- accumE 0 condCount <&> fmap notebookParameters -- Same data as notebook
    resultUnion1 <- AD.mkGtkPopupSelect
        window
        iconTheme
        (pure ())
        (constantDialog condDialog)
        condActivate
    reactimate $ resultUnion1 <&> \(n :: Int, mVal) ->
      case mVal of
        Nothing -> putStrLn "Cond dialog cancelled."
        Just v -> print (n, v)
    -- Connect the Loop dialog button
    loopCount <- registerIOSignal showLoopDialogButton Gtk.onButtonClicked $
        return ((), (+1))
    loopActivate <- accumE 0 loopCount <&> fmap (,0)
    resultLoop <- AD.mkGtkPopupSelect
        window
        iconTheme
        (pure ())
        (constantDialog loopDialog1)
        loopActivate
    reactimate $ resultLoop <&> \(n :: Int, mVal) ->
      case mVal of
        Nothing -> putStrLn "Loop dialog cancelled"
        Just v -> print (n, v)
    reactimate $ putStrLn (drawTree $ showGadget $ dialogGadget loopDialog1) <$ loopCount
    {-
    -- Connect the tree table dialog button.
    treeTableCount <- registerIOSignal showTreeTableButton Gtk.onButtonClicked $
        return ((), (+1))
    treeTableActivate <- accumE 0 treeTableCount <&> fmap treeTableParameters
    resultForest <- mkGtkPopup window treeTableActivate
    reactimate $ resultForest <&> \(n, mSetter) ->
      case mSetter of
        Nothing -> putStrLn "Tree Table cancelled."
        Just setter -> print (n, setter testTreeTable)
    -}
  actuate net
  Gtk.main
  where
    popupParameters n = (n, initialValue)
    -- treeParameters n = (True, n, nullTreeData, treeDialog n)
    notebookParameters n = (n, Right "Foo!")
    -- treeTableParameters n = (True, n, testTreeTable, treeTableDialog n)


loopDialog1 :: Dialog () () Int Int
loopDialog1 = Dialog "Loop Demo 1" OkApplyButton $ getInitial $ \n0 ->
  let btns = [("+1", (+1)), ("+5", (+5)), ("+10", (+10)), ("Reset", const n0)]
  in accum (buttonBar btns <<< typedTextBox)
