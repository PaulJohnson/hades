{-# LANGUAGE OverloadedStrings #-}
{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

-- |
module Main (main) where

import App.ModelMain
import Causality.FieldNames
import DSM.SafetyCase
import qualified GI.Gtk as Gtk
import GSN.FieldNames
import Paths_dsm
import Reactive.Banana.GI.DataIconTheme


modelDescriptor :: IO (MainDescriptor SafetyModel)
modelDescriptor = do
   icons <- iconPaths
   return MainDescriptor {
      programName = "dsm",
      programTitle = "Diametric Safety Case Manager 1.3",
      programXtn = "dsm",
      programStock = entityIcon,
      programIconDirs = icons,
      programFirstLoad = Just "initial-model.dsm",
      programAfterLoad = return ()
   }


main :: IO ()
main = do
   dataTheme <- getDataIconTheme
   dataDir <- getDataDir
   Gtk.iconThemeSetCustomTheme dataTheme $ Just "Diametric"
   Gtk.iconThemeSetSearchPath dataTheme [dataDir]
   modelMain =<< modelDescriptor


iconPaths :: IO [FilePath]
iconPaths = sequence [causalityIcons, gsnIcons]
