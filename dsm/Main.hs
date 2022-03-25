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


modelDescriptor :: IO (MainDescriptor SafetyModel)
modelDescriptor = do
   paths <- iconPaths
   icons <- dialogIcons
   return MainDescriptor {
      programName = "dsm",
      programTitle = "Diametric Safety Case Manager 1.3",
      programXtn = "dsm",
      programStock = entityIcon,
      programIconDirs = paths,
      programDialogIcons = icons,
      programFirstLoad = Just "initial-model.dsm",
      programAfterLoad = return ()
   }


main :: IO ()
main = do
   desc <- modelDescriptor
   modelMain desc


iconPaths :: IO [FilePath]
iconPaths = sequence [causalityIcons, gsnIcons]


dialogIcons :: IO Gtk.IconTheme
dialogIcons = do
   base <- getDataDir
   r <- Gtk.iconThemeNew
   Gtk.iconThemeSetCustomTheme r $ Just "Diametric"
   Gtk.iconThemeSetSearchPath r [base]
   return r
