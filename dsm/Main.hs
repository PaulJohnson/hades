{-# LANGUAGE OverloadedStrings #-}
{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

-- |
module Main (main) where

import App.ModelMain
import DSM.SafetyCase
import qualified GI.Gtk as Gtk
import Paths_dsm
import Reactive.Banana.GI.DataIconTheme
import System.FilePath


modelDescriptor :: IO (MainDescriptor SafetyModel)
modelDescriptor = do
  baseFolder <- getDataDir
  return MainDescriptor {
    programName = "dsm",
    programTitle = "Diametric Safety Case Manager",
    programXtn = "dsm",
    programStock = entityIcon,
    programIconDirs = [baseFolder],
    programAfterLoad = return (),
    programDocs = baseFolder </> "documentation",
    programSamples = Just $ baseFolder </> "examples"
  }


main :: IO ()
main = do
  dataTheme <- getDataIconTheme
  dataDir <- getDataDir
  Gtk.iconThemeSetCustomTheme dataTheme $ Just "Diametric"
  Gtk.iconThemeSetSearchPath dataTheme [dataDir]
  modelMain =<< modelDescriptor
