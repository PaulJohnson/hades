{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

-- |
module Main (main) where

import App.ModelMain
import Causality.FieldNames
import DSM.SafetyCase
import GSN.FieldNames


modelDescriptor :: IO (MainDescriptor SafetyModel)
modelDescriptor = do
   paths <- iconPaths
   return MainDescriptor {
      programName = "dsm",
      programTitle = "Diametric Safety Case Manager 1.3",
      programXtn = "dsm",
      programStock = entityIcon,
      programIconDirs = paths,
      programFirstLoad = Just "initial-model.dsm",
      programAfterLoad = return ()
   }


main :: IO ()
main = do
   desc <- modelDescriptor
   modelMain desc


iconPaths :: IO [FilePath]
iconPaths = sequence [causalityIcons, gsnIcons]
