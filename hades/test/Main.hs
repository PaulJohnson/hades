{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

-- |
module Main where

import System.Exit
import Test.AutoMonad.Exceptions
import Test.GI.PackageTree as GI
import Test.Report.TestDocument
import Test.Expression

main :: IO ()
main = do
   putStrLn "Running document tests. Look in demo-files for the results."
   r1 <- runDocumentTest
   putStrLn "Running expression tests."
   r2 <- runExpressionTests
   putStrLn "Running GI tests."
   r3 <- GI.runTests
   putStrLn "Running AutoMonad tests."
   r4 <- runAutoMonadTests
   if and [r1, r2, r3, r4] then exitSuccess else exitFailure
