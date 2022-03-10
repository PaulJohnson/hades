{-# LANGUAGE Arrows #-}

{-
Copyright © Paul Johnson 2021. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.
-}


module Matrix where

import Control.Concurrent.STM
import Control.Concurrent.TState
import Control.Lens
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Data.Tree
import Network.JavaScript
import Network.Webits
import Network.Webits.ArrowDialog
import Reactive.Banana.ArrowDialog hiding (send)
import Reactive.Banana.Common
import Test.QuickCheck

import Types

import Debug.Trace


matrixColumns :: [(Text, [(Text, GadgetF' e w DemoData)])]
matrixColumns = [
      ("Group One", [
            ("Text", focusing demoText displayMemo),
            ("Number", focusing demoNum typedTextBox),
            ("Choice", focusing demoEnum boundedCombo),
            ("Boolean", focusing demoBool tickBox)
         ]), ("Group Two", [
            ("Text", focusing demoText displayMemo),
            ("Date", focusing demoDate $ dateBox shortDate),
            ("Icon", focusing demoIcon $ iconBox (const True)),
            ("Colour", focusing demoColour colourBox)
         ])

   ]


type Model = Map Int DemoData

type MatrixItems = [(Int, [Int])]


demoMatrixAction :: TState Bool Model -> (Bool -> Model -> STM ()) -> MatrixItems -> Elmer ()
demoMatrixAction modelState modelWriter keys = do
   top <- asks elmerTop
   eng <- asks elmerEngine
   gOut <- renderGadget (pure ()) (GadgetData True <$> modelState) $ demoMatrix keys
   mapM_ (showWebit top) $ gadgetView gOut
   liftIO $ send eng $ gadgetDomCode gOut
   void $ forever $ liftIO $ atomically $ do
      update <- gadgetEvents gOut
      case update of
         Left {} -> return ()
         Right (k, updateF) -> do
            traceM $ "demoMatrixAction: got an event on row " <> show k
            oldVal <- tStateBehaviour $ gadgetOutput gOut
            let newVal = ix k %~ updateF $ oldVal ^. gdValue
            modelWriter False newVal


-- | Generate a model with N items, and a random selection of rows and sub-rows to display.
arbitraryMatrix :: Int -> Gen (Model, MatrixItems)
arbitraryMatrix n = do
   ds <- vector n
   let
      ns = [0 .. n-1]
      mdl = M.fromList $ zip ns ds
   subs <- vectorOf n $ sublistOf ns
   return (mdl, zip ns subs)


-- | Display a selection of keys taken from the model.
demoMatrix :: [(Int, [Int])] -> Gadget' e (Either w (Int, DemoData -> DemoData)) Model
demoMatrix = accum . forestTable matrixColumns . const . map (uncurry mknod)
   where
      mknod k ks = Node (k, Lens1 $ at k . defaultIso demoDefault) $ map (`mknod` []) ks


-- | Create an "Iso'" betweeen a @Maybe@ value and a plain value by using a
-- default that equates to @Nothing@. Useful for things like
--
-- > optionalText = defaultIso ""
--
-- which will equate an empty string with @Nothing@.
--
-- This is not quite a true Iso because @Just d@ will round-trip to @Nothing@ when
-- @d@ is the default value.
defaultIso :: (Eq a) => a -> Iso' (Maybe a) a
defaultIso d = iso f b
   where
      f Nothing = d
      f (Just v) = v
      b v = if v == d then Nothing else Just v
