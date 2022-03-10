{-# LANGUAGE RecursiveDo #-}
{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

{- |

This module provides a single function that links the Reactive Banana world to the
automaton monads defined by "Hades.Abstract.AutoMonad".
-}

module Hades.Abstract.AutoBanana where

import Control.Monad.Fix
import Hades.Abstract.AutoMonad
import Reactive.Banana


-- | A Reactive Banana wrapper for AutoMonad.
applyEvents :: (MonadFix m, MonadMoment m) => StreamNext i o e s -> Event i -> m (Event o)
applyEvents initial input = mdo
   state <- stepper initial $ snd <$> result
   let result = applyInput <$> state <@> input
   return $ fst <$> result
