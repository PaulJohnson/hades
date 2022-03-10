{-
Copyright © Paul Johnson 2021. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

{- | A @TState@ is an approximation to the concepts of \"events\" and \"behaviours\" from functional
reactive programming, cast into a form suitable for @STM@ transactions. The event stream is
represented by a transaction which will 'retry' until an event occurs, and the behaviour is
represented by a transaction which always returns a result.
-}

module Control.Concurrent.TState (
   TState (..),
   newTState,
   newTStateIO,
   tStateDupIO,
   mapTState1,
   filterTStateMaybe,
   filterTStateChanges,
   unionTStates
) where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import GHC.Conc
import GHC.Stack
import Data.Maybe

{- | This arranges STM transactions into something akin to functional reactive programming
with arrows. The Behaviour component is always available (the underlying implementation is a
set of TVars), while the Change component is only available instantaneously when the value
changes (the underlying implementation is one or more Queues or similar).

If there are events queued up then the event value and behaviour value may not be equal, but
if the event queue is empty then the behaviour will be the most recent value from it.

If multiple events are queued up then 'tStateEvent' will drop all but the last one.

Reading events is a destructive operation; once an event is read it can never be seen by
any other transaction. Hence any @TState@ should be duplicated before reading events
(behaviour is not an issue).

The results from the applicative and functor instances will read from the argument event streams.
Duplicating the result will do a deep duplication on all the event sources.

The event channel carries an extra value which is propogated via events but not via the behaviour.
-}
data TState a b = TState {
      tStateBehaviour :: STM b,  -- ^ Never retries.
      tStateEvent :: STM (a, b),  -- ^ Retries until an event occurs, and consumes that event.
      tStateDup :: STM (TState a b)
         -- ^ Duplicate this TState. From now on events will appear on both this and the original.
   } deriving Functor

instance Applicative (TState a) where
   pure v = let r = TState (return v) retry (return r) in r
   f <*> v = TState (tStateBehaviour f <*> tStateBehaviour v) (e1 `orElse` e2) dup
      where
         e1 = do
            (a, ef1) <- tStateEvent f
            b1 <- tStateBehaviour v
            return (a, ef1 b1)
         e2 = do
            (a, eb1) <- tStateEvent v
            f1 <- tStateBehaviour f
            return (a, f1 eb1)
         dup = do
            f1 <- tStateDup f
            v1 <- tStateDup v
            return $ f1 <*> v1


-- | Create a new @TState@, along with a transaction that writes a new value to it.
newTState :: HasCallStack => b -> STM (TState a b, a -> b -> STM ())
newTState x = do
      b <- newTVar x
      e <- newBroadcastTChan
      e1 <- dupTChan e
      let
         r = TState {
               tStateBehaviour = readTVar b,
               tStateEvent = getLatest e1,
               tStateDup = dup
            }
         writer av bv = {-# SCC tState_writer #-} do
            writeTVar b bv
            writeTChan e (av, bv)
         dup = {-# SCC tState_dup #-} do
            newE <- dupTChan e
            return $ r {tStateEvent = getLatest newE}
      return (r, writer)
   where
      getLatest c = do
         r <- readTChan c
         e <- isEmptyTChan c
         if e then return r else getLatest c


newTStateIO :: (MonadIO m) => b -> m (TState a b, a -> b -> STM ())
newTStateIO = liftIO . atomically . newTState


-- Convenience function to duplicate the TState in the IO monad.
tStateDupIO :: (MonadIO m) => TState a b -> m (TState a b)
tStateDupIO = liftIO . atomically . tStateDup


-- | Equivalent of fmap for the first type argument.
mapTState1 :: (a1 -> a2) -> TState a1 b -> TState a2 b
mapTState1 f ts = TState {
      tStateBehaviour = tStateBehaviour ts,
      tStateEvent = (_1 %~ f) <$> tStateEvent ts,
      tStateDup = mapTState1 f <$> tStateDup ts
   }


-- | Creates a dupe of the @TState@ which filters out @Nothing@.
filterTStateMaybe :: (HasCallStack, MonadIO m) =>
   b    -- ^ Initial value if the argument's value is @Nothing@e.
   -> TState a (Maybe b)   -- ^ Value stream to duplicate.
   -> m (TState a b)
filterTStateMaybe d s1 = do
   ((o, writer), s2) <- liftIO $ atomically $ do
      i <- tStateBehaviour s1
      s2 <- tStateDup s1
      output <- newTState $ fromMaybe d i
      return (output, s2)
   threadId <- liftIO $ forkIO $ forever $ atomically $
      tStateEvent s2 >>= \case
         (a, Just b) -> writer a b
         (_, Nothing) -> retry
   liftIO $ labelThread threadId "filterTStateMaybe"
   return o


-- | Creates a dupe of the @TState@ which filters out events that are equal to the old value.
filterTStateChanges :: (HasCallStack, Eq b, MonadIO m) => TState a b -> m (TState a b)
filterTStateChanges s1 = do
   ((o, writer), s2) <- liftIO $ atomically $ do
      i <- tStateBehaviour s1
      s2 <- tStateDup s1
      output <- newTState i
      return (output, s2)
   threadId <- liftIO $ forkIO $ forever $ atomically $ do
      oldVal <- tStateBehaviour o
      newVal <- tStateEvent s2
      when (oldVal /= snd newVal) $ uncurry writer newVal
   liftIO $ labelThread threadId "filterTStateChanges"
   return o


-- | Provide the union of events in a list of TStates.
unionTStates :: (HasCallStack, MonadIO m) =>
   b   -- ^ Initial value of the union.
   -> [TState a b]
   -> m (TState a b)
unionTStates initial inputs = do
   (output, sendOut) <- newTStateIO initial
   threadId <- liftIO $ forkIO $ forever $ atomically $
      foldr (orElse . tStateEvent) retry inputs >>= uncurry sendOut
   liftIO $ labelThread threadId "unionTStates"
   return output
