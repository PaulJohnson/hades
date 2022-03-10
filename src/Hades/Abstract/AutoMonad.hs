{-# LANGUAGE UndecidableInstances #-}

{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

{- |

__An automaton monad.__

A conventional automaton that takes a stream of type @i@ and outputs a stream of type @o@ is:

> newtype Auto i o = Auto (i -> (o, Auto i o))

However to create one of these you have to have an explicit state machine in which each
value of @Auto@ is a separate state.

This version provides a monadic interface with the "yield" function, so that the programmer can
define a sequence of computations in which "yield" passes an output to the caller along with a
continuation which accepts the new input. From the point of view of a monadic action in the
automaton, "yield" looks like a normal monadic action which asks the outside world for some
input. From the point of view of the application the whole computation looks like a state machine
where each input produces an output and a new state.
-}

module Hades.Abstract.AutoMonad (
   AutoClass (..),
   AutoS (..),
   AutoT (..),
   throwAutoT,
   catchAutoT,
   startMachine,
   StateStream (..),
   StreamNext (..),
   startStream,
   applyInput,
   applyThrow,
   StateStreamT (..),
   StreamNextT (..),
   startStreamT,
   startStreamT1,
   applyInputT,
   applyThrowT
) where

import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.State
import Data.Void


class (Monad m) => AutoClass i o m | m -> i, m -> o where
   -- | Yield a value of type @o@ and obtain a new value of type @i@ from outside the cage.
   yield :: o -> m i


-- | State machines with exception handlers. At each step the caller can either provide
-- input of type @i@ or an exception of type @e@. In the latter case from inside the @AutoT@
-- it looks like "yield" threw the exception.
newtype AutoS i o e m = AutoS {runAutoS :: m (o, i -> AutoS i o e m, e -> AutoS i o e m) }


-- | Continuation monad transformer for state machine actions with exception handlers.
--
-- The first function is the exception handler. The second is the continuation.
newtype AutoT i o e m a =
   AutoT {
      runAutoT ::
         (e -> AutoS i o e m)
         -> ((e -> AutoS i o e m) -> a -> AutoS i o e m)
         -> AutoS i o e m
   }

instance (Functor m) => Functor (AutoT i o e m) where
   fmap f (AutoT act) = AutoT $ \eh1 k -> act eh1 (\eh2 -> k eh2 . f)

instance (Monad m) => Applicative (AutoT i o e m) where
   pure v = AutoT $ \eh k -> k eh v
   f <*> v = do
      f1 <- f
      f1 <$> v

instance (Monad m) => Monad (AutoT i o e m) where
   return = pure
   v >>= f = AutoT $ \eh1 k -> runAutoT v eh1 $ \eh2 x -> runAutoT (f x) eh2 k

instance MonadTrans (AutoT i o e) where
   lift act = AutoT $ \eh k -> AutoS $ act >>= runAutoS . k eh

instance (MonadIO m) => MonadIO (AutoT i o e m) where
   liftIO io = AutoT $ \eh k -> AutoS $ liftIO io >>= runAutoS . k eh

instance (MonadState s m) => MonadState s (AutoT i o e m) where
   get = lift get
   put = lift . put

instance (Monad m) => AutoClass i o (AutoT i o e m) where
   yield v = AutoT $ \eh k -> AutoS $ return (v, k eh, eh)




-- | Exception handling in @AutoT@ cannot follow the normal semantics for monad
-- transformers because the simulated flow of control for the @AutoT@ actions do not match
-- that of the inner monad @m@. It is therefore necessary for the caller to catch exceptions
-- in @m@ and translate those back to exceptions in @AutoT@.
--
-- For this reason @AutoT@ is not an instance of @MonadError@. The @throwAutoT@ and @catchAutoT@
-- will behave as expected within the context of @AutoT@, but exceptions elsewhere in the
-- monad stack must be handled manually.
throwAutoT :: e -> AutoT i o e m a
throwAutoT e = AutoT $ \eh _ -> eh e

catchAutoT :: (Monad m) => AutoT i o e m a -> (e -> AutoT i o e m a) -> AutoT i o e m a
catchAutoT (AutoT act) handler = AutoT $ \eh1 k ->
      let eh2 e = runAutoT (handler e) eh1 $ \eh3 x -> runAutoT (return x) eh3 k
      in act eh2 k


-- | The initial state for an @AutoT@ action. The default exception handler calls "error".
--
-- The @Void@ return type means that the machine has no stop state. The simplest way to implement
-- this is to use "forever".
startMachine :: (Show e) => AutoT i o e m Void -> AutoS i o e m
startMachine act = runAutoT act eh $ error "Can't happen: AutoT terminated."
   where
      eh e = AutoS $ error $ "Uncaught exception in AutoT: " <> show e


-- | The StateStream monad describes a stateful stream processor.
-- The AutoT transformer allows the monad to "yield" a result and receive the next
-- event at any stage in the computation.
newtype StateStream i o e s a = StateStream {runStream :: AutoT i o e (State s) a}
    deriving (Functor, Applicative, Monad, MonadState s, AutoClass i o)


-- | The state that is threaded through "applyInput".
data StreamNext i o e s = StreamNext {
      streamInput :: i -> AutoS i o e (State s),
         -- ^ State transition for an input.
      streamThrow :: e -> AutoS i o e (State s),
         -- ^ State transition to throw an exception.
      streamState :: s
         -- ^ Current state value.
   }


-- | Create the stream processor from the StateStream action. The @Void@ type means that the
-- action can never finish. Typically this is done using "forever" or a recursive tail call.
startStream :: (Show e) => StateStream i o e s Void -> s -> (o, StreamNext i o e s)
startStream act state0 =
   let
      (AutoS m) = startMachine $ runStream act
      ((out1, inF, exceptF), state1) = runState m state0
   in (out1, StreamNext inF exceptF state1)


-- | Pass an input value in to the stream processor and get the resulting output.
applyInput :: StreamNext i o e s -> i -> (o, StreamNext i o e s)
applyInput nxt i =
   let
      (AutoS m) = streamInput nxt i
      ((out1, inF, exceptF), state1) = runState m $ streamState nxt
   in (out1, StreamNext inF exceptF state1)


-- | Pass an exception in to the stream processor. Within the "StateStream" action this will cause
-- "yield" to throw the exception.
applyThrow :: StreamNext i o e s -> e -> (o, StreamNext i o e s)
applyThrow nxt e =
   let
      (AutoS m) = streamThrow nxt e
      ((out1, inF, exceptF), state1) = runState m $ streamState nxt
   in (out1, StreamNext inF exceptF state1)


-- | The StateStream monad describes a stateful stream processor on an underlying monad.
newtype StateStreamT i o e s m a = StateStreamT {runStreamT :: AutoT i o e (StateT s m) a}
   deriving (Functor, Applicative, Monad, MonadState s, AutoClass i o)

instance MonadTrans (StateStreamT i o eh s) where
   lift action = StateStreamT $ lift $ lift action


-- | The state that is threaded through "applyInputT".
data StreamNextT i o e s m = StreamNextT {
      streamInputT :: i -> AutoS i o e (StateT s m),
         -- ^ State transition for an input.
      streamThrowT :: e -> AutoS i o e (StateT s m),
         -- ^ State transition to throw an exception.
      streamStateT :: s
         -- ^ Current state value.
   }


-- | The initial state for a "StateStreamT". This executes the "StateStreamT" action up to the
-- first "yield".
startStreamT :: (Monad m, Show e) =>
   StateStreamT i o e s m Void -> s -> m (o, StreamNextT i o e s m)
startStreamT act state0 = do
   let (AutoS m) = startMachine $ runStreamT act
   ((out1, inF, exceptF), state1) <- runStateT m state0
   return (out1, StreamNextT inF exceptF state1)


-- | An alternative way of starting a "StateStreamT" machine. This defers any execution in
-- @m@ until the first input is ready.
startStreamT1 :: (Monad m, Show e) =>
   (i -> StateStreamT i o e s m Void) -> s -> StreamNextT i o e s m
startStreamT1 act = StreamNextT inF throwF
   where
      inF i = runAutoT (runStreamT $ act i) throwF $ error "Cannot happen: StateStreamT Void exited."
      throwF e = AutoS $ error $ "Uncaught exception in StateStreamT: " <> show e


-- | Pass an input value to a monadic stream processor and get the resulting output.
applyInputT :: (Monad m, Show e) => StreamNextT i o e s m -> i -> m (o, StreamNextT i o e s m)
applyInputT nxt i = do
   let (AutoS m) = streamInputT nxt i
   ((out1, inF, exceptF), state1) <- runStateT m $ streamStateT nxt
   return (out1, StreamNextT inF exceptF state1)


-- | Pass an exception to a monadic stream processor and get the resulting output. Within the
-- "StateStreamT" monad this will cause "yield" to throw the exception.
applyThrowT :: (Monad m, Show e) => StreamNextT i o e s m -> e -> m (o, StreamNextT i o e s m)
applyThrowT nxt e = do
   let (AutoS m) = streamThrowT nxt e
   ((out1, inF, exceptF), state1) <- runStateT m $ streamStateT nxt
   return (out1, StreamNextT inF exceptF state1)
