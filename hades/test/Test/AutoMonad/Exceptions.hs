{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

{- | Test the AutoT monad transformer, including exception handling.
-}

module Test.AutoMonad.Exceptions where

import Control.Monad.Except
import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Hades.Abstract.AutoMonad
import Test.HUnit hiding (State)


runAutoMonadTests :: IO Bool
runAutoMonadTests = do
    cts <- runTestTT autoMonadTest
    return $ errors cts == 0 && failures cts == 0

autoMonadTest :: Test
autoMonadTest = TestList [
    TestCase $ runTest "AutoMonad normal termination" (testMachine 57) testCaseNormal,
    TestCase $ runTest "AutoMonad exception" (testMachine 57) testCaseUncaught
  ]


-- | Exception messages.
type Message = String

-- | Exceptions in the AutoT monad
data AutoException = LimitExceeded Integer | OtherFailure Message deriving Show

-- | An underlying monad that supports exceptions (with a different type)
newtype TestMonad a = TestMonad {runTestMonad :: ExceptT Message (State Integer) a}
  deriving (Functor, Applicative, Monad, MonadState Integer, MonadError String)

type TestMachine a = AutoT Integer Text AutoException TestMonad a

-- | Repeatedly prompts for two numbers and emits their total plus the running total. Throws an
-- AutoT exception if an input is greater than 20, and an ExceptT exception if the running total
-- exceeds 100.
testMachine :: Integer -> TestMachine Void
testMachine t =
    catchAutoT ( do
        total <- lift get
        let
          msg = "Last total = " <> T.pack (show t) <>
            ". Running total = " <> T.pack (show total) <> ".  "
        p1 <- getParameter (Just msg) 1
        p2 <- getParameter (Just $ "Got " <> T.pack (show p1) <> ".  ") 2
        let newTotal = total + p1 + p2
        when (newTotal > 100) $ lift $ throwError $ "Total limit exceeded: " <> show newTotal
        lift $ put newTotal
        testMachine $ p1 + p2
      ) $ \e -> do
        lift $ put 0
        lift $ throwError $ "Exception caught by catchAutoT: " <> show e
  where
    getParameter :: Maybe Text -> Integer -> TestMachine Integer
    getParameter msg n = catchAutoT ( do
        x <- yield $ case msg of
          Just m -> m <> "Parameter " <> T.pack (show n) <> ":"
          Nothing -> "Parameter " <> T.pack (show n) <> ":"
        when (x > 20) $ throwAutoT $ LimitExceeded x
        return x
      ) $ \e -> do
        lift $ put 0
        getParameter (Just $ "Exception " <> T.pack (show e) <> ". Try again.  ") n


-- | A test case is a list of expected yields and cooresponding inputs.
data TestSequence = TestSequence {
    testSteps :: [(Text, Either AutoException Integer)],
      -- ^ The expected "yield" value and the input to provide in return.
    testFinalYield :: Either Message Text
      -- ^ The final expected "yield" value, or an expected exception from the inner monad.
  }


-- | Run a test on a list of inputs and/or exceptions, and print the output for inspection in the
-- form of a @TestSequence@.
demoTest :: TestMachine Void -> [Either AutoException Integer] -> IO ()
demoTest machine steps = do
    putStrLn "TestSequence {\n      testSteps = ["
    runSteps 0 m1 steps
  where
    m1 = startMachine machine
    runSteps v1 autoS inputs = do
      case runState (runExceptT $ runTestMonad $ runAutoS autoS) v1 of
        (Left e, _) ->
          putStrLn $ "      ]\n      testFinalYield = Left " <> show e
        (Right (out, inF, exceptF), v2) -> case inputs of
          [] -> putStrLn $ "      ]\n      testFinalYield = Right " <> show out <> "\n   }"
          (input1: rest) -> do
            putStrLn $ "            (" <> show out <> ", " <> show input1 <> "),"
            runSteps v2 (either exceptF inF input1) rest


-- | Run a test on a @TestSequence@. Report and fail on any discrepancies.
runTest :: String -> TestMachine Void -> TestSequence -> Assertion
runTest name machine steps = assertEqual name expected results
  where
    m1 = startMachine machine
    expected = map (Right . fst) (testSteps steps) ++ [testFinalYield steps]
    results = runSteps 0 m1 $ map snd $ testSteps steps
    runSteps v1 autoS inputs = do
      case runState (runExceptT $ runTestMonad $ runAutoS autoS) v1 of
        (Left e, _) -> [Left e]
        (Right (out, inF, exceptF), v2) -> case inputs of
          [] -> [Right out]
          (input1: rest) ->
            Right out : runSteps v2 (either exceptF inF input1) rest




testCaseNormal :: TestSequence
testCaseNormal = TestSequence {
    testSteps = [
        ("Last total = 57. Running total = 0.  Parameter 1:", Right 3),
        ("Got 3.  Parameter 2:", Right 5),
        ("Last total = 8. Running total = 8.  Parameter 1:", Right 7),
        ("Got 7.  Parameter 2:", Left (OtherFailure "input exception")),
        ("Exception OtherFailure \"input exception\". Try again.  Parameter 2:", Right 24),
        ("Exception LimitExceeded 24. Try again.  Parameter 2:", Right 4),
        ("Last total = 11. Running total = 19.  Parameter 1:", Right 7),
        ("Got 7.  Parameter 2:", Right 13)
      ],
    testFinalYield = Right "Last total = 20. Running total = 39.  Parameter 1:"
  }

testCaseUncaught :: TestSequence
testCaseUncaught =  TestSequence {
    testSteps = [
        ("Last total = 57. Running total = 0.  Parameter 1:", Right 3),
        ("Got 3.  Parameter 2:", Right 5),
        ("Last total = 8. Running total = 8.  Parameter 1:", Right 7),
        ("Got 7.  Parameter 2:", Left (OtherFailure "input exception")),
        ("Exception OtherFailure \"input exception\". Try again.  Parameter 2:", Right 24),
        ("Exception LimitExceeded 24. Try again.  Parameter 2:", Right 4),
        ("Last total = 11. Running total = 19.  Parameter 1:", Right 7),
        ("Got 7.  Parameter 2:", Right 13),
        ("Last total = 20. Running total = 39.  Parameter 1:", Right 20),
        ("Got 20.  Parameter 2:", Right 20),
        ("Last total = 40. Running total = 79.  Parameter 1:", Right 20),
        ("Got 20.  Parameter 2:", Right 20)
    ],
    testFinalYield = Left "Total limit exceeded: 119"
  }
