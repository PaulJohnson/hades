{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedLabels #-}

{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

-- |

module Test.Expression (
   runExpressionTests
) where

import Control.Lens
import qualified Data.Map as M
import Data.Text(Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Model.Reflection.Parser
import Model.Reflection.Types
import Model.Reflection.Values
import Test.HUnit
import Text.Megaparsec as MP


testEnv :: ExtensionValues
testEnv = M.fromList $ map (_1 %~ migrateField) [
      ("TestString", ExtText "The quick, brown fox jumped over the lazy dog."),
      ("The Answer", ExtInt 42),
      ("n", ExtInt 5),
      ("Flag", ExtBool True),
      ("False flag", ExtBool False),
      ("X", ExtReal 45.125),   -- Chosen for exact binary representation.
      ("Y", ExtReal 2.125),
      ("TheDay", ExtDate $ fromGregorian 2018 11 30),
      ("Christmas", ExtDate $ fromGregorian 2018 12 25),
      ("Find", ExtInt 4)   -- Same name as a function.
   ]


-- | Set of known identifiers. All keys in "testEnv" plus \"NoValue\".
knownFields :: FieldTable
knownFields = M.fromList $ map genField [
      ("TestString", ModelText),
      ("The Answer", ModelInt),
      ("n", ModelInt),
      ("Flag", ModelBool),
      ("False flag", ModelBool),
      ("X", ModelReal),
      ("Y", ModelReal),
      ("TheDay", ModelDate),
      ("Christmas", ModelDate),
      ("Find", ModelInt),
      ("NoValue", ModelText)
   ]
   where
      genField (n, t) = let fid = migrateField n in (fid, Field False fid n One $ BuiltInDef t)


goodCases :: [(ExtValue, Text)]
goodCases = [
      (ExtNone,         "NoValue"),
      (ExtNone,         "NoValue + 4"),
      (ExtInt (-42),    "-[The Answer]"),
      (ExtBool True,    "[The Answer]=42"),
      (ExtText "Foo",   "\"Foo\""),
      (ExtInt 4,        "Find"),
      (ExtDate $ fromGregorian 2018 4 5, "'5 April 2018'"),
      (ExtInt 42,       "[The Answer] "),
      (ExtInt 47,       "[The Answer] + n"),
      (ExtReal 3.125,   "X-[The Answer]"),
      (ExtReal 39.875,  "[The Answer] - [Y]"),
      (ExtInt 210,      "[The Answer] * n"),
      (ExtInt 8,        "[The Answer] / n"),
      (ExtInt 214,      "4 + [The Answer] * n"),
      (ExtInt 230,      "(4 + [The Answer]) * n"),
      (ExtReal 21.235294117647058, "X / Y"),
      (ExtBool True,    "X > Y"),
      (ExtBool False,   "X > Y & X < Y"),
      (ExtInt 25,       "Christmas - [TheDay]"),
      (ExtText "Foo! The quick, brown fox jumped over the lazy dog. Bark!",
                        "\"Foo! \" ++ TestString ++ \" Bark!\""),
      (ExtDate $ fromGregorian 2018 12 3, "TheDay + 3"),
      (ExtDate $ fromGregorian 2018 12 20, "Christmas - 5"),
      (ExtText "The",   "Strip ( \"  The   \"  )"),
      (ExtText "42 wibble", "Strip([The Answer] ++ \" wibble  \n\n\t\")"),
      (ExtInt 46,       "Characters(TestString)"),
      (ExtInt 9,        "  Words ( TestString) "),
      (ExtInt 0,        "Find(\"The\", TestString)"),
      (ExtInt 4,        "Find(\"quick\", TestString)"),
      (ExtInt (-1),     "Find(\"wibble\", TestString)"),
      (ExtText "quick", "Mid(TestString, 4, 2+3)"),
      (ExtText ".1",    "Mid(X, 2, 2)"),
      (ExtText "The ",  "Left ( TestString, 4)"),
      (ExtText "dog.",  "Right (TestString, 4)"),
      (ExtText "brown fox", "MidWords(TestString, 2, 2)"),
      (ExtText "The quick,", "LeftWords (TestString, 2)"),
      (ExtText "lazy dog.", "RightWords (TestString, 2)"),
      (ExtInt 2,         "If (2>3, 5, 2)"),
      (ExtInt 5,         "If (Flag, 5, 2)"),
      (ExtInt 45,        "Round(X)"),
      (ExtText "THE QUICK,", "Upper (LeftWords (TestString, 2))"),
      (ExtText "the quick,", "Lower (LeftWords (TestString, 2))"),
      (ExtInt 4,          "Min (43, 52, 4, 23, 8)"),
      (ExtReal 45.125,    "Max (4.2, 5, 8, X, Y, 20.2)"),
      (ExtNone,           "Min ()"),
      (ExtNone,           "Max ()"),
      (ExtNone,           "Min (NoValue, NoValue)"),
      (ExtInt 23,         "Max (NoValue, 23, NoValue, 4)"),
      (ExtText "wibble",  "Max (\"foo\", \"bar\", \"wibble\", \"baz\")"),
      (ExtDate $ fromGregorian 2018 11 30, "Min (TheDay, Christmas)"),
      (ExtInt 25,         "Day(Christmas)"),
      (ExtInt 12,         "Month(Christmas)"),
      (ExtNone,           "Month(NoValue)"),
      (ExtInt 2018,       "Year(Christmas)"),
      (ExtInt 48,         "Week(TheDay)"),
      (ExtInt 5,          "Weekday(TheDay)"),
      (ExtDate $ fromGregorian 2019 1 25, "AddMonth (Christmas, 1)"),
      (ExtDate $ fromGregorian 2019 2 28, "AddMonth (TheDay, 3)"),
      (ExtText "Friday",  "DayName(TheDay)"),
      (ExtText "November", "MonthName (TheDay)")
   ]


-- | Expressions that should yield an error.
badCases :: [Text]
badCases = [
      "TestString + 1",            -- Type mismatch.
      "[The Answer] = \"42\"",     -- Type mismatch.
      "Find(TestString)",          -- Too few arguments.
      "Left(TestString,3,4,5)",    -- Too many arguments.
      "Left(TestString,TheDay,4)", -- Not a number.
      "If (TestString, 4, 5)",     -- Not a boolean.
      "Day (TestString)",          -- Not a date.
      "-TestString",               -- Illegal unary operation.
      "4 / 0"                      -- Integer division by zero.
   ]


-- | Check that the expression in the text returns the @ExtValue@.
doGoodCase :: (ExtValue, Text) -> Test
doGoodCase (v, e1) = TestCase $
   case MP.parse (topExpr mempty knownFields) "" e1 of
      Left err -> assertFailure $ "Parse failed: " <> errorBundlePretty err
      Right e2 -> assertEqual (T.unpack e1) (Right v) (evaluate e2 testEnv ())


-- | Check that the expression returns an error. We don't care about the exact message.
doBadCase :: Text -> Test
doBadCase e1 = TestCase $
   case MP.parse (topExpr mempty knownFields) "" e1 of
      Left err -> assertFailure $ "Parse failed: " <> errorBundlePretty err
      Right e2 -> case evaluate e2 testEnv () of
         Left _ -> return ()  -- Correctly returned an error.
         Right v -> assertFailure $ "Bad case " <> show e1 <> " returned a value of " <> show v


runExpressionTests :: IO Bool
runExpressionTests = do
      results <- runTestTT $ TestList [goods, bads]
      putStrLn $ showCounts results
      return (errors results == 0 && failures results == 0)
   where
      goods = TestLabel "Good cases" $ TestList $ map doGoodCase goodCases
      bads = TestLabel "Bad cases" $ TestList $ map doBadCase badCases
