{-# OPTIONS_GHC -fno-warn-orphans #-}

{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}


{- |
-}

module Model.Reflection.Parser (
  FunctionName,
  Function,
  FunctionTable,
  arity,
  getArg,
  notNone,
  Expr (..),
  Opr (..),
  topExpr,
  evaluate
) where

import Control.Lens
import Control.Monad
import Control.Monad.Combinators.Expr
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.LocalTime
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Model.Reflection.Types
import Model.Reflection.Values
import Reactive.Banana.Common (datePrism, shortDate)
import System.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec as MP
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex


type Parser = Parsec Text Text

-- Orphan instance. Too bad.
instance ShowErrorComponent Text where
 showErrorComponent = T.unpack


-- | Local version of lexeme.
lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme space


-- | Operator.
data Opr =
  PlusOp | MinusOp | TimesOp | DivOp
  | EqualOp | NotEqualOp
  | LessOp | LessEqOp | GreaterOp | GreaterEqOp
  | AndOp | OrOp | NotOp
  | ConcatOp
  deriving Eq

instance Show Opr where show = T.unpack . operatorName

operatorName :: Opr -> Text
operatorName v = fromMaybe "?" $ lookup v $ map (\(x,y) -> (y, x)) opList

-- | Parse an operator.
operator :: Parser Opr
operator = label "operator" $ lexeme $ do
    opStr <- T.pack <$> some (oneOf opChars)
    case lookup opStr opList of
      Just v -> return v
      Nothing -> customFailure $ "Unrecognised operator " <> opStr
  where
    opChars :: String
    opChars = "+=-*/<>&|!"

-- | The list of operators and their text representations.
opList :: [(Text, Opr)]
opList = [
    ("+", PlusOp), ("-", MinusOp), ("*", TimesOp), ("/", DivOp),
    ("=", EqualOp), ("!=", NotEqualOp),
    ("<", LessOp), ("<=", LessEqOp), (">", GreaterOp), (">=", GreaterEqOp),
    ("&", AndOp), ("|", OrOp), ("!", NotOp),
    ("++", ConcatOp)
  ]

type FunctionName = Text

-- ^ The vector of argument values to the function, the entity field values (including built-in
-- values) and an application-specific environment.
-- Returns either an error message or the result value.
type Function e = Vector ExtValue -> ExtensionValues -> e -> Either Text ExtValue


-- | Handy lookup table
type FunctionTable e = Map FunctionName (Function e)


-- | Check the number of arguments.
arity :: FunctionName -> Vector ExtValue -> Int -> Either Text ()
arity nm argVals n = if V.length argVals /= n
  then Left $ nm <> " takes " <> T.pack (show n) <>
      " arguments, not " <> T.pack (show (V.length argVals)) <> "."
  else Right ()


-- | Get one of the arguments.
getArg :: FunctionName -> Vector ExtValue -> Int -> Either Text ExtValue
getArg nm argVals n = if n >= V.length argVals
  then Left $ "Not enough arguments for " <> nm
  else Right $ argVals ! n


-- | Only evaluate v if the numbered arguments are not ExtNone. Otherwise return ExtNone.
notNone :: FunctionName -> Vector ExtValue -> [Int] -> Either Text ExtValue -> Either Text ExtValue
notNone nm argVals ns v = do
  args <- mapM (getArg nm argVals) ns
  if ExtNone `elem` args then return ExtNone else v


-- | Evaluate a value as an Integer
asInt :: ExtValue -> Either Text Integer
asInt (ExtInt n) = Right n
asInt (ExtReal n) = Right $ round n
asInt v = Left $ "Not a number: " <> T.take 20 (displayValue v)


-- | Evaluate a value as a boolean.
asBool :: ExtValue -> Either Text Bool
asBool (ExtBool b) = Right b
asBool v = Left $ "Not a boolean: " <> T.take 20 (displayValue v)


-- | Extract date as (y, m, d) triple.
asDate :: ExtValue -> Either Text (Integer, Int, Int)
asDate (ExtDate d) = Right $ toGregorian d
asDate v = Left $ "Not a date: " <> T.take 20 (displayValue v)


-- | The date as a Day.
asDay :: ExtValue -> Either Text Day
asDay (ExtDate d) = Right d
asDay v = Left $ "Not a date: " <> T.take 20 (displayValue v)


-- | Extract date as (y, w, d) triple.
asDateW :: ExtValue -> Either Text (Integer, Int, Int)
asDateW (ExtDate d) = Right $ toWeekDate d
asDateW v = Left $ "Not a date: " <> T.take 20 (displayValue v)


-- | Functions provided for all expression evaluators.
builtInFunctions :: FunctionTable e
builtInFunctions = M.fromList [
    ("Characters", \argVals _ _ -> do
      arity "Characters" argVals 1
      ExtInt . fromIntegral . T.length . displayValue <$> getArg "Characters" argVals 0),
    ("Words", \argVals _ _ -> do
      arity "Words" argVals 1
      ExtInt . fromIntegral . length . T.words . displayValue <$> getArg "Words" argVals 0),
    ("Strip", \argVals _ _ -> do
      arity "Strip" argVals 1
      ExtText . T.strip . displayValue <$> getArg "Strip" argVals 0),
    ("Find", \argVals _ _ -> do
      arity "Find" argVals 2
      key <- displayValue <$> getArg "Find" argVals 0
      str <- displayValue <$> getArg "Find" argVals 1
      let (t1, t2) = T.breakOn key str
      return $ ExtInt $ if T.length t2 > 0
        then fromIntegral $ T.length t1
        else (-1)),  -- Not found.
    ("Mid", \argVals _ _ -> do
      arity "Mid" argVals 3
      notNone "Mid" argVals [1, 2] $ do
        str <- T.unpack . displayValue <$> getArg "Mid" argVals 0
        start <- getArg "Mid" argVals 1 >>= asInt
        cnt <- getArg "Mid" argVals 2 >>= asInt
        return $ ExtText $ T.pack $ midList (fromIntegral start) (fromIntegral cnt) str),
    ("Left", \argVals _ _ -> do
      arity "Left" argVals 2
      notNone "Left" argVals [1] $ do
        str <- displayValue <$> getArg "Left" argVals 0
        cnt <- getArg "Left" argVals 1 >>= asInt
        return $ ExtText $ T.take (fromIntegral cnt) str),
    ("Right", \argVals _ _ -> do
      arity "Right" argVals 2
      notNone "Right" argVals [1] $ do
        str <- displayValue <$> getArg "Right" argVals 0
        cnt <- getArg "Right" argVals 1 >>= asInt
        return $ ExtText $ T.takeEnd (fromIntegral cnt) str),
    ("MidWords", \argVals _ _ -> do
      arity "MidWords" argVals 3
      notNone "MidWords" argVals [1, 2] $ do
        strs <- T.words . displayValue <$> getArg "MidWords" argVals 0
        start <- getArg "MidWords" argVals 1 >>= asInt
        cnt <- getArg "MidWords" argVals 2 >>= asInt
        return $ ExtText $ T.unwords $ midList (fromIntegral start) (fromIntegral cnt) strs),
    ("LeftWords", \argVals _ _ -> do
      arity "LeftWords" argVals 2
      notNone "LeftWords" argVals [1] $ do
        strs <- T.words . displayValue <$> getArg "LeftWords" argVals 0
        cnt <- getArg "LeftWords" argVals 1 >>= asInt
        return $ ExtText $ T.unwords $ take (fromIntegral cnt) strs),
    ("RightWords", \argVals _ _ -> do
      arity "RightWords" argVals 2
      notNone "RightWords" argVals [1] $ do
        strs <- T.words . displayValue <$> getArg "RightWords" argVals 0
        cnt <- getArg "RightWords" argVals 1 >>= asInt
        return $ ExtText $ if cnt < 1
          then ""
          else T.unwords $ midList (fromIntegral (-cnt)) maxBound strs),
    ("If", \argVals _ _ -> do
      arity "If" argVals 3
      notNone "If" argVals [0] $ do
        b <- getArg "If" argVals 0 >>= asBool
        tVal <- getArg "If" argVals 1
        fVal <- getArg "If" argVals 2
        return $ if b then tVal else fVal),
    ("Round", \argVals _ _ -> do
      arity "Round" argVals 1
      notNone "Round" argVals [0] $ ExtInt <$> (getArg "Round" argVals 0 >>= asInt)),
    ("Upper", \argVals _ _ -> do
      arity "Upper" argVals 1
      notNone "Upper" argVals [0] $
        ExtText . T.toUpper . displayValue <$> getArg "Upper" argVals 0),
    ("Lower", \argVals _ _ -> do
      arity "Lower" argVals 1
      notNone "Lower" argVals [0] $
        ExtText . T.toLower . displayValue <$> getArg "Lower" argVals 0),
    ("Min", \argVals _ _ -> do
      case filter (/= ExtNone) $ V.toList argVals of
        [] -> return ExtNone
        xs -> return $ minimum xs),
    ("Max", \argVals _ _ -> do
      case filter (/= ExtNone) $ V.toList argVals of
        [] -> return ExtNone
        xs -> return $ maximum xs),
    ("Day", \argVals _ _ -> do
      arity "Day" argVals 1
      notNone "Day" argVals [0] $ do
        (_, _, d) <- getArg "Day" argVals 0 >>= asDate
        return $ ExtInt $ fromIntegral d),
    ("Month", \argVals _ _ -> do
      arity "Month" argVals 1
      notNone "Month" argVals [0] $ do
        (_, m, _) <- getArg "Month" argVals 0 >>= asDate
        return $ ExtInt $ fromIntegral m),
    ("Year", \argVals _ _ -> do
      arity "Year" argVals 1
      notNone "Year" argVals [0] $ do
        (y, _, _) <- getArg "Year" argVals 0 >>= asDate
        return $ ExtInt $ fromIntegral y),
    ("Week", \argVals _ _ -> do
      arity "Week" argVals 1
      notNone "Week" argVals [0] $ do
        (_, w, _) <- getArg "Week" argVals 0 >>= asDateW
        return $ ExtInt $ fromIntegral w),
    ("Weekday", \argVals _ _ -> do
      arity "Weekday" argVals 1
      notNone "Weekday" argVals [0] $ do
        (_, _, d) <- getArg "Weekday" argVals 0 >>= asDateW
        return $ ExtInt $ fromIntegral d),
    ("DayName", \argVals _ _ -> do
      arity "DayName" argVals 1
      notNone "DayName" argVals [0] $ do
        (_, _, d) <- getArg "DayName" argVals 0 >>= asDateW
        return $ ExtText $ dayNames ! d),  -- Safe because d must be in range 1-7.
    ("MonthName", \argVals _ _ -> do
      arity "MonthName" argVals 1
      notNone "MonthName" argVals [0] $ do
        (_, m, _) <- getArg "MonthName" argVals 0 >>= asDate
        return $ ExtText $ monthNames ! m),  -- Safe because m must be in range 1-12.
    ("AddMonth", \argVals _ _ -> do
      arity "AddMonth" argVals 2
      notNone "AddMonth" argVals [0, 1] $ do
        d <- getArg "AddMonth" argVals 0 >>= asDay
        i <- getArg "AddMonth" argVals 1 >>= asInt
        return $ ExtDate $ addGregorianMonthsClip i d),
    ("Today", \argVals _ _ -> do
      arity "Today" argVals 0
      return $ unsafePerformIO $ ExtDate . localDay . zonedTimeToLocalTime <$> getZonedTime)
      -- UnsafePerformIO can safely assume the current date does not change.
  ]
  where
    dayNames = V.fromList ["", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
        "Saturday", "Sunday"]
    monthNames = V.fromList ["", "January", "February", "March", "April", "May", "June", "July",
        "August", "September", "October", "November", "December"]


-- | Filter predicate expressions.
data Expr e =
  FieldId FieldId
  | Application (Function e) [Expr e]
  | BinaryOp Opr (Expr e) (Expr e)
  | UnaryOp Opr (Expr e)
  | ValueLit ExtValue


-- | Top level filter expression. This includes leading and trailing spaces and an EOF.
-- The function table has the built-in functions added automatically. Built-in functions can be
-- overridden by functions of the same name, but this is not a good idea.
topExpr ::
  FunctionTable e
  -> FieldTable   -- ^ Known identifiers for extension values.
  -> Parser (Expr e)
topExpr funcs ftable = do
    space
    r <- expr (M.union funcs builtInFunctions) $ fieldsByName ftable
    space
    eof
    return r


-- | Expression parser. Note that the top level caller should also allow for
-- leading spaces. So something like @space >> expr@
expr :: FunctionTable e -> Map FieldName Field -> Parser (Expr e)
expr funcs ftable = makeExprParser term opTable
  where
    term =
        term1 <|>
        longIdent ftable <|>
        parExp <|>
        dateLit <|>
        stringLit <|>
        numberLit
    term1 = do -- A sequence of letters could be an identifier or function name or Boolean.
      ident <- label "name" $ lexeme $ do
        c1 <- letterChar
        cs <- many (alphaNumChar <|> char '_')
        return $ T.pack $ c1 : cs
      isFunc <- lookAhead $ try (lexeme (char '(') >> return True) <|> return False
      if isFunc
        then label "function arguments" $ do
          void $ lexeme $ char '('
          function <- case M.lookup ident funcs of
            Just f -> return f
            Nothing -> fail $ "Not a valid function name: " <> T.unpack ident
          es <- sepBy (expr funcs ftable) $ lexeme $ char ','
          void $ lexeme $ char ')'
          return $ Application function es
        else case booleanName ident of
            Just v -> return v
            Nothing -> case M.lookup ident ftable of
              Just f -> return $ FieldId $ fieldId f
              Nothing -> fail $ "Unknown value name: " <> T.unpack ident
    parExp = do
      void $ lexeme $ char '('
      e <- expr funcs ftable
      void $ lexeme $ char ')'
      return e
    opTable =
      [  [  unaryOp MinusOp ],
        [  infixOp TimesOp, infixOp DivOp ],
        [  infixOp PlusOp, infixOp MinusOp ],
        [  infixOp ConcatOp ],
        [  infixOp LessOp, infixOp LessEqOp, infixOp GreaterOp, infixOp GreaterEqOp ],
        [  infixOp EqualOp, infixOp NotEqualOp ],
        [  unaryOp NotOp ],
        [  infixOp AndOp ],
        [  infixOp OrOp ]
      ]
    unaryOp opr1 = Prefix $ UnaryOp opr1 <$ theOp opr1
    infixOp opr1 = InfixL $ BinaryOp opr1 <$ theOp opr1
    theOp opr1 = label (show opr1) $ MP.try $ do
      opr2 <- operator
      if opr1 == opr2
        then return opr2
        else fail $ "Not the right operator: expected " <> show opr1 <> " got " <> show opr2
    booleanName "Yes" = Just $ ValueLit $ ExtBool True
    booleanName "No" = Just $ ValueLit $ ExtBool False
    booleanName "True" = Just $ ValueLit $ ExtBool True
    booleanName "False" = Just $ ValueLit $ ExtBool False
    booleanName _ = Nothing
    

-- | Parse a field name. A field name can either be a single word or several words in square
-- brackets, such as "[Foo bar]". The bracketed form can also include special characters except
-- for a close square bracket.
longIdent :: Map FieldName Field -> Parser (Expr e)
longIdent ftable = label "long name" $ lexeme $ do
  _ <- char '['
  str <- many $ anySingleBut ']'
  _ <- char ']'
  let txt = T.pack str
  case M.lookup txt ftable of
    Just f -> return $ FieldId $ fieldId f
    Nothing -> fail $ "Unknown value name: [" <> str <> "]"


-- | Literal date. A date is enclosed in single quotes. E.g. @'2 June 2005'@.  See "datePrism".
dateLit :: Parser (Expr e)
dateLit = label "date" $ lexeme $ do
  _ <- char '\''
  str <- many $ alphaNumChar <|> char '/' <|> char ' '
  _ <- char '\''
  case T.pack str ^? datePrism shortDate of  -- Format ignored because we only read the date.
    Just d -> return $ ValueLit $ ExtDate d
    Nothing -> customFailure $ "Unreadable date: '" <> T.pack str <> "'"


-- | Literal strings are enclosed in double quotes. Within the string quotes and backslashes
-- are escaped. E.g. @\"This string contains \\\"foo\\\".\"@
stringLit :: Parser (Expr e)
stringLit = label "string" $ lexeme $ fmap (ValueLit . ExtText . T.pack) $
  char '"' >> manyTill Lex.charLiteral (char '"')


-- | Numeric literals are either integers or floats.
numberLit :: Parser (Expr e)
numberLit = label "number" $
  MP.try (ValueLit . ExtReal <$> lexeme Lex.float) <|>
  (ValueLit . ExtInt <$> lexeme Lex.decimal)



-- | Evaluate an expression in the context of an entity.
evaluate :: Expr e -> ExtensionValues -> e -> Either Text ExtValue
evaluate (FieldId nm) vs _ = Right $ fromMaybe ExtNone $ M.lookup nm vs
evaluate (Application f es) vs e = evaluateFunction f es vs e
evaluate (ValueLit v) _ _ = Right v
evaluate (UnaryOp opr expr1) vs e = evaluateUnaryOp opr expr1 vs e
evaluate (BinaryOp opr expr1 expr2) vs e = evaluateBinaryOp opr expr1 expr2 vs e


-- | Apply a function to an evaluated list of arguments.
evaluateFunction :: Function e -> [Expr e] -> ExtensionValues -> e -> Either Text ExtValue
evaluateFunction func exprs vs env = do
  argVals <- V.fromList <$> mapM (\args -> evaluate args vs env) exprs
  func argVals vs env


-- Take the middle items from a list. Negative start counts from the end.
midList :: Int -> Int -> [a] -> [a]
midList start1 cnt xs = take cnt $ drop start2 xs
  where
    start2 = if start1 < 0 then length xs + start1 else start1


-- | Evaluate a unary operator and its argument.
evaluateUnaryOp :: Opr -> Expr e -> ExtensionValues -> e -> Either Text ExtValue
evaluateUnaryOp opr expr1 vs e = case (opr, evaluate expr1 vs e) of
  (_, Left msg) -> Left msg
  (MinusOp, Right (ExtInt i)) -> Right $ ExtInt (-i)
  (MinusOp, Right (ExtReal r)) -> Right $ ExtReal (-r)
  (NotOp, Right (ExtBool b)) -> Right $ ExtBool $ not b
  (_, Right v) -> Left $ "Illegal operation: " <> operatorName opr <> " " <> valueTypeName v



-- | Evaluate a binary operator with its two arguments.
evaluateBinaryOp :: Opr -> Expr e -> Expr e -> ExtensionValues -> e -> Either Text ExtValue
evaluateBinaryOp opr expr1 expr2 vs e = case (v1, v2) of
    (Left msg, Right _) -> Left msg
    (Right _, Left msg) -> Left msg
    (Left msg1, Left msg2) -> Left $ msg1 <> ", " <> msg2
    (Right v1a, Right v2a) ->
      let (v1b, v2b) = promoteBinary v1a v2a
      in case (opr,v1b,v2b) of
        (PlusOp, ExtInt i1, ExtInt i2) -> Right $ ExtInt $ i1 + i2
        (PlusOp, ExtReal r1, ExtReal r2) -> Right $ ExtReal $ r1 + r2
        (PlusOp, ExtDate d, ExtInt i) -> Right $ ExtDate $ i `addDays` d
        (PlusOp, ExtInt i, ExtDate d) -> Right $ ExtDate $ i `addDays` d
        (MinusOp, ExtInt i1, ExtInt i2) -> Right $ ExtInt $ i1 - i2
        (MinusOp, ExtReal r1, ExtReal r2) -> Right $ ExtReal $ r1 - r2
        (MinusOp, ExtDate d1, ExtDate d2) -> Right $ ExtInt $ d1 `diffDays` d2
        (MinusOp, ExtDate d, ExtInt i) -> Right $ ExtDate $ (-i) `addDays` d
        (TimesOp, ExtInt i1, ExtInt i2) -> Right $ ExtInt $ i1 * i2
        (TimesOp, ExtReal r1, ExtReal r2) -> Right $ ExtReal $ r1 * r2
        (DivOp, ExtInt i1, ExtInt i2) ->
          if i2 == 0 then Left "Division by zero" else Right $ ExtInt $ i1 `div` i2
        (DivOp, ExtReal r1, ExtReal r2) -> Right $ ExtReal $ r1 / r2
        (EqualOp, ExtInt i1, ExtInt i2) -> Right $ ExtBool $ i1 == i2
        (EqualOp, ExtReal r1, ExtReal r2) -> Right $ ExtBool $ r1 == r2
        (EqualOp, ExtText t1, ExtText t2) ->
          Right $ ExtBool $ T.toCaseFold t1 == T.toCaseFold t2
        (EqualOp, ExtDate d1, ExtDate d2) -> Right $ ExtBool $ d1 == d2
        (NotEqualOp, ExtInt i1, ExtInt i2) -> Right $ ExtBool $ i1 /= i2
        (NotEqualOp, ExtReal r1, ExtReal r2) -> Right $ ExtBool $ r1 /= r2
        (NotEqualOp, ExtText t1, ExtText t2) ->
          Right $ ExtBool $ T.toCaseFold t1 /= T.toCaseFold t2
        (NotEqualOp, ExtDate d1, ExtDate d2) -> Right $ ExtBool $ d1 /= d2
        (LessOp, ExtInt i1, ExtInt i2) -> Right $ ExtBool $ i1 < i2
        (LessOp, ExtReal r1, ExtReal r2) -> Right $ ExtBool $ r1 < r2
        (LessOp, ExtText t1, ExtText t2) -> Right $ ExtBool $ T.toCaseFold t1 < T.toCaseFold t2
        (LessOp, ExtDate d1, ExtDate d2) -> Right $ ExtBool $ d1 < d2
        (LessEqOp, ExtInt i1, ExtInt i2) -> Right $ ExtBool $ i1 <= i2
        (LessEqOp, ExtReal r1, ExtReal r2) -> Right $ ExtBool $ r1 <= r2
        (LessEqOp, ExtText t1, ExtText t2) ->
          Right $ ExtBool $ T.toCaseFold t1 <= T.toCaseFold t2
        (LessEqOp, ExtDate d1, ExtDate d2) -> Right $ ExtBool $ d1 <= d2
        (GreaterOp, ExtInt i1, ExtInt i2) -> Right $ ExtBool $ i1 > i2
        (GreaterOp, ExtReal r1, ExtReal r2) -> Right $ ExtBool $ r1 > r2
        (GreaterOp, ExtText t1, ExtText t2) ->
          Right $ ExtBool $ T.toCaseFold t1 > T.toCaseFold t2
        (GreaterOp, ExtDate d1, ExtDate d2) -> Right $ ExtBool $ d1 > d2
        (GreaterEqOp, ExtInt i1, ExtInt i2) -> Right $ ExtBool $ i1 >= i2
        (GreaterEqOp, ExtReal r1, ExtReal r2) -> Right $ ExtBool $ r1 >= r2
        (GreaterEqOp, ExtText t1, ExtText t2) ->
          Right $ ExtBool $ T.toCaseFold t1 >= T.toCaseFold t2
        (GreaterEqOp, ExtDate d1, ExtDate d2) -> Right $ ExtBool $ d1 >= d2
        (AndOp, ExtBool b1, ExtBool b2) -> Right $ ExtBool $ b1 && b2
        (OrOp, ExtBool b1, ExtBool b2) -> Right $ ExtBool $ b1 || b2
        (ConcatOp, _, _) -> Right $ ExtText $ displayValue v1a <> displayValue v2a
        (_, ExtNone, _) -> Right ExtNone
        (_, _, ExtNone) -> Right ExtNone
        _ -> Left $ "Illegal operation: " <>
            valueTypeName v1a <> " " <>
            operatorName opr <> " " <>
            valueTypeName v2a
  where
    v1 = evaluate expr1 vs e
    v2 = evaluate expr2 vs e
    promoteBinary (ExtInt i) (ExtReal r) = (ExtReal $ fromIntegral i, ExtReal r)
    promoteBinary (ExtReal r) (ExtInt i) = (ExtReal r, ExtReal $ fromIntegral i)
    promoteBinary x y = (x, y)
