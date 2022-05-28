{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

{- |
-}
module Model.Matrices.Base (
  -- * Types
  MatrixId,
  HasMatrices (..),
  FieldColumnData (FieldColumnData),
  fieldColumnText,
  fieldColumnLink,
  MatrixColumnData (..),
  _FieldColumn,
  _ExprColumn,
  _QueryColumn,
  _LookupColumn,
  MatrixColumn (MatrixColumn),
  matrixColumnName,
  matrixColumnData,
  SortDir (..),
  SortOrder (..),
  showSortOrder,
  ColumnGroup (ColumnGroup),
  groupQuery,
  groupColumns,
  groupSort,
  Matrix (Matrix),
  matrixName,
  matrixDescription,
  matrixDefaultInput,
  matrixGroups,
  matrixInputSet,
  -- * Functions
  compileMatrix,
  runMatrix,
  -- * Tree Utilties
  compareValues,
  sortForest,
  sortTree,
  mapTreeM,
  mapForestM,
  walkForestM
) where

import Control.Applicative
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Types (typeMismatch, Parser)
import Data.Default
import Data.Either
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree
import qualified Data.Vector as V
import Model.Abstract.PackageTree
import Model.Reflection.Reflective
import Model.Reflection.Types
import Model.Reflection.Values
import Model.Query.Base
import Model.Query.Diagram
import Control.Arrow


type MatrixId = ModelId


-- | Prism for the matrix in a model entity.
class (Queryable a) => HasMatrices a where
  _Matrix :: Prism' a Matrix


-- | Directly maps one or two fields in the entity to a cell in the matrix. The optional second
-- field is a URL which is displayed as the target of a hyperlink.
data FieldColumnData = FieldColumnData {
    _fieldColumnText :: FieldId,
    _fieldColumnLink :: Maybe FieldId
  } deriving (Eq, Show)

instance ToJSON FieldColumnData where
  toJSON (FieldColumnData t l) = object ["text" .= t, "link" .= l]

instance FromJSON FieldColumnData where
  parseJSON (String f) = return $ FieldColumnData (migrateField1 f) Nothing
  parseJSON (Object v) = FieldColumnData <$>
      (migrateField1 <$> v .: "text") <*>
      (fmap migrateField1 <$> v .: "link")
  parseJSON x = typeMismatch "FieldColumnData" x

instance Default FieldColumnData where
  def = FieldColumnData nameField Nothing

fieldColumnText :: Lens' FieldColumnData FieldId
fieldColumnText = lens _fieldColumnText $ \s f -> s {_fieldColumnText = f}

fieldColumnLink :: Lens' FieldColumnData (Maybe FieldId)
fieldColumnLink = lens _fieldColumnLink $ \s f -> s {_fieldColumnLink = f}


data MatrixColumnData =
  -- | A direct mapping from one or two fields in the entity to a cell in the matrix.
  -- The text can be edited within the matrix but the link cannot.
  FieldColumn FieldColumnData
  -- | An expression mapping multiple entity fields to a value with a specified type. The @Bool@
  -- is @True@ for multi-line results, @False@ for a single line.
  | ExprColumn Bool Text
  -- | A query which returns a set of entities. These will be displayed as a list within the cell.
  | QueryColumn QueryId
  -- | A lookup table which searches for its inputs in the other columns and displays the output.
  | LookupColumn ModelId
  deriving (Eq, Show)

_FieldColumn :: Prism' MatrixColumnData FieldColumnData
_FieldColumn = prism FieldColumn $ \case {FieldColumn f -> Right f; v -> Left v}

_ExprColumn :: Prism' MatrixColumnData (Bool, Text)
_ExprColumn = prism (uncurry ExprColumn) $ \case {ExprColumn b e -> Right (b, e); v -> Left v}

_QueryColumn :: Prism' MatrixColumnData QueryId
_QueryColumn = prism QueryColumn $ \case {QueryColumn q -> Right q; v -> Left v}

_LookupColumn :: Prism' MatrixColumnData ModelId
_LookupColumn = prism LookupColumn $ \case {LookupColumn m -> Right m; v -> Left v}


data MatrixColumn = MatrixColumn {
    _columnName :: Text,
    _columnData :: MatrixColumnData
  } deriving (Eq, Show)

instance ToJSON MatrixColumn where
  toJSON (MatrixColumn n (FieldColumn f)) = object ["name" .= n, "field" .= f]
  toJSON (MatrixColumn n (ExprColumn b e)) = object ["name" .= n, "note" .= b, "expression" .= e]
  toJSON (MatrixColumn n (QueryColumn q)) = object ["name" .= n, "query" .= q]
  toJSON (MatrixColumn n (LookupColumn t)) = object ["name" .= n, "lookup" .= t]

instance FromJSON MatrixColumn where
  parseJSON = withObject "Matrix column" $ \v ->
    MatrixColumn <$> v .: "name" <*> (
        FieldColumn <$> v .: "field" <|>
        ExprColumn <$> v .:? "note" .!= True <*> v .: "expression" <|>
        QueryColumn <$> v .: "query" <|>
        LookupColumn <$> v .: "lookup"
      )


matrixColumnName :: Lens' MatrixColumn Text
matrixColumnName = lens _columnName $ \s n -> s {_columnName = n}

matrixColumnData :: Lens' MatrixColumn MatrixColumnData
matrixColumnData = lens _columnData $ \s d -> s {_columnData = d}


-- | Matrix column groups are sorted by a field value, either in ascending or descending order.
data SortDir = Ascending | Descending deriving (Eq, Ord, Enum, Bounded, Show, Read)

instance ToJSON SortDir where
  toJSON = String . T.pack . show

instance FromJSON SortDir where
  parseJSON = withText "sort order" $ \case
    "Ascending" -> return Ascending
    "Descending" -> return Descending
    s -> fail $ "Unknown sort order: " <> show s


-- | A sort order is the list of pairs of field names and sort directions. If two entities are
-- equal according to the first entry then the next entry is applied, and so on.
newtype SortOrder = SortOrder {getSortOrder :: [(FieldId, SortDir)]} deriving Eq

instance ToJSON SortOrder where
  toJSON (SortOrder xs) = toJSON xs

instance FromJSON SortOrder where
  parseJSON = withArray "Sort order" $ fmap (SortOrder . V.toList) . mapM parsePair
    where
      parsePair v = do
        (f, d) <- parseJSON v
        return (migrateField1 f, d)


-- A sort order represented as a comma separated list with up and down arrows denoting sort
-- direction.
showSortOrder :: FieldTable -> SortOrder -> Text
showSortOrder tbl (SortOrder fs) = T.intercalate ", " $ map showItem fs
  where
    showName f = case M.lookup f tbl of
      Just field -> field ^. fieldName
      Nothing -> "-?-"
    showItem (f, o) = showName f <> case o of
      Ascending -> "▲"   -- U+25B2 black up-pointing triangle.
      Descending -> "▼"  -- U+25BC black down-pointing triangle.


-- | Each column group contains a query for the entities
-- to be displayed and zero or more columns for the data. Each entity will thus become a row
-- in this column group, and will also be passed as input to the query in the next column group.
-- Hence the execution of these queries will produce a tree of entities, with lists of columns
-- being applied respectively at each level of the tree.
data ColumnGroup = ColumnGroup {
    _groupQuery :: QueryId,
    _groupColumns :: [MatrixColumn],
    _groupSort :: SortOrder
  } deriving (Eq)

instance ToJSON ColumnGroup where
  toJSON (ColumnGroup q cs sortFields) =
    object ["query" .= q, "columns" .= cs, "sort" .= sortFields]

instance FromJSON ColumnGroup where
  parseJSON = withObject "Column group" $ \v -> do
      sorting1 <- v .:? "sort"
      ColumnGroup <$>
        v .: "query" <*>
        v .: "columns" <*>
        case sorting1 of
            Nothing -> return $ SortOrder [(nameField, Ascending)]
            Just j -> readSort j
    where
      readSort :: Value -> Parser SortOrder
      readSort v = SortOrder . map (first migrateField1) <$>
          (parseJSON v <|> (map (, Ascending) <$> parseJSON v))


groupQuery :: Lens' ColumnGroup QueryId
groupQuery = lens _groupQuery $ \s q -> s {_groupQuery = q}

groupColumns :: Lens' ColumnGroup [MatrixColumn]
groupColumns = lens _groupColumns $ \s c -> s {_groupColumns = c}

groupSort :: Lens' ColumnGroup SortOrder
groupSort = lens _groupSort $ \s fs -> s {_groupSort = fs}


-- | A matrix consists of one or more column groups.
-- Each of these column groups has a query and a list of columns.
data Matrix = Matrix {
    _matrixName :: Name,
    _matrixDescription :: Text,
    _matrixDefaultInput :: Set ModelId,
    _matrixGroups :: [ColumnGroup]
  } deriving (Eq)

instance ToJSON Matrix where
  toJSON (Matrix nm desc inputs cgs) = object [
      "type" .= ("matrix" :: Text),
      "name" .= nm,
      "description" .= desc,
      "input" .= inputs,
      "groups" .= cgs
    ]

instance FromJSON Matrix where
  parseJSON = withObject "Matrix" $ \v -> do
    checkType v "matrix"
    Matrix <$>
        v .: "name" <*>
        v .:? "description" .!= "" <*>
        v .:? "input" .!= S.empty <*>
        v .: "groups"

instance Reflective Matrix where
  reflectiveName _ = Variant "Matrix"
  reflectiveDefaults = [Matrix (Name "") "" S.empty []]
  reflectiveBuiltIn _ = [nameField, descriptionField]
  reflectiveGet m = M.fromList [
      (nameField, ExtText $ m ^. matrixName . nameText),
      (descriptionField, ExtText $ m ^. matrixDescription)
    ]
  reflectiveSet m = do
    nm <- Name . T.strip <$> extract (m ^. matrixName . nameText) _ExtText nameField
    desc <- extract (m ^. matrixDescription) _ExtText descriptionField
    return $ matrixName .~ nm $ matrixDescription .~ desc $ m
  reflectiveBuiltInRefs = mempty
  reflectiveArrows = mempty


matrixName :: Lens' Matrix Name
matrixName = lens _matrixName $ \s n -> s {_matrixName = n}

matrixDescription :: Lens' Matrix Text
matrixDescription = lens _matrixDescription $ \s t -> s {_matrixDescription = t}

matrixDefaultInput :: Lens' Matrix (Set ModelId)
matrixDefaultInput = lens _matrixDefaultInput $ \s x -> s {_matrixDefaultInput = x}

matrixGroups :: Lens' Matrix [ColumnGroup]
matrixGroups = lens _matrixGroups $ \s cgs -> s {_matrixGroups = cgs}


-- | The set of variant types which are acceptable inputs to the first query in the matrix.
matrixInputSet :: (EntityClass v, Queryable v) => Matrix -> Model v -> Set (Variant v)
matrixInputSet matrix model =
  fromMaybe mempty $ do -- Maybe monad
    qId <- matrix ^? matrixGroups . ix 0 . groupQuery
    modelContents model ^? ix qId . entityContents . _QueryDiagram . queryInputs


-- | Convert a matrix into the list of queries that will implement it. If any of the queries fail
-- to compile then relevant error messages are produced instead.
compileMatrix :: (EntityClass v, Queryable v) =>
  Matrix -> Model v -> Either [Text] [Query v]
compileMatrix matrix model =
    if null errss then Right queries else Left $ concat $ zipWith prependNumber [1..] errss
  where
    (errss, queries) = partitionEithers $ map (compile . _groupQuery) $ matrix ^. matrixGroups
    compile qId = do  -- Either monad
      qd <- case modelContents model ^? ix qId . entityContents . _QueryDiagram of
            Just qd -> Right qd
            Nothing -> Left
              [matrix ^. matrixName . nameText <> ": Query does not exist (deleted?)."]
      compileQueryDiagram model $ qd ^. queryDiagram
    prependNumber (n :: Int) = map ((T.pack (show n) <> ": ") <>)


-- | Generate the forest of entity IDs corresponding to the nested queries, along with any warning
-- messages from running the queries.
runMatrix :: [Query v] -> Model v -> Set ModelId -> (Forest ModelId, [Text])
runMatrix [] _ _ = ([], [])
runMatrix (query : queries) model inputs =
    (map (fmap fst) result, topMsgs ++ concatMap snd (concatMap flatten result))
  where
    result :: Forest (ModelId, [Text])
    result = getNodes queries [] $ S.toList topSet
    (topSet, topMsgs) = runQuery query [] model inputs
    -- getNodes :: [Query v] -> [ModelId] -> [ModelId] -> [Tree (ModelId, [Text])]
    getNodes qs outers  = map $ \x -> getNode qs (x:outers) x
    -- getNode :: [Query v] -> [ModelId] -> ModelId -> Tree (ModelId, [Text])
    getNode [] _ x = Node (x, []) []
    getNode (q:qs) outers x = Node (x, msgs) $ getNodes qs outers $ S.toList cs
      where
        (cs, msgs) = runQuery q outers model $ S.singleton x


-- | Compare two entities by their field values.
compareValues :: SortOrder -> ExtensionValues -> ExtensionValues -> Ordering
compareValues (SortOrder []) _ _ = EQ
compareValues (SortOrder ((f, Ascending):fs)) e1 e2 =
  case compare (e1 ^? ix f) (e2 ^? ix f) of
    EQ -> compareValues (SortOrder fs) e1 e2
    r -> r
compareValues (SortOrder ((f, Descending):fs)) e1 e2 =
  case compare (e2 ^? ix f) (e1 ^? ix f) of
    EQ -> compareValues (SortOrder fs) e1 e2
    r -> r


-- | Sort the entities in the forest according to the fieldnames. Each "SortOrder" is
-- used to sort the corresponding layer of the forest.
sortForest :: (EntityClass v) => [SortOrder] -> Forest (Entity v) -> Forest (Entity v)
sortForest [] trees = trees
sortForest (f1:fs) trees = map (sortTree fs) $ sortBy nodeCompare trees
  where
    nodeCompare (Node v1 _) (Node v2 _) =
      compareValues f1 (entityPropertiesWithStamps v1) (entityPropertiesWithStamps v2)

-- | Sort the children of the argument according to the fieldnames.
sortTree :: (EntityClass v) => [SortOrder] -> Tree (Entity v) -> Tree (Entity v)
sortTree fs (Node v cs) = Node v $ sortForest fs cs


-- | Apply a monadic action to each value in the forest.
mapForestM :: (Monad m) => (a -> m b) -> Forest a -> m (Forest b)
mapForestM f = mapM (mapTreeM f)

-- | Apply a monadic action to each value in the tree.
mapTreeM :: (Monad m) => (a -> m b) -> Tree a -> m (Tree b)
mapTreeM f (Node v trees) = Node <$> f v <*> mapForestM f trees


-- | Apply a different monadic action at each level of the forest. The first function in the
-- list is applied to the first level, the second at the second level, and so on. If the list
-- terminates before the forest then the forest is truncated at that level. The parameter passed
-- is the current value followed by its ancestors in the tree.
walkForestM :: (Monad m) => [[a] -> m b] -> Forest a -> m (Forest b)
walkForestM = walkForestM1 []
  where
    walkTreeM ancestors f fs (Node v cs) =
      let allValues = v : ancestors
      in Node <$> f allValues <*> walkForestM1 allValues fs cs
    walkForestM1 _ [] = const $ return []
    walkForestM1 ancestors (f:fs) = mapM (walkTreeM ancestors f fs)
