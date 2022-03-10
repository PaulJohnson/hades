{-# LANGUAGE TypeFamilies #-}

{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

{- |

This module defines the type system used for extending models.

Type definitions have to be represented in at least two ways: with textual names for
field types that refers to some symbol table, and a "compiled" version with the actual
field type data. There might also need to be a \"Maybe\" version for type names not found.
A naive implementation would have these as distinct types, such as:

> data StructRaw = StructRaw [(Text, TypeName)]
>
> data StructMaybe = StructMaybe [(Text, Maybe TypeDef)]
>
> data Struct = Struct [(Text, TypeDef)]

A solution is to use \"Higher Kinded Data\" (HKD). This uses a functor applied to
each field in a data type and generic functions to transform between functors.
See http://reasonablypolymorphic.com/blog/higher-kinded-data for details.

This lets us define a collection of data types to define the HADES type system and then
instantiate them with type names, maybes, or any other functor.

The gain in this case is particularly marked because the only type that actually uses HKD
is the "Field". All the rest simply have the functor paramter tramp down through them.

In addition to those listed below, there is an additional function exported by this module.
This cannot have its type included in the code, so Haddock will not display it. The reason is
that it uses the "HKD" type family which is non-injective. The function is:

> elaborateTypeDef :: (Functor f, Functor g, Applicative m) =>
>    Elaboration m f g -> TypeDef' f -> TypeDef' g

This will convert a "TypeDef'" from one functor to another.
-}

module Model.Reflection.Types (

   -- * Aeson Utility Operators
   (..=),
   (..:),
   -- * Ranges With Optional Bounds
   Range,
   validRange,
   inRange,
   rangePrism,
   rangeDefault,
   RangeMap,
   rangeMap,
   -- * Basic Data Types
   FieldId,
   TypeName,
   BuiltInType (..),
   builtInNames,
   builtInName,
   DecoType (..),
   EnumItem (EnumItem),
   enumItemName,
   enumItemIcon,
   enumItemColour,
   EnumType,
   -- * Composite Data Types
   FieldName,
   Field (Field, fieldReadOnly, fieldId),
   fieldName,
   fieldMulti,
   fieldType,
   migrateField,
   migrateField1,
   Cardinality (..),
   cardinalityToText,
   cardinalityFromText,
   cardinalityText,
   FieldTable,
   fieldsByName,
   fieldNameIso,
   builtInFields,
   withBuiltIn,
   Multiplicity (..),
   TypeDef (..),
   _BuiltInDef,
   _DecoBoolDef,
   _DecoIntDef,
   _DecoRealDef,
   _EnumDef,
   typeDescription
) where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Orphan ()
import Data.Aeson.Types
import Data.Array hiding (inRange)
import Data.Default
import qualified Data.HashMap.Strict as H
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.UUID as U
import qualified Data.UUID.V5 as U
import Reactive.Banana.Common
import qualified Text.Read as R
import qualified Data.ByteString as B


-- | Type-restricted version of "(.=)" to silence compiler warnings about defaults.
(..=) :: (KeyValue kv) => Text -> Text -> kv
(..=) = (.=)


-- | Type restricted version of "(.:)" to silence compiler warnings about defaults.
(..:) :: Object -> Text -> Parser Text
(..:) = (.:)


-- | Inclusive range with optional boundaries.
type Range a = (Maybe a, Maybe a)


validRange :: (Ord a) => Range a -> Bool
validRange (Just v1, Just v2) = v1 <= v2
validRange _ = True


-- | True iff the value is in the range.
inRange :: (Ord a) => Range a -> a -> Bool
inRange (b1, b2) v = maybe True (v >=) b1 && maybe True (v <=) b2


-- | Prism for values within the range.
rangePrism :: (Ord a) => Range a -> Prism' a a
rangePrism rng = prism id $ \v -> if inRange rng v then Right v else Left v


-- | A default value within the range. The result is the first valid one of; the second
-- argument, the lower bound and the upper bound.
rangeDefault :: (Ord a) => Range a -> a -> a
rangeDefault rng@(lower, upper) d =
   if inRange rng d
      then d
      else fromMaybe (fromMaybe d upper) lower


-- Associates ranges of one type with values of another type. If the ranges overlap then
-- the first matching range is used.
type RangeMap a b = [(Range a, b)]


-- | Apply a "RangeMap" to a value.
rangeMap :: (Ord a) => RangeMap a b -> a -> Maybe b
rangeMap rs x = snd <$> find ((`inRange` x) . fst) rs


-- | The Higher Kinded Data type family. Used to elide the Identity functor from
-- the base case.
type family HKD f a where
   HKD Identity a = a
   HKD f        a = f a


-- | Field type identifiers.
type FieldId = U.UUID


type TypeName = Text


-- | The base types built in to HADES.
data BuiltInType =
      ModelBool
      | ModelInt   -- ^ Arbitrary precision integers.
      | ModelReal  -- ^ Double precision float.
      | ModelText  -- ^ Short one-line text strings.
      | ModelURL   -- ^ Hyperlinks.
      | ModelNote  -- ^ Long text strings, possibly with multiple paragraphs.
      | ModelDate
      deriving (Eq, Ord, Enum, Bounded, Ix)

instance Show BuiltInType where
   show = T.unpack . builtInName

instance Default BuiltInType where def = ModelText


builtInNames :: [(BuiltInType, Text)]
builtInNames = [
      (ModelBool, "Boolean"),
      (ModelInt, "Integer"),
      (ModelReal, "Real"),
      (ModelText, "Text"),
      (ModelURL, "URL"),
      (ModelNote, "Note"),
      (ModelDate, "Date")
   ]


-- | Name of the built-in type.
builtInName :: BuiltInType -> Text
builtInName t = tbl ! t
   where
      tbl = array (minBound, maxBound) builtInNames


-- | Some of the basic types can be \"decorated\" with icons and colours and boundaries.
-- Each decorated version is a new type.
data DecoType a where
   -- | Icons and colours for @False@ and @True@ respectively.
   DecoBool :: Maybe (IconName, IconName) -> Maybe (Colour, Colour) -> DecoType Bool
   -- | Icons and colours for ranges within type @a@.
   DecoRange :: Range a -> RangeMap a (Maybe IconName, Maybe Colour) -> DecoType a

instance (Eq a) => Eq (DecoType a) where
   (DecoBool i1 c1) == (DecoBool i2 c2)  = (i1 == i2) && (c1 == c2)
   (DecoRange r1 m1) == (DecoRange r2 m2)  = (r1 == r2) && (m1 == m2)
   _ == _  = False

instance (Show a) => Show (DecoType a) where
   show (DecoBool icons colours) =
         "DecoBool (False = (" <> falseIcon <> ", " <> falseColour <>
            "), True = (" <> trueIcon <> ", " <> trueColour <> "))"
      where
         falseIcon = fromMaybe "-" $ icons ^? _Just . _1 . to T.unpack
         trueIcon = fromMaybe "-" $ icons ^? _Just . _2 . to T.unpack
         falseColour = fromMaybe "-" $ colours ^? _Just . _1 . re colourPrism . to T.unpack
         trueColour = fromMaybe "-" $ colours ^? _Just . _2 . re colourPrism . to T.unpack
   show (DecoRange r m) =
         "DecoRange " <> showRange r <> ": [" <> intercalate ", " (map showSub m) <> "]"
      where
         showRange (l, u) = "(" <> showM l <> " to " <> showM u <> ")"
         showSub (r1, (mIcon, mColour)) =
            showRange r1 <> " -> " <> "(" <> maybe "-" T.unpack mIcon  <> ", " <>
                  fromMaybe "-" (mColour ^? _Just . re colourPrism . to T.unpack) <> ")"
         showM = maybe "-" show

instance {-# OVERLAPPING #-} Default (DecoType Bool) where def = DecoBool Nothing Nothing

instance Default (DecoType a) where def = DecoRange (Nothing, Nothing) mempty


instance (ToJSON a) => ToJSON (DecoType a) where
   toJSON (DecoBool icons colours) = object [
         "icons" .= icons,
         "colours" .= over (_Just . both) getColour colours ]
   toJSON (DecoRange rng rngMap) = object [
         "range" .= rng,
         "map" .= map (over (_2 . _2 . _Just) getColour) rngMap ]

-- A well-typed FromJSON instance for DecoType is not possible.

-- | Read a Decorated Range from JSON. A @FromJSON@ instance would lead to overlapping instances.
decoRangeFromJSON :: (FromJSON a) => Object -> Parser (DecoType a)
decoRangeFromJSON v = DecoRange <$>
      v .: "range" <*>
      (map (over (_2 . _2 . _Just) Colour) <$> (v .: "map"))


-- | An enumeration consists of a list of items, each of which has a name and display properties.
data EnumItem = EnumItem {
      _enumItemName :: Text,
      _enumItemIcon :: Maybe IconName,
      _enumItemColour :: Maybe Colour
   } deriving (Eq, Show)

enumItemName :: Lens' EnumItem Text
enumItemName = lens _enumItemName $ \s t -> s {_enumItemName = t}

enumItemIcon :: Lens' EnumItem (Maybe IconName)
enumItemIcon = lens _enumItemIcon $ \s i -> s {_enumItemIcon = i}

enumItemColour :: Lens' EnumItem (Maybe Colour)
enumItemColour = lens _enumItemColour $ \s c -> s {_enumItemColour = c}


instance ToJSON EnumItem where
   toJSON i = object [
            "name" .= _enumItemName i,
            "icon" .= _enumItemIcon i,
            "colour" .= (getColour <$> _enumItemColour i)
         ]

instance FromJSON EnumItem where
   parseJSON = withObject "EnumItem" $ \v -> EnumItem <$>
      (T.strip <$> (v .: "name")) <*>
      v .:? "icon" <*>
      (fmap Colour <$> (v .:? "colour"))


type EnumType = [EnumItem]


-- | Data fields have a multiplicity.
data Multiplicity =
   One    -- ^ Exactly one of these is included.
   | Optional   -- ^ The field is optional: a value may or may not be present.
   deriving (Read, Show, Eq, Ord, Enum, Bounded)

instance ToJSON Multiplicity where
   toJSON m = String $ T.pack $ show m

instance FromJSON Multiplicity where
   parseJSON = withText "Multiplicity" $ \str ->
      case R.readMaybe $ T.unpack str of
         Just m -> return m
         Nothing -> fail "Multiplicity not recognised."


type FieldName = Text


-- | A field is a field name, multiplicity and type definition.
data Field = Field {
      fieldReadOnly :: Bool,  -- ^ Only True for certain built-in fields.
      fieldId :: FieldId,
      _fieldName :: FieldName,
      _fieldMulti :: Multiplicity,
      _fieldType :: TypeDef
   } deriving (Eq)

instance Show Field where
   show (Field ro fid nm mult typ) =
      "Field " <> if ro then "(RO) " else "" <> show fid <> " " <> show nm <> " " <>
         show mult <> " " <> show (typeDescription typ)

instance ToJSON Field where
   toJSON (Field _ fid nm mult typ) = object [
         "id" .= fid,
         "name" .= nm,
         "multi" .= mult,
         "type" .= typ
      ]

instance FromJSON Field where
   parseJSON = withObject "Field" $ \obj -> do
            nm <- T.strip <$> (obj .: "name")
            multi <- obj .: "multi"
            typ <- obj .: "type"
            fid <- obj .:? "id" .!= migrateField nm
            return $ Field False fid nm multi typ


fieldName :: Lens' Field FieldName
fieldName = lens _fieldName $ \s n -> s {_fieldName = n}

fieldMulti :: Lens' Field Multiplicity
fieldMulti = lens _fieldMulti $ \s m -> s {_fieldMulti = m}

fieldType :: Lens' Field TypeDef
fieldType = lens _fieldType $ \s t -> s {_fieldType = t}


-- | Old-style fields from before Version 1.4 did not have an ID, and were identified by name.
-- This caused issues when the name changed. Migration to fields that are identified by a UUID
-- required that these old fields be assigned a UUID. This was done using the UUID V5 scheme
-- which uses the SHA-1 hash of the fieldname.
migrateField :: FieldName -> FieldId
migrateField = U.generateNamed U.nil . B.unpack . T.encodeUtf8


-- | Like "migrateField" but also checks to see if the text is already a UUID. If it is then
-- it parses that directly instead of treating it as a field name to be migrated.
migrateField1 :: Text -> FieldId
migrateField1 str = case U.fromText str of
   Just fid -> fid
   Nothing -> migrateField str

type FieldTable = Map FieldId Field


-- | Fields indexed by name instead of ID.
fieldsByName :: FieldTable -> Map FieldName Field
fieldsByName = M.fromList . map (\f -> (f ^. fieldName, f)) . M.elems


-- | Useful in dialogs that allow the selection of multiple fields. If any field IDs or names
-- are not found in the @FieldTable@ then they are silently dropped (so not a true ISO).
fieldNameIso :: FieldTable -> Iso' [FieldId] [FieldName]
fieldNameIso ftable = iso
      (mapMaybe $ \fid -> view fieldName <$> M.lookup fid ftable)
      (mapMaybe $ \nm -> fieldId <$> M.lookup nm revTable)
   where
      revTable = fieldsByName ftable


-- | Cardinality annotations. Originally introduced for GSN Edges, but might also be used elsewhere.
data Cardinality =
   CardinalityOne     -- ^ Exactly one. The default.
   | CardinalityOpt   -- ^ Optional; 0 or 1.
   | CardinalityMany  -- ^ 0 to @n@.
   | CardinalityMany1  -- ^ 1 to @n@.
   deriving (Eq, Ord, Enum, Bounded)

instance Show Cardinality where
   show = T.unpack . cardinalityToText

instance ToJSON Cardinality where
   toJSON = Data.Aeson.String . cardinalityToText

instance FromJSON Cardinality where
   parseJSON = withText "Gsn Edge Cardinality" $ \txt ->
      case cardinalityFromText txt of
         Just c -> return c
         Nothing -> fail $ "Not a GSN Edge Cardinality Type: " <> show txt


cardinalityToText :: Cardinality -> Text
cardinalityToText CardinalityOne = "One"
cardinalityToText CardinalityOpt = "Optional"
cardinalityToText CardinalityMany = "0-n"
cardinalityToText CardinalityMany1 = "1-n"


cardinalityFromText :: Text -> Maybe Cardinality
cardinalityFromText "One" = Just CardinalityOne
cardinalityFromText "Optional" = Just CardinalityOpt
cardinalityFromText "0-n" = Just CardinalityMany
cardinalityFromText "1-n" = Just CardinalityMany1
cardinalityFromText _ = Nothing


-- | A Prism for converting between "Text" and "GsnEdgeCard" using the functions above.
cardinalityText :: Prism' Text Cardinality
cardinalityText = prism' cardinalityToText cardinalityFromText


-- | Field names that are built in to the model.
--
-- This is a non-modular and non-extensible piece of design. When a new built-in field is added
-- to the metamodel it has to be included here by editing this list.
--
-- ToDo: make built-in fields properly extensible.
builtInFields :: FieldTable
builtInFields = M.fromList $ map mkPair [
      ("Name", One, BuiltInDef ModelText),
      ("Description", One, BuiltInDef ModelNote),
      ("Hidden", One, BuiltInDef ModelBool),
      ("Count", One, BuiltInDef ModelInt),
      ("Inhibit", One, BuiltInDef ModelBool),
      ("Developed", One, BuiltInDef ModelBool),
      ("Instantiated", One, BuiltInDef ModelBool),
      ("Cardinality", One, cardDef)
   ]
   where
      cardDef = EnumDef $ map item [minBound .. maxBound]
      item i = EnumItem (cardinalityToText i) Nothing Nothing
      mkPair (nm, multi, typ) = let fid = migrateField nm in (fid, Field False fid nm multi typ)


-- | Add the built-in fields to the field table, overriding any of the same name.
--
-- Note: if new built-in fields are defined then they must be manually added
-- to this module as well.
withBuiltIn :: FieldTable -> FieldTable
withBuiltIn = M.union builtInFields


-- | Like "withBuiltIn" but with the stamp fields too.



data TypeDef =
   BuiltInDef BuiltInType
   | DecoBoolDef (DecoType Bool)
   | DecoIntDef (DecoType Integer)
   | DecoRealDef (DecoType Double)
   | EnumDef EnumType
   deriving (Eq, Show)

instance ToJSON TypeDef where
   toJSON (BuiltInDef ty) = object ["kind" ..= "built-in", "type" .= T.pack (show ty)]
   toJSON (DecoBoolDef decoBool) =
      let Object obj = toJSON decoBool
      in Object $ H.insert "kind" "deco-bool" obj
   toJSON (DecoIntDef decoRange) =
      let Object obj = toJSON decoRange
      in Object $ H.insert "kind" "deco-int" obj
   toJSON (DecoRealDef decoRange) =
      let Object obj = toJSON decoRange
      in Object $ H.insert "kind" "deco-real" obj
   toJSON (EnumDef enumType) = object [
      "kind" .= ("enum" :: Text),
      "values" .= enumType ]

instance FromJSON TypeDef where
   parseJSON = withObject "TypeDef" $ \v ->
         (v ..: "kind") >>= \case
            "built-in" -> do
               nm <- v .: "type"
               case M.lookup nm builtIns of
                  Nothing -> fail $ "Unknown built-in type: " ++ show nm
                  Just ty -> return $ BuiltInDef ty
            "deco-bool" -> DecoBoolDef <$> (DecoBool <$>
               v .:? "icons" <*>
               (over (_Just . both) Colour <$> (v .:? "colours")))
            "deco-int" -> DecoIntDef <$> decoRangeFromJSON v
            "deco-real" -> DecoRealDef <$> decoRangeFromJSON v
            "enum" -> EnumDef <$> v .: "values"
            e -> fail $ "Unrecognised kind of type: " ++ show e
      where
         builtIns = M.fromList $ map (uncurry $ flip (,)) builtInNames


_BuiltInDef :: Prism' TypeDef BuiltInType
_BuiltInDef = prism BuiltInDef $ \case {BuiltInDef t -> Right t; v -> Left v}

_DecoBoolDef :: Prism' TypeDef (DecoType Bool)
_DecoBoolDef = prism DecoBoolDef $ \case {DecoBoolDef t -> Right t; v -> Left v}

_DecoIntDef :: Prism' TypeDef (DecoType Integer)
_DecoIntDef = prism DecoIntDef $ \case {DecoIntDef t -> Right t; v -> Left v}

_DecoRealDef :: Prism' TypeDef (DecoType Double)
_DecoRealDef = prism DecoRealDef $ \case {DecoRealDef t -> Right t; v -> Left v}

_EnumDef :: Prism' TypeDef EnumType
_EnumDef = prism EnumDef $ \case {EnumDef t -> Right t; v -> Left v}



typeDescription :: TypeDef -> Text
typeDescription (BuiltInDef t) = T.pack $ show t
typeDescription DecoBoolDef {} = "Decorated Bool"
typeDescription DecoIntDef {} = "Decorated Int"
typeDescription DecoRealDef {} = "Decorated Real"
typeDescription EnumDef {} = "Enumeration"
