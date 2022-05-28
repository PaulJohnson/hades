
{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

{- |

This module defines extension values based on reflective types.
-}

module Model.Reflection.Values (
  ExtValue (..),
  _ExtNone,
  _ExtBool,
  _ExtInt,
  _ExtReal,
  _ExtText,
  _ExtDate,
  valueMaybe,
  displayValue,
  Variant (Variant),
  variantName,
  variantCoerce,
  Extensions,
  ExtensionValues,
  extValue,
  valueTypeName,
  validateMulti,
  validateExtension,
  validateField,
  extTypeError,
  valueSearch
) where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Orphan ()
import Data.Aeson.Types
import Data.Default
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Format
import Model.Reflection.Types
import Network.URI
import Text.NaturalOrder


-- | Extension value stored according to the schema in a TypeDef.
--
-- The "ExtUnion" value can store data from all its variants rather than just the one
-- it currently holds. This enables users to switch between variants without losing data.
-- The name of the currently selected variant is held in the text field at the front.
data ExtValue =
  ExtNone  -- ^ The default value.
  | ExtBool Bool
  | ExtInt Integer
  | ExtReal Double
  | ExtText Text
  | ExtDate Day
  deriving (Eq, Show)

instance Ord ExtValue where
  compare ExtNone ExtNone          = EQ
  compare (ExtBool x) (ExtBool y)  = compare x y
  compare (ExtInt x) (ExtInt y)    = compare x y
  compare (ExtInt x) (ExtReal y)   = compare (fromIntegral x) y
  compare (ExtReal x) (ExtInt y)   = compare x (fromIntegral y)
  compare (ExtReal x) (ExtReal y)  = compare x y
  compare (ExtText x) (ExtText y)  = naturalOrder x y
  compare (ExtDate x) (ExtDate y)  = compare x y
  compare x y                      = compare (displayValue x) (displayValue y)

instance Default ExtValue where
  def = ExtNone

instance ToJSON ExtValue where
  toJSON ExtNone = object ["type" ..= "none"]
  toJSON (ExtBool b) = object ["type" ..= "bool", "value" .= b]
  toJSON (ExtInt i) = object ["type" ..= "int", "value" .= i]
  toJSON (ExtReal d) = object ["type" ..= "real", "value" .= d]
  toJSON (ExtText t) = object ["type" ..= "text", "value" .= t]
  toJSON (ExtDate d) = object ["type" ..= "date", "value" .= d]

instance FromJSON ExtValue where
  parseJSON = withObject "Extension value" $ \obj ->
    obj .: "type" >>= \case
      "none" -> return ExtNone
      "bool" -> ExtBool <$> obj .: "value"
      "int" -> ExtInt <$> obj .: "value"
      "real" -> ExtReal <$> obj .: "value"
      "text" -> ExtText <$> obj .: "value"
      "date" -> ExtDate <$> obj .: "value"
      str -> fail $ "Unrecognised Extension Value type: " <> str

_ExtNone :: Prism' ExtValue ()
_ExtNone = prism (const ExtNone) $ \case {ExtNone -> Right (); v -> Left v}

_ExtBool :: Prism' ExtValue Bool
_ExtBool = prism ExtBool $ \case {ExtBool b -> Right b; v -> Left v}

_ExtInt :: Prism' ExtValue Integer
_ExtInt = prism ExtInt $ \case {ExtInt i -> Right i; v -> Left v}

_ExtReal :: Prism' ExtValue Double
_ExtReal = prism ExtReal $ \case {ExtReal d -> Right d; v -> Left v}

_ExtText :: Prism' ExtValue Text
_ExtText = prism ExtText $ \case {ExtText t -> Right t; v -> Left v}

_ExtDate :: Prism' ExtValue Day
_ExtDate = prism ExtDate $ \case {ExtDate d -> Right d; v -> Left v}


-- | Convert a prism into a lens. "Nothing" maps to "ExtNone"
valueMaybe :: Prism' ExtValue a -> Lens' ExtValue (Maybe a)
valueMaybe prsm = lens (preview prsm) setter
  where
    setter s Nothing = if isJust $ s ^? prsm then ExtNone else s
    setter _ (Just v) = v ^. re prsm


-- | Name of the type of value.
valueTypeName :: ExtValue -> Text
valueTypeName ExtNone = "Nothing"
valueTypeName ExtBool {} = "Boolean"
valueTypeName ExtInt {} = "Integer"
valueTypeName ExtReal {} = "Real"
valueTypeName ExtText {} = "Text"
valueTypeName ExtDate {} = "Date"


-- | Textual representation of a value.
displayValue :: ExtValue -> Text
displayValue ExtNone = ""
displayValue (ExtBool False) = "No"
displayValue (ExtBool True) = "Yes"
displayValue (ExtInt n) = T.pack $ show n
displayValue (ExtReal r) = T.pack $ show r
displayValue (ExtText txt) = txt
displayValue (ExtDate d) = T.pack $ formatTime defaultTimeLocale "%d %b %y" d


-- | If the multiplicity matches the length of the list then @Nothing@, otherwise an error message.
validateMulti :: TypeName -> Multiplicity -> Maybe a -> [Text]
validateMulti nm One Nothing = [nm <> ": Required field is missing."]
validateMulti _ One (Just _) = []
validateMulti _ Optional _ = []


-- | Compare the type definition against the actual data and return a list of mismatches.
validateExtension :: TypeDef -> ExtValue -> [Text]
validateExtension (BuiltInDef ModelBool) ExtBool {} = []
validateExtension (BuiltInDef ModelInt) ExtInt {} = []
validateExtension (BuiltInDef ModelReal) ExtReal {} = []
validateExtension (BuiltInDef ModelText) ExtText {} = []
validateExtension (BuiltInDef ModelURL) (ExtText url) =
  ["Invalid URL: " <> url | isURIReference $ T.unpack url]
validateExtension (BuiltInDef ModelNote) ExtText {} = []
validateExtension (BuiltInDef ModelDate) ExtDate {} = []
validateExtension DecoBoolDef {} ExtBool {} = []
validateExtension DecoIntDef {} ExtInt {} = []
validateExtension DecoRealDef {} ExtReal {} = []
validateExtension EnumDef {} ExtText {} = []
validateExtension typ val = [extTypeError typ val]


-- | Validate the field multiplicity and type against the value held in the @Map@.
validateField :: Field -> Map FieldId ExtValue -> [Text]
validateField (Field _ fid nm multi typ) values =
  validateMulti nm multi mVal ++ case mVal of
    Nothing -> []
    Just v -> validateExtension typ v
  where
    mVal = M.lookup fid values



-- | Report a discrepancy between the expected and actual types.
extTypeError :: TypeDef -> ExtValue -> Text
extTypeError t v = "Type error: expected " <> typeDescription t <> ", got " <> valueTypeName v


-- | Reflective types come in named variants. The type parameter is a phantom.
newtype Variant a = Variant {_variantName :: Text} deriving (Eq, Ord)

instance Show (Variant a) where
  show (Variant txt) = T.unpack txt

instance ToJSON (Variant a) where
  toJSON (Variant txt) = toJSON txt

instance ToJSONKey (Variant a) where
  toJSONKey = toJSONKeyText _variantName

instance FromJSON (Variant a) where
  parseJSON v = Variant <$> parseJSON v

instance FromJSONKey (Variant a) where
  fromJSONKey = FromJSONKeyText Variant


variantName :: Iso' (Variant a) Text
variantName = iso _variantName Variant

variantCoerce :: Variant a -> Variant b
variantCoerce = Variant . _variantName


-- | A set of extension fields is defined by a mapping from a variant name to a list of fields.
type Extensions a = Variant a -> [FieldId]


-- | Instances of Reflective have to store a map of extension values along with their built-in
-- data.
type ExtensionValues = Map FieldId ExtValue


-- | Store extension values in a map, using "ExtNone" as the non-existent value.
-- The most common use is for storing @ExtValue@s in an @ExtensionValues@ table,
-- as the type specialises to
--
-- > extValue :: FieldId -> Lens' ExtensionValues ExtValue
extValue :: (Ord k) => k -> Lens' (Map k ExtValue) ExtValue
extValue fName = lens (M.findWithDefault ExtNone fName) setter
  where
    setter tbl ExtNone = M.delete fName tbl
    setter tbl v = M.insert fName v tbl


-- | Find any values that match the given search string. The result is a list of fields where
-- the search string was found and the start index of the match. All fields are searched using
-- their text representation. If the boolean is @True@ then the search is case-blind.
valueSearch :: Bool -> Text -> ExtensionValues -> [(FieldId, Int)]
valueSearch caseBlind needle = concatMap search . M.toList
  where
    search :: (FieldId, ExtValue) -> [(FieldId, Int)]
    search (fName, v) =
      map ((fName, ) . T.length . fst) $
      T.breakOnAll needle1 $
      caseF $
      displayValue v
    caseF = if caseBlind then T.toCaseFold else id
    needle1 = caseF needle
