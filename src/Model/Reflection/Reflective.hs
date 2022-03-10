{-# LANGUAGE NoMonomorphismRestriction #-}
{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}


{- |
-}

module Model.Reflection.Reflective (
   Reflective (..),
   reflectiveNames,
   reflection,
   valueReferences,
   Extract,
   extract,
   valueFormat
) where

import Control.Lens
import Control.Monad.State
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Model.Reflection.NamedRelation (Relation)
import Model.Reflection.References
import Model.Reflection.Types
import Model.Reflection.Values
import Reactive.Banana.Common


-- | A reflective type can describe its type and expose its value in terms of
-- that description. All extension fields have value "ExtNone" by default. Access to values
-- is by way of the "ix" function from "Ixed".
class Reflective a where
   -- | A reflective type may have a fixed name, or it may be a union where each variant has
   -- a distinct name.
   reflectiveName :: a -> Variant a
   -- | The original design for Variant simply had them as the human-readable name. However this
   -- caused issues when a name change was required because the old name was already baked
   -- into the JSON of existing models. Hence this function to translate from the Variant's internal
   -- name to the user interface name. The default is just the "variantName".
   variantUserLabel :: Variant a -> Text
   variantUserLabel (Variant n) = n
   -- | One default value of the type for each reflective name.
   reflectiveDefaults :: [a]
   -- | Some fields are built in to the underlying data type.
   reflectiveBuiltIn :: a -> [FieldId]
   -- | Read the built-in fields of @a@.
   reflectiveGet :: a -> ExtensionValues
   -- | Write the built-in fields, and return any unused fields.
   reflectiveSet :: a -> Extract a
   -- | Built-in references between variants of @a@.
   reflectiveBuiltInRefs :: RefTypeTable a
   -- | Built-in notions of \"arrows\". These are variants which are represented by lines
   -- on a diagram and have \"tail\" and \"head\" relationships to other entities in the model.
   reflectiveArrows :: Map (Variant a) (Relation, Relation)

   -- ToDo: Figure out some kind of type hierarchy.


-- | All possible results from "reflectiveName"
reflectiveNames :: (Reflective a) => [Variant a]
reflectiveNames = map reflectiveName reflectiveDefaults


-- | The full set of fields, built in first followed by the extensions.
reflection :: (Reflective a) => Extensions a -> a -> [FieldId]
reflection ext v = reflectiveBuiltIn v ++ ext (reflectiveName v)


-- | Given a "RefTypeTable" and a value, find all the reference fields that can apply to the
-- value. The @Bool@ flag is true if the argument can only reference one instance of the
-- types listed.
valueReferences :: (Reflective a) =>
   RefTypeTable a -> a -> [(Relation, Bool, Set (Variant a))]
valueReferences tbl v = filter (not . S.null . view _3) $ map f fullTable
   where
      varName = reflectiveName v
      fullTable = refTypeTableToList $ reflectiveBuiltInRefs <> tbl
      f (field, typ) =
         let (single, vars) = refTypeOtherSide varName typ
         in (field, single, vars)


-- | Tiny monad for managing reflective extension data. An instance of "Reflective"
-- will have some built-in fields which must be marshalled in and out of "ExtensionValues".
-- This assists with extracting the built-in fields.
type Extract a = State ExtensionValues a


-- | Destructively read a value from the state, with a default if it is not found or is the
-- wrong type.
extract :: a -> Prism' ExtValue a -> FieldId -> Extract a
extract d prsm k = do
   s <- get
   put (M.delete k s)
   return $ fromMaybe d $ M.findWithDefault ExtNone k s ^? prsm


-- | Icon and colour to display the value with.
valueFormat :: TypeDef -> ExtValue -> (Maybe IconName, Maybe Colour)
valueFormat (DecoBoolDef (DecoBool icons colours)) (ExtBool b) =
   let f b1 = if b1 then snd else fst
   in (f b <$> icons, f b <$> colours)
valueFormat (DecoIntDef (DecoRange _ m)) (ExtInt i) =
   fromMaybe (Nothing, Nothing) $ rangeMap m i
valueFormat (DecoIntDef (DecoRange _ m)) (ExtReal r) =
   fromMaybe (Nothing, Nothing) $ rangeMap m $ round r
valueFormat (DecoRealDef (DecoRange _ m)) (ExtInt i) =
   fromMaybe (Nothing, Nothing) $ rangeMap m $ fromIntegral i
valueFormat (DecoRealDef (DecoRange _ m)) (ExtReal r) =
   fromMaybe (Nothing, Nothing) $ rangeMap m r
valueFormat (EnumDef enums) v =
   let txt = displayValue v
   in maybe (Nothing, Nothing) (\(EnumItem _ i c) -> (i, c)) $
      find ((txt ==) . view enumItemName) enums
valueFormat _ _ = (Nothing, Nothing)
