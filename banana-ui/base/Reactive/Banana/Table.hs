{-# LANGUAGE GADTs #-}

{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the banana-ui-gtk library. The banana-ui-gtk library is
proprietary and confidential. Copying is prohibited 
-}

{- |

A dialog table is a rectangular grid where each row represents a value in a list and each
column represents a field in the row values.

Tables are updated via the 'RowChange' type. This allows displays to be updated more efficiently
than if the only thing known was that something in the table had changed.
-}
module Reactive.Banana.Table where

import Control.Lens
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Reactive.Banana.Common


-- | Ways in which column data may be displayed.
data FieldEditor a where
   EditBool :: FieldEditor Bool
   -- | Edit the column as text. Only entries with a valid read will be accepted.
   EditEntry :: Prism' Text a -> FieldEditor a
   -- | Edit a multi-line text.
   EditNote :: FieldEditor Text
   -- | Select column value from a built-in type.
   EditEnum :: (Ord a) => (a -> Text) -> [a] -> FieldEditor a
   -- | Column represents a date.
   EditDate :: DateFormat -> FieldEditor Day
   -- | Column represents a colour.
   EditColour :: FieldEditor Colour
   -- | Column contains an icon identified by its name. The function argument is a predicate to
   -- filter the icon context names so only some context tabs are displayed in the dialog.
   EditIcon :: (Text -> Bool) -> FieldEditor Text
   -- | Select the column value from a menu of text strings provided by the user.
   EditCombo :: [Text] -> FieldEditor Text
   -- | Display a read-only column.
   EditFixed :: (a -> Text) -> FieldEditor a
   -- | The argument field is optional and may be left blank. Nested "Opt" is not
   -- supported.
   Opt :: FieldEditor a -> FieldEditor (Maybe a)


-- | Utiltity for enumerations with a "Show" instance.
enumField :: (Ord a, Show a) => [a] -> FieldEditor a
enumField = EditEnum $ T.pack . show

-- | The list of values from @minBound@ to @maxBound@.
allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound .. maxBound]


-- | A field of type @a@ within a table of type @row@.
data TableField row a = TableField {
      columnName :: Text,
      fieldValue :: Traversal' row a,
      fieldIcon :: Maybe (a -> Text),  -- ^ Icon name.
      fieldEditor :: FieldEditor a,
      fieldColour :: Maybe (a -> Colour)
   }


-- | Data for a table column.
data TableColumn row = forall a . TableColumn (TableField row a)


-- | Convenience function for constructing a @TableColumn@.
mkField ::
   Text
   -> Traversal' row a
   -> Maybe (a -> Text)   -- ^ Icon name
   -> FieldEditor a
   -> Maybe (a -> Colour)  -- ^ Colour
   -> TableColumn row
mkField txt lns icon ed clr = TableColumn $ TableField txt lns icon ed clr


type Table row = [TableColumn row]


-- | How can this table be edited?
data TableEditing a =
   TableAdd Text a | TableDelete | TableShuffle deriving (Eq, Ord, Show)
