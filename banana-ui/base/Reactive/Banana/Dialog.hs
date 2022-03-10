{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}

{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the banana-ui-gtk library. The banana-ui-gtk library is
proprietary and confidential. Copying is prohibited 
-}

{- |

__Reactive dialogs using lenses.__

Given a data type with lens accessors you can create a dialog for
values of the data type with its widgets mapped on to the data type via lenses. When one field
is updated all the other widgets are updated as well, so for instance widgets for the lower and
upper bounds of a range can be linked to a widget showing the current range size.

More sophisticated dialogs are enabled by the "Handler" arguments to dialog widgets. You can
use these to attach events and behaviors to value changes within the dialog, and thereby provide
behaviors signaling valid or invalid values, or enabling or disabling parts of the dialog.
-}

module Reactive.Banana.Dialog (
   DialogWrapper (..),
   promoteWrapper,
   DialogSelector,
   promoteSelector,
   Dialog (..),
   SubDialog (..),
   UnionTabData (..),
   DialogElement (..),
   promoteElement,
   elementEnable1,
   elementLabel1,
   DialogSpecifier (..),
   MemoBoxSize (..),
   memoBoxCss,
   TreeOper (..),
   simpleTextBox,
   typedTextBox,
   maybeTextBox,
   simpleMenu,
   rangeMenu,
   boundedMenu,
   isStandardContext,
   gtkIconStandardContexts
) where

import Control.Event.Handler
import Control.Lens
import Data.Set (Set)
import Data.Text (Text, pack)
import Data.Time.Calendar
import Data.Tree
import Reactive.Banana.Common
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.Banana.Menu
import Reactive.Banana.Table


-- | A function from an item to the appropriate "Dialog" (if any) for that item.
--
-- Dialog selectors and wrappers are used for sum types. Given types @Foo@ and @Bar@, with
-- corresponding dialogs @fooDialog :: Dialog Foo@ and @barDialog :: Dialog Bar@ you can define:
--
-- > type Foobar = Either Foo Bar
-- >
-- > foobarSelector :: DialogSelector Foobar
-- > foobarSelector (Left _) = Just $ DialogWrapper _left $ return fooDialog
-- > foobarSelector (Right _) = Just $ DialogWrapper _right $ return barDialog
type DialogSelector v = v -> Maybe (DialogWrapper v)


-- | As for "promoteWrapper".
promoteSelector :: Traversal' v w -> DialogSelector w -> DialogSelector v
promoteSelector trav selector v = case v ^? trav of
   Nothing -> Nothing
   Just w -> promoteWrapper trav <$> selector w


-- | For sum types a "Traversal" is used to map the variant to an appropriate "Dialog". The
-- dialog is in the "MomentIO" monad so it can use its own events and behaviours internally, and
-- can also access the current state of its output. The "Behavior" passed to the argument is
-- the current state of the dialog, allowing for feedback mechanisms within the dialog.
data DialogWrapper v =
   forall s . DialogWrapper (Traversal' v s) (MomentIO (Dialog s))


-- | If type @Foo@ has a "DialogSelector" defined then this enables the result to be promoted
-- to an enclosing type @FooBar@ by using an additional traversal (usually a prism) @_Foo@:
--
-- > data FooBar = FooBar Foo Bar
-- >
-- > fooBarSelector (FooVal foo) = promoteWrapper _Foo <$> fooSelector foo
-- > fooBarSelector (BarVal bar) = promoteWrapper _Bar <$> barSelector bar
promoteWrapper :: Traversal' v w -> DialogWrapper w -> DialogWrapper v
promoteWrapper prism1 (DialogWrapper prism2 dialog) = DialogWrapper (prism1 . prism2) dialog


-- | A dialog for editing values of type @s@. Note that nested dialogs do not propogate their
-- validity upwards, so it is up to the application to ensure that if @s1@ is a precondition
-- for the whole dialog and @s2@ is a precondition for part of it then @s1@ implies @s2@.
data Dialog s = Dialog {
   dialogTitle :: Text,   -- ^ Dialog name
   dialogMain :: SubDialog s
}


-- | Abstract type for the contents of dialog boxes.
data SubDialog s =
   forall a . (Eq a) => Exec (s -> a) (a -> SubDialog s)
      -- ^ Provides access to the input value from the application. The dialog changes whenever
      -- the @a@ value is not equal to the previous one.
   | ExecEvent (SubDialog s) (Event (s, SubDialog s))
      -- ^ Provides access to events from other widgets.
   | HBox [[SubDialog s]]
   | VBox [[SubDialog s]]
      -- ^ HBoxes and VBoxes act as usual. Dividers are inserted between sublists.
   | Grid [Text] [Text] [[DialogElement s]]
      -- ^ Elements are arranged in a grid. The innter lists are of the rows. Names are ignored.
      -- The frirst two arguments are the column headers and the row headers respectively.
   | forall a . Frame (s -> Bool) (Lens' s a) (Dialog a)
      -- ^ Named frame around a group of elements. The behaviour enables or disables the contents.
      -- If the internal "dialogValid" behavior is False then the frame outline will be red.
      --
      -- The use of a lens to map from @s@ to @a@ allows common dialogs to be associated with
      -- common data types that are used in larger contexts. For instance an @Address@ field
      -- might appear as an element of @Employee@ and @Customer@, and their dialogs can reuse
      -- a common address dialog.
   | Elements [DialogElement s]
      -- ^ Bottom level elements to be displayed together, with labels and fields aligned.
   | BigElement (DialogElement s)
      -- ^ An element given its own frame. Useful for larger elements such as MemoBoxes which
      -- look awkward in an aligned list.
   | ButtonBar [(Text, s -> s)]
      -- ^ A row of buttons that edit the data in some way.
   | forall a . UnionTab (Lens' s a) [UnionTabData a]
      -- ^ A list of alternative dialogs depending on which variant of a union type is selected.
   | Message (s -> Text)
      -- ^ A message for the user.
   | ValidityCheck (s -> Bool) (SubDialog s)
      -- ^ Check if the subdialog results match the predicate.
   | ValidityMessage (s -> [Text]) (SubDialog s)
      -- ^ If the function returns error messages then display them.


-- | Data for a single tab in a notebook.
data UnionTabData a = forall b . UnionTabData {
      tabLabel :: Text,
      tabPrism :: Prism' a b,
      tabDefault :: b,
      tabSubDialog :: SubDialog b
   }

-- | Primitive dialog elements.
data DialogElement s =
   forall a . LensElement {
         elementEnable :: s -> Bool,
         elementLabel :: Text,   -- ^ The field name for this element.
         elementSpec :: DialogSpecifier a,
         elementLens :: Lens' s a, -- ^ Maps from the value being edited @s@ to the element @a@.
         elementChanged :: Handler a  -- ^ Called when the element value is changed.
      } -- ^ A GUI widget tied directly to a data value through a lens.
   | forall a . SimpleElement {
         elementEnable :: s -> Bool,
         elementLabel :: Text,  -- ^ The field name for this element.
         elementSpec :: DialogSpecifier a,
         elementValue :: a,         -- ^ The initial value of the element.
         elementChanged :: Handler a  -- ^ Called when the value changes.
      } -- ^ A GUI widget linked to an event and emitting an event, but not tied to a data value.
   | IconDecorated {
         elementIcon :: s -> Text,  -- ^ The icon name for this value, or \"blank-icon\" for blank.
         element :: DialogElement s
      } -- ^ Add an icon to the left of the element.
   | ColourDecorated {
         elementColour :: s -> Maybe Colour,  -- The background colour for this element.
         element :: DialogElement s
      } -- ^ Add a variable background colour to the widget.
   | LinkDecorated {
         elementLink :: s -> Maybe Text,  -- ^ The hyperlink to add.
         element :: DialogElement s
      }  -- ^ Add a hyperlink to the left of the inner widget.


elementEnable1 :: DialogElement s -> s -> Bool
elementEnable1 (IconDecorated _ i) = elementEnable1 i
elementEnable1 (ColourDecorated _ i) = elementEnable1 i
elementEnable1 (LinkDecorated _ i) = elementEnable1 i
elementEnable1 v = elementEnable v


elementLabel1 :: DialogElement s -> Text
elementLabel1 (IconDecorated _ i) = elementLabel1 i
elementLabel1 (ColourDecorated _ i) = elementLabel1 i
elementLabel1 (LinkDecorated _ i) = elementLabel1 i
elementLabel1 v = elementLabel v


-- | Add an extra lens to an element
promoteElement :: Lens' s1 s2 -> DialogElement s2 -> DialogElement s1
promoteElement lns (LensElement e lbl spec lns1 chng) =
   LensElement (e . view lns) lbl spec (lns . lns1) chng
promoteElement lns (SimpleElement e lbl spec v chng) =
   SimpleElement (e . view lns) lbl spec v chng
promoteElement lns (IconDecorated iconF e) =
   IconDecorated (iconF . view lns) $ promoteElement lns e
promoteElement lns (ColourDecorated colF e) =
   ColourDecorated (colF . view lns) $ promoteElement lns e
promoteElement lns (LinkDecorated linkF e) =
   LinkDecorated (linkF . view lns) $ promoteElement lns e


-- | The type of dialog element.
--
-- [@TextBoxSpec@] A editable text box for a textual representation of type @a@. The Prism should
-- map a valid string onto the equivalent value.
--
-- [@MemoBoxSpec@] An editable multi-line text box. Parameters are width and height in pixels.
-- Note that applications should use "smallMemo", "mediumMemo" or "largeMemo" for consistency in
-- look and feel.
--
-- [@FixedSpec@] A field that cannot be edited.
--
-- [@MenuSpec@] A menu of options. Note that separators and sub-menus are not supported.
--
-- [@TickBox@] Tick for True, untick for False.
--
-- [@DateSpec@] A text field which will accept dates in most common formats and display them
-- in the format given. Uses POSIX-style date format strings. Two common ones are provided as
-- "longDate" and "shortDate".
--
-- [@TableSpec@] A table of values. Takes a list of flags, a default value for new rows and a
-- table specification.
--
-- [@ForestTableSpec@] Presents a forest of items in a table with columns defined by the "Table"
-- argument. If the "DialogSelector" is present then activating a row will bring up the sub-dialog
-- that it specifies.
--
-- [@ForestEditorSpec@] Allows a forest of items to be edited by drag-and-drop, insertion and
-- deletion. The arguments are: the name function for each item, the edit menu for each item,
-- a predicate for whether an item is allowed to have children, and a "DialogSelector" for
-- activated items. Internally this uses the @gi-gtk-hs@ tree model, so it is not suitable for use
-- on very large trees. It also does not update the displayed forest when the input behaviour
-- changes.
data DialogSpecifier a where
   -- | Single-line textbox.
   TextBoxSpec :: Prism' Text a -> DialogSpecifier a
   -- | Multi-line textbox. If the second argument is @True@ then the box will grow vertically
   -- to consume the available space.
   MemoBoxSpec :: MemoBoxSize -> Bool -> DialogSpecifier Text
   -- | Non-editable text. A double-click will activate the selector.
   FixedTextSpec :: (a -> Text) -> Maybe (DialogSelector a) -> DialogSpecifier a
   -- | Non-editable text which may take multiple lines.
   FixedMemoSpec ::
      (a -> Text) -> Maybe (DialogSelector a) -> DialogSpecifier a
   -- | Select a single item from a list. The tuple contains the item label,
   -- an optional icon name, an optional background colour, and the item itself.
   MenuSpec :: (Eq a) => [(Text, Maybe Text, Maybe Colour, a)] -> DialogSpecifier a
   -- | Select items from a tree or list. Only nodes with a @Just@ value may be
   -- picked, and branches with no valid children will be trimmed. The @a@ values in the
   -- forest must all be distinct. The texts are the node name and optional node tooltip.
   TreeSelectorSpec :: (Ord a) =>
      Forest (Text, Maybe Text, Maybe a) -> DialogSpecifier (Set a)
   TickBox :: DialogSpecifier Bool
   DateSpec :: DateFormat -> DialogSpecifier Day
   DateSpecMaybe :: DateFormat -> DialogSpecifier (Maybe Day)
   IconSpec :: (Text -> Bool) -> DialogSpecifier Text
   -- | Select an icon. The argument is a predicate for the icon context name.
   IconSpecMaybe :: (Text -> Bool) -> DialogSpecifier (Maybe Text)
   ColourSpec :: DialogSpecifier Colour
   ColourSpecMaybe :: DialogSpecifier (Maybe Colour)
   -- | Present a list of items in a table.
   -- If the "DialogSelector" is present then activating a row will bring up a sub-dialog.
   TableSpec :: (Eq a) =>
      [TableEditing a] -> Table a -> Maybe (DialogSelector a) -> DialogSpecifier [a]
   -- | Present a forest of items in a grid. The argument describes the fields to be shown
   -- at each level of the forest.
   ForestTableSpec :: (Eq a) => [(Text, [DialogElement a])] -> DialogSpecifier (Forest a)
   -- | Edit a complete forest, with item addition, deletion, move and menus.
   ForestEditorSpec :: (Eq a) =>
      (a -> Text)   --  Text label for a tree item.
      -> (Maybe a -> Menu (TreeOper a))  -- Edit menu for tree item.
      -> (a -> Bool)               -- Can this item have children?
      -> Maybe (DialogSelector a)  -- Activation pops up an edit dialog.
      -> DialogSpecifier (Forest a)


-- | Hint about how much text is expected in the memobox.
data MemoBoxSize =
   MemoSmall   -- ^ A small amounts of text, such as a sentence or two.
   | MemoMedium   -- ^ several sentences or a couple of short paragraphs.
   | MemoLarge  -- ^ Extended pieces of text.
   deriving (Eq, Ord, Show, Read)


-- | CSS class names for memo box sizes.
memoBoxCss :: MemoBoxSize -> Text
memoBoxCss MemoSmall = "memo-small"
memoBoxCss MemoMedium = "memo-medium"
memoBoxCss MemoLarge = "memo-large"


-- | Tree operations. Elements can be added or deleted. The terms *before*, *after* and *in*
-- refer to the item right clicked by the user.
data TreeOper a = TreeAddBefore a | TreeAddAfter a | TreeAddIn a  | TreeDelete


-- | A single text box holding just a string.
simpleTextBox :: DialogSpecifier Text
simpleTextBox = TextBoxSpec $ prism' id Just


-- | A text box for arbitrary data types via their @Read@ and @Show@ instances.
typedTextBox :: (Show a, Read a) => DialogSpecifier a
typedTextBox = TextBoxSpec textPrism


-- | A text box for "Maybe" values. A blank entry equates to "Nothing".
maybeTextBox :: (Show a, Read a) => DialogSpecifier (Maybe a)
maybeTextBox = TextBoxSpec $ prismToMaybe textPrism


-- | A menu for a list of showable items.
simpleMenu :: (Show a, Eq a) => [a] -> DialogSpecifier a
simpleMenu = MenuSpec . map (\x -> (pack $ show x, Nothing, Nothing, x))


-- | A menu for a range within an enumerated type.
rangeMenu :: (Enum a, Show a, Eq a) => (a, a) -> DialogSpecifier a
rangeMenu (x1, x2) = simpleMenu [x1 .. x2]


-- | A menu for the whole of an enumerated type.
--
-- Be careful what you use this for. @boundedMenu :: DialogSpecifier Int@ will compile,
-- but will run out of memory trying to generate an entry for every value.
boundedMenu :: (Bounded a, Enum a, Show a, Eq a) => DialogSpecifier a
boundedMenu = rangeMenu (minBound, maxBound)


-- | True if the string is a member of the standard icon contexts.
isStandardContext :: Text -> Bool
isStandardContext = (`elem` gtkIconStandardContexts)


-- A convenience list of all the standard icon contexts provided by GTK. Useful to prevent
-- all the application icons from being shown in the icon selection dialog.
gtkIconStandardContexts :: [Text]
gtkIconStandardContexts = [
      "Actions",
      "Animations",
      "Applications",
      "Categories",
      "Devices",
      "Emblems",
      "Emotes",
      "FileSystems",
      "International",
      "MimeTypes",
      "Places",
      "Status",
      "Stock"
   ]
