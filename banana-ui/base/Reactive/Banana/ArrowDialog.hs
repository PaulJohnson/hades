{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}

{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the banana-ui-gtk library. The banana-ui-gtk library is
proprietary and confidential. Copying is prohibited 
-}

{- |

Arrowized language for defining abstract graphical user interfaces. Gadgets act as a combination
of stream and value transformer, in that the input and ouput values are considered to have both a
current value and an event when that value changes. When a value is edited in a gadget this triggers
an event to update the downstream gadgets.

Gadgets also have side channels that are not part of the transformer semantics. These are the
environment (@e@ type parameter) and the event writer (@w@ type parameter). The former is used for
information that would otherwise have to tramp unchanged alongside every input and output. The
latter is useful when a large structure is being edited and the application needs the deltas rather
than the modified whole.
-}

module Reactive.Banana.ArrowDialog (
   -- * Basic Types
   ComboItem (..),
   ClickableItem (..),
   Orientation (..),
   MemoBoxSize (..),
   FilePathPurpose (..),
   FilePathFilterItem (..),
   FilePathFilter,
   -- * Gadget Types
   Gadget,
   Gadget',
   GadgetF,
   GadgetF',
   PrismaticGadget (..),
   Lens1 (..),
   TreeOper (..),
   -- * Basic Combinators
   focusing,
   focusingOver,
   prismatic,
   prismaticOver,
   traversing,
   traversingOver,
   accum,
   initially,
   getInitial,
   getEnv,
   getInitialEnv,
   withEnv,
   send,
   sendMap,
   sendEmpty,
   -- * Dynamic Gadgets
   exec,
   cond,
   unionTab,
   -- * Decorations
   enabled,
   optional,
   optionalOver,
   intercept,
   icon,
   coloured,
   linked,
   styled,
   styled1,
   frame,
   plainFrame,
   simpleFrame,
   message,
   message1,
   scrolled,
   -- * Grouping
   form,
   tabForm,
   box,
   grid,
   forestTable,
   -- * Data Validation
   validate,
   validateOver,
   validate1,
   validateText,
   validateTextOver,
   validateText1,
   -- * Data Entry Fields
   textBox,
   simpleTextBox,
   typedTextBox,
   maybeTextBox,
   displayText,
   memoBox,
   displayMemo,
   clickableList,
   clickableSingle,
   clickableDouble,
   comboBox,
   simpleCombo,
   rangeCombo,
   boundedCombo,
   radio,
   simpleRadio,
   rangeRadio,
   boundedRadio,
   tickBox,
   dateBox,
   iconBox,
   colourBox,
   imageDisplay,
   treeSelector,
   forestEditor,
   filePathSelector,
   table,
   buttonBar,
   image,
   readOnlyText,
   readOnlyMemo,
   buttonIO,
   -- * Pop-up Dialogs
   textPopup,
   memoPopup,
   -- GTK Icon Utilities
   isStandardContext,
   gtkIconStandardContexts,
   -- * Dialogs
   DialogButtons (..),
   Dialog (..),
   Dialog',
   promoteDialog,
   DialogSelector,
   DialogSelector',
   promoteDialogSelector,
   constantDialog,
   -- * Debugging
   showGadget,
   traceGadget,
   traceGadgetOver,
   traceGadget1
) where

import Control.Arrow
import Control.Category
import Control.Lens
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text, pack)
import Data.Time.Calendar
import Data.Tree
import Prelude hiding (id, (.))
import Reactive.Banana.Common
import Reactive.Banana.GadgetPrimitives
import Reactive.Banana.Menu
import Reactive.Banana.Table


-- | A gadget that views its inputs and outputs through a lens.
--
-- @focusing id@ is often used to turn a "Gadget" into a "GadgetF"
focusing :: Lens i o a b -> Gadget e w a b -> GadgetF e w i o
focusing = Focusing


-- | As for "focusing", but for when the inner Gadget already outputs a function.
focusingOver :: Lens i o a b -> GadgetF e w a b -> GadgetF e w i o
focusingOver lns g = withLens lns $ \getter _ ->
      focusing (lens getter $ \i f -> lns %~ f $ i) g
      -- There is probably a simple and clever way of doing this.


-- | A gadget that views its inputs and outputs through a "Prism". Its output is a function which
-- overwrites the input using "re", so even if the input doesn't match the prism the output will.
prismatic :: a -> Prism s t a b -> Gadget e w a b -> Gadget e w s t
prismatic = Prismatic


-- | As for "prismatic", but for when the inner Gadget already outputs a function.
prismaticOver :: a -> Prism' s a -> GadgetF' e w a -> GadgetF' e w s
prismaticOver = PrismaticOver


-- | A gadget that views its inputs and outputs through a "Traversal". Its output is a function
-- which sets the relevant part of the input, so it is typically used
-- for a dialog element that accesses one part of a larger structure.
--
-- Note that traversals cannot change the shape of the input, so @traversing _Left@
-- will ignore any @Right@ values rather than changing them.
traversing :: a -> Traversal i o a b -> Gadget e w a b -> GadgetF e w i o
traversing = Traversing


-- | As for "traversing", but for when the inner Gadget already outputs a function.
traversingOver :: a -> Traversal i o a b -> GadgetF e w a b -> GadgetF e w i o
traversingOver = TraversingOver


-- | Accumulate edits. The converse of "focusing" and its relatives.
accum :: GadgetF' e w a -> Gadget' e w a
accum = Accum


-- | A fixed initial value, regardless of the form inputs. After that it emits its input. Used in
-- recursive loops to set an initial value.
initially :: a -> Gadget' e w a
initially = Initially


-- | Get the initial input, for when the gadget should not be recomputed if the input changes.
getInitial :: (i -> Gadget e w i o) -> Gadget e w i o
getInitial = GetInitial


-- | Get access to the environment. Ignores its input.
getEnv :: Gadget e w i e
getEnv = GetEnv


-- | Get the initial environment.
getInitialEnv :: (e -> Gadget e w i o) -> Gadget e w i o
getInitialEnv = GetInitialEnv


-- | Create a subsiduary environment.
withEnv :: Gadget e w i e' -> Gadget e' w i o -> Gadget e w i o
withEnv = WithEnv


-- | Send event data via the side channel when the input changes. Any data sent this way will
-- not be blocked by downstream validation checks, so put validation upstream of this gadget.
send :: (a -> Maybe w) -> Gadget' e w a
send = Send


-- | Modify the data sent over the side channel.
sendMap :: (w1 -> Maybe w2) -> Gadget e w1 i o -> Gadget e w2 i o
sendMap = SendMap


-- | Block data sent over the side channel. Useful for coercing the types of gadgets that never
-- send anything.
sendEmpty :: Gadget e w1 i o -> Gadget e w2 i o
sendEmpty = sendMap $ const Nothing


-- | Dynamic gadgets. A new gadget is computed when the derived @a@ value is changed by the user.
-- Any state in the old one is lost. Changes to @a@ from the application are ignored.
exec :: (Eq a) => (a -> Gadget e w i o) -> Gadget e w (a, i) o
exec = Exec


-- | Select the first matching prism from the list and display the associated gadget.
-- Unlike "exec" the gadgets are persistent. The
-- first argument is the default when none of the prisms match.
cond :: [PrismaticGadget e w i o] -> Gadget e w i o
cond = Cond


-- | Multiple tabs, each of which has a dialog for one variant.
unionTab :: [(Text, PrismaticGadget e w i o)] -> Gadget e w i o
unionTab = UnionTab


-- | The gadget is only enabled in the GUI when the @Bool@ input is true.
enabled :: Gadget e w i o -> Gadget e w (i, Bool) o
enabled = Enabled


-- | Optional values. Most arguments are decorated with a tick-box to enable them, but some have
-- special cases. E.g @optional textBox@ equates blank to @Nothing@.
optional :: i -> Gadget e w i o -> Gadget e w (Maybe i) (Maybe o)
optional = Optional


-- | Variant of "optional" for a "GadgetF".
optionalOver :: i -> GadgetF e w i o -> GadgetF e w (Maybe i) (Maybe o)
optionalOver d g = Optional d g >>> arr update
   where
      -- update :: Maybe (i -> o) -> Maybe i -> Maybe o
      update Nothing _ = Nothing
      update (Just f) i = Just $ f $ fromMaybe d i


-- | Intercept single or double clicks (first argument @True@ for double clicks) and open a
-- pop-up dialog based on the content of the underlying gadget. The dialog result is used
-- as the new input for the underlying gadget.
--
-- Bear in mind that intercepting single button presses will also catch any double-button presses
-- as two single-button presses.
intercept :: Bool -> DialogSelector' e w a -> Gadget' e w a -> Gadget' e w a
intercept = Intercept


-- | Decorate with an icon. The function should return an icon name or \"blank-icon\" for blank.
icon :: (i -> Text) -> Gadget e w i o -> Gadget e w i o
icon = Icon


-- | Decorate with a background colour. The foreground will be either white or black
-- for contrast.
coloured :: (i -> Maybe Colour) -> Gadget e w i o -> Gadget e w i o
coloured = Coloured


-- | Display a hyperlink button with the gadget.
linked :: (i -> Maybe Text) -> Gadget e w i o -> Gadget e w i o
linked = Linked


-- | Display the inner gadget with a named style. In GTK this is the class name in the CSS.
styled :: (i -> Maybe Text) -> Gadget e w i o -> Gadget e w i o
styled = Styled


-- | As for "styled", but with a fixed style name.
styled1 :: Text -> Gadget e w i o -> Gadget e w i o
styled1 str = styled (const $ Just str)

-- | Frame drawn around the argument, optionally with a text label.
frame :: (i -> Maybe Text) -> Gadget e w i o -> Gadget e w i o
frame = Frame


-- | Frame with no label.
plainFrame :: Gadget e w i o -> Gadget e w i o
plainFrame = Frame $ const Nothing


-- | Frame with a constant label.
simpleFrame :: Text -> Gadget e w i o -> Gadget e w i o
simpleFrame txt = Frame $ const $ Just txt


-- | Gadgets aligned vertically or horizontally with labels. This only emits the latest edit, so
-- it must be enclosed in an "accum".
form :: Orientation -> [(Text, GadgetF' e w a)] -> GadgetF' e w a
form = Form


-- | Similar function to "form", but the gadgets are presented in a set of tabs.
tabForm :: [(Text, GadgetF' e w a)] -> GadgetF' e w a
tabForm = TabForm

-- | Child widgets are grouped together, either horizontally or vertically. Sublists have
-- separator lines between them. As with "form" it must be enclosed in an "accum".
box :: Orientation -> [[GadgetF' e w a]] -> GadgetF' e w a
box = Box


-- | Elements are arranged in a grid. The innter lists are of the rows.
-- The first two arguments are the column headers and the row headers respectively. As with "form"
-- it must be enclosed in an "accum".
grid :: [Text] -> [Text] -> [[GadgetF' e w a]] -> GadgetF' e w a
grid = Grid


-- | Read-only message for the user. The displayed message is a function of the input.
message :: (e -> a -> Text) -> Gadget' e w a
message = Message


-- | Constant message regardless of input.
message1 :: Text -> Gadget' e w a
message1 txt = Message $ const $ const txt


-- | Add scroll bars to the argument if necessary.
scrolled :: Gadget e w i o -> Gadget e w i o
scrolled = Scrolled


-- | Validate the output against a predicate, signalling an error and blocking the output
-- if it is not met.
validate :: (o -> Bool) -> Gadget e w i o -> Gadget e w i o
validate f = Validate $ const $ const f


-- | As for "validate", but for gadgets that output a function.
validateOver :: (o -> Bool) -> GadgetF e w i o -> GadgetF e w i o
validateOver f = Validate (\_ i o -> f $ o i)


-- | Validate the output against the input and environment.
validate1 :: (e -> i -> o -> Bool) -> Gadget e w i o -> Gadget e w i o
validate1 = Validate


-- | Validate the output against a predicate which may return an error message.
validateText :: (o -> Maybe Text) -> Gadget e w i o -> Gadget e w i o
validateText f = ValidateText $ const $ const f


-- | As for "validateText", but for gadgets that output a function.
validateTextOver :: (o -> Maybe Text) -> GadgetF e w i o -> GadgetF e w i o
validateTextOver f = ValidateText (\_ i o -> f $ o i)


-- | Validate the output against the input.
validateText1 :: (e -> i -> o -> Maybe Text) -> Gadget e w i o -> Gadget e w i o
validateText1 = ValidateText


-- | Single line text input. The prism is used in reverse to validate the input.
--
-- There is a nasty gotcha with @textBox@ and its relatives; when a text box is updated from an
-- upstream arrow it will send two updates, one for an empty string and a second for the new value.
-- This transient behavior can cause issues. It can be detected using "traceGadget".
textBox :: (e -> Prism' Text a) -> Gadget' e w a
textBox = TextBox


-- | A single text box just holding a string.
simpleTextBox :: Gadget' e w Text
simpleTextBox = TextBox $ const id


-- | A text box for arbitrary data types via their @Read@ and @Show@ instances.
typedTextBox :: (Show a, Read a) => Gadget' e w a
typedTextBox = TextBox $ const textPrism


-- | A text box for "Maybe" values. A blank entry equates to "Nothing".
maybeTextBox :: (Show a, Read a) => Gadget' e w (Maybe a)
maybeTextBox = TextBox $ const $ prismToMaybe textPrism


-- | Text displayed as a label, but presents a text box for editing when clicked.
displayText ::Gadget e w Text Text
displayText = DisplayText


-- | Multi-line text input. If the @Bool@ is true the box can expand vertically.
-- to fill the available space.
memoBox :: MemoBoxSize -> Bool -> Gadget' e w Text
memoBox = MemoBox

-- | Multi-line, multi-paragraph text displayed as a label, but presents a text box for
-- editing when clicked.
displayMemo :: Gadget' e w Text
displayMemo = DisplayMemo

-- | A box containing a list of items separated by commas. Each item will emit the
-- relevant value when clicked. The widget will be given the style \"clickable-list\".
clickableList :: Gadget e w [ClickableItem a] (Maybe a)
clickableList = ClickableList


-- | A clickableList where each item can be selected with a single click.
clickableSingle :: (a -> Text) -> Gadget e w [a] (Maybe a)
clickableSingle f = ClickableList <<< arr (map mkClick)
   where
      mkClick v = ClickableItem (f v) (Just v) Nothing mempty


-- | A clickableList where each item can be selected with a double click.
clickableDouble :: (a -> Text) -> Gadget e w [a] (Maybe a)
clickableDouble f = ClickableList <<< arr (map mkClick)
   where
      mkClick v = ClickableItem (f v) Nothing (Just v) mempty


-- | Menu selection box. Menu items are derived from the environment.
comboBox :: (Eq a) => (e -> [ComboItem a]) -> Gadget' e w a
comboBox = Combo


-- | A menu for a fixed list of Showable items.
simpleCombo :: (Show a, Eq a) => [a] -> Gadget' e w a
simpleCombo = Combo . const . map (\x -> ComboItem (pack $ show x) Nothing Nothing x)


-- | A menu for a range within an enumerated type.
rangeCombo :: (Enum a, Show a, Eq a) => (a, a) -> Gadget' e w a
rangeCombo (x1, x2) = simpleCombo [x1 .. x2]


-- | A menu for the whole of an enumerated type.
--
-- Be careful what you use this for. @boundedCombo :: Gadget' Int@ will compile,
-- but will run out of memory trying to generate an entry for every value.
boundedCombo :: (Bounded a, Enum a, Show a, Eq a) => Gadget' e w a
boundedCombo = rangeCombo (minBound, maxBound)


-- | Radio buttons for a list of items.
radio :: (Eq a) => (e -> [(Text, a)]) -> Gadget' e w a
radio = Radio


-- | Radio buttons for a fixed list of showable items.
simpleRadio :: (Show a, Eq a) => [a] -> Gadget' e w a
simpleRadio = Radio . const . map (\x -> (pack $ show x, x))


-- | Radio buttons for a range within an enumerated type.
rangeRadio :: (Enum a, Show a, Eq a) => (a, a) -> Gadget' e w a
rangeRadio (x1, x2) = simpleRadio [x1 .. x2]


-- | Radio buttons for the whole of an enumerated type. See warning for 'boundedCombo'.
boundedRadio :: (Bounded a, Enum a, Show a, Eq a) => Gadget' e w a
boundedRadio = rangeRadio (minBound, maxBound)

-- | Boolean tick-box. In combination with "optional" this will give a three-item menu.
tickBox :: Gadget' e w Bool
tickBox = TickBox


-- | Select a date from a calendar, or type it in.
dateBox :: DateFormat -> Gadget' e w Day
dateBox = DateBox


-- | Select an icon. The argument is a predicate for the icon context name.
iconBox :: (Text -> Bool) -> Gadget' e w IconName
iconBox = IconBox


-- | Select a colour.
colourBox :: Gadget' e w Colour
colourBox = ColourBox


-- | Displays the image from the ByteString.
imageDisplay :: MemoBoxSize -> Gadget' e w (Maybe ByteString)
imageDisplay = ImageDisplay


-- | Select items from a tree or a list. Only nodes with a @Just@ value may be picked and
-- branches with no valid children will be trimmed. The @a@ values in the forest must all
-- be distinct. The texts are the node name and optional node tooltip.
--
-- For the moment the forest is computed only on widget creation and is not updated when the
-- environment changes.
treeSelector :: (Ord a) => (e -> Forest (Text, Maybe Text, Maybe a)) -> Gadget' e w (Set a)
treeSelector = TreeSelector


-- | Present a list of items in a table. If the "DialogSelector" is present then activating
-- a row will bring up a sub-dialog. This comes with its own scroll box.
--
-- The contents of the table are treated as a single value, with everything updated on each change.
-- Hence this is only suitable for small lists.
table :: (Eq a) => [TableEditing a] -> Table a -> Maybe (DialogSelector e w a a) -> Gadget' e w [a]
table = Table


-- | Present a forest of data items. Each level in the tree is edited through the gadgets in
-- the corresponding column group. When a value changes the gadget emits the corresponding key
-- and update function. The GTK Grid widget is given the class \"forest-table\".
forestTable ::
   [(Text, [(Text, GadgetF' e w a)])]
      -- ^ Groups of columns. The name of the first group is ignored. Each column has a heading
      -- and a gadget template.
   -> (e -> Forest (k, Lens1 s a))
      -- ^ The data to edit is represented as a forest of lenses, each accessing some underlying
      -- data type. This is computed on widget creation and is not updated when the env changes.
   -> GadgetF' e (Either w (k, a -> a)) s
forestTable = ForestTable


-- | Edit a forest of data items using a tree view. Items can be dragged and dropped to reorgainse
-- them, or clicked for individual editing via the menu and dialog selector.
forestEditor :: (Eq a) =>
   (e -> a -> Text)   --  Text label for a tree item.
   -> (Maybe a -> Menu (TreeOper a))  -- Edit menu for tree item.
   -> (a -> Bool)               -- Can this item have children?
   -> Maybe (DialogSelector e w a a)  -- Activation pops up an edit dialog.
   -> Gadget' e w (Forest a)
forestEditor = ForestEditor


-- | A file selector, with a text box that pops up a native file selector dialog when clicked.
filePathSelector ::
   Text   -- ^ Title for the selector dialog.
   -> FilePathPurpose
   -> [FilePathFilter]
   -> Bool   -- ^ Display an overwrite confirmation dialog when an existing file is selected.
   -> Gadget' e w FilePath
filePathSelector = FilePathSelector

-- | Text box with read-only contents. Clicking the text will pop up a dialog for the contents.
textPopup :: (e -> a -> Text) -> DialogSelector e w a a -> Gadget' e w a
textPopup f sel = FixedText f $ Just sel


-- | Buttons that modify the data in some way. This passes the input to the output unchanged
-- unless a button is pressed, in which case the output is the relevant function applied to the
-- input.
buttonBar :: [(Text, a -> a)] -> GadgetF' e w a
buttonBar = ButtonBar


-- | Perform an IO action when the button is clicked. If the action fails then a message box
-- will be displayed with an internal error and @Nothing@ will be returned. The output is @Nothinng@
-- until the button is pressed.
buttonIO :: Text -> (e -> i -> IO o) -> Gadget e w i (Maybe o)
buttonIO = ButtonIO


-- | Display a fixed image. Returns click coordinates.
image :: FilePath -> Gadget' e w (Int, Int)
image = Image


-- | Read-only text field.
readOnlyText :: (e -> a -> Text) -> Gadget' e w a
readOnlyText f = FixedText f Nothing


-- | Multi-line text. Clicking it will pop up a dialog for the contents.
memoPopup :: MemoBoxSize -> Bool -> (e -> a -> Text) -> DialogSelector e w a a -> Gadget' e w a
memoPopup size expand f sel = FixedMemo size expand f $ Just sel


-- | Memo box with read-only contents.
readOnlyMemo :: MemoBoxSize -> Bool -> (e -> a -> Text) -> Gadget' e w a
readOnlyMemo size expand f = FixedMemo size expand f Nothing


-- | True if the string is a member of the standard icon contexts. This is an embarassing bit of
-- GTK-specific code which is needed to implement application-specific icon dialogs.
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
      "Legacy",
      "MimeTypes",
      "Places",
      "Status",
      "Stock",
      "UI"
   ]


-- | Print the input and output values to StdErr.
traceGadget :: (Show i, Show o) => String -> Gadget e w i o -> Gadget e w i o
traceGadget str = Trace str show (const show)


-- | Print the input and output to StdErr, with the output function applied to the latest input.
traceGadgetOver :: (Show i, Show o) => String -> GadgetF e w i o -> GadgetF e w i o
traceGadgetOver str = Trace str show (\i f -> show $ f i)


-- | Generalised tracing: provide your own functions to display the input and output.
traceGadget1 ::  String -> (i -> String) -> (i -> o -> String) -> Gadget e w i o -> Gadget e w i o
traceGadget1 = Trace
