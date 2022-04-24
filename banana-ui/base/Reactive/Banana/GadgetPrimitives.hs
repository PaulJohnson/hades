{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE Rank2Types #-}

{-
Copyright © Paul Johnson 2019. See LICENSE file for details.
-}

{- |

Primitive language for gadget-based GUIs. This exports all of the constructors for "Gadget".
These are normally used only by combinator libraries and interpreters.
-}

module Reactive.Banana.GadgetPrimitives where

import Control.Arrow
import Control.Category
import Control.Lens
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text, unpack)
import Data.Time.Calendar
import Data.Tree
import Prelude hiding (id, (.))
import Reactive.Banana.Common
import Reactive.Banana.Menu
import Reactive.Banana.Table


-- | Items in a combobox menu.
data ComboItem a = ComboItem {
      menuItemLabel :: Text,
      menuItemIcon :: Maybe Text,
      menuItemColour :: Maybe Colour,
      menuItemValue :: a
   }

instance Functor ComboItem where
   fmap f (ComboItem l i c v) = ComboItem l i c $ f v


-- | Items in a clickable memo box. Single click values are best used for selection in pop-up
-- dialolgs, with double-click used for immediate activation elsewhere in the application. If both
-- are provided then a double click will emit both in undefined order.
data ClickableItem a = ClickableItem {
      clickText :: Text,
      clickSingle :: Maybe a,
      clickDouble :: Maybe a,
      clickMenu :: Maybe (Menu a)  -- ^ Shown on right click.
   }


-- | Composite gadget orientation.
data Orientation = Vertical | Horizontal deriving (Eq, Show)


-- | Hint about how much text is expected in the memobox.
data MemoBoxSize =
   MemoSmall   -- ^ A small amounts of text, such as a sentence or two.
   | MemoMedium   -- ^ several sentences or a couple of short paragraphs.
   | MemoLarge  -- ^ Extended pieces of text.
   deriving (Eq, Ord, Show, Read)


-- | Purpose for a FilePath gadget.
data FilePathPurpose =
   FilePathOpen
   | FilePathSave
   | FilePathFolder
   deriving (Show, Eq)


-- | Filter rules for a FilePath gadget.
data FilePathFilterItem =
   FilePathGlob Text   -- ^ Glob pattern (e.g. @*.txt@).
   | FilePathMime Text   -- ^ Mime type.
   | FilePathImages     -- ^ Image files.
   deriving (Show, Eq)


type FilePathFilter = (Text, [FilePathFilterItem])


-- | Tree operations for forest editing. Elements can be added or deleted.
-- The terms *before*, *after* and *in* refer to the item right clicked by the user.
data TreeOper a = TreeAddBefore a | TreeAddAfter a | TreeAddIn a  | TreeDelete


-- | Gadget where input and output are the same type.
type Gadget' e w a = Gadget e w a a


-- | Gadget where the output is a function.
type GadgetF e w i o = Gadget e w i (i -> o)


-- | Gadget where the ouptut is an endofunction.
type GadgetF' e w a = Gadget e w a (a -> a)


-- | Pair of prism and gadget used for conditionals.
data PrismaticGadget e w i o =
   forall a b . PrismaticGadget a (Prism i o a b) (Gadget e w a b)


-- | Lens wrapped in a newtype to avoid impredicative polymorphism.
newtype Lens1 s a = Lens1 {getLens :: Lens' s a}


-- | The type parameters of a Gadget are:
--
-- [@e@] The read-only environment for the gadget.
--
-- [@w@] The write-only side channel for data events.
--
-- [@i@] The input type.
--
-- [@o@] The output type.
data Gadget e w i o where
   Null :: Gadget' e w a
   -- | A pure function from input to output. The implementation of "arr" in the "Arrow" instance.
   Pure :: (i -> o) -> Gadget e w i o
   -- | Gadget composition. The implementation of "(.)" in the "Arrow" instance
   Dot :: Gadget e w b c -> Gadget e w a b -> Gadget e w a c
   -- | Gadget output as a tuple. The output changes whenever either gadget changes output.
   -- The implementation of "(****)" in the "Arrow" instance.
   Prod :: Gadget e w i1 o1 -> Gadget e w i2 o2 -> Gadget e w (i1, i2) (o1, o2)
   -- | Gadgets with feedback.
   Loop :: Gadget e w (i, s) (o, s) -> Gadget e w i o
   -- | Accumulate edits.
   Accum :: GadgetF' e w a -> Gadget' e w a
   -- | Get the initial input.
   GetInitial :: (i -> Gadget e w i o) -> Gadget e w i o
   -- | Get the initial environment.
   GetInitialEnv :: (e -> Gadget e w i o) -> Gadget e w i o
   -- | A fixed initial value, regardless of the form inputs. After that it emits its input.
   Initially :: a -> Gadget' e w a
   -- | Get access to the environment.
   GetEnv :: Gadget e w i e
   -- | Create a subsiduary environment.
   WithEnv :: Gadget e w i e' -> Gadget e' w i o -> Gadget e w i o
   -- | A gadget that writes event data to the side channel.
   Send :: (a -> Maybe w) -> Gadget' e w a
   -- | Modify the data sent on the side channel.
   SendMap :: (w1 -> Maybe w2) -> Gadget e w1 i o -> Gadget e w2 i o
   -- | A gadget that views its inputs and outputs through a lens.
   Focusing :: Lens i o a b -> Gadget e w a b -> GadgetF e w i o
   -- | A gadget that views its inputs and outputs through a "Prism". Typically used for
   -- a dialog element that applies to a single variant of a union type.
   Prismatic :: a -> Prism s t a b -> Gadget e w a b -> Gadget e w s t
   -- | As for "Prismatic", but for when the inner Gadget already ouptuts a function.
   PrismaticOver :: a -> Prism' s a -> GadgetF' e w a -> GadgetF' e w s
   -- | A gadget that views its inputs and outputs through a "Traversal". Typically used
   -- for a dialog element that accesses one part of a larger structure.
   -- Note that traversals cannot change the shape of the input, so @Traversing _Left@
   -- will ignore any @Right@ values rather than changing them.
   Traversing :: a -> Traversal i o a b -> Gadget e w a b -> GadgetF e w i o
   -- | As for "Traversing", but for when the inner Gadget already outputs a function.
   TraversingOver :: a -> Traversal i o a b -> GadgetF e w a b -> GadgetF e w i o
   -- -- | Accumulate edits.
   -- Accum :: GadgetF' e w a -> Gadget' e w a
   -- | Dynamic gadgets. A new gadget is computed when the derived @a@ value changes, but this
   -- means that any state in the old one is lost.
   Exec :: (Eq a) => (a -> Gadget e w i o) -> Gadget e w (a, i) o
   -- | Select the first matching predicate from the list and display the associated gadget.
   -- Unlike "Exec" the gadgets are persistent and receive all inputs, even when not displayed. The
   -- first argument is the default when none of the predicates are true, so the argument must be
   -- non-empty.
   Cond :: [PrismaticGadget e w i o] -> Gadget e w i o
   -- | Multiple tabs, each of which has a dialog for one variant. The argument must be non-empty.
   UnionTab :: [(Text, PrismaticGadget e w i o)] -> Gadget e w i o
   -- | The gadget is only enabled in the GUI when the @Bool@ input is true.
   Enabled :: Gadget e w i o -> Gadget e w (i, Bool) o
   -- | Optional values. Most arguments are decorated with a tick-box to enable them, but some have
   -- special cases. E.g @Optional TextBox@ equates blank to @Nothing@.
   Optional :: i -> Gadget e w i o -> Gadget e w (Maybe i) (Maybe o)
   -- | Intercept single or double clicks (first argument @True@ for double clicks) and open a
   -- pop-up dialog based on the content of the underlying gadget. The dialog result is used
   -- as the new output and input for the underlying gadget.
   Intercept :: Bool -> DialogSelector' e w a -> Gadget' e w a -> Gadget' e w a
   -- | Decorate with an icon. The function should return an icon name or \"blank-icon\" for blank.
   Icon :: (i -> Text) -> Gadget e w i o -> Gadget e w i o
   -- | Decorate with a background colour. The foreground will be either white or black
   -- for contrast.
   Coloured :: (i -> Maybe Colour) -> Gadget e w i o -> Gadget e w i o
   -- | Display a hyperlink button with the gadget.
   Linked :: (i -> Maybe Text) -> Gadget e w i o -> Gadget e w i o
   -- | With the named style. What this means depends on the back end.
   Styled :: (i -> Maybe Text) -> Gadget e w i o -> Gadget e w i o
   -- | Frame drawn around the argument, optionally with a text label.
   Frame :: (i -> Maybe Text) -> Gadget e w i o -> Gadget e w i o
   -- | Gadgets aligned vertically or horizontally with labels.
   Form :: Orientation -> [(Text, GadgetF' e w a)] -> GadgetF' e w a
   -- | Similar to form, except that elements are presented in tabs.
   TabForm :: [(Text, GadgetF' e w a)] -> GadgetF' e w a
   -- | Child gadgets are grouped together, either horizontally or vertically. Sublists have
   -- separator lines between them.
   Box :: Orientation -> [[GadgetF' e w a]] -> GadgetF' e w a
   -- | Elements are arranged in a grid. The inner lists are of the rows. Names are ignored.
   -- The frirst two arguments are the column headers and the row headers respectively.
   Grid :: [Text] -> [Text] -> [[GadgetF' e w a]] -> GadgetF' e w a
   -- | Validate the output against a predicate, signalling an error and blocking the output
   -- if it is not met.
   Validate :: (e -> i -> o -> Bool) -> Gadget e w i o -> Gadget e w i o
   -- | Validate the output against a predicate which may return an error message.
   ValidateText :: (e -> i -> o -> Maybe Text) -> Gadget e w i o -> Gadget e w i o
   -- | Text input. The prism is used in reverse to validate the input.
   TextBox :: (e -> Prism' Text a) -> Gadget' e w a
   -- | Text displayed as a label, but presents a text box for editing when clicked.
   DisplayText :: Gadget e w Text Text
   -- | Multi-line text input. If the @Bool@ is true the box can expand vertically
   -- to fill the available space.
   MemoBox :: MemoBoxSize -> Bool -> Gadget e w Text Text
   -- | Multi-line text displayed as a label, but presents a text box for editing when clicked.
   DisplayMemo :: Gadget e w Text Text
   -- | Menu selection box
   Combo :: (Eq a) => (e -> [ComboItem a]) -> Gadget' e w a
   -- | Group of radio buttons. At most one can be selected.
   Radio :: (Eq a) => (e -> [(Text, a)]) -> Gadget' e w a
   -- | Boolean tick-box.
   TickBox :: Gadget' e w Bool
   -- | Read-only message for the user.
   Message :: (e -> a -> Text) -> Gadget' e w a
   -- | Text box with read-only contents. If the optional selector is provided then clicking the
   -- message will pop up a dialog.
   FixedText :: (e -> a -> Text) -> Maybe (DialogSelector e w a a) -> Gadget' e w a
   -- | Memo box with read-only contents.
   FixedMemo ::
      MemoBoxSize -> Bool -> (e -> a -> Text) -> Maybe (DialogSelector e w a a) -> Gadget' e w a
   -- | A box containing a list of items separated by commas. Each item will emit the
   -- relevant value when clicked.
   ClickableList :: Gadget e w [ClickableItem a] (Maybe a)
   -- | Select a date from a calendar, or type it in.
   DateBox :: DateFormat -> Gadget' e w Day
   -- | Select an icon. The argument is a predicate for the icon context name.
   IconBox :: (Text -> Bool) -> Gadget' e w IconName
   -- | Select a colour.
   ColourBox :: Gadget' e w Colour
   -- | Show an image binary from the ByteString.
   ImageDisplay :: MemoBoxSize -> Gadget' e w (Maybe ByteString)
   -- | Select items from a tree or a list. Only nodes with a @Just@ value may be picked, and
   -- branches with no valid children will be trimmed. The @a@ values in the forest must all
   -- be distinct. The texts are the node name and optional node tooltip.
   TreeSelector :: (Ord a) => (e -> Forest (Text, Maybe Text, Maybe a)) -> Gadget' e w (Set a)
   -- | Present a list of items in a table. If the "DialogSelector" is present then activating
   -- a row will bring up a sub-dialog. This comes with its own scroll box.
   Table :: (Eq a) =>
      [TableEditing a] -> Table a -> Maybe (DialogSelector e w a a) -> Gadget' e w [a]
   -- | Present a tree of data from an underlying data type. When a node changes value send the
   -- key and new node pair as an event.
   ForestTable ::
      [(Text, [(Text, GadgetF' e w a)])]
      -> (e -> Forest (k, Lens1 s a))
      -> GadgetF' e (Either w (k, a -> a)) s
   -- | Edit a forest of items by clicking and dragging.
   ForestEditor :: (Eq a) =>
      (e -> a -> Text)   --  Text label for a tree item.
      -> (Maybe a -> Menu (TreeOper a))  -- Edit menu for tree item.
      -> (a -> Bool)               -- Can this item have children?
      -> Maybe (DialogSelector e w a a)  -- Activation pops up an edit dialog.
      -> Gadget' e w (Forest a)
   -- | A file selector. The @Bool@ is True for overwrite confirmation.
   FilePathSelector ::
      Text -> FilePathPurpose -> [FilePathFilter] -> Bool -> Gadget' e w FilePath
   -- | Puts scroll bars around its argument if necessary.
   Scrolled :: Gadget e w i o -> Gadget e w i o
   -- | Buttons that modify the inner gadget in some way.
   ButtonBar :: [(Text, a -> a)] -> GadgetF' e w a
   -- | Perform an IO action when the button is clicked.
   ButtonIO :: Text -> (e -> i -> IO o) -> Gadget e w i (Maybe o)
   -- | Display a fixed image. Returns click coordinates.
   Image :: FilePath -> Gadget' e w (Int, Int)
   -- | Print the input and output values to StdErr.
   Trace :: String -> (i -> String) -> (i -> o -> String) -> Gadget e w i o -> Gadget e w i o

instance Functor (Gadget e w i) where
   fmap f g = g >>> Pure f

instance Applicative (Gadget e w i) where
   pure v = arr $ const v
   f <*> v = f &&& v >>> arr (uncurry ($))

instance Category (Gadget e w) where
   id = Null
   (.) = Dot

instance Arrow (Gadget e w) where
   arr = Pure
   (***) = Prod

instance ArrowLoop (Gadget e w) where
   loop = Loop

-- | Pop-up dialogs can have different buttons at the bottom.
data DialogButtons =
   OkButton  -- ^ OK and Cancel buttons.
   | OkApplyButton  -- ^ OK, Apply and Cancel buttons.
   | CloseButton Text -- ^ A single button with the specified text. Use for read-only dialogs.
   deriving (Eq, Show)


-- | A dialog is a gadget that can be popped up in its own window with buttons.
data Dialog e w i o = Dialog {
      dialogTitle :: Text,
      dialogButtons :: DialogButtons,
      dialogGadget :: Gadget e w i o
   }


-- | Disable the Apply button in a Dialog, for instance if the following actions are not idempotent.
disableApply :: Dialog e w i o -> Dialog e w i o
disableApply d = case dialogButtons d of
   OkApplyButton -> d {dialogButtons = OkButton}
   _ -> d


-- | Dialog where the input and output are the same type.
type Dialog' e w a = Dialog e w a a


-- | Change the type of a dialog.
promoteDialog :: (Gadget e w i o -> Gadget e w i' o') -> Dialog e w i o -> Dialog e w i' o'
promoteDialog f (Dialog t b g) = Dialog t b $ f g


-- | A function from an item to the appropriate "Dialog" (if any) for that item.
--
-- Dialog selectors are used for sum types. Given types @Foo@ and @Bar@, with
-- corresponding gadgets @fooGadget :: Gadget' Foo@ and @barGadget :: Gadget' Bar@
-- you can define:
--
-- > type Foobar = Either Foo Bar
-- >
-- > foobarSelector :: DialogSelector Foobar
-- > foobarSelector (Left v) = const $ Just $ Dialog "Foo" OkButton $ prismatic _Left fooGadget
-- > foobarSelector (Right v) = const $ Just $ Dialog "Bar" OkButton $ prismatic _Right barGadget
--
-- This makes the environment available in two ways: one passed through the selector argument,
-- and the other via the gadget. The latter may change during the lifetime of the gadget, so
-- aspects of the gadget can be made static or dynamic as required.
type DialogSelector e w i o = e -> i -> Maybe (Dialog e w i o)

type DialogSelector' e w a = DialogSelector e w a a


-- | Disable the Apply button in a dialog selector.
disableApply1 :: DialogSelector e w i o -> DialogSelector e w i o
disableApply1 sel e v = disableApply <$> sel e v


-- | Change the type of a "DialogSelector".
promoteDialogSelector ::
   a -> Prism' s a -> DialogSelector' e w a -> DialogSelector' e w s
promoteDialogSelector d prsm sel e v =
   promoteDialog (Prismatic d prsm) <$> sel e (fromMaybe d $ v ^? prsm)


-- | A dialog selector which always produces the same dialog.
constantDialog :: Dialog e w i o -> DialogSelector e w i o
constantDialog = const . const . Just


-- | Render the gadget as a tree of text for debugging.
showGadget :: Gadget e w i o -> Tree String
showGadget Null = Node "Null" []
showGadget Pure {} = Node "Pure" []
showGadget (Dot g1 g2) = Node "Dot" [showGadget g2, showGadget g1]  -- Order is more intuitive
showGadget (Prod g1 g2) = Node "Prod" [showGadget g1, showGadget g2]
showGadget (Loop g) = Node "Loop" [showGadget g]
-- showGadget (Feedback g) = Node "Feedback" [showGadget g]
showGadget (Accum g) = Node "Accum" [showGadget g]
showGadget Initially {} = Node "Initially" []
showGadget GetInitial {} = Node "GetInitial" []
showGadget GetInitialEnv {} = Node "GetInitialEnv" []
showGadget GetEnv = Node "GetEnv" []
showGadget (WithEnv e g) = Node "WithEnv" [showGadget e, showGadget g]
showGadget (Send _) = Node "Send" []
showGadget (SendMap _ g) = Node "SendMap" [showGadget g]
showGadget (Focusing _ g) = Node "Focusing" [showGadget g]
showGadget (Prismatic _ _ g) = Node "Prismatic" [showGadget g]
showGadget (PrismaticOver _ _ g) = Node "PrismaticOver" [showGadget g]
showGadget (Traversing _ _ g) = Node "Traversing" [showGadget g]
showGadget (TraversingOver _ _ g) = Node "TraversingOver" [showGadget g]
-- showGadget (Accum g) = Node "Accum" [showGadget g]
showGadget Exec {} = Node "Exec ..." []
showGadget (Cond gs) = Node "Cond" $ map (\(PrismaticGadget _ _ g) -> showGadget g) gs
showGadget (UnionTab gs) = Node "UnionTab" $ map showItem gs
   where
      showItem (l, PrismaticGadget _ _ g) =
         let (Node txt cs) = showGadget g
         in Node (unpack l <> " " <> txt) cs
showGadget (Enabled g) = Node "Enabled" [showGadget g]
showGadget (Optional _ g) = Node "Optional" [showGadget g]
showGadget (Intercept b _ g) =
      Node ("Intercept " <> if b then "single-click " else "double-click ") [showGadget g]
showGadget (Icon _ g) = Node "Icon" [showGadget g]
showGadget (Coloured _ g) = Node "Coloured" [showGadget g]
showGadget (Linked _ g) = Node "Linked" [showGadget g]
showGadget (Styled _ g) = Node "Styled" [showGadget g]
showGadget (Frame _ g) = Node "Frame" [showGadget g]
showGadget (Form o gs) = Node ("Form " <> show o) $ map showItem gs
   where
      showItem (l, g) =
         let (Node txt cs) = showGadget g
         in Node (unpack l <> ": " <> txt) cs
showGadget (TabForm gs) = Node "TabForm" $ map showItem gs
   where
      showItem (l, g) =
         let (Node txt cs) = showGadget g
         in Node (unpack l <> ": " <> txt) cs
showGadget (Box o gss) = Node ("Box " <> show o) $ map showGadget $ concat gss
showGadget (Validate _ g) = Node "Validate" [showGadget g]
showGadget (ValidateText _ g) = Node "ValidateText" [showGadget g]
showGadget TextBox {} = Node "TextBox" []
showGadget DisplayText = Node "DisplayText" []
showGadget MemoBox {} = Node "MemoBox" []
showGadget DisplayMemo = Node "DisplayMemo" []
showGadget Combo {} = Node "Combo" []
showGadget Radio {} = Node "Radio" []
showGadget TickBox = Node "TickBox" []
showGadget Message {} = Node "Message" []
showGadget FixedText {} = Node "FixedText" []
showGadget FixedMemo {} = Node "FixeMemo" []
showGadget ClickableList = Node "ClickableList" []
showGadget (DateBox fmt) = Node ("DateBox " <> show fmt) []
showGadget IconBox {} = Node "IconBox" []
showGadget ColourBox = Node "ColourBox" []
showGadget (ImageDisplay sz) = Node ("ImageDisplay " <> show sz) []
showGadget TreeSelector {} = Node "TreeSelector ..." []
showGadget Table {} = Node "Table ..." []
showGadget (ForestTable groups _) =
      Node "ForestTable" $ concatMap (map showPair . snd) groups
   where
      showPair (nm, g) = let (Node txt cs) = showGadget g in Node (unpack nm <> ": " <> txt) cs
showGadget ForestEditor {} = Node "ForestEditor" []
showGadget (FilePathSelector t _ _ _) = Node ("FilePathSelector " ++ show t) []
showGadget (Grid colH rowH cells) = Node "Grid" $
      concat $ zipWith (\rh row -> zipWith (showCell rh) colH row) rowH cells
   where
      showCell rh ch cell =
         let (Node nm cs) = showGadget cell
         in Node ("(" <> unpack ch <> ", " <> unpack rh <>"): " <> nm) cs
showGadget Scrolled {} = Node "Scrolled" []
showGadget (ButtonBar bs) =
   Node ("Buttons " <> intercalate ", " (map (unpack . fst) bs)) []
showGadget ButtonIO {} = Node "ButtonIO" []
showGadget (Image path) = Node ("Image " <> path) []
showGadget (Trace str _ _ g) = Node ("Trace " <> str) [showGadget g]
