{-# LANGUAGE ScopedTypeVariables #-}
{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}



{- |
-}

module Model.Reflection.Dialogs (
   -- * Editing Types
   fieldDialog,
   extensionDialog,
   referenceTypeDialog,
   subsetDialog,
   subsetGadget,
   showSubset,
   expressionEntry,
   -- * Relationship Types
   referenceListDialog,
   -- * Editing Data
   fieldElement,
   fieldElement1,
   addFieldDecoration
) where

import Control.Arrow
import Control.Category
import Control.Lens
import Control.Monad
import Data.Char
import Data.Default
import Data.Default.Orphan ()
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as U
import Model.Reflection.NamedRelation (Relation)
import Model.Reflection.Parser
import Model.Reflection.References
import Model.Reflection.Reflective
import Model.Reflection.Types
import Model.Reflection.Values
import Network.URI
import Prelude hiding (id, (.))
import Reactive.Banana.ArrowDialog
import qualified Reactive.Banana.ArrowDialog as AD
import Reactive.Banana.Common
import Reactive.Banana.Table
import Text.Megaparsec


-- | Create a gadget to edit values described by a field. If the first argument is @True@ then
-- Note boxes will expand vertically to fill the available space.
fieldElement :: Bool -> Field -> Gadget' e w ExtValue
fieldElement vExpand (Field ro _ _ multi t) = case t of
      (BuiltInDef m) -> basicGadget ro m
      DecoBoolDef (DecoBool icons colours) ->
         maybe id (coloured . bool) colours $
         maybe id (icon . (fromMaybe "blank-icon" .) . bool) icons $
         multiMod _ExtBool tickBox
      DecoBoolDef DecoRange {} ->  -- Can't happen, but type checker can't figure that out.
         multiMod _ExtBool tickBox
      DecoIntDef (DecoRange r rm) ->
         coloured (snd <=< range _ExtInt rm) $
         (if rmapIcons rm
            then icon (fromMaybe "blank-icon" . (fst <=< range _ExtInt rm))
            else id) $
         multiMod _ExtInt $ textBox (const $ textPrism . rangePrism r)
      DecoRealDef (DecoRange r rm) ->
         coloured (snd <=< range _ExtReal rm) $
         (if rmapIcons rm
            then icon (fromMaybe "blank-icon" . (fst <=< range _ExtReal rm))
            else id) $
         multiMod _ExtReal $ textBox (const $ textPrism . rangePrism r)
      EnumDef enum1 ->
         multiMod id $
         comboBox $ const $ map (\(EnumItem nm i c) -> ComboItem nm i c (ExtText nm)) enum1
   where
      bool :: (Show a) => (a, a) -> ExtValue -> Maybe a
      bool p (ExtBool True) = Just $ snd p
      bool p (ExtBool False) = Just $ fst p
      bool _ _ = Nothing
      range :: (Ord a) => Prism' ExtValue a -> RangeMap a b -> ExtValue -> Maybe b
      range prsm rm = preview $ prsm . to (rangeMap rm) . _Just
      rmapIcons = any $ isJust . fst . snd   -- Does the RangeMap have any icons in it?
      multiMod :: (Default a) => Prism' ExtValue a -> Gadget' e w a -> Gadget' e w ExtValue
      multiMod prsm = case multi of
            Optional -> prismatic def (optionalExtension . below prsm) . AD.optional def
            One -> prismatic def prsm
      basicGadget :: Bool -> BuiltInType -> Gadget' e w ExtValue
      basicGadget _ ModelBool = multiMod _ExtBool tickBox
      basicGadget False ModelInt = multiMod _ExtInt typedTextBox
      basicGadget True  ModelInt = multiMod _ExtInt $ readOnlyText $ const $ T.pack . show
      basicGadget False ModelReal = multiMod _ExtReal typedTextBox
      basicGadget True  ModelReal = multiMod _ExtReal $ readOnlyText $ const $ T.pack . show
      basicGadget False ModelText = multiMod _ExtText simpleTextBox
      basicGadget True  ModelText = multiMod _ExtText $ styled1 "table-cell" $ message $ const id
      basicGadget False ModelURL = multiMod _ExtText $ textBox $ const urlPrism
      basicGadget True  ModelURL = multiMod _ExtText $ styled1 "table-cell" $ message $ const id
      basicGadget False ModelNote = multiMod _ExtText $ memoBox MemoSmall vExpand
      basicGadget True  ModelNote = multiMod _ExtText $ styled1 "table-cell" $ message $ const id
      basicGadget False ModelDate = multiMod _ExtDate $ dateBox longDate
      basicGadget True  ModelDate =
         multiMod _ExtDate $ styled1 "table-cell" $ message $ const $ T.pack . show
      -- Prism which allows a valid URL to pass but not any other text.
      urlPrism :: Prism' Text Text
      urlPrism = prism id $ \str -> if isURIReference $ T.unpack str then Right str else Left str
      optionalExtension :: Iso' ExtValue (Maybe ExtValue)
      optionalExtension = iso forward $ fromMaybe ExtNone
         where
            forward ExtNone = Nothing
            forward x = Just x


-- | Add icons and colours to the element suitable for boolean values.
addDecoBool :: Maybe (IconName, IconName) -> Maybe (Colour, Colour)
   -> Gadget e w ExtValue b -> Gadget e w ExtValue b
addDecoBool icons colours =
   let
      iDeco = case icons of
         Nothing -> id
         Just (falseI, trueI) -> icon $ \v ->
            case v ^? _ExtBool of
               Just True -> trueI
               Just False -> falseI
               Nothing -> "blank-icon"
      cDeco = case colours of
         Nothing -> id
         Just (falseC, trueC) -> coloured $ \v ->
            case v ^?  _ExtBool of
               Just True -> Just trueC
               Just False -> Just falseC
               Nothing -> Nothing
   in iDeco . cDeco


-- | Add icons and colours to the element suitable for ordered values.
addDecoRange :: (Ord a) =>
   RangeMap a (Maybe IconName, Maybe Colour)
   -> Prism' ExtValue a
   -> Gadget e w ExtValue b
   -> Gadget e w ExtValue b
addDecoRange m prsm =
   let
      iDeco = if any (isJust . fst . snd) m
         then icon $ \v -> fromMaybe "blank-icon" $ join $ do
            v1 <- v ^? prsm
            fst <$> rangeMap m v1
         else id
      cDeco = if any (isJust . snd . snd) m
         then coloured $ \v -> join $ do
            v1 <- v ^? prsm
            snd <$> rangeMap m v1
         else id
   in iDeco . cDeco


-- | Add icons and colours to the element from an enumeration type.
addDecoEnum :: EnumType -> Gadget e w ExtValue b -> Gadget e w ExtValue b
addDecoEnum typ =
   let
      tbl = M.fromList $ map (\i -> (i ^. enumItemName, i)) typ
      iDeco = if any (isJust . preview enumItemIcon) typ
         then icon $ \v ->
               fromMaybe "blank-icon" $ tbl ^? ix (displayValue v) . enumItemIcon . _Just
         else id
      cDeco = if any (isJust . preview enumItemColour) typ
         then coloured $ \v -> tbl ^? ix (displayValue v) . enumItemColour . _Just
         else id
   in iDeco . cDeco


-- | Add icons and colours as specified by a type to any dialog element.
addFieldDecoration :: TypeDef -> Gadget' e w ExtValue -> Gadget' e w ExtValue
addFieldDecoration (DecoBoolDef (DecoBool icons colours)) = addDecoBool icons colours
addFieldDecoration (DecoIntDef (DecoRange _ m)) = addDecoRange m _ExtInt
addFieldDecoration (DecoRealDef (DecoRange _ m)) = addDecoRange m _ExtReal
addFieldDecoration (EnumDef typ) = addDecoEnum typ
addFieldDecoration _ = id


-- | Like "fieldElement", but promoted to work on a table of "ExtensionValues".
fieldElement1 :: Bool -> Field -> (Text, GadgetF' e w ExtensionValues)
fieldElement1 vExpand field = (
      field ^. fieldName,
      focusing (extValue $ fieldId field) $ fieldElement vExpand field
   )


-- | Dialog for field names that can be used in variants.
--
-- New fields require unique IDs. This is a pure function so it can't provide unique IDs. Instead
-- new fields are given a FieldId of @nil@. This must be replaced with a fresh UUID by the caller.
fieldDialog :: Dialog' e w [Field]
fieldDialog = Dialog "Fields" OkApplyButton $
      simpleFrame "Fields" $
      prismatic def extraIso $
      validateText validFields $
      table [TableDelete] fieldColumns (Just fieldDialogSelector)
   where
      validFields fs =
         let
            nms = map (view fieldName) $ fs ++ M.elems builtInFields
         in listToMaybe $ ["Names must not be empty." | any T.null nms]
            ++ map (("Duplicate name: " <>) . head) (filter ((> 1) . length) $ group $ sort nms)
      fieldColumns = [
            mkField "Name" (fieldName . strips) Nothing (EditEntry id) Nothing,
            mkField "Multiplicity" fieldMulti Nothing (enumField [One, Optional]) Nothing,
            mkField "Type" fieldType Nothing (EditFixed typeDescription) Nothing
         ]
      blankName = " -?- "
      blankEntry = Field False U.nil blankName One $ BuiltInDef ModelText
      -- Add and remove the blank entry.
      extraIso :: Iso' [Field] [Field]
      extraIso = iso (++ [blankEntry]) (filter ((T.strip blankName /=) . T.strip . view fieldName))
      -- Pseudo-ISO to strip leading or trailing spaces.
      strips :: Iso' FieldName FieldName
      strips = iso T.strip T.strip


-- | Edit a field.
fieldDialogSelector :: DialogSelector' e w Field
fieldDialogSelector = constantDialog $ Dialog "Edit data field" OkApplyButton $
      accum $ focusing fieldType $ unionTab [
            ("Built-in", PrismaticGadget def _BuiltInDef builtInDialog),
            ("Deco\nBool", PrismaticGadget def _DecoBoolDef decoBoolDialog),
            ("Deco\nInt", PrismaticGadget def _DecoIntDef decoRangeDialog),
            ("Deco\nReal", PrismaticGadget def _DecoRealDef decoRangeDialog),
            ("Enumeration", PrismaticGadget def _EnumDef enumTypeDialog)
         ]


-- | Dialog for extensions to a single variant.
extensionDialog :: FieldTable -> Variant a -> Dialog' e w [FieldId]
extensionDialog fieldTable (Variant variant) =
   Dialog variant OkApplyButton $
      accum $
      focusingOver (fieldNameIso fieldTable) $
      validateTextOver validExtensions $
      frame (const $ Just "Extension Fields") $
      focusing extraIso $
      table [TableDelete, TableShuffle] extensionTable Nothing
   where
      validExtensions :: [FieldName] -> Maybe Text
      validExtensions fs = listToMaybe $
            ["Names must not be empty." | any T.null fs] ++
            map (("Duplicate field: " <>) . head)
               (filter ((> 1) . length) $ group $ sort fs) ++
            map ("Unknown field: " <> )
               (filter (\f1 -> not $ S.member f1 nameSet) fs)
      nameSet = S.fromList $ map (view fieldName) $ M.elems fieldTable
      extensionTable :: Table FieldName
      extensionTable = [mkField "Field Name" id Nothing (EditCombo fieldList) Nothing]
      fieldList = M.keys fieldTable2
      blankText = " -?- "
      extraIso :: Iso' [FieldName] [FieldName]
      extraIso = iso (++ [blankText]) (filter $ (/= T.strip blankText) . T.strip)
      fieldTable2 :: Map FieldName Field
      fieldTable2 = fieldsByName fieldTable


-- | Dialog for selecting a subset from a universe.
subsetDialog :: (Ord a) => Text -> (a -> Text) -> Set a -> Dialog' e w (Set a)
subsetDialog title valName = Dialog title OkApplyButton . subsetGadget valName


-- | Subdialog for selecting a subset from a universe.
subsetGadget :: (Ord a) => (a -> Text) -> Set a -> Gadget' e w (Set a)
subsetGadget valName universe1 = accum $
      accum (box Horizontal [[
            form Vertical $ map checkBox uni1,
            form Vertical $ map checkBox uni2
         ]]) >>>
      buttonBar [("Set all", const universe1), ("Clear all", const mempty)]
   where
      (uni1, uni2) = splitAt ((S.size universe1 + 1) `div` 2) $ sortOn valName $ S.toList universe1
      checkBox x = (valName x, focusing (contains x) tickBox)


-- | Describe a subset of a universe as either the set of elements or the set of non-elements.
-- depending on which is shorter.
showSubset :: (Ord a) => (a -> Text) -> Set a -> Set a -> Text
showSubset valName universe1 xs
   | S.null xs =
      "<Nothing>"
   | xs == universe1 =
      "<Everything>"
   | S.size xs < (3 + S.size universe1 `div` 2) =
      T.intercalate ", " $ sort $ map valName $ S.toList xs
   | otherwise =
      "All except " <> T.intercalate ", " (sort $ map valName $ S.toList $ universe1 S.\\ xs)


-- | A sub-dialog component for entering expressions. As the expression is typed it is checked for
-- errors.
expressionEntry ::
   FunctionTable e
   -> FieldTable   -- ^ Known value identifiers.
   -> Gadget' e w Text
expressionEntry ftable knownFields =
      validateText check $ memoBox MemoSmall True
   where
      check v = case parse (topExpr ftable knownFields) "" v of
         Left bundle -> Just $ T.pack $ parseErrorTextPretty $ NE.head $ bundleErrors bundle
         Right _ -> Nothing


-- | Sub-dialog for selecting one of the built-in types.
builtInDialog :: Gadget' e w BuiltInType
builtInDialog = accum $ form Vertical [("Built-in Type:", focusing id builtInMenu)]
   where
      builtInMenu =
         simpleCombo [ModelBool, ModelInt, ModelReal, ModelText, ModelURL, ModelNote, ModelDate]


-- Sub-dialog for designing decorated booleans.
decoBoolDialog :: Gadget' e w (DecoType Bool)
decoBoolDialog =
      arr (\(DecoBool i c) -> (i, c)) >>> (icons *** colours) >>> arr (uncurry DecoBool)
   where
      icons = accum $ simpleFrame "Icons" $ optionalOver def $ form Vertical [
               ("Icon for \"False\":", focusing _1 $
               validate (not . T.null) $ iconBox $ not . isStandardContext),
               ("Icon for \"True\":",  focusing _2 $
               validate (not . T.null) $ iconBox $ not . isStandardContext)
            ]
      colours = accum $ simpleFrame "Colours" $ optionalOver def $ form Vertical [
            ("Colour for \"False\":", focusing _1 colourBox),
            ("Colour for \"True\":",  focusing _2 colourBox)
         ]


-- | Sub-dialog for designing decorated ranges.
decoRangeDialog :: (Read a, Show a, Ord a) => Gadget e w (DecoType a) (DecoType a)
decoRangeDialog =
      arr splitRange >>> (editRange *** editMap) >>> arr (uncurry DecoRange)
   where
      splitRange (DecoRange r m) = (r,m)
      splitRange _ = ((Nothing, Nothing), [])
      editRange =
         validate validRange $
         accum $ form Vertical [
               ("Lower bound:", focusing _1 maybeTextBox),
               ("Upper bound:", focusing _2 maybeTextBox)
            ]
      editMap =
         frame (pure $ Just "Subrange Attributes (first match wins)") $
         validate (all (validRange . fst)) $
         accum $ focusing extraIso $
         table [TableDelete, TableShuffle] subrangeTable Nothing
      subrangeTable = [
            mkField "Lower" (_1 . _1) Nothing (Opt (EditEntry textPrism)) Nothing,
            mkField "Upper" (_1 . _2) Nothing (Opt (EditEntry textPrism)) Nothing,
            mkField "Icon" (_2 . _1) Nothing (Opt (EditIcon $ not . isStandardContext)) Nothing,
            mkField "Colour" (_2 . _2) Nothing (Opt EditColour) Nothing
         ]
      blankSubrange = ((Nothing, Nothing), (Nothing, Nothing))
      extraIso :: (Eq a) => Iso'
            (RangeMap a (Maybe IconName, Maybe Colour))
            (RangeMap a (Maybe IconName, Maybe Colour))
      extraIso = iso (++ [blankSubrange]) (filter (/= blankSubrange))


-- | Table definition for enumerated items.
enumValuesTable :: Table EnumItem
enumValuesTable = [
      mkField "Item" enumItemName Nothing (EditEntry id) Nothing,
      mkField "Icon" enumItemIcon Nothing (Opt (EditIcon $ not . isStandardContext)) Nothing,
      mkField "Colour" enumItemColour Nothing (Opt EditColour) Nothing
   ]


-- | Dialog definition for enumerated types. The first part of the tuple is the type name.
enumTypeDialog :: Gadget e w EnumType EnumType
enumTypeDialog =
      frame (pure $ Just "Enumerated Values") $ accum $ focusing extraIso $
      table [TableDelete, TableShuffle] enumValuesTable Nothing
   where
      extraIso :: Iso' [EnumItem] [EnumItem]
      extraIso = iso
         (++ [blankEnumItem])
         (filter ((/= T.strip blankName) . T.strip . view enumItemName))
      blankName = " -?- "
      blankEnumItem :: EnumItem
      blankEnumItem = EnumItem blankName Nothing Nothing


-- | Dialog showing a list of extension reference types and enabling editing.
referenceListDialog :: (Reflective a) => Dialog' e w [(Relation, RefType a)]
referenceListDialog =
   Dialog "Reference Types" OkApplyButton $ accum $
      simpleFrame "Reference Names" $ focusing (extraIso blankRef) refTable
   where
      refTable =
         withEnv (arr namesTaken) $
            table
               [TableDelete]
               [mkField "Reference type name" _1 Nothing (EditFixed id) Nothing]
               $ Just $ constantDialog referenceTypeDialog
      blankRef = (blankRefName, RefType [] (False, False))
      extraIso :: (Relation, b) -> Iso' [(Relation, b)] [(Relation, b)]
      extraIso r = iso (++ [r]) (filter ((/= (r ^. _1 . to T.strip)) . T.strip . view _1))
      namesTaken xs =
         S.fromList $
         map fst $
         refTypeTableToList reflectiveBuiltInRefs ++ xs   -- Include built-in reference type names.


-- | Placeholder name for potential new reference types in the table.
blankRefName :: Text
blankRefName = " -?- "


-- | Dialog for editing a single reference type.
referenceTypeDialog :: (Reflective a) => Dialog' (Set FieldName) w (FieldName, RefType a)
referenceTypeDialog = Dialog "Reference Type" OkApplyButton $ accum $
      validateTextOver typeIsValid $ box Vertical [[
            form Vertical [("Name:", focusing _1 $
                  validateText1 nameIsValid $
                  arr (T.dropAround isSpace) <<< simpleTextBox <<< arr hideBlank)],
            form Horizontal [
                  ("Side A multiplicity:", focusing (_2 . refTypeSingles . _1) singleMenu),
                  ("Side B multiplicity:", focusing (_2 . refTypeSingles . _2) singleMenu)
               ],
            simpleFrame "Entity Types" $ focusing (_2 . refTypeSets . extraType) typeTable
         ]]
   where
      hideBlank str
         | T.strip str == T.strip blankRefName  = ""
         | otherwise = str
      singleMenu :: Gadget' e w Bool
      singleMenu = comboBox $ const [
            ComboItem "Many" Nothing Nothing False,
            ComboItem "One" Nothing Nothing True
         ]
      typeTable :: (Reflective a) => Gadget' e w [(Set (Variant a), Set (Variant a))]
      typeTable = table
            [TableDelete]
            [
               mkField "Side A" _1 Nothing (EditFixed showSet) Nothing,
               mkField "Side B" _2 Nothing (EditFixed showSet) Nothing
            ]
            $ Just $ constantDialog sidesDialog
      extraType :: (Ord b) => Iso' [(Set b, Set b)] [(Set b, Set b)]
      extraType = iso (++ [(mempty, mempty)]) (filter (\(s1, s2) -> not $ S.null s1 && S.null s2))
      allVariants :: (Reflective a) => Set (Variant a)
      allVariants = S.fromList $ map reflectiveName reflectiveDefaults
      showSet :: (Reflective a) => Set (Variant a) -> Text
      showSet = showSubset variantUserLabel allVariants
      sidesDialog :: (Reflective a) => Dialog' e w (Set (Variant a), Set (Variant a))
      sidesDialog = Dialog "Edit two sides of the relationship" OkApplyButton $ accum $
         box Horizontal [
               [simpleFrame "Side A can contain:" $ focusing _1 $
                  subsetGadget variantUserLabel allVariants],
               [simpleFrame "Side B can contain:" $ focusing _2 $
                  subsetGadget variantUserLabel allVariants]
            ]
      nameIsValid _ _ newName
         | T.null newName  = Just "Enter a name."
         | otherwise  = Nothing
      typeIsValid typ
         | isManyOne typ  = msg <$> bothSides typ
         | otherwise      = Nothing
         where
            msg (Variant nm) = nm <> " appears on both sides of a one-many type."
      bothSides typ = listToMaybe $ S.toList $ S.intersection sideA sideB
         where
            sideA = foldr (S.union . fst) S.empty $ typ ^. _2 . refTypeSets
            sideB = foldr (S.union . snd) S.empty $ typ ^. _2 . refTypeSets
      isManyOne typ = let (a, b) = typ ^. _2 . refTypeSingles in a /= b
