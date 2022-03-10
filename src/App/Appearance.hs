{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedLabels #-}
{-
Copyright © Paul Johnson 2020. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.
-}

{- |
Users control diagram appearance through a cascade of "Appearance" objects. These are attached
to diagrams, packages and components. An Appearance object attached to a package controls
all of the diagrams contained directly or indirectcly in the package, unless overridden by
an Appearance in a child package or a diagram.

@Appearance@ is a @Monoid@, so the @Appearance@ for a diagram is the @mconcat@ of the ancestral
@Appearance@ objects attached to the diagram and its containers going up to the root.

The root has a fixed @Appearance@ which displays only the Name and Description fields
with no labels. Hence this is the default.
-}
module App.Appearance (
   -- * Class Definition
   AppearanceClass (..),
   -- * Type Definitions
   FieldStyle (..),
   EntityShowField (EntityShowField),
   entityShowFieldStyle,
   entityShowFieldId,
   EntityShowFields (EntityShowFields),
   fieldsShown,
   -- * Diagram Appearance
   DiagramAppearance (DiagramAppearance),
   cloneDiagramAppearance,
   arrowLabels,
   entityShowFields,
   highlights,
   coerceDiagramAppearance,
   diagramAppearanceClean,
   diagramAppearanceDialog,
   diagramAppearanceGadget,
   -- * Package Appearance
   PackageAppearance,
   clonePackageAppearance,
   packageAppearanceClean,
   packageAppearanceDialog,
   packageAppearanceGadget,
   -- * Diagram Layouts
   diagramLayout
) where

import Control.Applicative hiding (optional)
import Control.Arrow
import Control.Lens hiding ((.=))
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Colour as C
import qualified Data.Colour.SRGB as C
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.GdkPixbuf as Gdk
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango
import GI.Pango (AttrOp ((:=)))
import Hades.GI.BasicShapes
import Model.Abstract.DiagramType
import Model.Abstract.PackageTree
import Model.Abstract.Properties
import Model.Query.Base
import Model.Query.Diagram
import Model.Reflection.Reflective
import Model.Reflection.Types
import Model.Reflection.Values
import Reactive.Banana.ArrowDialog
import Reactive.Banana.Common
import Reactive.Banana.GI.Common
import Reactive.Banana.Table


-- | Diagram appearances can be stored in package meta-data.
class (EntityClass v) => AppearanceClass t v where
   getDiagramAppearance :: PackageMeta v -> DiagramAppearance t v


-- | How the name of a field ought to be displayed relative to its value in a diagram.
data FieldStyle = FieldStyleNone | FieldStyleOutdent | FieldStyleAbove
   deriving (Eq, Show, Enum)

instance ToJSON FieldStyle where
   toJSON FieldStyleNone = String "None"
   toJSON FieldStyleOutdent = String "Outdent"
   toJSON FieldStyleAbove = String "Above"

instance FromJSON FieldStyle where
   parseJSON (String "None") = return FieldStyleNone
   parseJSON (String "Outdent") = return FieldStyleOutdent
   parseJSON (String "Above") = return FieldStyleAbove
   parseJSON (String txt) = fail $ "Unrecognised FieldNameStyle: " <> show txt
   parseJSON v = typeMismatch "FieldNameStyle" v


-- | If and how a field within an entity should be displayed in a diagram.
data EntityShowField = EntityShowField {
      _entityShowFieldStyle :: FieldStyle,
      _entityShowFieldId :: FieldId
   } deriving (Eq, Show)

instance ToJSON EntityShowField where
   toJSON (EntityShowField style fid)= object [
         "style" .= style,
         "field".= fid
      ]

instance FromJSON EntityShowField where
   parseJSON = withObject "EntityShowField" $ \obj -> do
      style <- obj .: "style"
      fid <- obj .:? "field" >>= \case
         Just fid -> return fid
         Nothing -> migrateField <$> obj .: "name"
      return $ EntityShowField style fid


entityShowFieldStyle :: Lens' EntityShowField FieldStyle
entityShowFieldStyle = lens _entityShowFieldStyle $ \s v -> s {_entityShowFieldStyle = v}

entityShowFieldId :: Lens' EntityShowField FieldId
entityShowFieldId = lens _entityShowFieldId $ \s n -> s {_entityShowFieldId = n}


-- | The fields to be shown for different entity types in a diagram.
--
-- The contents are stored as an association list rather than a "Map" because the latter breaks
-- the table gadget in the dialog. The monoid instance is left-biased; it does not concatenate
-- the field lists.
newtype EntityShowFields a =
   EntityShowFields { _fieldsShown :: [(Variant a, [EntityShowField])] }
   deriving (Eq, Show)

instance Semigroup (EntityShowFields a) where
   EntityShowFields v1 <> EntityShowFields v2 = EntityShowFields $
      M.toList $ M.union (M.fromList v1) (M.fromList v2)

instance Monoid (EntityShowFields a) where
   mempty = EntityShowFields mempty

instance ToJSON (EntityShowFields a) where
   toJSON (EntityShowFields v) = toJSON v

instance FromJSON (EntityShowFields a) where
   parseJSON v = EntityShowFields <$> (parseJSON v <|> (M.toList <$> parseJSON v))
       -- Allow for older files stored as a map.


-- | Each variant will have a list of fields to be shown.
fieldsShown :: Iso
      (EntityShowFields a)
      (EntityShowFields b)
      [(Variant a, [EntityShowField])]
      [(Variant b, [EntityShowField])]
fieldsShown = iso _fieldsShown EntityShowFields


-- | Convert the fields shown to a "Map".
fieldsShownMap :: EntityShowFields a -> Map (Variant a) [EntityShowField]
fieldsShownMap = M.fromList . view fieldsShown


-- | Clean up the "fieldsShown": merge duplicates and remove blank keys.
showFieldsClean :: EntityShowFields a -> EntityShowFields a
showFieldsClean = fieldsShown %~
   M.toList . M.fromListWith (flip (++)) . filter (("" /=) . view variantName . fst)

-- | Appearance view settings for diagrams. This is the version for the user interface and
-- storage.
--
-- The type parameters are dummies. The first is the type of diagram that this is controlling,
-- the second is the type of entity in the model.
--
-- The "Semigroup" instance is left-biased.
data DiagramAppearance t v = DiagramAppearance {
      _arrowLabels :: Maybe Bool,   -- | True if arrows are to be labelled.
      _entityShowFields :: EntityShowFields v,
      _highlights :: [(QueryId, Colour)]
   } deriving (Eq, Show)

instance ToJSON (DiagramAppearance t v) where
   toJSON (DiagramAppearance a f h) = object [
         "arrowNames" .= a,
         "fields" .= f,
         "highlights" .= map (over _2 getColour) h
      ]

instance FromJSON (DiagramAppearance t v) where
   parseJSON = withObject "Diagram appearance" $ \obj ->
      DiagramAppearance <$>
         obj .:? "arrowNames" <*>
         obj .:? "fields" .!= mempty <*>
         (map (over _2 Colour) <$> obj .: "highlights")

instance Semigroup (DiagramAppearance t v) where
   DiagramAppearance n1 f1 h1 <> DiagramAppearance n2 f2 h2 =
      DiagramAppearance (n1 <|> n2) (f1 <> f2) (h1 ++ h2)

instance Monoid (DiagramAppearance t v) where
   mempty = DiagramAppearance Nothing mempty []


-- | Create a copy of the diagram appearance with model IDs of the queries updated according to the
-- provided function.
cloneDiagramAppearance ::
      (QueryId -> Maybe QueryId) -> DiagramAppearance t v -> DiagramAppearance t v
cloneDiagramAppearance f = highlights . each . _1 %~ (\v -> fromMaybe v $ f v)


-- | If true, label the arrows on a diagram.
arrowLabels :: Lens' (DiagramAppearance t v) (Maybe Bool)
arrowLabels = lens _arrowLabels $ \s b -> s{_arrowLabels = b}


-- | When the diagram is displayed each entity type will be looked up here to find the list
-- of fields that should be shown.
entityShowFields :: Lens
      (DiagramAppearance t v1)
      (DiagramAppearance t v2)
      (EntityShowFields v1)
      (EntityShowFields v2)
entityShowFields = lens _entityShowFields $ \s f -> s {_entityShowFields = f}


-- | When the diagram is displayed it will be passed as an input to each query listed. If a
-- diagram element is found in the query output then it will be displayed in the corresponding
-- colour. If a diagram element is found in multiple query outputs then the first one wins.
highlights :: Lens' (DiagramAppearance t v) [(QueryId, Colour)]
highlights = lens _highlights $ \s h -> s{_highlights = h}


-- | Change the type of a DiagramAppearance.
coerceDiagramAppearance :: DiagramAppearance t1 v -> DiagramAppearance t2 v
coerceDiagramAppearance (DiagramAppearance l f h) = DiagramAppearance l f h


-- | Clean up the fields shown in the diagram appearance.
diagramAppearanceClean :: DiagramAppearance t v -> DiagramAppearance t v
diagramAppearanceClean = entityShowFields %~ showFieldsClean


diagramAppearanceDialog :: (EntityClass v, HasDiagrams p v, Queryable v) =>
   Variant v -> Dialog' (Model v) w (DiagramAppearance t v)
diagramAppearanceDialog var =
   Dialog (variantUserLabel var <> " Style") OkApplyButton $ accum (diagramAppearanceGadget var)


diagramAppearanceGadget :: (EntityClass v, HasDiagrams p v, Queryable v) =>
   Variant v -> GadgetF' (Model v) w (DiagramAppearance t v)
diagramAppearanceGadget var =
      box Vertical [[
            form Vertical
                  [("Show arrow labels:", focusing arrowLabels $ optional False tickBox)],
            box Horizontal [[
                  simpleFrame "Show Fields" $
                        focusing (entityShowFields . fieldsShown) $
                        entityShowTable variants,
                  simpleFrame "Highlights" $
                        focusing (highlights . highlightIso) highlightTable
               ]]
         ]]
   where
      variants = M.findWithDefault [] var diagramEntityTypes
      -- Tabular form of highlight settings, including blank value.
      highlightIso :: Iso' [(QueryId, Colour)] [(Set QueryId, Colour)]
      highlightIso = iso
            ((++ [blankHighlight]) . map (_1 %~ S.singleton))
            (map (_1 %~ fromJust . S.lookupMin) . filter (/= blankHighlight))
               -- fromJust is safe because Nothing values are filtered out.
         where
            blankHighlight = (S.empty, Colour C.black)



-- | Table of diagram queries mapping to highlight colours.
highlightTable :: (EntityClass v, Queryable v) => Gadget' (Model v) w [(Set QueryId, Colour)]
highlightTable = getInitialEnv $ \model -> table
      [TableDelete, TableShuffle]
      [
         mkField "Query" _1 Nothing (EditFixed $ queryName model) Nothing,
         mkField "Colour" _2 Nothing EditColour Nothing
      ] $
      Just querySelector
   where
      querySelector =
         constantDialog $
         promoteDialog (accum . focusing _1) $
         refPicker True (not . isn't _QueryDiagram . view entityContents)
      queryName :: EntityClass v => Model v -> Set QueryId -> Text
      queryName model s = case S.toList s of
         [] -> " - ? - "
         qId:_ -> fromMaybe " - ? - " $ modelContents model ^? ix qId . entityName . nameText


-- | Table of fields to show for different types of entity.
entityShowTable :: (EntityClass v) =>
   [Variant v]   -- ^ Entity variant names.
   -> Gadget' (Model v) w [(Variant v, [EntityShowField])]
entityShowTable varNames =
   validateText uniqueVariants $ getInitialEnv $ \model ->
      let ftable = withStampFields $ withBuiltIn $ modelFields model
      in table
            [TableAdd "Type" (Variant "", []) , TableDelete]
            [
               mkField "Type" _1 Nothing (EditFixed variantUserLabel) Nothing,
               mkField "Fields" _2 Nothing (EditFixed $ fieldList ftable) Nothing
            ]
            (Just showFieldDialog)
   where
      uniqueVariants xs =
         -- "head" safe here because "group" sublists are never zero length.
         case map head $ filter ((1 <) . length) $ group $ sort $ map fst xs of
            [] -> Nothing
            v:_ -> Just $ variantUserLabel v <> " listed twice."
      fieldList ftable =
         T.intercalate ", " .
         map (view fieldName) .
         mapMaybe ((`M.lookup` ftable) .
         view entityShowFieldId)
      -- showFieldDialog :: DialogSelector' (Model v) w (Variant v, [EntityShowField])
      showFieldDialog = constantDialog $
         Dialog "Diagram fields" OkButton $ proc (var1, fields1) -> do
            var2 <- accum $ form Vertical [
                  ("Select Entity Type:", focusing id varNameCombo)
               ] -< var1
            fields2 <- exec fieldsGadget -< (var2, fields1)
            returnA -< (var2, fields2)
      -- fieldsGadget :: Variant v -> Gadget' (Model v) w [EntityShowField]
      fieldsGadget var = getInitialEnv $ \model ->
            let
               exts v = M.findWithDefault [] v $ modelExtensions model
               fieldIds = maybe [] (reflection exts) $
                  find ((var ==) . reflectiveName) reflectiveDefaults
               ftable = withStampFields $ withBuiltIn $ modelFields model
               revTable = fieldsByName ftable
               fieldNames = fieldIds ^. fieldNameIso ftable
               showFieldsIso :: Iso' [EntityShowField] [(FieldName, FieldStyle)]
               showFieldsIso = iso
                  (mapMaybe $ \sf -> ftable ^?
                     ix (sf ^. entityShowFieldId) . fieldName . to (, sf ^. entityShowFieldStyle))
                  (mapMaybe $ \(nm, sty) -> revTable ^?
                     ix nm . to fieldId . to (EntityShowField sty))
            in
               if var `elem` varNames
               then accum $ focusing showFieldsIso $ table
                     [TableAdd "Add field" ("Name", FieldStyleOutdent), TableShuffle, TableDelete]
                     [
                        mkField "Field" _1 Nothing (EditCombo fieldNames) Nothing,
                        mkField "Label Style" (_2 . styleName) Nothing (EditCombo styleNames) Nothing
                     ]
                     Nothing
               else arr id
      -- varNameCombo :: Gadget' (Model v) w (Variant v)
      varNameCombo = comboBox $ const $
            map (\v -> ComboItem (variantUserLabel v) Nothing Nothing v) varNames
      styleNames = map (view styleName) [FieldStyleNone, FieldStyleOutdent, FieldStyleAbove]
      styleName :: Iso' FieldStyle Text
      styleName = iso writeStyle readStyle
         where
            writeStyle FieldStyleNone = "None"
            writeStyle FieldStyleOutdent = "Outdented"
            writeStyle FieldStyleAbove = "Above"
            readStyle "Outdented" = FieldStyleOutdent
            readStyle "Above" = FieldStyleAbove
            readStyle _ = FieldStyleNone


-- | The appearance data for a package consists of the appearance data of each type of diagram.
-- No type data is kept: it is for each type of diagram to recognise its own appearance data.
type PackageAppearance a = Map (Variant a) (DiagramAppearance () a)


-- | Duplicate the PackageAppearance with updated model IDs where appropriate.
clonePackageAppearance :: (ModelId -> Maybe ModelId) -> PackageAppearance a -> PackageAppearance a
clonePackageAppearance f = fmap (cloneDiagramAppearance f)


packageAppearanceDialog :: (EntityClass v, HasDiagrams p v, Queryable v) =>
   Dialog' (Model v) w (PackageAppearance v)
packageAppearanceDialog =
   Dialog "Style of Diagrams in this Package" OkApplyButton $ accum packageAppearanceGadget


packageAppearanceGadget :: (EntityClass v, HasDiagrams p v, Queryable v) =>
   GadgetF' (Model v) w (PackageAppearance v)
packageAppearanceGadget = tabForm $ map tab $ M.keys diagramEntityTypes
   where
      tab var = (
            variantUserLabel var,
            focusingOver (at1 var) $ diagramAppearanceGadget var
         )
      at1 k = lens
            (M.findWithDefault mempty k)
            (\s v -> if v == mempty then M.delete k s else M.insert k v s)


-- | Clean up the fields shown for each diagram type.
packageAppearanceClean :: PackageAppearance v -> PackageAppearance v
packageAppearanceClean = each %~ diagramAppearanceClean


-- | Display entity fields on a diagram of type @t@.
diagramLayout :: (EntityClass v) =>
   EntityShowFields v -> ModelEdit v w (HadesLayout ())
diagramLayout style = withBaseType $ current >>= \case
   Nothing -> return $ layoutText [] "Internal error: root entity should not appear on a diagram."
   Just ent -> do
      let
         typ = ent ^. entityContents . to reflectiveName
         values = entityPropertiesWithStamps ent
      ftable <- withStampFields . withBuiltIn . modelFields <$> getModel
      return $
         forM_ (style ^. to fieldsShownMap . at typ . non defaultFields) $ \fs -> do
            let
               fid = fs ^. entityShowFieldId
               fVal = values ^. at fid . non ExtNone
            case ftable ^? ix fid . fieldName of
               Nothing -> return ()
               Just fname -> case fs ^. entityShowFieldStyle of
                  FieldStyleNone -> layoutField fVal (ftable ^. at fid)
                  FieldStyleOutdent -> layoutLabelledField fname fVal (ftable ^. at fid)
                  FieldStyleAbove -> case fVal of
                     ExtNone -> return ()
                     _ -> do
                        layoutHLine
                        layoutTitle Nothing fname
                        layoutField (values ^. at fid . non ExtNone) (ftable ^. at fid)
   where
      indent = 20
      defaultFields = [EntityShowField FieldStyleNone descriptionField]
      layoutField ExtNone _ = return ()
      layoutField v Nothing = layoutParagraphs [] $ displayValue v
      layoutField v (Just (Field _ _ _ _ typ)) = do
         let
            (icon1, colour) = valueFormat typ v
            textPart = do
               attrList <- colourText colour 0 maxBound
               sequence_ $
                  intersperse layoutParagraphGap $
                  map (layoutText
                        [(`Pango.layoutSetAttributes` Just attrList)]) $
                  T.splitOn "\n\n" $ displayValue v
               when (typ == BuiltInDef ModelNote) layoutParagraphGap
         pix <- join <$> mapM getIcon icon1
         case pix of
            Nothing -> textPart
            Just p -> do
               iconWidth <- fromIntegral <$> Gdk.pixbufGetWidth p
               void $ layoutParallel iconWidth 3
                  (layoutPixbuf False Nothing p)
                  textPart
      layoutLabelledField _ ExtNone _ = return ()
      layoutLabelledField lbl v mField = do
         let
            mTyp = mField ^? _Just . fieldType
            txt = lbl <> ": " <> displayValue v
            (icon1, colour) = maybe (Nothing, Nothing) (`valueFormat` v) mTyp
         attrList <- colourText
               colour
               (fromIntegral $ T.length lbl + 2)
               (fromIntegral $ T.length txt)
         boldAttr1 <- Pango.attrWeightNew Pango.WeightBold
         Pango.set boldAttr1 [#endIndex := fromIntegral $ T.length lbl + 1]
         Pango.attrListInsert attrList boldAttr1  -- This disowns boldAttr1, so can't use it again.
         let
            textPart = layoutText [
                  (`Pango.layoutSetIndent` (-indent)),
                  (`Pango.layoutSetAttributes` Just attrList)
               ] txt
         case mTyp of
            Nothing -> textPart
            _ ->
               case icon1 of
                  Nothing -> textPart
                  Just _ -> do  -- No way in Pango to have an icon inlined with text, so bodge it.
                     boldAttr2 <- Pango.attrWeightNew Pango.WeightBold
                     boldList <- Pango.attrListNew
                     Pango.attrListInsert boldList boldAttr2
                     layoutText [(`Pango.layoutSetAttributes` Just boldList)] $ lbl <> ":"
                     void $ layoutParallel
                           (fromIntegral indent)
                           0
                           (return ())
                           (layoutField v mField)
      getIcon nm = do  -- Not sure about performance here. Does GTK3 cache the pixmaps?
         thm <- Gtk.iconThemeGetDefault
         let sz = 16
         Gtk.iconThemeLookupIcon thm nm sz [] >>= \case
            Nothing -> return Nothing
            Just info -> Just <$> Gtk.iconInfoLoadIcon info
      colourText Nothing _ _ = Pango.attrListNew
      colourText (Just colour) s e = do
         let
            (bgr, bgg, bgb) = colourForFont colour
            (fgr, fgg, fgb) = colourForFont $ contrastText colour
         fgAttr <- Pango.attrForegroundNew fgr fgg fgb
         Pango.set fgAttr [#startIndex := s, #endIndex := e]
         bgAttr <- Pango.attrBackgroundNew bgr bgg bgb

         lst <- Pango.attrListNew
         Pango.attrListInsert lst fgAttr
         Pango.attrListInsert lst bgAttr
         return lst
      colourForFont c =
         let C.RGB r g b = C.toSRGB $ getColour c
         in (round $ r * 65535, round $ g * 65535, round $ b * 65535)
