{-# LANGUAGE Arrows #-}

{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}


{- |

Queries use reflection, but are not themselves part of the reflective mechanism. Query diagrams
appear in the model, but their contents are not avatars of anything else in the model.
-}

module Model.Query.Diagram (
   Queryable (..),
   QueryBox (QueryBox, queryBoxId),
   queryBoxShape,
   queryBoxData,
   QueryData (..),
   _Step, _Filter, _FilterVariants, _SubQuery, _Variants, _Entities, _Forwards, _Backwards,
   QueryArrow (QueryArrow, queryArrowId),
   queryArrowShape,
   queryElementDialog,
   QueryDiagram (..),
   queryDiagramWrapper,
   ViewContext (QueryContext),
   queryContextId,
   queryDiagram,
   queryInputs,
   queryBackgroundMenu,
   queryActivation,
   compileQueryDiagram,
   queryDiagramToolbar,
   chooseQueryDialog,
   queryForest,
   queryTest,
   clickableEntitySet
) where

import Control.Applicative
import Control.Arrow
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.State
import Data.Aeson
import Data.Default
import Data.Either
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree
import Data.UUID (UUID)
import qualified Data.UUID as U
import qualified Data.Vector as V
import Hades.Abstract as H
import Hades.GI.BasicShapes
import Model.Abstract.DiagramType
import Model.Abstract.PackageTree
import Model.Abstract.Properties
import Model.Query.Base
import Model.Reflection.Dialogs
import Model.Reflection.Parser
import Model.Reflection.NamedRelation (Relation)
import Model.Reflection.References
import Model.Reflection.Reflective
import Model.Reflection.Types
import Model.Reflection.Values
import Reactive.Banana.ArrowDialog
import Reactive.Banana.GI.ErrorBox
import Reactive.Banana.Menu
import Text.Megaparsec


-- | A reflective type can support queries as long as it can contain query diagrams.
class (Reflective v) => Queryable v where
   -- | Prism for query diagrams in type @v@.
   _QueryDiagram :: Prism' v (QueryDiagram v)
   -- | If this is a diagram then return the list of model entities for which it contains
   -- avatars. If this is not a diagram then return @[]@.
   queryAvatars :: v -> [ModelId]


-- | Move to the entities which are represented in the input diagrams.
inDiagramStep :: (EntityClass v, Queryable v) => QueryStep v
inDiagramStep = QueryStep $ \_ model inputs ->
      (S.fromList $ concatMap (d model) $ S.toList inputs, [])
   where
      d m uuid = maybe [] queryAvatars $ modelContents m ^? ix uuid . entityContents


-- | Move to the diagrams which contain the input entities.
findDiagramsStep :: (EntityClass v, Queryable v) => QueryStep v
findDiagramsStep = QueryStep $ \_ model inputs ->
      (S.fromList $ map entityId $ filter (check inputs) $ M.elems $ modelContents model, [])
   where
      check i e =
         not $ S.null $ S.intersection i $ S.fromList $ queryAvatars $ e ^. entityContents


data QueryData v =
   Step (Set Relation)   -- ^ Follow the named relations to connected entities.
   | Filter Text  -- ^ Filter the entities according to the expression in the text.
   | FilterVariants (Set (Variant v))  -- ^ Filter out variants not in the set.
   | SubQuery (Maybe UUID)  -- ^ Insert the named diagram as a query.
   | Variants (Set (Variant v))  -- ^ Adds all instances of the named variant types.
   | Entities (Set UUID)   -- ^ Adds all the identified entities.
   | Forwards (Map (Variant v) (Relation, Relation))  -- ^ Follow the named types of arrows.
   | Backwards (Map (Variant v) (Relation, Relation))
      -- ^ Follow the named types of arrows backwards.
   | Parent    -- ^ Move to the parents of the current entities.
   | Children  -- ^ Move to the children of the current entities.
   | InDiagram  -- ^ Move to the contents of the current diagrams.
   | FindDiagrams  -- ^ Move to the diagrams that contain the current entities.
   | QueryInput  -- ^ The input to the query.
   | QueryOutput  -- ^ The output from the query.
   | QueryComment Text  -- ^ Comment box.
   deriving (Show, Eq)

instance Default (QueryData v) where def = QueryInput


_Step :: Prism' (QueryData v) (Set Relation)
_Step = prism Step $ \case {Step rs -> Right rs; v -> Left v}

_Filter :: Prism' (QueryData v) Text
_Filter = prism Filter $ \case {Filter txt -> Right txt; v -> Left v}

_FilterVariants :: Prism' (QueryData v) (Set (Variant v))
_FilterVariants = prism FilterVariants $ \case {FilterVariants vs -> Right vs; v -> Left v}

_SubQuery :: Prism' (QueryData v) (Maybe UUID)
_SubQuery = prism SubQuery $ \case {SubQuery q -> Right q; v -> Left v}

_Variants :: Prism' (QueryData v) (Set (Variant v))
_Variants = prism Variants $ \case {Variants vs -> Right vs; v -> Left v}

_Entities :: Prism' (QueryData v) (Set UUID)
_Entities = prism Entities $ \case {Entities es -> Right es; v -> Left v}

_Forwards :: Prism' (QueryData v) (Map (Variant v) (Relation, Relation))
_Forwards = prism Forwards $ \case {Forwards vs -> Right vs; v -> Left v}

_Backwards :: Prism' (QueryData v) (Map (Variant v) (Relation, Relation))
_Backwards = prism Backwards $ \case {Backwards vs -> Right vs; v -> Left v}

_QueryComment :: Prism' (QueryData v) Text
_QueryComment = prism QueryComment $ \case {QueryComment txt -> Right txt; v -> Left v}


-- | Query diagram elements are not avatars for model entities. However they need access to
-- names, UUIDs and reflective data for all model elements.
type QueryScript v = ModelScript HadesRender v (QueryDiagram v)

type QueryDelta v a = Delta (QueryElement v) a

type QueryView v = View (QueryElement v)


-- | The diagramatic representation of component of a query.
data QueryBox v = QueryBox {
      queryBoxId :: UUID,   -- ^ The ID of this box in the query diagram.
      _queryBoxShape :: Rectangle,
      _queryBoxData :: QueryData v
   } deriving (Eq)

instance ToJSON (QueryBox v) where
   toJSON box1 =
      object $ [
         "queryBoxId" .= queryBoxId box1,
         "shape" .= (box1 ^. queryBoxShape)
      ] ++ case box1 ^. queryBoxData of
         Step rs ->         ["type" ..= "step",      "relations" .= rs]
         Filter txt ->      ["type" ..= "filter",    "text" .= txt]
         FilterVariants vars -> ["type" ..= "filterVariants", "variants" .= vars]
         SubQuery uuid ->   ["type" ..= "subquery",  "query" .= uuid]
         Variants vars ->   ["type" ..= "variants",  "variants" .= vars]
         Entities uuids ->  ["type" ..= "entities",  "entities" .= uuids]
         Forwards vars ->   ["type" ..= "forwards",  "variants" .= vars]
         Backwards vars ->  ["type" ..= "backwards", "variants" .= vars]
         Parent ->          ["type" ..= "parent"]
         Children ->        ["type" ..= "children"]
         InDiagram ->       ["type" ..= "inDiagram"]
         FindDiagrams ->    ["type" ..= "findDiagrams"]
         QueryInput ->      ["type" ..= "queryInput"]
         QueryOutput ->     ["type" ..= "queryOutput"]
         QueryComment txt ->     ["type" ..= "comment", "text" .= txt]

instance FromJSON (QueryBox v) where
   parseJSON = withObject "Query diagram box" $ \v -> do
      uuid <- v .: "queryBoxId"
      shape <- v .: "shape"
      let f = QueryBox uuid shape
      typ <- v ..: "type"
      case typ of
         "step" -> f . Step <$> v .: "relations"
         "filter" -> f . Filter <$> v .: "text"
         "filterVariants" -> f . FilterVariants <$> v .: "variants"
         "subquery" -> f . SubQuery <$> v .: "query"
         "variants" -> f . Variants <$> v .: "variants"
         "entities" -> f . Entities <$> v .: "entities"
         "forwards" -> f . Forwards <$> v .: "variants"
         "backwards" -> f . Backwards <$> v .: "variants"
         "parent" -> return $ f Parent
         "children" -> return $ f Children
         "inDiagram" -> return $ f InDiagram
         "findDiagrams" -> return $ f FindDiagrams
         "queryInput" -> return $ f QueryInput
         "queryOutput" -> return $ f QueryOutput
         "comment" -> f . QueryComment <$> v .: "text"
         txt -> fail $ "Unknown query box type: " <> T.unpack txt


queryBoxShape :: Lens' (QueryBox v) Rectangle
queryBoxShape = lens _queryBoxShape $ \box1 s -> box1 {_queryBoxShape = s}


queryBoxData :: Lens' (QueryBox v) (QueryData v)
queryBoxData = lens _queryBoxData $ \box1 d -> box1 {_queryBoxData = d}


queryBoxHandles :: (EntityClass v, Queryable v) => QueryBox v -> QueryDelta v [QueryView v]
queryBoxHandles box1 = forM handles $ \(p, shapeF) ->
      simpleHandle "Resize " mempty squareHandle (p, QElemBox . shapeF)
   where
      handles = boxHandles (queryBoxShape . rectangleShape) box1


-- | The type label for the query box.
queryBoxLabel :: QueryBox v -> Text
queryBoxLabel box1 = case box1 ^. queryBoxData of
   Step {} -> "Step"
   Filter {} -> "Filter"
   FilterVariants {} -> "Filter Types"
   SubQuery {} -> "SubQuery"
   Variants {} -> "Types"
   Entities {} -> "Entities"
   Forwards {} -> "Follow forwards"
   Backwards {} -> "Follow backwards"
   Parent -> "Parent entity"
   Children -> "Child entities"
   InDiagram -> "Diagram contents"
   FindDiagrams -> "Find diagrams containing the inputs"
   QueryInput -> "Input"
   QueryOutput -> "Output"
   QueryComment _ -> "Comment"


queryBoxDescription :: (EntityClass v, Queryable v) => QueryBox v -> QueryDelta v Text
queryBoxDescription box1 = case box1 ^. queryBoxData of
      Step rs -> return $ T.intercalate ", " $ S.toList rs
      Filter txt -> return txt
      FilterVariants vs -> return $ showVariants vs
      SubQuery (Just uuid) -> liftBase $ getEntityName uuid
      SubQuery Nothing -> return "<Nothing selected>"
      Variants vs -> return $ showVariants vs
      Entities uuids -> liftBase $ T.intercalate ", " <$> mapM getEntityName (S.toList uuids)
      Forwards vs -> return $ showSubset variantUserLabel arrowNames $ S.fromList $ M.keys vs
      Backwards vs -> return $ showSubset variantUserLabel arrowNames $ S.fromList $ M.keys vs
      QueryInput -> do
         mId <- getDiagramModelId
         liftBase $ lift $ do
            goToEntity mId
            c <- current
            case c ^? _Just . entityContents . _QueryDiagram . queryInputs of
               Nothing -> return "- Internal Error -"  -- Should never happen.
               Just vs -> return $ showVariants vs
      QueryComment txt -> return txt
      _ -> return ""
   where
      arrowNames = S.fromList $ M.keys reflectiveArrows


-- | Show the names of the variants in the set, or those that are not in the set if that is shorter.
showVariants :: (Reflective a) => Set (Variant a) -> Text
showVariants vars = showSubset variantUserLabel universe1 vars
   where
      universe1 = S.fromList reflectiveNames
      _ = universe1 == vars  -- Tie universe to type of argument.


queryBoxView :: (EntityClass v, Queryable v) => QueryBox v -> QueryDelta v (QueryView v)
queryBoxView box1 = do
      desc <- queryBoxDescription box1
      scale <- use deltaZoom
      return View {
            viewDraw = case box1 ^. queryBoxData of
               QueryComment txt -> do
                  let cbox = CommentBox $ box1 ^. queryBoxShape . to shapeBounds
                  drawCommentBox cbox
                  runLayout_ (shapeInnerBox cbox) $ layoutParagraphs [] txt
               _ -> do
                  drawPolygon $ rectangleCorners $ box1 ^. queryBoxShape
                  textBlock (queryBoxLabel box1) [desc] $ box1 ^. queryBoxShape . to shapeInnerBox,
            viewTouched = shapeTouched (box1 ^. queryBoxShape) scale,
            viewBox = box1 ^. queryBoxShape . to shapeBounds,
            viewMenu = queryBoxMenu,
            viewAction = handleDelegatedAction
               (QElemBox box1)
               (yieldProperties $ QElemBox box1)
               (const yieldViews)
         }
   where
      queryBoxMenu pt =
         let basic = basicShapeMenu (queryBoxId box1) pt
         in case box1 ^. queryBoxData of
            QueryInput -> Menu [[menuItem "Edit inputs" editInputs]] <> basic
            _ -> basic
      editInputs = mkScriptAction $ do
         mId <- getDiagramModelId
         changed <- liftBase $ do
            c <- lift $ do
               goToEntity mId
               current
            case c ^? _Just . entityContents . _QueryDiagram . queryInputs of
               Nothing -> return False  -- Do nothing. Should never happen.
               Just qi -> do
                  let
                     d = const $ return $ Just $ subsetDialog
                           "Query input types"
                           variantUserLabel
                           (S.fromList reflectiveNames)
                  openDialog d qi >>= \case
                     Nothing -> return False
                     Just newVal -> lift $ modifyValue $ queryInputs .~ newVal
         when changed $ do
            nm <- liftBase $ lift currentName
            tellCheckpoint $ "Edited inputs of " <> nm
         yieldViews


-- | Get the name of the referenced entity in the context of the current model.
getEntityName :: (EntityClass v) => UUID -> QueryScript v Text
getEntityName uuid = lift $ goToEntity uuid >> currentName


data QueryArrow = QueryArrow {
      queryArrowId :: UUID,
      _queryArrowShape :: LineShape
   } deriving (Eq)

instance ToJSON QueryArrow where
   toJSON a = object [
         "queryArrowId" .= queryArrowId a,
         "shape" .= (a ^. queryArrowShape)
      ]

instance FromJSON QueryArrow where
   parseJSON = withObject "Query diagram arrow" $ \v ->
         fixBogus <$> (QueryArrow <$> v .: "queryArrowId" <*> v .: "shape")
      where
         -- Some old files have zero length query arrows in the top left corner. Make them visible.
         fixBogus arrow =
            let (end1, end2) = arrow ^. queryArrowShape . lineEnds
            in if bogus end1 && bogus end2
               then queryArrowShape . lineEnds . _2 %~ (`moveEnd` (50,20)) $ arrow
               else arrow
         bogus (Unconnected pt) = pt == Point 0 0
         bogus _ = False

queryArrowShape :: Lens' QueryArrow LineShape
queryArrowShape = lens _queryArrowShape $ \arrow s -> arrow {_queryArrowShape = s}


queryArrowHandles :: (EntityClass v, Queryable v) => QueryArrow -> QueryDelta v [QueryView v]
queryArrowHandles arrow =
      lineHandles (arrow ^. queryArrowShape) squareHandle promoteShape (const $ return ())
   where
      promoteShape s = QElemArrow $ queryArrowShape .~ s $ arrow


queryArrowView :: (EntityClass v, Queryable v) => QueryArrow -> QueryDelta v (QueryView v)
queryArrowView arrow = do
   scale <- use deltaZoom
   let
      shape = arrow ^. queryArrowShape
      (start, end) = shape ^. lineEnds
   return View {
         viewDraw = drawPolyLine
               (closedArrowHead 30 10 False)
               []
               (connectorPoint start)
               (V.toList $ shape ^. lineMiddle)
               (connectorPoint end)
               Nothing,
         viewTouched = shapeTouched shape scale,
         viewBox = growBox 12.0 $ shapeBounds shape,  -- Allow for arrow head.
         viewMenu =
               lineMenu (\s -> QElemArrow $ queryArrowShape .~ s $ arrow) shape
               <> basicShapeMenu (queryArrowId arrow),
         viewAction = handleDelegatedAction
               (QElemArrow arrow)
               yieldViews
               (const yieldViews)
      }


-- | Query diagrams are composed of boxes and arrows.
data QueryElement v = QElemBox (QueryBox v) | QElemArrow QueryArrow deriving (Eq)

instance HasId (QueryElement v) where
   identifier (QElemBox x) = queryBoxId x
   identifier (QElemArrow x) = queryArrowId x

instance ToJSON (QueryElement v) where
   toJSON (QElemBox x) = toJSON x
   toJSON (QElemArrow x) = toJSON x

instance FromJSON (QueryElement v) where
   parseJSON v =
      QElemBox <$> parseJSON v
      <|> QElemArrow <$> parseJSON v

instance (EntityClass v, Queryable v) => Viewable (QueryElement v) where
   data ViewContext (QueryElement v) = QueryContext {queryContextId :: QueryId}
   type Paint (QueryElement v) = HadesRender
   type Base (QueryElement v) = QueryScript v
   itemHandles (QElemBox bx) = queryBoxHandles bx
   itemHandles (QElemArrow a) = queryArrowHandles a
   itemTouched (QElemBox bx) = shapeTouched $ bx ^. queryBoxShape
   itemTouched (QElemArrow a) = shapeTouched $ a ^. queryArrowShape
   itemInBox (QElemBox bx) = shapeInBox $ bx ^. queryBoxShape
   itemInBox (QElemArrow a) = shapeInBox $ a ^. queryArrowShape
   itemBounds (QElemBox bx) = bx ^. queryBoxShape . to shapeBounds
   itemBounds (QElemArrow a) = a ^. queryArrowShape . to shapeBounds
   itemResize d (QElemBox bx) = do
      let newItem = QElemBox $ queryBoxShape %~ shapeResize d $ bx
      updateItem newItem
      return newItem
   itemResize _ v = return v
   itemMove = itemDrag
   itemDrag d (QElemBox bx) = do
      let newItem = QElemBox $ queryBoxShape %~ shapeDrag d $ bx
      updateItem newItem
      return newItem
   itemDrag d (QElemArrow a) = do
      let newItem = QElemArrow $ queryArrowShape %~ shapeDrag d $ a
      updateItem newItem
      return newItem
   itemView (QElemBox bx) = queryBoxView bx
   itemView (QElemArrow a) = queryArrowView a
   itemChildren _ = return []
   itemDescription (QElemBox bx) = return $ queryBoxLabel bx
   itemDescription (QElemArrow _) = return "Query arrow"
   itemType _ = "Query"

instance (EntityClass v, Queryable v) => Connectable (QueryElement v) where
   itemCanConnect _ QElemBox {} _ _ = return False
   itemCanConnect _ QElemArrow {} (QElemBox (QueryBox _ _ QueryComment {})) _ = return False
   itemCanConnect _ QElemArrow {} QElemBox {} _ = return True
   itemCanConnect _ QElemArrow {} QElemArrow {} _ = return False
   itemConnectors = _QElemArrow . queryArrowShape . lineEnds . both
   itemConnectionPoint (QElemBox bx) _ = return $ shapeConnectionPoint $ bx ^. queryBoxShape
   itemConnectionPoint (QElemArrow a) _ = return $ shapeConnectionPoint $ a ^. queryArrowShape
   itemAdjust (QElemArrow a) = do
      newShape <- lineUpdate $ a ^. queryArrowShape
      updateItem $ QElemArrow $ queryArrowShape .~ newShape $ a
   itemAdjust _ = return ()


_QElemBox :: Prism' (QueryElement v) (QueryBox v)
_QElemBox = prism QElemBox $ \case
   QElemBox bx -> Right bx
   v -> Left v

_QElemArrow :: Prism' (QueryElement v) QueryArrow
_QElemArrow = prism QElemArrow $ \case
   QElemArrow arrow -> Right arrow
   v -> Left v


-- | Properties dialog selector for query elements within the diagram.
queryElementDialog :: (EntityClass v, Queryable v) =>
   DialogSelector' (Model v) () (QueryElement v)
queryElementDialog e a =
   case a ^? _QElemBox . queryBoxData >>= queryDataDialog e of
      Just (Dialog t b g) ->
         Just $ Dialog t b $ accum $ traversing def (_QElemBox . queryBoxData) g
      Nothing ->
         Nothing


-- | Properties dialog selector for query data within a box.
queryDataDialog :: (EntityClass v, Queryable v) =>
   DialogSelector' (Model v) () (QueryData v)
queryDataDialog model qd =
   case qd of
      Step {} -> Just $ queryStepDialog model
      Filter {} -> Just $ Dialog "Filter condition" OkApplyButton $
            prismatic "" _Filter $
            expressionEntry entityFunctions $
            withStampFields $ withBuiltIn $ modelFields model
      FilterVariants {} ->
         Just $ promoteDialog (prismatic mempty _FilterVariants) queryVariantsDialog
      SubQuery {} ->
         Just $ promoteDialog (prismatic mempty $ _SubQuery . resultIso) chooseQueryDialog
      Variants {} ->
         Just $ promoteDialog (prismatic mempty _Variants) queryVariantsDialog
      Entities {} ->
         Just $ promoteDialog (prismatic mempty _Entities) queryEntitiesDialog
      Forwards {} ->
         Just $ promoteDialog (prismatic mempty _Forwards) $ queryArrowVariantsDialog False
      Backwards {} ->
         Just $ promoteDialog (prismatic mempty _Backwards) $ queryArrowVariantsDialog True
      QueryComment {} ->
         Just $ promoteDialog (prismatic mempty _QueryComment) queryCommentDialog
      _ -> Nothing
   where
      resultIso :: Iso' (Maybe a) (Set a)
      resultIso = iso
            (maybe S.empty S.singleton)
            (\s -> case S.toList s of {[x] -> Just x; _ -> Nothing})
         -- Not a proper Iso, but close enough as long as only one element can be selected.


-- | Select which relationship types this query step will transit.
queryStepDialog :: (EntityClass v) => Model v -> Dialog' (Model v) () (QueryData v)
queryStepDialog model =
      Dialog "Step via:" OkApplyButton $ prismatic mempty _Step $
         validate (not . S.null) $ accum $ form Vertical $ map relationTick allRelations
   where
      allRelations =
         map fst $ refTypeTableToList $ reflectiveBuiltInRefs <> modelRefTypes model
      relationTick :: Text -> (Text, GadgetF' e () (Set Text))
      relationTick rel = (rel, focusing (contains rel) tickBox)


-- | Chose another query to invoke within the current one.
chooseQueryDialog :: (EntityClass v, Queryable v) => Dialog' (Model v) () (Set UUID)
chooseQueryDialog = refPicker True (isJust . preview (entityContents . _QueryDiagram))


queryVariantsDialog :: (Reflective v) => Dialog' e () (Set (Variant v))
queryVariantsDialog =
   subsetDialog "Entity types" variantUserLabel (S.fromList reflectiveNames)


queryEntitiesDialog :: (EntityClass v, Reflective v) => Dialog' (Model v) () (Set UUID)
queryEntitiesDialog =
      Dialog "Entity Set Selection" OkApplyButton $
         validate (not . S.null) $ treeSelector queryTree1
   where
      queryTree1 model = case queryTree model of
         Left msg ->
            cannotHappen ("Query dialog could not get entity tree.\n" <> T.pack (show msg)) []
         Right t -> t
      queryTree model = evalModelEdit id model $ do
         goToRoot
         fmap (fmap treeFunc) <$> modelPackageForest
      treeFunc ent = (
            ent ^. entityName . nameText,
            ent ^? entityContents . to reflectiveGet . ix descriptionField . to displayValue,
            Just $ entityId ent)


-- | False for following arrows forwards, True for following backwards.
queryArrowVariantsDialog :: (Reflective v) =>
   Bool -> Dialog' e () (Map (Variant v) (Relation, Relation))
queryArrowVariantsDialog backFlag =
      Dialog msg OkApplyButton $
         accum $ buttonBar [
               ("Set all", const reflectiveArrows),
               ("Clear all", const mempty)
            ] <<< accum (box Horizontal [[
                  form Vertical $ map variantFlag uni1,
                  form Vertical $ map variantFlag uni2
               ]])
   where
      msg = "Follow arrows " <> if backFlag then "backwards" else "forwards"
      universe1 = M.toList reflectiveArrows
      (uni1, uni2) = splitAt ((length universe1 + 1) `div` 2) universe1
      variantFlag (v, rs) = (variantUserLabel v, focusing (at v . isoPair rs) tickBox)
      isoPair :: a -> Iso' (Maybe a) Bool
      isoPair rs = iso isJust $ \b -> if b then Just rs else Nothing


-- | Dialog for comment text.
queryCommentDialog :: Dialog' e () Text
queryCommentDialog = Dialog "Query Comment" OkApplyButton $ memoBox MemoMedium True


-- | The variant of the entity type that holds query diagrams.
data QueryDiagram v = QueryDiagram {
         _queryDiagram :: DiagramEntity (QueryElement v),
         _queryInputs :: Set (Variant v),
         _queryDescription :: Text
      }
   deriving (Eq)

instance (EntityClass v, Queryable v) => ToJSON (QueryDiagram v) where
   toJSON (QueryDiagram d i txt) = object [
         "diagram" .= d,
         "inputs" .= i,
         "description" .= txt
      ]

instance (EntityClass v, Queryable v) => FromJSON (QueryDiagram v) where
   parseJSON v = newDiagram v <|> plainDiagram v
      where
         newDiagram = withObject "Query Diagram" $ \v1 -> QueryDiagram <$>
            v1 .: "diagram" <*>
            v1 .: "inputs" <*>
            v1 .: "description"
         plainDiagram v1 = do
            d <- parseJSON v1
            return $ QueryDiagram d (S.fromList reflectiveNames) ""

instance (EntityClass v, Queryable v) =>
   Avatar HadesRender v (QueryDiagram v) (QueryElement v) where
      type DiagramMetadata (QueryElement v) = ()
      getDiagramModelId = queryContextId <$> use deltaContext
      updateViewContext = const return
      entityAvatar _ _ = return Nothing  -- Query elements are not avatars of anything.
      ascendedId _ = Nothing
      avatarChildren _ = return []
      updateAvatar _ = return False

instance (EntityClass v, Queryable v) => Reflective (QueryDiagram v) where
   reflectiveName _ = Variant "Query"
   reflectiveDefaults =
      let everything = S.fromList $ map reflectiveName reflectiveDefaults
      in [QueryDiagram (DiagramEntity (Name "Query-") mempty mempty) everything mempty]
   reflectiveBuiltIn _ = [nameField, descriptionField]
   reflectiveGet (QueryDiagram d _ txt) =
      reflectiveGet d <> M.singleton descriptionField (ExtText txt)
   reflectiveSet (QueryDiagram d i txt) = do
      d1 <- reflectiveSet d
      txt1 <- extract txt _ExtText descriptionField
      return $ QueryDiagram d1 i txt1
   reflectiveBuiltInRefs = mempty
   reflectiveArrows = mempty


-- Wrapper for query diagrams.
queryDiagramWrapper :: (EntityClass v, Queryable v) =>
   Entity v -> ModelEdit v (QueryDiagram v) (Maybe (EntityWrapper HadesRender v))
queryDiagramWrapper ent = usingTraversal $ \trv ->
      return $ do  -- Maybe monad
         qd <- ent ^? entityContents . trv
         return $ DiagramWrapper {
            wrappedEntityId = entityId ent,
            diagramToEdit = qd ^. queryDiagram . diagramEntityContents,
            diagramEntityTraversal = queryDiagram,
            diagramTraversal = trv,
            diagramDialog = queryElementDialog,
            diagramContext = QueryContext $ entityId ent,
            diagramMenu = Just queryBackgroundMenu,
            diagramToolbar = const queryDiagramToolbar
         }


queryDiagram :: Lens' (QueryDiagram v) (DiagramEntity (QueryElement v))
queryDiagram = lens _queryDiagram $ \s d -> s {_queryDiagram = d}

queryInputs :: Lens' (QueryDiagram v) (Set (Variant v))
queryInputs = lens _queryInputs $ \s i -> s {_queryInputs = i}

-- Following code is included for consistency but is currently unused, so commented out.
-- queryDescription :: Lens' (QueryDiagram v) Text
-- queryDescription = lens _queryDescription $ \s t -> s {_queryDescription = t}


-- | Activation script for query diagrams.
queryActivation :: (EntityClass v, HasDiagrams HadesRender v, Queryable v) =>
   Entity v
   -> ModelScript HadesRender v (QueryDiagram v) (Maybe Text)
queryActivation e = do
   openDiagram $ entityId e
   return Nothing


-- | Background menu for queries: just a test option.
queryBackgroundMenu ::
   (EntityClass v, Queryable v, Avatar p v w d, d ~ QueryElement v) => Point -> Menu (Action d)
queryBackgroundMenu _ = Menu [[
      menuItem "Test query" $ mkScriptAction $ do
            modelId <- queryContextId <$> use deltaContext
            liftBase (lift $ goToEntity modelId >> current) >>= \case
               Nothing -> liftBase $ lift $ throwUser "Query has been deleted."
               Just ent ->
                  case ent ^? entityContents . _QueryDiagram of
                     Nothing -> cannotHappen "Current entity does not contain a query." $ return ()
                     Just qd ->
                        void $ liftBase $ openDialog (queryTest qd) ()
            yieldViews
   ]]


-- | Convert a query diagram into a query. Returns the query and a list of warnings.
compileQueryDiagram :: (EntityClass v, Queryable v) =>
   Model v -> DiagramEntity (QueryElement v) -> Either [Text] (Query v)
compileQueryDiagram model = compile S.empty
   where
      {- Query diagrams are compiled as graphs. Nodes 1 and 2 are priviledged as the
         input and output nodes respectively. Additional edges are generated to link
         these to the input and output steps in the diagram, which are treated as normal
         no-op steps. See Query.Base.compileQuery for details. -}
      compile stack (DiagramEntity n d _) =
            if null errs
               then Right $ compileQuery steps (edges ++ mapMaybe ioLink elems)
               else Left $ concat errs
         where
            elems = d ^. diagramContents . to M.toList
            (errs, elements2) = partitionEithers $ map (compileElem stack queryName) elems
            (steps, edges) = partitionEithers elements2
            queryName = n ^. nameText
      -- compileElem :: (Named v, Reflective v) =>
      --    Set UUID -> Text -> (UUID, QueryElement v)
      --    -> Either [Text] (Either (UUID, QueryStep v) (UUID, UUID))
      compileElem stack queryName (uuid, QElemBox bx) = case bx ^. queryBoxData of
         Step rs -> Right $ Left (uuid, step rs)
         Filter txt -> case parse (topExpr entityFunctions ftable) "" txt of
            Right parsed -> Right $ Left (uuid, filterStep $ \extVals ent ->
                  case evaluate parsed (entityPropertiesWithStamps ent <> extVals) model of
                     Right (ExtBool b) -> Right b
                     Right ExtNone -> Right False
                     Right v -> Left $
                           queryName <> ": Filter formula returned " <> valueTypeName v <>
                              " instead of Bool."
                     Left msg -> Left msg
               )
            Left bundle ->
               Left $ map (T.pack . parseErrorTextPretty) $ NE.take 3 $ bundleErrors bundle
         FilterVariants vs -> Right $ Left (uuid, filterStep $ \_ ent ->
                  Right $ reflectiveName (ent ^. entityContents) `S.member` vs
               )
         SubQuery (Just subId) ->
            if subId `S.member` stack
               then Left [queryName <> ": Recursive subquery detected. Cannot run this."]
               else case modelContents model ^? ix subId . entityContents . _QueryDiagram of
                  Just qd -> case compile (S.insert subId stack) (qd ^. queryDiagram) of
                     Left subErrs -> Left subErrs
                     Right sq -> Right $ Left (uuid, subQuery sq)
                  Nothing -> Left [queryName <> ": SubQuery: Query does not exist (deleted?)."]
         SubQuery Nothing -> Left [queryName <> ": SubQuery: No query selected."]
         Variants vs ->
            let newEnts = fst $ runStep (filterVariants vs) [] model $
                     S.fromList $ map (entityId . snd) $ M.toList $ modelContents model
            in Right $ Left (uuid, QueryStep $ \_ _ inputs -> (S.union inputs newEnts, []))
         Entities uuids ->
            Right $ Left (uuid, QueryStep $ \_ _ inputs -> (S.union inputs uuids, []))
         Forwards tbl -> Right $ Left (uuid, foldr
               (inStep . \(v, (r1, r2)) -> followArrows (S.singleton v) r1 r2)
               noStep
               $ M.toList tbl)
         Backwards tbl -> Right $ Left (uuid, foldr
               (inStep . \(v, (r1, r2)) -> followArrows (S.singleton v) r2 r1)
               noStep
               $ M.toList tbl)
         Parent -> Right $ Left (uuid, parentStep)
         Children -> Right $ Left (uuid, childrenStep)
         InDiagram -> Right $ Left (uuid, inDiagramStep)
         FindDiagrams -> Right $ Left (uuid, findDiagramsStep)
         QueryInput -> Right $ Left (uuid, mempty)
         QueryOutput -> Right $ Left (uuid, mempty)
         QueryComment _ -> Right $ Left (uuid, mempty)
      compileElem _ queryName (_, QElemArrow a) = case a ^. queryArrowShape . lineEnds of
         (Connected _ uuid1 _, Connected _ uuid2 _) -> Right $ Right (uuid1, uuid2)
         (Unconnected {}, Connected {}) -> Left [queryName <> ": Line from nowhere."]
         (Connected {}, Unconnected {}) -> Left [queryName <> ": Line to nowhere."]
         (Unconnected {}, Unconnected {}) -> Left [queryName <> ": Floating line."]
      -- If the elmenent is an input or an output, generate the appropriate extra link with U.nil
      ioLink :: (UUID, QueryElement v) -> Maybe (UUID, UUID)
      ioLink (_, QElemArrow _) = Nothing
      ioLink (uuid, QElemBox bx) = case bx ^. queryBoxData of
         QueryInput -> Just (U.nil, uuid)
         QueryOutput -> Just (uuid, U.nil)
         _ -> Nothing
      ftable = withStampFields $ withBuiltIn $ modelFields model


queryDiagramToolbar :: (EntityClass v, Queryable v) =>
   DeltaToolbar (QueryElement v)
queryDiagramToolbar = DeltaToolbar [[
         DeltaTool "query-forwards" "Follow arrows forward" $
               DeltaToolAction $ newBoxTool $ Forwards mempty,
         DeltaTool "query-backwards" "Follow arrows backward" $
               DeltaToolAction $ newBoxTool $ Backwards mempty,
         DeltaTool "query-step" "Step to related entities" $
               DeltaToolAction $ newBoxTool $ Step mempty,
         DeltaTool "query-parent" "Parents of the entities in the model tree" $
               DeltaToolAction $ newBoxTool Parent,
         DeltaTool "query-children" "Children of the entities in the model tree" $
               DeltaToolAction $ newBoxTool Children,
         DeltaTool "query-in-diagram" "The contents of diagrams" $
               DeltaToolAction $ newBoxTool InDiagram,
         DeltaTool "query-find-diagrams" "All diagrams containing the entities" $
               DeltaToolAction $ newBoxTool FindDiagrams,
         DeltaTool "query-filter" "Filter entities with a formula" $
               DeltaToolAction $ newBoxTool $ Filter "",
         DeltaTool "query-filter-variants" "Filter entities by type" $
               DeltaToolAction $ newBoxTool $ FilterVariants mempty,
         DeltaTool "query-subquery" "Sub-query: run another query inside this one." $
               DeltaToolAction $ newBoxTool $ SubQuery Nothing
      ], [
         DeltaTool "query-variants" "Start with all entities of these types" $
               DeltaToolAction $ newBoxTool $ Variants mempty,
         DeltaTool "query-entities" "Start with these entities" $
               DeltaToolAction $ newBoxTool $ Entities mempty,
         DeltaTool "query-input" "Start with the entities that were inputs to this query" $
               DeltaToolAction $ newBoxTool QueryInput,
         DeltaTool "query-output" "Send results out from this query" $
               DeltaToolAction $ newBoxTool QueryOutput
      ], [
         DeltaTool "query-arrow" "Link query stages into a pipeline" $
               DeltaToolAction newArrowTool
      ], [
         DeltaTool "basic_comment" "Comment" $
               DeltaToolAction $ newBoxTool $ QueryComment ""
   ]]
   [alignmentTools]

newBoxTool :: (EntityClass v, Queryable v) =>
   QueryData v -> Action (QueryElement v)
newBoxTool qd = mkScriptAction $ do
      let mkBox dId s = QElemBox . QueryBox dId (Rectangle s)
      newDiagramBoxTool qd queryDataDialog mkBox qbPaint msg
   where
      qbPaint (QElemBox (QueryBox _ shape _)) = do
         drawPolygon $ rectangleCorners shape
         textBlock boxLabel [] $ shapeBounds shape
      qbPaint _ = cannotHappen "Query newBoxTool: produced something other than a box." $ return ()
      msg = "Added `" <> boxLabel <> "' to query diagram"
      boxLabel = queryBoxLabel $ QueryBox U.nil (Rectangle NoBox) qd


newArrowTool :: (EntityClass v, Queryable v) => Action (QueryElement v)
newArrowTool = mkScriptAction $
      newDiagramArrowTool () Nothing mkArrow "Added arrow to query diagram"
   where
      mkArrow dId shape _ = QElemArrow $ QueryArrow dId shape


-- | Extract the forest from the model and select all the members of the argument set.
queryForest :: (EntityClass v, Queryable v) =>
   Model v -> Forest (Text, Maybe Text, Maybe ModelId)
queryForest = modelFilter $ isJust . preview (entityContents . _QueryDiagram)


-- | A dialog that displays the inputs and outputs of a query. Returns the selected inputs.
queryTest :: (EntityClass v, Queryable v) =>
   QueryDiagram v -> DialogSelector' (Model v) () ()
queryTest qd model () = Just $
      case compileQueryDiagram model $ qd ^. queryDiagram of
         Left errs -> errorDialog $
            qname <> " contains the following errors:\n\n" <> T.intercalate "\n" errs
         Right query -> Dialog ("Test of " <> qname) (CloseButton "_Close") $ proc () -> do
            inputs <- simpleFrame "Inputs" $
                  modelSubsetSpecifier (const validInput) (const Nothing) -< mempty
            (rs, ws) <- arr $ runQuery query [] model -< inputs
            _ <- simpleFrame "Outputs" $ readOnlyMemo MemoSmall True showEntitySet -< rs
            _ <- simpleFrame "Warnings" $
                  readOnlyMemo MemoSmall True (const $ T.intercalate "\n") -< ws
            returnA -< ()
   where
      validInput e = S.member (e ^. entityContents . to reflectiveName) $ qd ^. queryInputs
      qname = qd ^. queryDiagram . diagramEntityName . nameText
      errorDialog msg = Dialog "Query test error" (CloseButton "_Close") $ message1 msg


-- | Take a set of entities and display them in a gadget. Double-clicks will activate the
-- entity, right clicks will pull up the "editableDisplayMenu".
clickableEntitySet :: (Editable p v, EntityClass v) =>
   Gadget' (Model v) (ModelScript p v v (Maybe Text)) (Set ModelId)
clickableEntitySet = proc s -> do
      model <- getEnv -< ()
      let ents = map clickableEntity $ mapMaybe (`M.lookup` modelContents model) $ S.toList s
      _ <- send id <<< clickableList -< ents
      returnA -< s
   where
      clickableEntity e = ClickableItem {
            clickText = e ^. entityName . nameText,
            clickSingle = Nothing,
            clickDouble = Just $ editableActivation $ Just e,
            clickMenu = editableDisplayMenu $ Just e
         }
