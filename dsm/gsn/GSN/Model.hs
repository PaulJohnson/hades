{-# LANGUAGE UndecidableInstances #-}

{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}


{- |

This module is intended to be imported qualified. For instance:

> import qualified GSN.Model as GSN
-}

module GSN.Model (
   -- * Diagram Type
   HasGsn (..),
   GsnDiagramType,
   gsnIsDiagram,
   gsnDiagramWrapper,
   gsnDiagramEntityTypes,
   BoxType (..),
   ArrowType (..),
   ModelSubtype (..),
   GsnElement,
   -- * Nodes
   GsnNode (..),
   createGoal,
   createStrategy,
   createSolution,
   createContext,
   createAssumption,
   createJustification,
   createGsnOption,
   gsnNodeType,
   gsnNodeVariant,
   gsnNodeName,
   gsnNodeText,
   gsnInstantiated,
   gsnDeveloped,
   drawGsnBoxRaw,
   -- * Edges
   GsnEdgeType (..),
   GsnEdge (GsnEdge, gsnEdgeType),
   gsnEdgeVariant,
   gsnEdgeName,
   -- * GSN Model Entities
   GsnItem,
   _GsnNodeItem,
   _GsnEdgeItem,
   _GsnDiagramItem,
   gsnItemName,
   GsnScript,
   gsnActivation,
   gsnEntityMenu,
   gsnAddEntityMenu,
   gsnToolbar,
   gsnStockItem,
   -- * Utilities
   dottedNumbers,
   gsnIcons
) where

import App.Appearance
import App.GenericDiagram
import Control.Applicative
import Control.Lens hiding ((.=), Context)
import Control.Monad
import Control.Monad.Trans.Class
import Data.Aeson
import Data.Char
import Data.Either
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Evidence.Model
import qualified GI.Cairo.Render as Cairo
import GSN.FieldNames
import Hades.Abstract
import Hades.GI.BasicShapes
import Model.Abstract.DiagramType
import Model.Abstract.PackageTree as P
import Model.Lookups.Base
import Model.Query.Diagram
import qualified Model.Reflection.NamedRelation as NR
import Model.Reflection.References
import Model.Reflection.Reflective
import Model.Reflection.Types
import Model.Reflection.Values
import Model.Report.Base
import Reactive.Banana.ArrowDialog (constantDialog)
import Reactive.Banana.Common
import Reactive.Banana.Menu
import Reactive.Banana.GI.ErrorBox


-- | Model types which include GSN. Used to gain access to GSN entities from other diagram types.
class HasGsn v where
   _Gsn :: Prism' v (GsnItem v)


-- | Unit type that exists for instances.
data GsnDiagramType v


type GsnElement v = Element (GsnDiagramType v) v

instance (
      AppearanceClass (GsnDiagramType v) v,
      Editable HadesRender v,
      EntityClass v,
      HasDiagrams HadesRender v,
      HasEvidence v,
      HasGsn v,
      HasLookups v,
      HasReports v) =>
   DiagramTypeClass (GsnDiagramType v) v where
      data BoxType (GsnDiagramType v) =
         GoalBox | StrategyBox | SolutionBox | ContextBox | AssumptionBox | JustificationBox
         | OptionBox | EvidenceBox deriving (Eq, Ord, Read, Show)
      data ArrowType (GsnDiagramType v) =
         GsnArrow GsnEdgeType | TraceArrow
            deriving (Eq, Ord, Read)
      data ModelSubtype (GsnDiagramType v) =
         GsnNodeItem GsnNode
         | GsnEdgeItem GsnEdge
         | (HasDiagrams HadesRender v) => GsnDiagramItem (DiagramEntity (GsnElement v))
      mkBox diagId modelId typ peg bound = case typ of
         GoalBox -> Box diagId modelId typ peg $ Rectangle bound
         StrategyBox -> Box diagId modelId typ peg $ Parallelogram bound
         SolutionBox -> Box diagId modelId typ peg $ Ellipse bound
         ContextBox -> Box diagId modelId typ peg $ Oval bound
         AssumptionBox -> Box diagId modelId typ peg $ Ellipse bound
         JustificationBox -> Box diagId modelId typ peg $ Ellipse bound
         OptionBox -> Box diagId modelId typ peg $ Diamond bound
         EvidenceBox -> Box diagId modelId typ peg $ Rectangle bound
      boxTypeDraw = drawGsnBox
      boxChildren _ = return []  -- GSN boxes have no children.
      boxChildConnection _ _ = return id
      boxCanPeg _ _ = return Nothing
      avatarType = gsnAvatarType
      diagramTypeName = const "GSN"
      arrowTailRelation TraceArrow = traceTailRelation
      arrowTailRelation (GsnArrow _) = NR.edgeFromRelation
      arrowHeadRelation TraceArrow = traceHeadRelation
      arrowHeadRelation (GsnArrow _) = NR.edgeToRelation
      arrowTailExportRelation = const Nothing
      arrowHeadExportRelation = const Nothing
      arrowHeadDraw _ (GsnArrow ContextArrow) = closedArrowHead 30 10 False
      arrowHeadDraw _ (GsnArrow SupportArrow) = closedArrowHead 30 10 True
      arrowHeadDraw _ TraceArrow = traceArrowHead
      arrowDecorations (Just (GsnEdgeItem e)) _ = gsnArrowDecorations e
      arrowDecorations _ _ = []
      arrowLineDashes (GsnArrow _) = []
      arrowLineDashes TraceArrow = traceArrowDash
      arrowCanConnect d arrow box _ = gsnArrowCanConnect d arrow box  -- Child argument ignored.

instance (
      AppearanceClass (GsnDiagramType v) v,
      Editable HadesRender v,
      EntityClass v,
      HasDiagrams HadesRender v,
      HasEvidence v,
      HasGsn v,
      HasLookups v,
      HasReports v) =>
   Eq (ModelSubtype (GsnDiagramType v)) where
      GsnNodeItem n1 == GsnNodeItem n2        = n1 == n2
      GsnEdgeItem e1 == GsnEdgeItem e2        = e1 == e2
      GsnDiagramItem d1 == GsnDiagramItem d2  = d1 == d2
      _ == _  = False

instance ToJSON (BoxType (GsnDiagramType v)) where
   toJSON GoalBox = Data.Aeson.String "Goal"
   toJSON StrategyBox = Data.Aeson.String "Strategy"
   toJSON SolutionBox = Data.Aeson.String "Solution"
   toJSON ContextBox = Data.Aeson.String "Context"
   toJSON AssumptionBox = Data.Aeson.String "Assumption"
   toJSON JustificationBox= Data.Aeson.String "Justification"
   toJSON OptionBox = Data.Aeson.String "Option"
   toJSON EvidenceBox = Data.Aeson.String "Evidence"

instance FromJSON (BoxType (GsnDiagramType v)) where
   parseJSON = withText "GSN box type" $ \case
      "Goal" -> return GoalBox
      "Strategy" -> return StrategyBox
      "Solution" -> return SolutionBox
      "Context" -> return ContextBox
      "Assumption" -> return AssumptionBox
      "Justification" -> return JustificationBox
      "Option" -> return OptionBox
      "Evidence" -> return EvidenceBox
      s -> fail $ "Expected GSN box type. Got " <> show s

instance Show (ArrowType (GsnDiagramType v)) where
   show (GsnArrow ContextArrow) = T.unpack $ contextArrowVariant ^. variantName
   show (GsnArrow SupportArrow) = T.unpack $ supportArrowVariant ^. variantName
   show TraceArrow = T.unpack $ traceVariant ^. variantName

instance ToJSON (ArrowType (GsnDiagramType v)) where
   toJSON (GsnArrow a) = toJSON a
   toJSON TraceArrow = Data.Aeson.String "Trace"

instance FromJSON (ArrowType (GsnDiagramType v)) where
   parseJSON v = (GsnArrow <$> parseJSON v) <|> parseTrace v
      where
         parseTrace = withText "Trace" $ \case
            "Trace" -> return TraceArrow
            s -> fail $ "Not a GSN Arrow type: " <> show s

instance (
      AppearanceClass (GsnDiagramType v) v,
      Editable HadesRender v,
      EntityClass v,
      HasDiagrams HadesRender v,
      HasEvidence v,
      HasGsn v,
      HasLookups v,
      HasReports v) =>
   ToJSON (ModelSubtype (GsnDiagramType v)) where
      toJSON (GsnNodeItem n) = toJSON n
      toJSON (GsnEdgeItem e) = toJSON e
      toJSON (GsnDiagramItem d) = toJSON d

instance (
      AppearanceClass (GsnDiagramType v) v,
      Editable HadesRender v,
      EntityClass v,
      HasDiagrams HadesRender v,
      HasEvidence v,
      HasGsn v,
      HasLookups v,
      HasReports v) =>
   FromJSON (ModelSubtype (GsnDiagramType v)) where
      parseJSON v =
            GsnNodeItem <$> parseJSON v
            <|> GsnEdgeItem <$> parseJSON v
            <|> GsnDiagramItem <$> parseJSON v

instance (
      AppearanceClass (GsnDiagramType v) v,
      Editable HadesRender v,
      EntityClass v,
      HasDiagrams HadesRender v,
      HasEvidence v,
      HasGsn v,
      HasLookups v,
      HasReports v) =>
   Reflective (ModelSubtype (GsnDiagramType v)) where
      reflectiveName (GsnNodeItem n) = gsnNodeVariant n
      reflectiveName (GsnEdgeItem e) = gsnEdgeVariant $ gsnEdgeType e
      reflectiveName (GsnDiagramItem d) = variantCoerce $ reflectiveName d
      reflectiveDefaults = [
               GsnNodeItem $ Goal (Name "") "" True True,
               GsnNodeItem $ Strategy (Name "") "" True True,
               GsnNodeItem $ Solution (Name "") "" True,
               GsnNodeItem $ Context (Name "") "" True,
               GsnNodeItem $ Assumption (Name "") "" True,
               GsnNodeItem $ Justification (Name "") "" True,
               GsnNodeItem $ GsnOption (Name "") "" True,
               mkEdge ContextArrow, mkEdge SupportArrow,
               GsnDiagramItem (DiagramEntity (Name "") mempty mempty)
            ]
         where
            mkEdge typ = GsnEdgeItem (GsnEdge typ (Name "") CardinalityOne)
      reflectiveBuiltIn (GsnNodeItem Goal {}) =
            [nameField, descriptionField, instantiatedField, developedField]
      reflectiveBuiltIn (GsnNodeItem Strategy {}) =
           [nameField, descriptionField, instantiatedField, developedField]
      reflectiveBuiltIn (GsnNodeItem _) = [nameField, descriptionField, instantiatedField]
      reflectiveBuiltIn (GsnEdgeItem _) = [nameField, cardinalityField]
      reflectiveBuiltIn _ = [nameField]
      reflectiveGet  (GsnNodeItem node) = M.fromList $
            [(nameField, ExtText $ node ^. gsnNodeName . nameText),
             (descriptionField, ExtText $ node ^. gsnNodeText),
             (instantiatedField, ExtBool $ node ^. gsnInstantiated)] ++
            maybe [] (\d -> [(developedField, ExtBool d)]) (node ^? gsnDeveloped)
      reflectiveGet (GsnEdgeItem (GsnEdge _ (Name nm) card)) = M.fromList [
            (nameField, ExtText nm),
            (cardinalityField, ExtText $ cardinalityToText card)]
      reflectiveGet (GsnDiagramItem (DiagramEntity (Name nm) _ _)) =
         M.singleton nameField $ ExtText nm
      reflectiveSet (GsnNodeItem n) = do
         nm <- set gsnNodeName . Name <$> extract (n ^. gsnNodeName . nameText) _ExtText nameField
         txt <- set gsnNodeText <$> extract (n ^. gsnNodeText) _ExtText descriptionField
         inst <- set gsnInstantiated <$> extract (n ^. gsnInstantiated) _ExtBool instantiatedField
         dev <- case n ^? gsnDeveloped of
            Just d -> set gsnDeveloped <$> extract d _ExtBool developedField
            Nothing -> return id
         return $ GsnNodeItem . nm . txt . inst . dev $ n
      reflectiveSet (GsnEdgeItem e) =
         GsnEdgeItem <$> (GsnEdge (gsnEdgeType e) <$>
            (Name . T.strip <$>
               extract (e ^. gsnEdgeName . nameText) _ExtText nameField) <*>
            (fromMaybe CardinalityOne . cardinalityFromText <$>
               extract (e ^. gsnEdgeCard . re cardinalityText) _ExtText cardinalityField))
      reflectiveSet (GsnDiagramItem d) = do
         nm <- Name . T.strip <$> extract (d ^. diagramEntityName . nameText) _ExtText nameField
         return $ GsnDiagramItem $ diagramEntityName .~ nm $ d
      reflectiveBuiltInRefs =
            arrowRelations contextArrowVariant
               [goalVariant, strategyVariant, gsnOptionVariant]
               [contextVariant, assumptionVariant, justificationVariant, gsnOptionVariant]
            <> arrowRelations supportArrowVariant
               [goalVariant, strategyVariant, gsnOptionVariant]
               [goalVariant, strategyVariant, solutionVariant, gsnOptionVariant]
            <> traceArrowRelations [solutionVariant] []
      reflectiveArrows = M.fromList [
            (contextArrowVariant, (NR.edgeFromRelation, NR.edgeToRelation)),
            (supportArrowVariant, (NR.edgeFromRelation, NR.edgeToRelation))
         ]


-- | True if this is a GSN diagram.
gsnIsDiagram :: (
      EntityClass v,
      HasDiagrams HadesRender v,
      HasEvidence v,
      HasGsn v,
      Queryable v) =>
   ModelSubtype (GsnDiagramType v) -> Bool
gsnIsDiagram GsnDiagramItem {} = True
gsnIsDiagram _ = False


-- | The wrapper for GSN diagrams.
gsnDiagramWrapper :: (
      AppearanceClass (GsnDiagramType v) v,
      Editable HadesRender v,
      EntityClass v,
      HasDiagrams HadesRender v,
      HasEvidence v,
      HasGsn v,
      HasLookups v,
      HasReports v,
      Queryable v) =>
   Entity v -> ModelEdit v (ModelSubtype (GsnDiagramType v)) (Maybe (EntityWrapper HadesRender v))
gsnDiagramWrapper ent =
   usingTraversal $ \trv ->
      case ent ^? entityContents . trv . _GsnDiagramItem of
         Nothing -> return Nothing
         Just de -> do
            ctx <- compileAppearance
                  (de ^. diagramEntityData) $
                  GenericContext (entityId ent) mempty
            return $ Just $ DiagramWrapper {
                  wrappedEntityId = entityId ent,
                  diagramToEdit = de ^. diagramEntityContents,
                  diagramEntityTraversal = _GsnDiagramItem,
                  diagramTraversal = trv,
                  diagramDialog = elementDialog,
                  diagramContext = ctx,
                  diagramMenu = Nothing,
                  diagramToolbar = const $ gsnToolbar (entityId ent) $ ent ^. entityParent
               }

-- | Entities shown on GSN diagrams.
gsnDiagramEntityTypes :: (HasDiagrams HadesRender v) => DiagramEntityTypes v
gsnDiagramEntityTypes = M.fromList [
      (gsnDiagramVariant, [goalVariant, strategyVariant, solutionVariant, contextVariant,
         assumptionVariant, justificationVariant, evidenceVariant])
   ]


data GsnNode =
   Goal {
         _gsnNodeName :: Name,
         _gsnNodeText :: Text,
         _gsnDeveloped :: Bool,
         _gsnInstantiated :: Bool }
   | Strategy {
         _gsnNodeName :: Name,
         _gsnNodeText :: Text,
         _gsnDeveloped :: Bool,
         _gsnInstantiated :: Bool }
   | Solution {_gsnNodeName :: Name, _gsnNodeText :: Text, _gsnInstantiated :: Bool}
   | Context {_gsnNodeName :: Name, _gsnNodeText :: Text, _gsnInstantiated :: Bool}
   | Assumption {_gsnNodeName :: Name, _gsnNodeText :: Text, _gsnInstantiated :: Bool}
   | Justification {_gsnNodeName :: Name, _gsnNodeText :: Text, _gsnInstantiated :: Bool}
   | GsnOption {_gsnNodeName :: Name, _gsnNodeText :: Text, _gsnInstantiated :: Bool}
   deriving (Eq, Show)

instance ToJSON GsnNode where
   toJSON (Goal n t d i) =
      object ["type" .= ("Goal" :: Text), "name" .= n, "text" .= t,
            "developed" .= d, "instantiated" .= i]
   toJSON (Strategy n t d i) =
      object ["type" .= ("Strategy" :: Text), "name" .= n, "text" .= t,
            "developed" .= d, "instantiated" .= i]
   toJSON (Solution n t i) =
      object ["type" .= ("Solution" :: Text), "name" .= n, "text" .= t, "instantiated" .= i]
   toJSON (Context n t i) =
      object ["type" .= ("Context" :: Text), "name" .= n, "text" .= t, "instantiated" .= i]
   toJSON (Assumption n t i) =
      object ["type" .= ("Assumption" :: Text), "name" .= n, "text" .= t, "instantiated" .= i]
   toJSON (Justification n t i) =
      object ["type" .= ("Justification" :: Text), "name" .= n, "text" .= t, "instantiated" .= i]
   toJSON (GsnOption n t i) =
      object ["type" .= ("Option" :: Text), "name" .= n, "text" .= t, "instantiated" .= i]

instance FromJSON GsnNode where
   parseJSON = withObject "GSN Node" $ \v -> v .: "type" >>= \case
      "Goal" -> Goal <$>
            v .: "name" <*>
            v .: "text" <*>
            v .:? "developed" .!= True <*>
            v .:? "instantiated" .!= True
      "Strategy" -> Strategy <$>
            v .: "name" <*>
            v .: "text" <*>
            v .:? "developed" .!= True <*>
            v .:? "instantiated" .!= True
      "Solution" ->
         Solution <$> v .: "name" <*> v .: "text" <*> v .:? "instantiated" .!= True
      "Context" ->
         Context <$> v .: "name" <*> v .: "text" <*> v .:? "instantiated" .!= True
      "Assumption" ->
         Assumption <$> v .: "name" <*> v .: "text" <*> v .:? "instantiated" .!= True
      "Justification" ->
         Justification <$> v .: "name" <*> v .: "text" <*> v .:? "instantiated" .!= True
      "Option" ->
         GsnOption <$> v .: "name" <*> v .: "text" <*> v .:? "instantiated" .!= True
      str -> fail $ "Unknown GSN node type: " <> T.unpack str

gsnNodeName :: Lens' GsnNode Name
gsnNodeName = lens _gsnNodeName $ \n txt -> n{_gsnNodeName = txt}

gsnNodeText :: Lens' GsnNode Text
gsnNodeText = lens _gsnNodeText $ \n txt -> n{_gsnNodeText = txt}

gsnInstantiated :: Lens' GsnNode Bool
gsnInstantiated = lens _gsnInstantiated $ \n b -> n{_gsnInstantiated = b}

gsnDeveloped :: Traversal' GsnNode Bool
gsnDeveloped f (Goal n t d i) = (\d' -> Goal n t d' i) <$> f d
gsnDeveloped f (Strategy n t d i) = (\d' -> Strategy n t d' i) <$> f d
gsnDeveloped _ v = pure v


-- | Create a goal with an appropriate default name to be added as a child of the current node.
-- Does not actually add the new entity.
createGoal :: (EntityClass v) => ModelEdit v w GsnNode
createGoal = do
   nums <- dottedNumbers <$> currentName
   let nm = Name $ "G" <> if T.null nums then nums else nums <> "."
   return $ Goal nm "" True True

-- | Create a strategy with an appropriate default name to be added as a child of the current
-- entity. Does not actually add the new strategy.
createStrategy :: (EntityClass v) => ModelEdit v w GsnNode
createStrategy = do
   nums <- dottedNumbers <$> currentName
   let nm = Name $ "S" <> if T.null nums then nums else nums <> "."
   return $ Strategy nm "" True True

-- | Create a solution with an appropriate default name to be added as a child of the current
-- entity. Does not actually add the new solution.
createSolution :: (EntityClass v) => ModelEdit v w GsnNode
createSolution = do
   nums <- dottedNumbers <$> currentName
   let nm = Name $ "Sn" <> if T.null nums then nums else nums <> "."
   return $ Solution nm "" True

-- | Create a context with an appropriate default name to be added as a child of the current
-- entity. Does not actually add the new context.
createContext :: (EntityClass v) => ModelEdit v w GsnNode
createContext = do
   nums <- dottedNumbers <$> currentName
   let nm = Name $ "C" <> if T.null nums then nums else nums <> "."
   return $ Context nm "" True

-- | Create an assumption with an appropriate default name to be added as a child of the current
-- entity. Does not actually add the new assumption.
createAssumption :: (EntityClass v) => ModelEdit v w GsnNode
createAssumption = do
   nums <- dottedNumbers <$> currentName
   let nm = Name $ "A" <> if T.null nums then nums else nums <> "."
   return $ Assumption nm "" True

-- | Create a justification with an appropriate default name to be added as a child of the current
-- entity. Does not actually add the new justification.
createJustification :: (EntityClass v) => ModelEdit v w GsnNode
createJustification = do
   nums <- dottedNumbers <$> currentName
   let nm = Name $ "J" <> if T.null nums then nums else nums <> "."
   return $ Justification nm "" True

-- | Create an option with an appropriate default name to be added as a child of the current
-- entity. Does not actually add the new option.
createGsnOption :: (EntityClass v) => ModelEdit v w GsnNode
createGsnOption = do
   nums <- dottedNumbers <$> currentName
   let nm = Name $ "Opt" <> if T.null nums then nums else nums <> "."
   return $ GsnOption nm "" True


-- | The box type for each node type.
gsnNodeType :: GsnNode -> BoxType (GsnDiagramType v)
gsnNodeType Goal {} = GoalBox
gsnNodeType Strategy {} = StrategyBox
gsnNodeType Solution {} = SolutionBox
gsnNodeType Context {} = ContextBox
gsnNodeType Assumption {} = AssumptionBox
gsnNodeType Justification {} = JustificationBox
gsnNodeType GsnOption {} = OptionBox


-- | The variant name for a node.
gsnNodeVariant :: GsnNode -> Variant a
gsnNodeVariant Goal {} = goalVariant
gsnNodeVariant Strategy {} = strategyVariant
gsnNodeVariant Solution {} = solutionVariant
gsnNodeVariant Context {} = contextVariant
gsnNodeVariant Assumption {} = assumptionVariant
gsnNodeVariant Justification {} = justificationVariant
gsnNodeVariant GsnOption {} = gsnOptionVariant


-- | The GSN shape decoration is a list of polygons.
gsnBoxDecoration ::
   BoundBox   -- ^ The shape definition.
   -> Bool    -- ^ Instantiated flag.
   -> Bool    -- ^ Developed flag.
   -> [[Point]]
gsnBoxDecoration NoBox _ _ = []
gsnBoxDecoration (BoundBox (Point x1 _) (Point x2 y)) instantiated developed =
         let
            decoSize = 20  -- Size of the box
            basePoint = Point ((x1 + x2) / 2) y  -- mid point of the bottom of the box.
            decoBox = BoundBox
               (basePoint `movePoint` (-decoSize / 2, 0))
               (basePoint `movePoint` (decoSize / 2, decoSize))
            diamond@[p1, p2, p3, p4] = diamondCorners decoBox  -- Safe because this is not NoBox
               -- Starting from the left corner going clockwise.
         in case (instantiated, developed) of
            (True, True) -> []
            (False, True) -> [diamond]
            (True, False) -> [[p1, p2, p3]]  -- Triangle from upper half of diamond.
            (False, False) -> [[p1, p2, p3], [p1, p3, p4]]  -- Diamond formed by two triangles.


-- | Draw the box, but override its position and size with the BoundBox argument. This can also
-- draw evidence boxes.
drawGsnBox :: (
      AppearanceClass (GsnDiagramType v) v,
      Editable HadesRender v,
      HasDiagrams HadesRender v,
      HasEvidence v,
      HasGsn v,
      HasLookups v,
      HasReports v) =>
   Box (GsnDiagramType v) v
   -> HadesLayout ()   -- ^ The contents of the box
   -> Maybe Colour     -- ^ The border colour to use (@Nothing@ means default, i.e. black.)
   -> Delta (GsnElement v) (BoundBox -> HadesRender ())
drawGsnBox box@(Box _ _ _ _ shape) contents border = do
      prsm <- liftBase $ lift $ usingTraversal return
      ent <- liftBase $ lift $ goToEntity (boxEntity box) >> current
      return $ maybe id withLineColour border <$> case boxType box of
         EvidenceBox -> drawEvidence ent contents border
         typ -> drawGsnBoxRaw
               typ
               (ent ^? _Just . entityContents . prsm . _GsnNodeItem)
               contents
               border
               shape


-- | Draw a GSN box. The redundant arguments for @shape@ and @BoundBox@ make this useful
-- in the "boxTypeDraw" function of "DiagramTypeClass".
drawGsnBoxRaw :: (BoxShapeClass shape) =>
   BoxType (GsnDiagramType v)
   -> Maybe GsnNode  -- ^ Source for the text to write in the box. @Nothing@ is assumed to be a Goal.
   -> HadesLayout ()  -- ^ Contents of the box.
   -> Maybe Colour  -- ^ Border colour, if not black.
   -> shape   -- ^ Shape to use for figuring out where the text should go.
   -> BoundBox  -- ^ Definition of the shape size and position.
   -> HadesRender ()
drawGsnBoxRaw typ mNode contents border shape1 boundBox = do
      let
         instantiated = fromMaybe True $ mNode ^? _Just . gsnInstantiated
         developed = fromMaybe True $ mNode ^? _Just . gsnDeveloped
         shape2 = shapeDefinition .~ boundBox $ shape1
      maybe id withLineColour border $ case typ of
         GoalBox -> drawPolygon $ rectangleCorners $ Rectangle boundBox
         StrategyBox -> drawPolygon $ parallelogramCorners $ Parallelogram boundBox
         SolutionBox -> drawEllipse $ Ellipse boundBox
         ContextBox -> drawOval $ Oval boundBox
         AssumptionBox -> do
            drawEllipse $ Ellipse boundBox
            drawCornerLetter boundBox 'A'
         JustificationBox -> do
            drawEllipse $ Ellipse boundBox
            drawCornerLetter boundBox 'J'
         OptionBox -> drawPolygon $ diamondCorners boundBox
         _ -> return ()  -- Should never happen.
      mapM_ drawPolygon $ gsnBoxDecoration boundBox instantiated developed
      runLayout_ (shapeInnerBox shape2) $ do
         case mNode of
            Just node -> layoutTitle Nothing $ node ^. gsnNodeName . nameText
            Nothing -> layoutTitle Nothing $ blankBoxName typ
         layoutParagraphGap
         contents
   where
      blankBoxName GoalBox = "Goal"
      blankBoxName StrategyBox = "Strategy"
      blankBoxName SolutionBox = "Solution"
      blankBoxName ContextBox = "Context"
      blankBoxName AssumptionBox = "Assumption"
      blankBoxName JustificationBox= "Justification"
      blankBoxName OptionBox = "Option"
      blankBoxName EvidenceBox = "Evidence"




-- | A GSN Edge is  the semantic content of an arrow on a GSN diagram.
data GsnEdgeType = ContextArrow | SupportArrow
   deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToJSON GsnEdgeType where
   toJSON ContextArrow = Data.Aeson.String "Context"
   toJSON SupportArrow = Data.Aeson.String "Support"

instance FromJSON GsnEdgeType where
   parseJSON = withText "GSN Edge Type" $ \case
      "Context" -> return ContextArrow
      "Support" -> return SupportArrow
      s -> fail $ "Not a GSN Edge Type: " <> show s


gsnEdgeVariant :: GsnEdgeType -> Variant a
gsnEdgeVariant ContextArrow = contextArrowVariant
gsnEdgeVariant SupportArrow = supportArrowVariant


-- | An arrow in a GSN diagram is an avatar for a GsnEdge entity. The only information this
-- records is the type of the arrow and what (if anything) it connects. Lack of connection
-- is indicated by a nil UUID.
--
-- If an arrow is connected then it should also have a corresponding entry in the model relations
-- table.
data GsnEdge = GsnEdge {
      gsnEdgeType :: GsnEdgeType,
      _gsnEdgeName :: Name,
      _gsnEdgeCard :: Cardinality
   } deriving (Eq, Show)


instance ToJSON GsnEdge where
   toJSON e = object [
         "type" .= gsnEdgeType e,
         "name" .= (e ^. gsnEdgeName),
         "card" .= (e ^. gsnEdgeCard)
      ]

instance FromJSON GsnEdge where
   parseJSON = withObject "GSN relationship" $ \v ->
      GsnEdge <$>
         v .: "type" <*>
         v .: "name" <*>
         v .:? "card" .!= CardinalityOne


gsnEdgeName :: Lens' GsnEdge Name
gsnEdgeName = lens _gsnEdgeName $ \v n -> v{_gsnEdgeName = n}

gsnEdgeCard :: Lens' GsnEdge Cardinality
gsnEdgeCard = lens _gsnEdgeCard $ \v c -> v{_gsnEdgeCard = c}


gsnArrowCanConnect :: (
      AppearanceClass (GsnDiagramType v) v,
      Editable HadesRender v,
      EntityClass v,
      HasDiagrams HadesRender v,
      HasEvidence v,
      HasGsn v,
      HasLookups v,
      HasReports v) =>
   ConnectionDirection
   -> Arrow (GsnDiagramType v) v
   -> Box (GsnDiagramType v) v
   -> Delta (GsnElement v) Bool
gsnArrowCanConnect connection arrow box = do
      let targetType = boxType box
      case arrowType arrow of
         GsnArrow ContextArrow -> return $ case connection of
            ConnectionFrom ->
               targetType `elem` [GoalBox, StrategyBox, OptionBox]
            ConnectionTo ->
               targetType `elem` [ContextBox, AssumptionBox, JustificationBox, OptionBox]
         GsnArrow SupportArrow -> do
            let
               shape = arrow ^. arrowShape
               (start, end) = shape ^. lineEnds
            startT <- terminatorType start
            endT <- terminatorType end
            return $ case connection of
               ConnectionFrom -> case endT of
                  Nothing -> targetType `elem` [GoalBox, StrategyBox, OptionBox]
                  Just GoalBox -> targetType `elem` [GoalBox, StrategyBox, OptionBox]
                  Just StrategyBox -> targetType `elem` [GoalBox, OptionBox]
                  Just SolutionBox -> targetType `elem` [GoalBox, OptionBox]
                  Just OptionBox -> targetType `elem` [GoalBox, StrategyBox]
                  _ -> False
               ConnectionTo -> case startT of
                  Nothing ->
                     targetType `elem` [GoalBox, StrategyBox, SolutionBox, OptionBox]
                  Just GoalBox ->
                     targetType `elem` [GoalBox, StrategyBox, SolutionBox, OptionBox]
                  Just StrategyBox ->
                     targetType `elem` [GoalBox, OptionBox]
                  Just OptionBox ->
                     targetType `elem` [GoalBox, StrategyBox, SolutionBox]
                  _ ->
                     False
         TraceArrow -> return $ case connection of
            ConnectionFrom -> targetType == SolutionBox
            ConnectionTo -> targetType == EvidenceBox
   where
      terminatorType Unconnected{} = return Nothing
      terminatorType (Connected _ target _) = fmap join $ withItem target $ \case
         ElemBox b -> return $ Just $ boxType b
         _ -> return Nothing

-- | Draw the cardinality as a decoration.
gsnArrowDecorations :: GsnEdge -> [ArrowDecoration]
gsnArrowDecorations edge = case edge ^. gsnEdgeCard of
      CardinalityOne -> []
      CardinalityOpt -> [ArrowDecoration 0.75 (circle False) Nothing]
      CardinalityMany -> [ArrowDecoration 0.75 (circle True) $ Just $ sideText "0:n"]
      CardinalityMany1 -> [ArrowDecoration 0.75 (circle True) $ Just $ sideText "1:n"]
   where
      circle solid = lift $ do
         Cairo.save
         Cairo.moveTo circleRadius 0
         Cairo.arc 0 0 circleRadius 0  (2*pi)
         Cairo.closePath
         if solid then Cairo.setSourceRGB 0 0 0 else Cairo.setSourceRGB 1 1 1
         Cairo.fillPreserve
         Cairo.setSourceRGB 0 0 0
         Cairo.stroke
         Cairo.restore
      sideText str =
         (dim, runLayout_ (BoundBox (Point 5 3) (Point (width-10) (height-6))) $ layoutText [] str)
      circleRadius = 4
      dim@(width, height) = (32,22)  -- Fixed size for text. Quick and dirty solution.

-- | GSN Model entities: nodes, edges and diagrams.
type GsnItem v = ModelSubtype (GsnDiagramType v)

_GsnNodeItem :: Prism' (GsnItem v) GsnNode
_GsnNodeItem = prism GsnNodeItem $ \case {GsnNodeItem n -> Right n; v -> Left v}

_GsnEdgeItem :: Prism' (GsnItem v) GsnEdge
_GsnEdgeItem = prism GsnEdgeItem $ \case {GsnEdgeItem e -> Right e; v -> Left v}

_GsnDiagramItem :: (
      EntityClass v,
      HasDiagrams HadesRender v,
      HasEvidence v,
      HasGsn v,
      Queryable v) =>
   Prism' (GsnItem v) (DiagramEntity (GsnElement v))
_GsnDiagramItem = prism GsnDiagramItem $ \case {GsnDiagramItem d -> Right d; v -> Left v}


gsnItemName :: (
      EntityClass v,
      HasDiagrams HadesRender v,
      HasEvidence v,
      HasGsn v,
      Queryable v) =>
   Lens' (GsnItem v) Name
gsnItemName = lens getName setName
   where
      getName (GsnNodeItem n) = n ^. gsnNodeName
      getName (GsnEdgeItem e) = e ^. gsnEdgeName
      getName (GsnDiagramItem d) = d ^. diagramEntityName
      setName (GsnNodeItem node) nm = GsnNodeItem $ gsnNodeName .~ nm $ node
      setName (GsnEdgeItem e) nm = GsnEdgeItem $ gsnEdgeName .~ nm $ e
      setName (GsnDiagramItem d) nm = GsnDiagramItem $ diagramEntityName .~ nm $ d


-- | Generate an avatar for the given entity.
gsnAvatarType :: (
      EntityClass v,
      HasDiagrams HadesRender v,
      HasEvidence v,
      HasGsn v,
      Queryable v) =>
   Traversal' v (GsnItem v) -> v -> AvatarCreation (GsnDiagramType v) v
gsnAvatarType prsm v =
   case v ^? prsm of
      Just (GsnNodeItem node) -> SimpleBox $ gsnNodeType node
      Just (GsnEdgeItem edge) -> SimpleArrow $ GsnArrow $ gsnEdgeType edge
      Just (GsnDiagramItem _) -> NoAvatar  -- Diagram links handled by GenericDiagram.
      Nothing -> case v ^? _Evidence of
         Just _ -> SimpleBox EvidenceBox
         Nothing -> case v ^? _Trace of
            Just _ -> SimpleArrow TraceArrow
            Nothing -> NoAvatar


-- | Scripts for editing a GSN diagram.
type GsnScript v a = ModelScript HadesRender v (GsnItem v) a


-- | Activation script for GSN entities.
gsnActivation ::
   (EntityClass v, HasDiagrams HadesRender v, HasEvidence v, HasGsn v, Queryable v) =>
      ActivationScript HadesRender v (GsnItem v)
gsnActivation Nothing = cannotHappen "gsnActivation: Nothing activated." $ return Nothing
gsnActivation (Just e) = do
   prsm <- lift $ usingTraversal return
   case e ^? entityContents . prsm  of
      Nothing -> return Nothing
      Just GsnDiagramItem {} -> do
         lift $ goToEntity $ entityId e
         openDiagram $ entityId e
         return Nothing
      Just _ ->
         editEntityProperties $ entityId e


-- | Context menu for GSN items in the model tree.
gsnEntityMenu ::
   (EntityClass v, HasDiagrams HadesRender v, HasEvidence v, HasGsn v, Queryable v) =>
      Entity v -> GsnItem v -> Menu (GsnScript v (Maybe Text))
gsnEntityMenu ent GsnDiagramItem {} = diagramEntityMenu $ entityId ent
gsnEntityMenu ent _ = entityMenu ent


-- | Menu for adding a new node or edge to the model.
gsnAddEntityMenu :: (
      EntityClass v,
      HasDiagrams HadesRender v,
      HasEvidence v,
      HasGsn v,
      Queryable v) =>
   ModelId -> Menu (GsnScript v (Maybe Text))
gsnAddEntityMenu parent = Menu [[
         menuItem "Add GSN Diagram" $ do
            let diag = GsnDiagramItem (DiagramEntity (Name "GSN Diagram ") mempty mempty) ^. re _Gsn
            insertEntity diag parent >>= \case
               Nothing -> return Nothing
               Just (nm, _) -> return $ Just $ "Insert " <> nm ^. nameText,
         subMenu "GSN Items" $ Menu [
               map nodeItem [("Goal", createGoal), ("Strategy", createStrategy),
                     ("Solution", createSolution), ("Context", createContext),
                     ("Assumption", createAssumption), ("Justification", createJustification),
                     ("Option", createGsnOption)],
               map edgeItem [minBound .. maxBound]
            ]
      ]]
   where
      nodeItem (lbl, creator) = menuItem lbl $
         addNode parent creator >>= \case
            Nothing -> return Nothing
            Just (nm, _) -> return $ Just $ "Insert " <> nm ^. nameText
      edgeItem typ = menuItem (T.pack $ show typ) $ do
         (nm, _) <- addEdge parent typ
         return $ Just $ "Insert " <> nm ^. nameText


-- | Add a node to the model and return the name and UUID. Opens a dialog box for the user to
-- enter the name and text.
addNode :: (EntityClass v, HasGsn v) =>
   ModelId  -- ^ Parent package for new node.
   -> ModelEdit v (GsnItem v) GsnNode
   -> GsnScript v (Maybe (Name, ModelId))
addNode uuid creator = do
      candidate <- lift $ goToEntity uuid >> creator
      insertEntity (GsnNodeItem candidate ^. re _Gsn) uuid


-- | Add a new edge to the model and return the name and UUID. Does not open a dialog box.
addEdge :: (
      EntityClass v,
      HasDiagrams HadesRender v,
      HasEvidence v,
      HasGsn v,
      Queryable v) =>
   ModelId -> GsnEdgeType -> GsnScript v (Name, ModelId)
addEdge uuid typ = lift $ do
   goToEntity uuid
   nm <- modelNewName (Name $ T.pack $ show typ)
   let edge = GsnEdgeItem (GsnEdge typ nm CardinalityOne)
   newEntity <- mkEntity $ edge ^. re _Gsn
   return (edge ^. gsnItemName, entityId newEntity)


-- | Toolbar for GSN diagrams. Provides easy insertion of new diagram elements.
gsnToolbar :: (
      AppearanceClass (GsnDiagramType v) v, Editable HadesRender v, EntityClass v, HasGsn v,
      HasDiagrams HadesRender v, HasEvidence v, HasLookups v, Queryable v, HasReports v) =>
   ModelId   -- ^ This diagram.
   -> ModelId   -- Model package where new entities are to be added.
   -> DeltaToolbar (GsnElement v)
gsnToolbar diagId parent = DeltaToolbar [[
            DeltaTool "gsn_goal" "Goal" $ DeltaToolAction $ newBoxTool createGoal,
            DeltaTool "gsn_strategy" "Strategy" $ DeltaToolAction $ newBoxTool createStrategy,
            DeltaTool "gsn_solution" "Solution" $ DeltaToolAction $ newBoxTool createSolution,
            DeltaTool "gsn_support_arrow" "Support" $ DeltaToolAction $
                  newArrowTool SupportArrow "Support"
         ], [
            DeltaTool "gsn_context" "Context" $ DeltaToolAction $ newBoxTool createContext,
            DeltaTool "gsn_assumption" "Assumption" $ DeltaToolAction $
                  newBoxTool createAssumption,
            DeltaTool "gsn_justification" "Justification" $ DeltaToolAction $
                  newBoxTool createJustification,
            DeltaTool "gsn_context_arrow" "Context" $ DeltaToolAction $
                  newArrowTool ContextArrow "Context"
         ], [
            DeltaTool "gsn_option" "Option" $ DeltaToolAction $ newBoxTool createGsnOption
         ], [
            DeltaTool "trace-arrow" "Trace" $ DeltaToolAction $ mkScriptAction $ newArrowEntityTool
                  True
                  parent
                  (Trace (Name "Trace") ^. re _Trace)
                  TraceArrow
         ], [
            DeltaTool "basic_comment" "Comment" $ DeltaToolAction newCommentTool,
            DeltaTool "emblem-symbolic-link" "Link" $ DeltaToolAction newLinkTool
         ]
      ]
      [
         alignmentTools,
         [viewControlTool
            diagId
            (_GsnDiagramItem . diagramEntityData)
            compile1
            (constantDialog $ diagramAppearanceDialog gsnDiagramVariant)
         ]]
   where
      compile1 ::
         (AppearanceClass (GsnDiagramType v) v, Editable HadesRender v, HasDiagrams HadesRender v,
               HasGsn v, HasEvidence v, HasLookups v, HasReports v, Queryable v) =>
         Model v
         -> DiagramAppearance (GsnDiagramType v) v
         -> ViewContext (GsnElement v)
         -> ViewContext (GsnElement v)
      compile1 m a ctx = fromRight ctx $ evalModelEdit _Gsn m $
            compileAppearance (diagramAppearanceClean a) ctx
      newBoxTool :: (AppearanceClass (GsnDiagramType v) v, Editable HadesRender v,
               HasDiagrams HadesRender v, HasGsn v, HasEvidence v, HasLookups v, HasReports v) =>
         ModelEdit v (ModelSubtype (GsnDiagramType v)) GsnNode -> Action (GsnElement v)
      newBoxTool creator = mkScriptAction $ do
         newNode <- liftBase $ lift $ goToEntity parent >> creator
         newBoxEntityTool parent (GsnNodeItem newNode ^. re _Gsn) $ gsnNodeType newNode
      newArrowTool :: (AppearanceClass (GsnDiagramType v) v, Editable HadesRender v,
               HasDiagrams HadesRender v, HasGsn v, HasEvidence v, HasLookups v, HasReports v) =>
         GsnEdgeType -> Text -> Action (GsnElement v)
      newArrowTool typ base = mkScriptAction $ newArrowEntityTool
               True
               parent
               (GsnEdgeItem (GsnEdge typ (Name base) CardinalityOne) ^. re _Gsn)
               (GsnArrow typ)


-- | GUI icon name for each kind of model entity.
gsnStockItem :: (
      AppearanceClass (GsnDiagramType v) v,
      Editable HadesRender v,
      EntityClass v,
      HasDiagrams HadesRender v,
      HasEvidence v,
      HasGsn v,
      HasLookups v,
      HasReports v) =>
   GsnItem v -> Text
gsnStockItem = (stockPrefix <>) . varNameToStock . reflectiveName
   where
      varNameToStock v = T.map underscoreSpace $ T.toLower $ v ^. variantName
      underscoreSpace ' ' = '_'
      underscoreSpace c = c
      stockPrefix = "gsn_"


-------------------------------------------
-- Utility functions for naming.
-------------------------------------------

-- | Find a \"version number\" string of the form n.m.o: digits with decimals interspersed.
-- If there is more than one matching string then return the last one. If there is no such
-- string then return an empty string.
dottedNumbers :: Text -> Text
dottedNumbers str = case map tidy $ unfoldr go str of
   [] -> ""
   strs -> last strs
   where
      tidy = T.dropAround (== '.')
      go s1 =
         let
            (s2, rest) = T.span testChar $ T.dropWhile (not . testChar) s1
            s3 = tidy s2
         in if T.null s3
               then Nothing
               else Just (s3, rest)
      testChar c = isDigit c || c == '.'
