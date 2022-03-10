{-# LANGUAGE UndecidableInstances #-}

{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}



{- |

-}

module Causality.Risk (
   HasBowTie (..),
   BowTieType (..),
   riskIsDiagram,
   riskDiagramWrapper,
   riskDiagramEntityTypes,
   Control (Control),
   controlName,
   controlText,
   Hazard,
   hazardName,
   hazardText,
   IsHazard (IsHazard),
   isHazardName,
   BowTie,
   bowTieName,
   ModelSubtype (..),
   bowTieIcon,
   bowTieEntityMenu,
   bowTieAddEntityMenu,
   bowTieActivation
) where

import App.Appearance
import App.GenericDiagram
import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Causality.FieldNames
import Causality.Model
import Data.Aeson
import qualified Data.Colour.Names as C
import qualified Data.Colour.SRGB as C
import Data.Either
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Evidence.Model
import qualified GSN.Model as GSN
   (HasGsn (..), _GsnNodeItem, BoxType (GoalBox), gsnNodeType, drawGsnBoxRaw)
import Hades.Abstract
import Hades.GI
import Model.Abstract.DiagramType
import Model.Abstract.PackageTree as P
import Model.Lookups.Base
import Model.Query.Diagram
import qualified Model.Reflection.NamedRelation as NR
import Model.Reflection.References
import Model.Reflection.Reflective
import Model.Reflection.Values
import Model.Report.Base
import Reactive.Banana.ArrowDialog (constantDialog)
import Reactive.Banana.Common
import Reactive.Banana.Menu
import Text.Read (readMaybe)


class HasBowTie v where
   _BowTie :: Prism' v (BowTie v)


-- | A model entity representing a possible high-level event in a bow-tie diagram.
data RiskEvent = RiskEvent {
      _riskEventName :: Name,
      _riskEventText :: Text
   } deriving (Eq)

instance ToJSON RiskEvent where
   toJSON risk = object [
         "type" .= ("RiskEvent" :: Text),
         "name" .= (risk ^. riskEventName),
         "text" .= (risk ^. riskEventText)
      ]

instance FromJSON RiskEvent where
   parseJSON = withObject "Bow Tie Event" $ \v -> do
      checkType v "RiskEvent"
      RiskEvent <$> v .: "name" <*> v .: "text"


riskEventName :: Lens' RiskEvent Name
riskEventName = lens _riskEventName $ \r nm -> r{_riskEventName = nm}

riskEventText :: Lens' RiskEvent Text
riskEventText = lens _riskEventText $ \r txt -> r{_riskEventText = txt}


-- | A model entity representing some way to reduce or mitigate a risk.
data Control = Control {
      _controlName :: Name,
      _controlText :: Text
   } deriving (Eq)

instance ToJSON Control where
   toJSON ctrl = object [
         "type" .= ("Control" :: Text),
         "name" .= (ctrl ^. controlName),
         "text" .= (ctrl ^. controlText)
      ]

instance FromJSON Control where
   parseJSON = withObject "Control" $ \v -> do
      checkType v "Control"
      Control <$> v .: "name" <*> v .: "text"


controlName :: Lens' Control Name
controlName = lens _controlName $ \c nm -> c{_controlName = nm}

controlText :: Lens' Control Text
controlText = lens _controlText $ \c txt -> c{_controlText = txt}


-- | An edge representing a risk that one event will lead to another.
newtype ThreatLine = ThreatLine {_threatLineName :: Name} deriving (Eq)

instance ToJSON ThreatLine where
   toJSON r = object [
         "type" .= ("ThreatLine" :: Text),
         "name" .= (r ^. threatLineName)
      ]

instance FromJSON ThreatLine where
   parseJSON = withObject "ThreatLine" $ \v -> do
      checkType v "RiskLine" <|> checkType v "ThreatLine"  -- Early version used incorrect name.
      ThreatLine <$> v .: "name"


threatLineName :: Iso' ThreatLine Name
threatLineName = iso _threatLineName ThreatLine


-- | An edge representing a link from an event to a hazard.
newtype IsHazard = IsHazard {_isHazardName :: Name} deriving (Eq)

instance ToJSON IsHazard where
   toJSON h = object [
         "type" .= ("IsHazard" :: Text),
         "name" .= (h ^. isHazardName)
      ]

instance FromJSON IsHazard where
   parseJSON = withObject "IsHazard" $ \v -> do
      checkType v "IsHazard"
      IsHazard <$> v .: "name"


isHazardName :: Iso' IsHazard Name
isHazardName = iso _isHazardName IsHazard


-- | A node representing a hazard in the system being modelled.
data Hazard = Hazard {
      _hazardName :: Name,
      _hazardText :: Text
   } deriving (Eq)

instance ToJSON Hazard where
   toJSON h = object [
         "type" .= ("Hazard" :: Text),
         "name" .= (h ^. hazardName),
         "text" .= (h ^. hazardText)
      ]

instance FromJSON Hazard where
   parseJSON = withObject "Hazard" $ \v -> do
      checkType v "Hazard"
      Hazard <$> v .: "name" <*> v .: "text"


hazardName :: Lens' Hazard Name
hazardName = lens _hazardName $ \h nm -> h{_hazardName = nm}

hazardText :: Lens' Hazard Text
hazardText = lens _hazardText $ \h txt -> h{_hazardText = txt}


-- | Unit type that exists only for its instances.
data BowTieType v = BowTieType

instance (
      AppearanceClass (BowTieType v) v,
      Editable HadesRender v,
      EntityClass v,
      HasBowTie v,
      HasDiagrams HadesRender v,
      HasEvidence v,
      GSN.HasGsn v,
      HasLookups v,
      HasReports v) =>
   DiagramTypeClass (BowTieType v) v where
      data BoxType (BowTieType v) =
         EventBox | StateBox | RiskEventBox | ControlBox | HazardBox | GoalBox | EvidenceBox
            deriving (Read, Show, Eq)
      data ArrowType (BowTieType v) =
         RiskArrow | HazardArrow | TraceArrow deriving (Read, Show, Eq)
      data ModelSubtype (BowTieType v) =
         BowTieHazard Hazard
         | BowTieRiskEvent RiskEvent
         | BowTieControl Control
         | BowTieIsHazard IsHazard
         | BowTieThreat ThreatLine
         | BowTieDiagram (BowTieDiagram v)
      mkBox diagId modelId typ peg bound =
         case typ of
            EventBox -> Box diagId modelId typ peg $ Parallelogram bound
            StateBox -> Box diagId modelId typ peg $ Oval bound
            RiskEventBox -> Box diagId modelId typ peg $ Rectangle bound
            ControlBox -> Box diagId modelId typ peg $ Rectangle bound
            HazardBox -> Box diagId modelId typ peg $ Rectangle bound
            GoalBox -> Box diagId modelId typ peg $ Rectangle bound
            EvidenceBox -> Box diagId modelId typ peg $ Rectangle bound
      boxTypeDraw = drawBowTieBox
      boxChildren = const $ return []
      boxChildConnection _ _ = return id
      boxCanPeg box arrow = case (boxType box, arrowType arrow) of
         (ControlBox, RiskArrow) -> return $ Just controlThreatRelation
         _ -> return Nothing
      avatarType prsm v = case v ^? prsm of
         Just BowTieHazard {} -> SimpleBox HazardBox
         Just BowTieRiskEvent {} -> SimpleBox RiskEventBox
         Just BowTieControl {} -> SimpleBox ControlBox
         Just BowTieIsHazard {} -> SimpleArrow HazardArrow
         Just BowTieThreat {} -> SimpleArrow RiskArrow
         Just BowTieDiagram {} -> NoAvatar
         Nothing -> if v ^? GSN._Gsn . GSN._GsnNodeItem . to GSN.gsnNodeType == Just GSN.GoalBox
                  then SimpleBox GoalBox
                  else case v ^? _Evidence of
                     Just _ -> SimpleBox EvidenceBox
                     Nothing -> case v ^? _Trace of
                        Just _ -> SimpleArrow TraceArrow
                        Nothing -> NoAvatar
      diagramTypeName = const "Bow Tie"
      arrowTailRelation TraceArrow = traceTailRelation
      arrowTailRelation _ = NR.edgeFromRelation
      arrowHeadRelation TraceArrow = traceHeadRelation
      arrowHeadRelation _ = NR.edgeToRelation
      arrowTailExportRelation _ = Nothing
      arrowHeadExportRelation _ = Nothing
      arrowHeadDraw _ RiskArrow = openArrowHead 45 10
      arrowHeadDraw _ HazardArrow = closedArrowHead 90 10 False
      arrowHeadDraw _ TraceArrow = traceArrowHead
      arrowLineDashes TraceArrow = traceArrowDash
      arrowLineDashes _ = []
      arrowCanConnect = bowTieArrowCanConnect

instance (
      AppearanceClass (BowTieType v) v,
      Editable HadesRender v,
      EntityClass v,
      HasBowTie v,
      HasDiagrams HadesRender v,
      HasEvidence v,
      GSN.HasGsn v,
      HasLookups v,
      HasReports v) =>
   Eq (ModelSubtype (BowTieType v)) where
      BowTieHazard x == BowTieHazard y         = x == y
      BowTieRiskEvent x == BowTieRiskEvent y   = x == y
      BowTieControl x == BowTieControl y       = x == y
      BowTieIsHazard x == BowTieIsHazard y     = x == y
      BowTieThreat x == BowTieThreat y         = x == y
      BowTieDiagram x == BowTieDiagram y       = x == y
      _ == _   = False

instance ToJSON (BoxType (BowTieType v)) where
   toJSON = String . T.pack . show

instance FromJSON (BoxType (BowTieType v)) where
   parseJSON = withText "Bow Tie Box type" $ \str ->
      case readMaybe (T.unpack str) of
         Just t -> return t
         Nothing -> fail $ "Not a Bow Tie Box type: " <> show str

instance ToJSON (ArrowType (BowTieType v)) where
   toJSON = String . T.pack . show

instance FromJSON (ArrowType (BowTieType v)) where
   parseJSON = withText "Bow Tie Arrow type" $ \str ->
      case readMaybe (T.unpack str) of
         Just t -> return t
         Nothing -> fail $ "Not a Bow Tie Arrow type: " <> show str

instance (
      AppearanceClass (BowTieType v) v,
      Editable HadesRender v,
      HasBowTie v,
      HasDiagrams HadesRender v,
      HasEvidence v,
      GSN.HasGsn v,
      HasLookups v,
      HasReports v) =>
   ToJSON (ModelSubtype (BowTieType v)) where
      -- All the variant types have a "type" field in their JSON.
      toJSON (BowTieHazard h) = toJSON h
      toJSON (BowTieRiskEvent r) = toJSON r
      toJSON (BowTieControl c) = toJSON c
      toJSON (BowTieIsHazard i) = toJSON i
      toJSON (BowTieThreat r) = toJSON r
      toJSON (BowTieDiagram d) = toJSON d

instance (
      AppearanceClass (BowTieType v) v,
      Editable HadesRender v,
      HasBowTie v,
      HasDiagrams HadesRender v,
      HasEvidence v,
      GSN.HasGsn v,
      HasLookups v,
      HasReports v) =>
   FromJSON (ModelSubtype (BowTieType v)) where
      parseJSON v =
         BowTieHazard <$> parseJSON v
         <|> BowTieRiskEvent <$> parseJSON v
         <|> BowTieControl <$> parseJSON v
         <|> BowTieIsHazard <$> parseJSON v
         <|> BowTieThreat <$> parseJSON v
         <|> BowTieDiagram <$> parseJSON v

instance Reflective (ModelSubtype (BowTieType v)) where
   reflectiveName BowTieHazard {} = causalHazardVariant
   reflectiveName BowTieRiskEvent {} = bowTieRiskEventVariant
   reflectiveName BowTieControl {} = causalControlVariant
   reflectiveName BowTieIsHazard {} = hazardEdgeVariant
   reflectiveName BowTieThreat {} = threatEdgeVariant
   reflectiveName BowTieDiagram {} = bowTieDiagramVariant
   variantUserLabel v
      | v == bowTieRiskEventVariant = "Event"
      | otherwise = v ^. variantName
   reflectiveDefaults = [
         BowTieHazard $ Hazard (Name "HAZ-") "",
         BowTieRiskEvent $ RiskEvent (Name "Risk-") "",
         BowTieControl $ Control (Name "Ctrl-") "",
         BowTieIsHazard $ IsHazard (Name "is-hazard-"),
         BowTieThreat $ ThreatLine (Name "ThreatLine-"),
         BowTieDiagram $ DiagramEntity (Name "Bow Tie") mempty mempty
      ]
   reflectiveBuiltIn BowTieHazard {} = [nameField, descriptionField]
   reflectiveBuiltIn BowTieRiskEvent {} = [nameField, descriptionField]
   reflectiveBuiltIn BowTieControl {} = [nameField, descriptionField]
   reflectiveBuiltIn BowTieIsHazard {} = [nameField]
   reflectiveBuiltIn BowTieThreat {} = [nameField]
   reflectiveBuiltIn BowTieDiagram {} = [nameField]
   reflectiveGet (BowTieHazard (Hazard (Name nm) txt)) = M.fromList
      [(nameField, ExtText nm), (descriptionField, ExtText txt)]
   reflectiveGet (BowTieRiskEvent (RiskEvent (Name nm) txt)) = M.fromList
      [(nameField, ExtText nm), (descriptionField, ExtText txt)]
   reflectiveGet (BowTieControl (Control (Name nm) txt)) = M.fromList
      [(nameField, ExtText nm), (descriptionField, ExtText txt)]
   reflectiveGet (BowTieIsHazard (IsHazard (Name nm))) =
      M.singleton nameField $ ExtText nm
   reflectiveGet (BowTieThreat (ThreatLine (Name nm))) =
      M.singleton nameField $ ExtText nm
   reflectiveGet (BowTieDiagram d) =
      M.singleton nameField $ ExtText $ d ^. diagramEntityName . nameText
   reflectiveSet (BowTieHazard h) = do
      nm <- Name . T.strip <$> extract (h ^. hazardName . nameText) _ExtText nameField
      txt <- extract (h ^. hazardText) _ExtText descriptionField
      return $ BowTieHazard $ Hazard nm txt
   reflectiveSet (BowTieRiskEvent r) = do
      nm <- Name . T.strip <$> extract (r ^. riskEventName . nameText) _ExtText nameField
      txt <- extract (r ^. riskEventText) _ExtText descriptionField
      return $ BowTieRiskEvent $ RiskEvent nm txt
   reflectiveSet (BowTieControl c) = do
      nm <- Name . T.strip <$> extract (c ^. controlName . nameText) _ExtText nameField
      txt <- extract (c ^. controlText) _ExtText descriptionField
      return $ BowTieControl $ Control nm txt
   reflectiveSet (BowTieIsHazard i) =
      BowTieIsHazard . IsHazard . Name . T.strip <$>
         extract (i ^. isHazardName . nameText) _ExtText nameField
   reflectiveSet (BowTieThreat r) =
      BowTieThreat . ThreatLine . Name . T.strip
         <$> extract (r ^. threatLineName . nameText) _ExtText nameField
   reflectiveSet (BowTieDiagram d) = do
      nm <- Name  . T.strip<$> extract (d ^. diagramEntityName . nameText) _ExtText nameField
      return $ BowTieDiagram $ diagramEntityName .~ nm $ d
   reflectiveBuiltInRefs =
      arrowRelations hazardEdgeVariant
            (bowTieRiskEventVariant : eventVariants)
            [causalHazardVariant] <>
      arrowRelations threatEdgeVariant
            (bowTieRiskEventVariant : eventVariants)
            ([causalControlVariant, bowTieRiskEventVariant] ++ eventVariants) <>
      mkManyToMany controlThreatRelation [causalControlVariant] [threatEdgeVariant]
   reflectiveArrows = M.fromList [
         (hazardEdgeVariant, (NR.edgeFromRelation, NR.edgeToRelation)),
         (threatEdgeVariant, (NR.edgeFromRelation, NR.edgeToRelation))
      ]


-- | True if this is a diagram.
riskIsDiagram :: BowTie v -> Bool
riskIsDiagram BowTieDiagram {} = True
riskIsDiagram _ = False


-- | The wrapper for Bow Tie diagrams.
riskDiagramWrapper :: (
      AppearanceClass (BowTieType v) v,
      Editable HadesRender v,
      HasDiagrams HadesRender v,
      HasBowTie v,
      HasEvidence v,
      GSN.HasGsn v,
      HasLookups v,
      HasReports v) =>
   Entity v -> ModelEdit v (BowTie v) (Maybe (EntityWrapper HadesRender v))
riskDiagramWrapper ent =
   usingTraversal $ \trv ->
      case ent ^? entityContents . trv . _BowTieDiagram of
         Nothing -> return Nothing
         Just de -> do
            ctx <- compileAppearance
                  (de ^. diagramEntityData) $
                  GenericContext (entityId ent) mempty
            return $ Just $ DiagramWrapper {
                  wrappedEntityId = entityId ent,
                  diagramToEdit = de ^. diagramEntityContents,
                  diagramEntityTraversal = _BowTieDiagram,
                  diagramTraversal = trv,
                  diagramDialog = elementDialog,
                  diagramContext = ctx,
                  diagramMenu = Nothing,
                  diagramToolbar = const $ bowTieToolbar (entityId ent) $ ent ^. entityParent
               }


riskDiagramEntityTypes :: DiagramEntityTypes v
riskDiagramEntityTypes = M.fromList [
      (bowTieDiagramVariant, [
            causalHazardVariant, bowTieRiskEventVariant, causalControlVariant, evidenceVariant
         ])
   ]

-- | Bow tie diagram type.
type BowTieDiagram v = DiagramEntity (ElemBowTie v)

-- | Elements that can appear in bow tie diagrams.
type ElemBowTie v = Element (BowTieType v) v

-- | The bow tie subtype of model entities.
type BowTie v = ModelSubtype (BowTieType v)


_BowTieHazard :: Prism' (BowTie v) Hazard
_BowTieHazard = prism BowTieHazard $ \case {BowTieHazard h -> Right h; v -> Left v}

_BowTieRiskEvent :: Prism' (BowTie v) RiskEvent
_BowTieRiskEvent = prism BowTieRiskEvent $ \case {BowTieRiskEvent r -> Right r; v -> Left v}

_BowTieControl :: Prism' (BowTie v) Control
_BowTieControl = prism BowTieControl $ \case {BowTieControl x -> Right x; v -> Left v}

_BowTieIsHazard :: Prism' (BowTie v) IsHazard
_BowTieIsHazard = prism BowTieIsHazard $ \case {BowTieIsHazard x -> Right x; v -> Left v}

_BowTieThreat :: Prism' (BowTie v) ThreatLine
_BowTieThreat = prism BowTieThreat $ \case {BowTieThreat x -> Right x; v -> Left v}

_BowTieDiagram :: Prism' (BowTie v) (BowTieDiagram v)
_BowTieDiagram = prism BowTieDiagram $ \case {BowTieDiagram x -> Right x; v -> Left v}

bowTieName :: Lens' (BowTie v) Name
bowTieName f = \case
   BowTieHazard h -> BowTieHazard <$> hazardName f h
   BowTieRiskEvent r -> BowTieRiskEvent <$> riskEventName f r
   BowTieControl c -> BowTieControl <$> controlName f c
   BowTieIsHazard i -> BowTieIsHazard <$> isHazardName f i
   BowTieThreat r -> BowTieThreat <$> threatLineName f r
   BowTieDiagram d -> BowTieDiagram <$> diagramEntityName f d

drawBowTieBox :: (
         AppearanceClass (BowTieType v) v,
         Editable HadesRender v,
         HasDiagrams HadesRender v,
         HasBowTie v,
         HasEvidence v,
         GSN.HasGsn v,
         HasLookups v,
         HasReports v) =>
   Box (BowTieType v) v
   -> HadesLayout ()
   -> Maybe Colour
   -> Delta (Element (BowTieType v) v) (BoundBox -> HadesRender ())
drawBowTieBox (Box _ modelId typ _ shape1) contents border = do
      ent <- liftBase $ lift $ goToEntity modelId >> current
      case typ of
         EventBox -> return $ drawBowTieRiskEventBox ent contents border shape1
         StateBox -> return $ drawBowTieRiskEventBox ent contents border shape1
         RiskEventBox -> return $ drawBowTieRiskEventBox ent contents border shape1
         ControlBox -> return $ drawBowTieControlBox ent contents border shape1
         HazardBox -> return $ drawBowTieHazardBox ent contents border shape1
         GoalBox -> return $ GSN.drawGsnBoxRaw
               GSN.GoalBox
               (ent ^? _Just . entityContents . GSN._Gsn . GSN._GsnNodeItem)
               contents
               border
               shape1
         EvidenceBox ->
            return $ drawEvidence ent contents border


drawBowTieControlBox :: (BoxShapeClass shape, HasBowTie v) =>
   Maybe (Entity v) -> HadesLayout () -> Maybe Colour -> shape -> BoundBox -> HadesRender ()
drawBowTieControlBox ent contents border shape1 boundBox = do
      let
         shape2 = shapeDefinition .~ boundBox $ shape1
         (border1, fill) = basicBox 2 (fromMaybe black border) controlGreen
      local (\e -> e {boxBorderStyle = border1, boxFillStyle = fill}) $
         drawPolygon $ rectangleCorners $ Rectangle $ shape2 ^. shapeDefinition
      runLayout_ (shapeInnerBox shape2) $ do
         layoutTitle Nothing $ case ent ^? _Just . entityContents . _BowTie . _BowTieControl of
            Just ctrl -> ctrl ^. controlName . nameText
            Nothing -> "Control"
         layoutParagraphGap
         contents
   where
      controlGreen = Colour $ C.sRGB24 200 255 200
      black = Colour C.black


drawBowTieRiskEventBox :: (BoxShapeClass shape, HasBowTie v) =>
   Maybe (Entity v) -> HadesLayout () -> Maybe Colour -> shape -> BoundBox -> HadesRender ()
drawBowTieRiskEventBox ent contents border shape1 boundBox = do
      let
         shape2 = shapeDefinition .~ boundBox $ shape1
      maybe id withLineColour border $
         drawPolygon $ rectangleCorners $ Rectangle $ shape2 ^. shapeDefinition
      runLayout_ (shapeInnerBox shape2) $ do
         layoutTitle Nothing $ case ent ^? _Just . entityContents . _BowTie . _BowTieRiskEvent of
            Just risk -> risk ^. riskEventName . nameText
            Nothing -> "Event"
         layoutParagraphGap
         contents


drawBowTieHazardBox :: (BoxShapeClass shape, HasBowTie v) =>
   Maybe (Entity v) -> HadesLayout () -> Maybe Colour -> shape -> BoundBox -> HadesRender ()
drawBowTieHazardBox ent contents border shape1 boundBox = do
      let
         shape2 = shapeDefinition .~ boundBox $ shape1
         (border1, fill) = basicBox 4 (fromMaybe black border) hazardYellow
      local (\e -> e {boxBorderStyle = border1, boxFillStyle = fill}) $
         drawPolygon $ rectangleCorners $ Rectangle $ shape2 ^. shapeDefinition
      runLayout_ (shapeInnerBox shape2) $ do
         layoutTitle Nothing $ case ent ^? _Just . entityContents . _BowTie . _BowTieHazard of
            Just haz -> haz ^. hazardName . nameText
            Nothing -> "Hazard"
         layoutParagraphGap
         contents
   where
      hazardYellow = Colour $ C.sRGB24 255 255 200
      black = Colour C.black


bowTieArrowCanConnect :: (
      AppearanceClass (BowTieType v) v,
      Editable HadesRender v,
      HasBowTie v,
      HasDiagrams HadesRender v,
      HasEvidence v,
      GSN.HasGsn v,
      HasLookups v,
      HasReports v) =>
   ConnectionDirection
   -> Arrow (BowTieType v) v
   -> Box (BowTieType v) v
   -> Maybe DiagramId
   -> Delta (ElemBowTie v) Bool
bowTieArrowCanConnect connection arrow box _ = do
   let targetType = boxType box
   case arrowType arrow of
      RiskArrow -> return $ case connection of
         ConnectionFrom -> targetType `elem` [EventBox, StateBox, RiskEventBox]
         ConnectionTo -> targetType `elem` [EventBox, StateBox, RiskEventBox, ControlBox]
      HazardArrow -> return $ case connection of
         ConnectionFrom -> targetType `elem` [EventBox, StateBox, RiskEventBox]
         ConnectionTo -> targetType == HazardBox
      TraceArrow -> return $ case connection of
         ConnectionFrom -> targetType == ControlBox
         ConnectionTo -> targetType `elem` [EvidenceBox, GoalBox]


bowTieToolbar :: (
      AppearanceClass (BowTieType v) v,
      Editable HadesRender v,
      EntityClass v,
      HasBowTie v,
      HasDiagrams HadesRender v,
      HasEvidence v,
      GSN.HasGsn v,
      HasLookups v,
      HasReports v) =>
   ModelId   -- ^ Entity ID of this diagram.
   -> ModelId  -- ^ Entity ID of the parent of this diagram.
   ->  DeltaToolbar (ElemBowTie v)
bowTieToolbar diagId parent = DeltaToolbar [[
         DeltaTool "causal_risk_event" "Event" $ DeltaToolAction $
            newBoxTool RiskEventBox $ BowTieRiskEvent $ RiskEvent (Name "Event ") "",
         DeltaTool "causal_hazard" "Hazard" $ DeltaToolAction $
            newBoxTool HazardBox $ BowTieHazard $ Hazard (Name "HAZ-") "",
         DeltaTool "causal_is_hazard" "Hazard arrow" $ DeltaToolAction $
            newArrowTool HazardArrow $ BowTieIsHazard $ IsHazard $ Name "Hazard arrow "
      ], [
         DeltaTool "causal_control" "Control" $ DeltaToolAction $
            newBoxTool ControlBox $ BowTieControl $ Control (Name "Control ") "",
         DeltaTool "causal_risk_line" "Threat line" $ DeltaToolAction $
            newArrowTool RiskArrow $ BowTieThreat $ ThreatLine $ Name "Threat line "
      ], [
         DeltaTool "trace-arrow" "Trace" $ DeltaToolAction $ mkScriptAction $ newArrowEntityTool
               True
               parent
               (Trace (Name "Trace") ^. re _Trace)
               TraceArrow
      ], [
         DeltaTool "basic_comment" "Comment" $ DeltaToolAction newCommentTool,
         DeltaTool "emblem-symbolic-link" "Link" $ DeltaToolAction newLinkTool
      ]]
      [
         alignmentTools,
         [viewControlTool
            diagId
            (_BowTieDiagram . diagramEntityData)
            compile1
            (constantDialog $ diagramAppearanceDialog bowTieDiagramVariant)
         ]]
   where
      compile1 m a ctx =
         fromRight ctx $ evalModelEdit _BowTie m $ compileAppearance (diagramAppearanceClean a) ctx
      newBoxTool typ item = mkScriptAction $ newBoxEntityTool parent (item ^. re _BowTie) typ
      newArrowTool typ item = mkScriptAction $
         newArrowEntityTool True parent (item ^. re _BowTie) typ


bowTieIcon :: BowTie v -> Text
bowTieIcon BowTieHazard {} = "causal_hazard"
bowTieIcon BowTieRiskEvent {} = "causal_risk_event"
bowTieIcon BowTieControl {} = "causal_control"
bowTieIcon BowTieIsHazard {} = "causal_is_hazard"
bowTieIcon BowTieThreat {} = "causal_risk_line"
bowTieIcon BowTieDiagram {} = "causal_bowtie"


bowTieEntityMenu :: (
      EntityClass v,
      HasBowTie v,
      HasDiagrams HadesRender v,
      HasEvidence v,
      GSN.HasGsn v,
      Queryable v) =>
   Entity v -> BowTie v -> Menu (ModelScript HadesRender v (BowTie v) (Maybe Text))
bowTieEntityMenu ent BowTieDiagram {} = diagramEntityMenu $ entityId ent
bowTieEntityMenu ent _ = entityMenu ent


bowTieAddEntityMenu :: (EntityClass v, HasBowTie v) =>
   ModelId -> Menu (ModelScript HadesRender v (BowTie v) (Maybe Text))
bowTieAddEntityMenu parent = Menu [[
         menuItem "Add Bow Tie Diagram" $ do
            let diag = BowTieDiagram $ DiagramEntity (Name "BowTie ") mempty mempty
            insertEntity (diag ^. re _BowTie) parent >>= \case
               Nothing -> return Nothing
               Just (nm, _) -> return $ Just $ "Insert " <> nm ^. nameText,
         subMenu "Bow Tie Items" $ Menu [[
               menuItem "Hazard" $ addEnt "HAZ-" $ \nm -> BowTieHazard $ Hazard nm "",
               menuItem "Event" $ addEnt "Event " $ \nm -> BowTieRiskEvent $ RiskEvent nm "",
               menuItem "Control" $ addEnt "Control " $ \nm -> BowTieControl $ Control nm "",
               menuItem "Hazard arrow" $ addEnt "Hazard arrow " $ BowTieIsHazard . IsHazard,
               menuItem "Threat line" $ addEnt "Threat line " $ BowTieThreat . ThreatLine
            ]]
      ]]
   where
      addEnt base f =
         insertEntity (f (Name base) ^. re _BowTie) parent >>= \case
            Nothing -> return Nothing
            Just (nm2, _) -> return $ Just $ "Insert " <> nm2 ^. nameText


bowTieActivation :: (
      EntityClass v,
      HasBowTie v,
      HasDiagrams HadesRender v,
      HasEvidence v,
      GSN.HasGsn v,
      Queryable v) =>
   ActivationScript HadesRender v (BowTie v)
bowTieActivation Nothing = return Nothing
bowTieActivation (Just e) = do
   prsm <- lift $ usingTraversal return
   case e ^? entityContents .prsm of
      Nothing -> return Nothing
      Just BowTieDiagram {} -> do
         openDiagram $ entityId e
         return Nothing
      Just _ ->
         editEntityProperties $ entityId e
