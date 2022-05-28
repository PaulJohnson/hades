{-# LANGUAGE LambdaCase #-}

{-
Copyright © Paul Johnson 2020. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

-- |

module DSM.SafetyCase where

import App.Appearance
import App.GenericDiagram
import Causality.FieldNames
import Causality.Model
import Causality.Risk
import Control.Lens
import Control.Monad.Trans
import Data.Aeson
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.UUID as UUID
import Evidence.Model
import GSN.FieldNames
import GSN.Model
import Hades.Abstract.Diagram
import Hades.GI
import Model.Abstract.BasicTypes
import Model.Abstract.DiagramType
import Model.Abstract.PackageTree
import Model.Lookups.Base
import Model.Matrices.Base
import Model.Query.Diagram
import Model.Reflection.NamedRelation (Relation)
import Model.Reflection.References
import Model.Reflection.Reflective
import Model.Reflection.Values
import Model.Report.Base
import Reactive.Banana.ArrowDialog
import Reactive.Banana.Combinators hiding ((<>))
import Reactive.Banana.Menu
import Data.Aeson.Types (Parser)


-- The top level model entities in model are GSN Items, Causal items, and query diagrams.
data SafetyModel =
  GsnItem (GsnItem SafetyModel)
  | CausalItem (Causal SafetyModel)
  | BowTieItem (BowTie SafetyModel)
  | BasicItem (ModelBasics SafetyModel)
  | EvidenceItem Evidence
  | TraceItem Trace
  deriving (Eq)


instance Reflective SafetyModel where
  reflectiveName (GsnItem v) = variantCoerce $ reflectiveName v
  reflectiveName (CausalItem v) = variantCoerce $ reflectiveName v
  reflectiveName (BowTieItem v) = variantCoerce $ reflectiveName v
  reflectiveName (BasicItem b) = variantCoerce $ reflectiveName b
  reflectiveName (EvidenceItem e) = variantCoerce $ reflectiveName e
  reflectiveName (TraceItem t) = variantCoerce $ reflectiveName t
  variantUserLabel v
    | v == bowTieRiskEventVariant = "Event"
    | otherwise = v ^. variantName
  reflectiveDefaults =
    map GsnItem reflectiveDefaults ++
    map BowTieItem reflectiveDefaults ++
    map BasicItem reflectiveDefaults ++
    map EvidenceItem reflectiveDefaults ++
    map TraceItem reflectiveDefaults
  reflectiveBuiltIn (GsnItem v) = reflectiveBuiltIn v
  reflectiveBuiltIn (CausalItem _) = []
  reflectiveBuiltIn (BowTieItem v) = reflectiveBuiltIn v
  reflectiveBuiltIn (BasicItem v) = reflectiveBuiltIn v
  reflectiveBuiltIn (EvidenceItem v) = reflectiveBuiltIn v
  reflectiveBuiltIn (TraceItem v) = reflectiveBuiltIn v
  reflectiveGet (GsnItem v) = reflectiveGet v
  reflectiveGet (CausalItem _) = mempty
  reflectiveGet (BowTieItem v) = reflectiveGet v
  reflectiveGet (BasicItem v) = reflectiveGet v
  reflectiveGet (EvidenceItem v) = reflectiveGet v
  reflectiveGet (TraceItem v) = reflectiveGet v
  reflectiveSet (GsnItem v) = GsnItem <$> reflectiveSet v
  reflectiveSet (CausalItem v) = CausalItem <$> reflectiveSet v
  reflectiveSet (BowTieItem v) = BowTieItem <$> reflectiveSet v
  reflectiveSet (BasicItem v) = BasicItem <$> reflectiveSet v
  reflectiveSet (EvidenceItem v) = EvidenceItem <$> reflectiveSet v
  reflectiveSet (TraceItem v) = TraceItem <$> reflectiveSet v
  reflectiveBuiltInRefs = mconcat [
      refTypeTableCoerce (reflectiveBuiltInRefs :: RefTypeTable (GsnItem SafetyModel)),
      refTypeTableCoerce (reflectiveBuiltInRefs :: RefTypeTable (BowTie SafetyModel)),
      refTypeTableCoerce (reflectiveBuiltInRefs :: RefTypeTable (ModelBasics SafetyModel)),
      refTypeTableCoerce (reflectiveBuiltInRefs :: RefTypeTable Evidence),
      refTypeTableCoerce (reflectiveBuiltInRefs :: RefTypeTable Trace)
    ]
  reflectiveArrows = mconcat [
      M.mapKeys variantCoerce
        (reflectiveArrows :: Map (Variant (GsnItem SafetyModel)) (Relation, Relation)),
      M.mapKeys variantCoerce
        (reflectiveArrows :: Map (Variant (BowTie SafetyModel)) (Relation, Relation)),
      M.mapKeys variantCoerce
        (reflectiveArrows :: Map (Variant (ModelBasics SafetyModel)) (Relation, Relation)),
      M.mapKeys variantCoerce
        (reflectiveArrows :: Map (Variant Evidence) (Relation, Relation)),
      M.mapKeys variantCoerce
        (reflectiveArrows :: Map (Variant Trace) (Relation, Relation))
    ]

instance EntityClass SafetyModel where
  type PackageMeta SafetyModel = PackageAppearance SafetyModel
  name = singular $  -- Safe because this covers all 6 possibilities.
    (_BowTieItem . bowTieName) `failing`
    (_CausalItem . causalName) `failing`
    (_GsnItem . gsnItemName) `failing`
    (_BasicItem . basicsName) `failing`
    (_EvidenceItem . evidenceName) `failing`
    (_TraceItem . traceName)
  _Package = _BasicItem . _BasicPackage
  entityCanHaveChild (BasicItem b) _ = basicsCanHaveChild b
  entityCanHaveChild _ _ = False
  entityCanMove _ = True
  entityExports _ = return []
  entityClone ent nm = case ent ^. entityContents of
    BasicItem b -> if basicsCanHaveChild b
      then Just $ clonePackage clonePackageAppearance ent nm
      else Just $ unsafeEntityClone ent nm
    _ -> Just $ unsafeEntityClone ent nm
  entityFunctions = tableFunctions

instance Editable HadesRender SafetyModel where
  editableActivation = safetyModelActivation
  editableTreeMenu = topEntityMenu
  editableDisplayMenu = topEditableMenu

instance AppearanceClass (GsnDiagramType SafetyModel) SafetyModel where
  getDiagramAppearance = coerceDiagramAppearance . M.findWithDefault mempty gsnDiagramVariant

instance AppearanceClass (CausalType SafetyModel) SafetyModel where
  getDiagramAppearance = coerceDiagramAppearance . M.findWithDefault mempty causalDiagramVariant

instance AppearanceClass (BowTieType SafetyModel) SafetyModel where
  getDiagramAppearance = coerceDiagramAppearance . M.findWithDefault mempty bowTieDiagramVariant

instance Queryable SafetyModel where
  _QueryDiagram =
    prism (BasicItem . BasicQuery) $ \case {BasicItem (BasicQuery q) -> Right q; v -> Left v}
  queryAvatars (GsnItem (GsnDiagramItem d)) =
      mapMaybe ascendedId $ d ^. diagramEntityContents . diagramList
  queryAvatars (BowTieItem (BowTieDiagram d)) =
      mapMaybe ascendedId $ d ^. diagramEntityContents . diagramList
  queryAvatars _ = []

instance HasMatrices SafetyModel where
  _Matrix = _BasicItem . _BasicMatrix

instance HasReports SafetyModel where
  _Report = _BasicItem . _BasicReport

instance HasBowTie SafetyModel where
  _BowTie = _BowTieItem

instance HasCausal SafetyModel where
  _Causal = _CausalItem

instance HasLookups SafetyModel where
  _LookupTable = _BasicItem . _BasicLookup

instance HasBasics SafetyModel where
  _Basics = _BasicItem

instance HasDiagrams HadesRender SafetyModel where
  isDiagram (GsnItem v) = gsnIsDiagram v
  isDiagram CausalItem {} = False
  isDiagram (BowTieItem v) = riskIsDiagram v
  isDiagram (BasicItem v) = basicsIsDiagram v
  isDiagram EvidenceItem {} = False
  isDiagram TraceItem {} = False
  getDiagramWrapper e = case e ^. entityContents of
    GsnItem {} -> withSubtype _GsnItem $ gsnDiagramWrapper e
    CausalItem {} -> return Nothing
    BowTieItem {} -> withSubtype _BowTieItem $ riskDiagramWrapper e
    BasicItem {} -> withSubtype _BasicItem $ basicsDiagramWrapper e
    EvidenceItem {} -> return Nothing
    TraceItem {} -> return Nothing
  diagramEntityTypes = foldr M.union mempty [
      gsnDiagramEntityTypes, riskDiagramEntityTypes, basicsDiagramEntityTypes
    ]
  diagramMapAvatars f ent =
    let
      newContents = case ent ^. entityContents of
        GsnItem (GsnDiagramItem d) -> GsnItem $ GsnDiagramItem $
            diagramEntityContents . diagramContents . each %~ elementMapAvatars f $ d
        BowTieItem (BowTieDiagram d) -> BowTieItem $ BowTieDiagram $
            diagramEntityContents . diagramContents . each %~ elementMapAvatars f $ d
        v -> v
    in entityContents .~ newContents $ ent

instance HasEvidence SafetyModel where
  _Evidence = _EvidenceItem
  _Trace = _TraceItem

instance HasGsn SafetyModel where
  _Gsn = _GsnItem

instance ToJSON SafetyModel where
  toJSON (GsnItem i) = toJSON i
  toJSON (CausalItem c) = toJSON c
  toJSON (BowTieItem b) = toJSON b
  toJSON (BasicItem b) = toJSON b
  toJSON (EvidenceItem e) = toJSON e
  toJSON (TraceItem t) = toJSON t

instance FromJSON SafetyModel where
  parseJSON v =
      (GsnItem <$> parseJSON v) <|>
      ignoreCausalJson v <|>
      (BowTieItem <$> parseJSON v) <|>
      (BasicItem <$> parseJSON v) <|>
      (EvidenceItem <$> parseJSON v) <|>
      (TraceItem <$> parseJSON v)
    where
      -- Causal models have been removed from the DSM. However they may be encounted in a few
      -- old files. This JSON parser replaces any causal component with an empty package.
      ignoreCausalJson :: Value -> Parser SafetyModel
      ignoreCausalJson v1 = do
        c :: ModelSubtype (CausalType SafetyModel) <- parseJSON v1
        return $ BasicItem $ BasicPackage $
            Package (Name $ (c ^. causalName . nameText) <> " (not supported)") mempty

_GsnItem :: Prism' SafetyModel (GsnItem SafetyModel)
_GsnItem = prism GsnItem $ \case {GsnItem i -> Right i; v -> Left v}

_CausalItem :: Prism' SafetyModel (Causal SafetyModel)
_CausalItem = prism CausalItem $ \case {CausalItem c -> Right c; v -> Left v}

_BowTieItem :: Prism' SafetyModel (BowTie SafetyModel)
_BowTieItem = prism BowTieItem $ \case {BowTieItem b -> Right b; v -> Left v}

_BasicItem :: Prism' SafetyModel (ModelBasics SafetyModel)
_BasicItem = prism BasicItem $ \case {BasicItem b -> Right b; v -> Left v}

_EvidenceItem :: Prism' SafetyModel Evidence
_EvidenceItem = prism EvidenceItem $ \case {EvidenceItem e -> Right e; v -> Left v}

_TraceItem :: Prism' SafetyModel Trace
_TraceItem = prism TraceItem $ \case {TraceItem t -> Right t; v -> Left v}

_ModelPackage :: Prism' SafetyModel (Package SafetyModel)
_ModelPackage = _BasicItem . _BasicPackage

_ModelQuery :: Prism' SafetyModel (QueryDiagram SafetyModel)
_ModelQuery = _BasicItem . _BasicQuery

_ModelMatrix :: Prism' SafetyModel Matrix
_ModelMatrix = _BasicItem . _BasicMatrix


-- | Context menu for the model tree.
topEntityMenu :: EntityMenu HadesRender SafetyModel SafetyModel
topEntityMenu maybeEnt =
  case maybeEnt of
    Nothing -> Just $ rootMenu _BasicItem $ appMenu UUID.nil
    Just e ->
      Just $ case e ^. entityContents of
        GsnItem g -> promoteScript _GsnItem <$> gsnEntityMenu e g
        CausalItem _ -> mempty
        BowTieItem b -> promoteScript _BowTieItem <$> bowTieEntityMenu e b
        BasicItem _ -> basicsMenu _BasicItem (appMenu $ entityId e) e
        EvidenceItem _ -> promoteScript _EvidenceItem <$> evidenceEntityMenu e
        TraceItem _ -> promoteScript _TraceItem <$> traceEntityMenu (entityId e)
  where
    appMenu uuid =
      (promoteScript _GsnItem <$> gsnAddEntityMenu uuid) <>
      (promoteScript _BowTieItem <$> bowTieAddEntityMenu uuid) <>
      addEvidenceEntityMenu uuid <>
      Menu [[menuItem "Edit diagram style" $ editPackageDiagramAppearance uuid]]


-- | Context menu for entity lists in matrices, searches etc.
topEditableMenu :: EntityMenu HadesRender SafetyModel SafetyModel
topEditableMenu Nothing = Nothing
topEditableMenu (Just e) = if e ^. entityContents . to isDiagram
  then Just $ Menu [[
      menuItem "Open Diagram" $ do
        openDiagram $ entityId e
        return Nothing,
      menuItem "Properties" $ editEntityProperties $ entityId e,
      menuItem "Find in model" $ do
        setModelSelection $ S.singleton $ entityId e
        return Nothing
    ]]
  else Just $ Menu [[
      menuItem "Properties" $ editEntityProperties $ entityId e,
      menuItem "Find in model" $ do
        setModelSelection $ S.singleton $ entityId e
        return Nothing
    ]]


editPackageDiagramAppearance ::
  ModelId -> ModelScript HadesRender SafetyModel SafetyModel (Maybe Text)
editPackageDiagramAppearance mId = do
  lift $ goToEntity mId
  eName <- lift currentName
  lift getMeta >>= \case
    Nothing -> lift $ throwUser $ eName <> " is not a package."
    Just appData ->
      openDialog (constantDialog packageAppearanceDialog) appData >>= \case
        Nothing -> return Nothing
        Just appData2 -> do
          lift $ setMeta $ packageAppearanceClean appData2
          return $ Just $ "Modified diagram styles in " <> eName


-- | Icon to use for the entity in the module tree.
entityIcon :: Entity SafetyModel -> Text
entityIcon ent = case ent ^. entityContents of
  GsnItem v -> gsnStockItem v
  CausalItem c -> causalIcon c
  BowTieItem b -> bowTieIcon b
  BasicItem v -> basicsIcon v
  EvidenceItem {} -> evidenceIconName
  TraceItem {} -> traceIconName



safetyModelActivation :: ActivationScript HadesRender SafetyModel SafetyModel
safetyModelActivation Nothing = return Nothing
safetyModelActivation (Just e) = case e ^. entityContents of
  GsnItem {} -> promoteScript _GsnItem $ gsnActivation $ Just e
  CausalItem {} -> return Nothing
  BowTieItem {} -> promoteScript _BowTieItem $ bowTieActivation $ Just e
  BasicItem {} -> basicsActivation e
  EvidenceItem {} -> promoteScript _EvidenceItem $ evidenceActivation $ Just e
  TraceItem {} -> promoteScript _TraceItem $ traceActivation $ Just e
