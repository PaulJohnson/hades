{-# LANGUAGE UndecidableInstances #-}

{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}


-- |
module Causality.Model (
  -- * Class for models that include causal entities
  HasCausal (..),
  -- * Causal model entity variants
  EventType (..),
  causalIsDiagram,
  causalDiagramWrapper,
  causalDiagramEntityTypes,
  eventTypeName,
  eventVariants,
  Event (Event, eventType),
  eventName,
  eventText,
  eventHidden,
  State (State),
  stateName,
  stateText,
  LogicType (..),
  logicIsState,
  logicIsEvent,
  logicTypeName,
  Logic (Logic, logicType),
  logicName,
  logicHidden,
  logicCount,
  Edge (TemporalEdge, TriggerEdge, GuardEdge),
  edgeName,
  edgeInhibit,
  ComponentModel (ComponentModel),
  componentName,
  fixOldComponents,
  componentCanHaveChild,
  instanceExports,
  -- * Causal model type
  ModelSubtype (..),
  _CausalEvent,
  _CausalState,
  _CausalLogic,
  _CausalEdge,
  _CausalComponent,
  causalName,
  causalExported,
  causalCanMove,
  -- * Causal Diagrams
  CausalType (..),
  Causal,
  drawCausalStateBox,
  drawCausalEventBox,
  drawCausalLogicBox,
  drawCausalInstanceBox,
  causalEntityMenu,
  addCausalEntityMenu,
  causalToolbar,
  causalIcon,
  causalActivation
) where

import App.Appearance
import App.GenericDiagram
import Causality.FieldNames
import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Aeson
import qualified Data.Colour.Names as C
import qualified Data.Colour.SRGB as C
import Data.Either
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Evidence.Model
import qualified GI.Pango as Pango
import qualified GSN.FieldNames as GSN (goalVariant)
import qualified GSN.Model as GSN
  (HasGsn (..), BoxType (GoalBox), gsnNodeType, _GsnNodeItem, drawGsnBoxRaw)
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
import Reactive.Banana.GI.ErrorBox
import Reactive.Banana.Menu
import Text.Read (readMaybe)


-- | Provides access to causal model entities from other modules.
class HasCausal v where
  _Causal :: Prism' v (Causal v)


-- | Events are classified by their safety significance.
data EventType =
  SimpleEvent  -- ^ No safety significance in itself.
  | MishapEvent  -- ^ An instance of a hazard.
  | ControlEvent   -- ^ Associated with a design feature that helps to prevent mishaps or
             -- reduce their impact.
  deriving (Eq, Ord, Enum, Bounded)

instance Show EventType where
  show = T.unpack . eventTypeName

instance ToJSON EventType where
  toJSON = Data.Aeson.String . eventTypeJson

instance FromJSON EventType where
  parseJSON = withText "Event type" $ \txt ->
      case M.lookup txt tbl of
        Just v -> return v
        Nothing -> fail $ "Not an Event type: " <> show txt
    where
      tbl = M.fromList $ [(eventTypeJson t, t) | t <- [SimpleEvent .. ControlEvent]]

eventTypeJson :: EventType -> Text
eventTypeJson = (<> "Event") . eventTypeName

eventTypeName :: EventType -> Text
eventTypeName SimpleEvent = "Simple Event"
eventTypeName MishapEvent = "Mishap Event"
eventTypeName ControlEvent = "Control Event"


-- | The variants of "Event".
eventVariants :: [Variant a]
eventVariants = map (Variant . eventTypeName) [SimpleEvent .. ControlEvent]


-- | An event is a stream of occurences, each of which is considered to be instantaneous at a
-- particular time.
data Event = Event {
    eventType :: EventType,
    _eventName :: Name,
    _eventText :: Text,
    _eventHidden :: Bool
  } deriving (Eq)

instance ToJSON Event where
  toJSON ev = object [
      "type" .= eventType ev,
      "name" .= (ev ^. eventName),
      "text" .= (ev ^. eventText),
      "hidden" .= (ev ^. eventHidden)
    ]

instance FromJSON Event where
  parseJSON = withObject "Event" $ \v ->
    Event <$>
      v .: "type" <*>
      v .: "name" <*>
      v .: "text" <*>
      v .: "hidden"


eventName :: Lens' Event Name
eventName = lens _eventName $ \v n -> v{_eventName = n}

eventText :: Lens' Event Text
eventText = lens _eventText $ \v t -> v{_eventText = t}

eventHidden :: Lens' Event Bool
eventHidden = lens _eventHidden $ \v b -> v{_eventHidden = b}


-- | A system or subsystem can be in one of a number of states, and transitions between those
-- states in response to events.
data State = State {
    _stateName :: Name,
    _stateText :: Text,
    _stateHidden :: Bool
  } deriving (Eq)

instance ToJSON State where
  toJSON st = object [
      "type" .= ("State" :: Text),
      "name" .= (st ^. stateName),
      "text" .= (st ^. stateText),
      "hidden" .= (st ^. stateHidden)
    ]

instance FromJSON State where
  parseJSON = withObject "State" $ \v -> do
    checkType v "State"
    State <$>
        v .: "name" <*>
        v .: "text" <*>
        v .:? "hidden" .!= False


stateName :: Lens' State Name
stateName = lens _stateName $ \v n -> v{_stateName = n}

stateText :: Lens' State Text
stateText = lens _stateText $ \v t -> v{_stateText = t}

stateHidden :: Lens' State Bool
stateHidden = lens _stateHidden $ \v b -> v{_stateHidden = b}


data LogicType =
  LogicCountState Integer  -- ^ Output is true if at least this many guard inputs are true.
  | LogicAndState   -- ^ Output is true iff all guard inputs are true. Less error prone than
              -- using "LogicCountState".
  | LogicAndEvent  -- ^ Output triggered when all input triggers have occured since last output.
  | LogicOrEvent -- ^ Output triggers if any input triggers.
    deriving (Eq, Show, Read)


-- | True iff the logic element processes state guards.
logicIsState :: LogicType -> Bool
logicIsState LogicCountState {} = True
logicIsState LogicAndState = True
logicIsState _ = False


-- | True iff the logic element processes event triggers.
logicIsEvent :: LogicType -> Bool
logicIsEvent LogicAndEvent = True
logicIsEvent LogicOrEvent = True
logicIsEvent _ = False


-- | What type of logic is this?
logicTypeName :: LogicType -> Variant a
logicTypeName t = if logicIsState t then causalStateLogicVariant else causalEventLogicVariant



-- | Causal logic provides composite guards and triggers based on a number of inputs. The details
-- depend on the type of the logic element.
data Logic = Logic {
    logicType :: LogicType,
    _logicName :: Name,
    _logicHidden :: Bool
  } deriving (Eq)

instance ToJSON Logic where
  toJSON (Logic typ nm h) = object $
    ["name" .= nm, "hidden" .= h] ++
      (case typ of
        LogicCountState n -> ["type" .= ("LogicCountState" :: Text), "n" .= n]
        t -> ["type" .= show t])

instance FromJSON Logic where
  parseJSON = withObject "Logic element" $ \v -> do
    nm <- v .: "name"
    h <- v .: "hidden"
    v .: "type" >>= \case
      "LogicCountState" -> do
        n <- v .: "n"
        return $ Logic (LogicCountState n) nm h
      str -> case readMaybe str of
        Just typ -> return $ Logic typ nm h
        Nothing -> fail "Unrecognised logic element type"


logicName :: Lens' Logic Name
logicName = lens _logicName $ \v n -> v {_logicName = n}

logicHidden :: Lens' Logic Bool
logicHidden = lens _logicHidden $ \v h -> v {_logicHidden = h}

-- | For "LogicCountState" elements there is a count. Otherwise there isn't.
logicCount :: Traversal' Logic Integer
logicCount f lg@(Logic (LogicCountState n) _ _) =
  (\n1 -> lg{logicType = LogicCountState n1}) <$> f n
logicCount _ lg = pure lg


-- | An instance of a component. A component can be instantiated multiple times.
newtype Instance = Instance {
    _instanceName :: Name
  } deriving (Eq)

instance ToJSON Instance where
  toJSON i = object [
      "type" .= ("instance" :: Text),
      "name" .= (i ^. instanceName)
    ]

instance FromJSON Instance where
  parseJSON = withObject "Instance" $ \v -> do
    checkType v "instance"
    Instance <$> v .: "name"

instanceName :: Lens' Instance Name
instanceName = lens _instanceName $ \v n -> v{_instanceName = n}


data Edge =
  -- | Describes a state transition mediated by an event,
  -- and hence a temporal order between states. A temporal edge connects a state
  -- to an event or an event to a state, similar to arcs in Petri nets.
  TemporalEdge {
      _edgeName :: Name
    }
  -- | Carries an event to trigger another event.
  | TriggerEdge {
      _edgeName :: Name
    }
  -- | Carries a state which may then be used as the condition of
  -- an event.
  | GuardEdge {
      _edgeName :: Name,
      _edgeInhibit :: Bool   -- ^ True if this guard inhibits its target.
    }
  deriving (Eq)


instance ToJSON Edge where
  toJSON (TemporalEdge nm) = object [
      "type" .= ("TemporalEdge" :: Text),
      "name" .= nm
    ]
  toJSON (TriggerEdge nm) = object [
      "type" .= ("TriggerEdge" :: Text),
      "name" .= nm
    ]
  toJSON (GuardEdge nm inhibit) = object [
      "type" .= ("GuardEdge" :: Text),
      "name" .= nm,
      "inhibit" .= inhibit
    ]

instance FromJSON Edge where
  parseJSON = withObject "Edge" $ \v -> v .: "type" >>= \case
    "TemporalEdge" -> TemporalEdge <$> v .: "name"
    "TriggerEdge" -> TriggerEdge <$> v .: "name"
    "GuardEdge" -> GuardEdge <$> v .: "name" <*> v .: "inhibit"
    txt -> fail $ "Not an edge type: " <> T.unpack txt


edgeName :: Lens' Edge Name
edgeName = lens _edgeName $ \s t -> s{_edgeName = t}


-- | Name of the type of the edge.
edgeTypeName :: Edge -> Variant a
edgeTypeName TemporalEdge {} = temporalEdgeVariant
edgeTypeName TriggerEdge {} = triggerEdgeVariant
edgeTypeName GuardEdge {} = guardEdgeVariant


-- | True iff this "GuardEdge" inhibits its target event. Does not apply to other "Edge" values.
edgeInhibit :: Traversal' Edge Bool
edgeInhibit f e@GuardEdge {} = (\i1 -> e {_edgeInhibit = i1}) <$> f (_edgeInhibit e)
edgeInhibit _ e = pure e


{- | Systems are modelled as collections of components. Each component is an instance of a
component model. For instance a fire alarm would have a component model describing its
behaviour. A system with dual redundant fire alarms would instantiate this component model twice.

Part models contain cause-effect networks. It isn't meaningful for the same state or event to
appear in two different component models, so these are required to be children of the component
model who's diagrams they appear in.
-}
newtype ComponentModel v = ComponentModel {_componentName :: Name} deriving (Eq)

instance ToJSON (ComponentModel v) where
  toJSON (ComponentModel nm) = object [
      "type" .= ("CausalComponent" :: Text),
      "name" .= nm
    ]

instance FromJSON (ComponentModel v) where
  parseJSON = withObject "Causal component" $ \v -> do
    checkType v "CausalComponent"
    ComponentModel <$> v .: "name"


componentName :: Iso' (ComponentModel v) Name
componentName = iso _componentName ComponentModel


-- | An early version of components had a single diagram belonging to the parent component.
-- Where such a diagram is found it is replaced by
-- a "ComponentModel" entity of the same name, and the diagram is moved to a new child entity.
fixOldComponents :: (EntityClass v, HasCausal v) => ModelEdit v (Causal v) ()
fixOldComponents = fromHere $ do
    goToRoot
    traverseChildren
    checkModel
  where
    traverseChildren = do
      childs <- M.elems <$> currentChildren
      forM_ childs $ \c -> fromHere $ do
        P.moveDown $ const $ return c
        fixCurrent
    fixCurrent =
      current >>= \case
        Nothing -> return ()  -- Should not happen.
        Just ent -> case ent ^? entityContents . _Causal of
          Nothing -> traverseChildren  -- Maybe this has a causal component as a descendant.
          Just (CausalDiagram diag) -> do  -- What we are looking for.
            void $ modifyValue $ const $ CausalComponent $ ComponentModel $
                diag ^. diagramEntityName
            let
              newDiagram =
                CausalDiagram $ diagramEntityName . nameText %~ (<> " Diagram") $ diag
            void $ mkEntity $ newDiagram ^. re _Causal
          Just _ -> return ()  -- Don't traverse inside any existing causal models.


-- | Component diagrams show the relationships between the entities in a component model.
type ComponentDiagram v = DiagramEntity (CausalNet v)


componentCanHaveChild :: (HasEvidence v) => Prism' v (Causal v) -> v -> Bool
componentCanHaveChild prsm v = case v ^? prsm of
    Just CausalEvent {} ->  True
    Just CausalState {} -> True
    Just CausalEdge {} -> True
    Just CausalLogic {} -> True
    Just CausalInstance {} -> True
    Just CausalDiagram {} -> True
    Just _ -> False
    Nothing -> case v ^? _Trace of
      Just _ -> True
      Nothing -> False


-- | Non-hidden model entities that are children of the component.
instanceExports :: (EntityClass v) =>
  ModelId  -- ^ The instance we want the exports for.
  -> ModelEdit v (Causal v) [Entity v]
instanceExports modelId = do
  prsm <- usingTraversal return
  components <- queryRelations $ NR.relation modelId componentInstanceRelation
  case S.toList components of
    [] ->   -- Instance of no component.
      return []
    component : _ -> do   -- One or more components. Pick the first and ignore any others.
      goToEntity component
      childs <- M.toList <$> currentChildren
      result <- forM childs $ \(_, childId) -> do
        goToEntity childId
        current >>= \case
          Just child ->
            case child ^? entityContents . cloneTraversal prsm . to causalExported of
              Just True -> return $ Just child
              _ -> return Nothing
          Nothing -> return Nothing
      return $ catMaybes result


-- | CausalType is a unit type that exists only for its instances. In particular the
-- "DiagramTypeClass" instance means that it is a type of diagram featuring boxes and arrows that
-- are avatars of entities of the "Causal" data type.
data CausalType v = CausalType

instance (
    AppearanceClass (CausalType v) v,
    Editable HadesRender v,
    EntityClass v,
    HasDiagrams HadesRender v,
    HasCausal v,
    HasEvidence v,
    GSN.HasGsn v,
    HasLookups v,
    HasReports v) =>
  DiagramTypeClass (CausalType v) v where
    data BoxType (CausalType v) =
        StateBox | EventBox | LogicBox | InstanceBox | HazardBox | GoalBox | EvidenceBox
        deriving (Read, Show, Eq)
    data ArrowType (CausalType v) =
        TemporalArrow | TriggerArrow | GuardArrow | HazardArrow | TraceArrow
        deriving (Eq, Read, Show)
    data ModelSubtype (CausalType v) =
      CausalEvent Event
      | CausalState State
      | CausalLogic Logic
      | CausalInstance Instance
      | CausalEdge Edge
      | CausalComponent (ComponentModel v)
      | CausalDiagram (ComponentDiagram v)
    mkBox diagId modelId typ peg bound =
      case typ of
        StateBox -> Box diagId modelId typ peg $ Oval bound
        EventBox -> Box diagId modelId typ peg $ Parallelogram bound
        LogicBox -> Box diagId modelId typ peg $ Diamond bound
        InstanceBox -> Box diagId modelId typ peg $ Rectangle bound
        HazardBox -> Box diagId modelId typ peg $ Rectangle bound
        GoalBox -> Box diagId modelId typ peg $ Rectangle bound
        EvidenceBox -> Box diagId modelId typ peg $ Rectangle bound
    boxTypeDraw = drawCausalBox
    boxChildren = causalBoxChildren
    boxChildConnection = causalBoxChildConnection
    boxCanPeg _ _ = return Nothing
    avatarType = causalAvatarType
    diagramTypeName _ = "Causal Net"
    arrowTailRelation TraceArrow = traceTailRelation
    arrowTailRelation _ = NR.edgeFromRelation
    arrowHeadRelation TraceArrow = traceHeadRelation
    arrowHeadRelation _ = NR.edgeToRelation
    arrowTailExportRelation _ = Just arrowTailInstance
    arrowHeadExportRelation _ = Just arrowHeadInstance
    arrowHeadDraw _ TemporalArrow = openArrowHead 45 10
    arrowHeadDraw _ TriggerArrow = closedArrowHead 30 10 True
    arrowHeadDraw causal GuardArrow =
      case causal ^? _Just . _CausalEdge . edgeInhibit of
        Just True -> multiArrowHead 1 [closedArrowHead 30 10 False, circleArrowHead 10 False]
        _ -> closedArrowHead 30 10 False
    arrowHeadDraw _ HazardArrow = closedArrowHead 90 10 False
    arrowHeadDraw _ TraceArrow = traceArrowHead
    arrowLineDashes TraceArrow = traceArrowDash
    arrowLineDashes _ = []
    arrowCanConnect = causalArrowCanConnect

instance (
    AppearanceClass (CausalType v) v,
    Editable HadesRender v,
    EntityClass v,
    HasDiagrams HadesRender v,
    HasCausal v,
    HasEvidence v,
    GSN.HasGsn v,
    HasLookups v,
    HasReports v) =>
  Eq (ModelSubtype (CausalType v)) where
    CausalEvent e1 == CausalEvent e2        = e1 == e2
    CausalState s1 == CausalState s2        = s1 == s2
    CausalLogic l1 == CausalLogic l2        = l1 == l2
    CausalInstance i1 == CausalInstance i2  = i1 == i2
    CausalEdge e1 == CausalEdge e2          = e1 == e2
    CausalComponent c1 == CausalComponent c2 = c1 == c2
    CausalDiagram d1 == CausalDiagram d2    = d1 == d2
    _ == _  = False

instance ToJSON (BoxType (CausalType v)) where
  toJSON = String . T.pack . show

instance FromJSON (BoxType (CausalType v)) where
  parseJSON = withText "Causal Box type" $ \str ->
    case readMaybe (T.unpack str) of
      Just t -> return t
      Nothing -> fail $ "Not a Causal Box type: " <> show str

instance ToJSON (ArrowType (CausalType v)) where
  toJSON = String . T.pack . show

instance FromJSON (ArrowType (CausalType v)) where
  parseJSON = withText "Causal Arrow type" $ \str ->
    case readMaybe (T.unpack str) of
      Just t -> return t
      Nothing -> fail $ "Not a Causal Arrow type: " <> show str

instance (
    AppearanceClass (CausalType v) v,
    Editable HadesRender v,
    EntityClass v,
    HasCausal v,
    HasDiagrams HadesRender v,
    HasEvidence v,
    GSN.HasGsn v,
    HasLookups v,
    HasReports v) =>
  ToJSON (ModelSubtype (CausalType v)) where
    -- All the variant types have a "type" field in their JSON.
    toJSON (CausalEvent e) = toJSON e
    toJSON (CausalState s) = toJSON s
    toJSON (CausalLogic e) = toJSON e
    toJSON (CausalInstance i) = toJSON i
    toJSON (CausalEdge e) = toJSON e
    toJSON (CausalComponent m) = toJSON m
    toJSON (CausalDiagram d) = toJSON d

instance (
    AppearanceClass (CausalType v) v,
    Editable HadesRender v,
    EntityClass v,
    HasCausal v, HasDiagrams HadesRender v,
    HasEvidence v,
    GSN.HasGsn v,
    HasLookups v,
    HasReports v) =>
  FromJSON (ModelSubtype (CausalType v)) where
    parseJSON v =
      CausalEvent <$> parseJSON v
      <|> CausalState <$> parseJSON v
      <|> CausalLogic <$> parseJSON v
      <|> CausalInstance <$> parseJSON v
      <|> CausalEdge <$> parseJSON v
      <|> CausalComponent <$> parseJSON v
      <|> CausalDiagram <$> parseJSON v

instance (EntityClass v) => Reflective (ModelSubtype (CausalType v)) where
  reflectiveDefaults = [
      mkEvent SimpleEvent, mkEvent MishapEvent, mkEvent ControlEvent,
      CausalState $ State nemo "" False,
      CausalComponent $ ComponentModel nemo,
      CausalDiagram $ DiagramEntity nemo mempty mempty
    ]
    ++ map (CausalLogic . (\t -> Logic t nemo False))
        [LogicCountState 1, LogicAndState, LogicAndEvent, LogicOrEvent]
    ++ map CausalEdge [TemporalEdge nemo, TriggerEdge nemo, GuardEdge nemo False]
    where
      nemo = Name ""
      mkEvent typ = CausalEvent $ Event typ nemo "" False
  reflectiveName (CausalEvent e) = Variant $ eventTypeName $ eventType e
  reflectiveName CausalState {} = causalStateVariant
  reflectiveName (CausalLogic l) = logicTypeName $ logicType l
  reflectiveName CausalInstance {} = causalInstanceVariant
  reflectiveName (CausalEdge e) = edgeTypeName e
  reflectiveName CausalComponent {} = causalComponentVariant
  reflectiveName CausalDiagram {} = causalDiagramVariant
  reflectiveBuiltIn CausalEvent {} = [nameField, descriptionField, hiddenField]
  reflectiveBuiltIn CausalState {} = [nameField, descriptionField, hiddenField]
  reflectiveBuiltIn (CausalLogic (Logic LogicCountState {} _ _)) =
    [nameField, countField, hiddenField]
  reflectiveBuiltIn CausalLogic {} = [nameField, hiddenField]
  reflectiveBuiltIn CausalInstance {} = [nameField]
  reflectiveBuiltIn (CausalEdge GuardEdge {}) = [nameField, inhibitField]
  reflectiveBuiltIn CausalEdge {} = [nameField]
  reflectiveBuiltIn CausalComponent {} = [nameField]
  reflectiveBuiltIn CausalDiagram {} = [nameField]
  reflectiveGet (CausalEvent e) = M.fromList [
      (nameField, ExtText $ e ^. eventName . nameText),
      (descriptionField, ExtText $ e ^. eventText),
      (hiddenField, ExtBool $ e ^. eventHidden)]
  reflectiveGet (CausalState s) = M.fromList [
      (nameField, ExtText $ s ^. stateName . nameText),
      (descriptionField, ExtText $ s ^. stateText)]
  reflectiveGet (CausalLogic (Logic (LogicCountState n) (Name nm) hidden)) = M.fromList [
      (nameField, ExtText nm),
      (countField, ExtInt n),
      (hiddenField, ExtBool hidden)]
  reflectiveGet (CausalLogic (Logic _ (Name nm) hidden)) = M.fromList [
      (nameField, ExtText nm),
      (hiddenField, ExtBool hidden)]
  reflectiveGet (CausalInstance (Instance (Name n))) = M.singleton nameField (ExtText n)
  reflectiveGet (CausalEdge (GuardEdge (Name n) inhibit)) = M.fromList [
      (nameField, ExtText n),
      (inhibitField, ExtBool inhibit)]
  reflectiveGet (CausalEdge e) =
      M.singleton nameField $ ExtText $ e ^. edgeName . nameText
  reflectiveGet (CausalComponent (ComponentModel nm)) =
      M.singleton nameField $ ExtText $ nm ^. nameText
  reflectiveGet (CausalDiagram d) =
      M.singleton nameField $ ExtText $ d ^. diagramEntityName . nameText
  reflectiveSet (CausalEvent e) = do
      nm <- Name . T.strip <$> extract (e ^. eventName . nameText) _ExtText nameField
      txt <- extract (e ^. eventText) _ExtText descriptionField
      h <- extract (e ^. eventHidden) _ExtBool hiddenField
      return $ CausalEvent $ Event (eventType e) nm txt h
  reflectiveSet (CausalState s) = do
      nm <- Name . T.strip <$> extract (s ^. stateName . nameText) _ExtText nameField
      txt <- extract (s ^. stateText) _ExtText descriptionField
      h <- extract (s ^. stateHidden) _ExtBool hiddenField
      return $ CausalState $ State nm txt h
  reflectiveSet (CausalLogic (Logic (LogicCountState n) (Name nm) hidden)) = do
      nm1 <- extract nm _ExtText nameField
      n1 <- extract n _ExtInt countField
      hidden1 <- extract hidden _ExtBool hiddenField
      return $ CausalLogic (Logic (LogicCountState n1) (Name $ T.strip nm1) hidden1)
  reflectiveSet (CausalLogic l) = do
      nm <- Name . T.strip <$> extract (l ^. logicName . nameText) _ExtText nameField
      hidden <- extract (l ^. logicHidden) _ExtBool hiddenField
      return $ CausalLogic $ Logic (logicType l) nm hidden
  reflectiveSet (CausalInstance i) = do
      nm <- Name . T.strip <$> extract (i ^. instanceName. nameText) _ExtText nameField
      return $ CausalInstance $ instanceName .~ nm $ i
  reflectiveSet (CausalEdge (GuardEdge (Name n) inhibit)) = CausalEdge <$>
      (GuardEdge <$>
        (Name . T.strip <$> extract n _ExtText nameField) <*>
        extract inhibit _ExtBool inhibitField)
  reflectiveSet (CausalEdge e) = do
      nm <- extract (e ^. edgeName . nameText) _ExtText nameField
      return $ CausalEdge $ edgeName . nameText .~ T.strip nm $ e
  reflectiveSet (CausalComponent (ComponentModel nm1)) = do
      nm2 <- extract (nm1 ^. nameText) _ExtText nameField
      return $ CausalComponent $ ComponentModel $ Name $ T.strip nm2
  reflectiveSet (CausalDiagram d) = do
      nm <- Name . T.strip <$> extract (d ^. diagramEntityName . nameText) _ExtText nameField
      return $ CausalDiagram $ diagramEntityName .~ nm $ d
  reflectiveBuiltInRefs =
      -- Ordinary arrow connections.
      arrowRelations temporalEdgeVariant
          (causalStateVariant : eventVariants)
          (causalStateVariant : eventVariants)
      <> arrowRelations triggerEdgeVariant
          (causalEventLogicVariant : eventVariants)
          (causalEventLogicVariant : eventVariants)
      <> arrowRelations guardEdgeVariant
          [causalStateVariant, causalStateLogicVariant]
          (causalStateLogicVariant : eventVariants)
      -- Arrows connected to instances have extra relations.
      <> mkOneToMany arrowHeadInstance
          [causalInstanceVariant]
          [triggerEdgeVariant, guardEdgeVariant]
      <> mkOneToMany arrowTailInstance
          [causalInstanceVariant]
          [triggerEdgeVariant, guardEdgeVariant]
      <> traceArrowRelations
          (causalStateVariant : eventVariants)
          [GSN.goalVariant, evidenceVariant]
      -- Components have instances
      <> mkOneToMany componentInstanceRelation [causalComponentVariant] [causalInstanceVariant]
  reflectiveArrows = M.fromList [
      (temporalEdgeVariant, (NR.edgeFromRelation, NR.edgeToRelation)),
      (triggerEdgeVariant, (NR.edgeFromRelation, NR.edgeToRelation)),
      (guardEdgeVariant, (NR.edgeFromRelation, NR.edgeToRelation)),
      (hazardEdgeVariant, (NR.edgeFromRelation, NR.edgeToRelation))
    ]


-- | Is this a causal diagram?
causalIsDiagram :: Causal v -> Bool
causalIsDiagram CausalDiagram {} = True
causalIsDiagram _ = False


-- | The wrapper for a causal diagram.
causalDiagramWrapper :: (
    AppearanceClass (CausalType v) v,
    Editable HadesRender v,
    EntityClass v,
    HasCausal v,
    HasDiagrams HadesRender v,
    HasEvidence v,
    GSN.HasGsn v,
    HasLookups v,
    HasReports v) =>
  Entity v -> ModelEdit v (Causal v) (Maybe (EntityWrapper HadesRender v))
causalDiagramWrapper ent =
  usingTraversal $ \trv ->
    case ent ^? entityContents . trv . _CausalDiagram of
      Nothing -> return Nothing
      Just de -> do
        ctx <- compileAppearance
            (de ^. diagramEntityData) $
            GenericContext (entityId ent) mempty
        return $ Just $ DiagramWrapper {
            wrappedEntityId = entityId ent,
            diagramToEdit = de ^. diagramEntityContents,
            diagramEntityTraversal = _CausalDiagram,
            diagramTraversal = trv,
            diagramDialog = elementDialog,
            diagramContext = ctx,
            diagramMenu = mempty,
            diagramToolbar = const $ causalToolbar (entityId ent) $ ent ^. entityParent
          }


-- | Entity types that appear in causal diagrams.
causalDiagramEntityTypes :: DiagramEntityTypes v
causalDiagramEntityTypes = M.fromList [
    (causalDiagramVariant,
        eventVariants ++ [causalStateVariant, causalStateLogicVariant, causalEventLogicVariant,
        evidenceVariant, causalInstanceVariant
      ])
  ]


-- | The subtype of causal entities in models.
type Causal v = ModelSubtype (CausalType v)


-- | Elements that can appear in diagrams of cause and effect.
type CausalNet v = Element (CausalType v) v


_CausalEvent :: Prism' (Causal v) Event
_CausalEvent = prism CausalEvent $ \case {CausalEvent e -> Right e; v -> Left v}

_CausalState :: Prism' (Causal v) State
_CausalState = prism CausalState $ \case {CausalState s -> Right s; v -> Left v}

_CausalEdge :: Prism' (Causal v) Edge
_CausalEdge = prism CausalEdge $ \case {CausalEdge e -> Right e; v -> Left v}

_CausalLogic :: Prism' (Causal v) Logic
_CausalLogic = prism CausalLogic $ \case {CausalLogic l -> Right l; v -> Left v}

_CausalInstance :: Prism' (Causal v) Instance
_CausalInstance = prism CausalInstance $ \case {CausalInstance i -> Right i; v -> Left v}

_CausalComponent :: Prism' (Causal v) (ComponentModel v)
_CausalComponent = prism CausalComponent $ \case {CausalComponent c -> Right c; v -> Left v}

_CausalDiagram :: Prism' (Causal v) (ComponentDiagram v)
_CausalDiagram = prism CausalDiagram $ \case {CausalDiagram d -> Right d; v -> Left v}

causalName :: Lens' (Causal v) Name
causalName f = \case
  CausalEvent e -> CausalEvent <$> eventName f e
  CausalState s -> CausalState <$> stateName f s
  CausalLogic l -> CausalLogic <$> logicName f l
  CausalInstance i -> CausalInstance <$> instanceName f i
  CausalEdge e -> CausalEdge <$> edgeName f e
  CausalComponent (ComponentModel nm) -> CausalComponent . ComponentModel <$> f nm
  CausalDiagram d -> CausalDiagram <$> diagramEntityName f d


-- | Is this entity exported?
causalExported :: Causal v -> Bool
causalExported (CausalEvent e) = not $ e ^. eventHidden
causalExported (CausalState s) = not $ s ^. stateHidden
causalExported (CausalLogic l) = not $ l ^. logicHidden
causalExported _ = False


-- | True if this entity can be moved to another parent.
--
-- Entities representing elements in causal components are tied to their parent component and
-- cannot be moved or placed in diagrams belonging to other components.
causalCanMove :: Causal v -> Bool
causalCanMove CausalComponent {} = True
causalCanMove _ = False


-- | Create a new instance of the component and return an avatar for it.
createInstance :: (
    AppearanceClass (CausalType v) v,
    Editable HadesRender v,
    EntityClass v,
    HasCausal v,
    HasDiagrams HadesRender v,
    HasEvidence v,
    GSN.HasGsn v,
    HasLookups v,
    HasReports v) =>
  Entity v -> Point -> Delta (CausalNet v) (Maybe (CausalNet v))
createInstance ent pt = do
  diagId <- use $ deltaContext . to diagramId
  newInst <- liftBase $ lift $ do
    goToEntity diagId
    P.moveUp  -- Causality models have no packages inside, so this is guaranteed to work.
    nm <- modelNewName $ Name (ent ^. entityContents . name . nameText <> " ")
    newInst <- mkEntity $ CausalInstance (Instance nm) ^. re _Causal
    addCheckedRelation componentInstanceRelation newInst ent
    fromHere $ do
      goToEntity $ entityId ent
      currentIsModified  -- New relationship for the component.
    return newInst
  let
    p1 = pt `movePoint` (-100, -50)
    p2 = pt `movePoint` (100, 50)
    shape = Rectangle $ BoundBox p1 p2
  Just <$> addItem (\newId -> ElemBox $ Box newId (entityId newInst) InstanceBox Nothing shape)



-- | Is the entity a sibling of this diagram?
isSibling :: (Avatar p v w (CausalNet v)) => Entity v -> Delta (CausalNet v) Bool
isSibling entity = do
  diagId <- use $ deltaContext . to diagramId
  liftBase $ lift $ do
    goToEntity diagId
    P.moveUp
    diagramParent <- currentId
    goToEntity $ entityId entity
    P.moveUp
    entParent <- currentId
    return $ diagramParent == entParent


causalAvatarType :: (
    AppearanceClass (CausalType v) v,
    Editable HadesRender v,
    EntityClass v,
    HasCausal v,
    HasDiagrams HadesRender v,
    HasEvidence v,
    GSN.HasGsn v,
    HasLookups v,
    HasReports v) =>
  Traversal' v (Causal v) -> v -> AvatarCreation (CausalType v) v
causalAvatarType prsm v = case v ^? prsm of
  Just CausalEvent {} -> AvatarScript $ createBoxAvatar EventBox
  Just CausalState {} -> AvatarScript $ createBoxAvatar StateBox
  Just CausalLogic {} -> AvatarScript $ createBoxAvatar LogicBox
  Just (CausalEdge TemporalEdge {}) -> AvatarScript $ createArrowAvatar TemporalArrow
  Just (CausalEdge TriggerEdge {}) -> AvatarScript $ createArrowAvatar TriggerArrow
  Just (CausalEdge GuardEdge {}) -> AvatarScript $ createArrowAvatar GuardArrow
  Just CausalInstance {} -> AvatarScript $ createBoxAvatar InstanceBox
  Just CausalComponent {} -> AvatarScript createInstance
  Just CausalDiagram {} -> NoAvatar
  Nothing -> if v ^? GSN._Gsn . GSN._GsnNodeItem . to GSN.gsnNodeType == Just GSN.GoalBox
    then SimpleBox GoalBox
    else case v ^? _Evidence of
      Just _ -> SimpleBox EvidenceBox
      Nothing -> case v ^? _Trace of
        Just _ -> SimpleArrow TraceArrow
        Nothing -> NoAvatar


-- | Create a box avatar for an entity.
createBoxAvatar :: (
    AppearanceClass (CausalType v) v,
    Editable HadesRender v,
    EntityClass v,
    HasCausal v,
    HasDiagrams HadesRender v,
    HasEvidence v,
    GSN.HasGsn v,
    HasLookups v,
    HasReports v) =>
  BoxType (CausalType v)
  -> Entity v
  -> Point
  -> Delta (CausalNet v) (Maybe (CausalNet v))
createBoxAvatar typ entity basePoint = do
    sib <- isSibling entity
    if sib
      then do
        newItem <- addItem $ \uuid ->
          ElemBox $ mkBox uuid (entityId entity) typ Nothing defaultBox
        return $ Just newItem
      else return Nothing
  where
    defaultBox = BoundBox
        (basePoint `movePoint` (-100,-50))
        (basePoint `movePoint` (100,50))


-- | Create an arrow avatar for an entity
createArrowAvatar :: (
    AppearanceClass (CausalType v) v,
    Editable HadesRender v,
    EntityClass v,
    HasCausal v,
    HasDiagrams HadesRender v,
    HasEvidence v,
    GSN.HasGsn v,
    HasLookups v,
    HasReports v) =>
  ArrowType (CausalType v)
  -> Entity v
  -> Point
  -> Delta (CausalNet v) (Maybe (CausalNet v))
createArrowAvatar typ entity basePoint = do
    sib <- isSibling entity
    if sib
      then do
        (eFrom, eTo) <- liftBase $ lift $ edgeLinks typ target
        fromConnector <- avatarConnector eFrom basePoint $
            basePoint `movePoint` (-arrowLength, 0)
        toConnector <- avatarConnector eTo basePoint $
            basePoint `movePoint` (arrowLength, 0)
        newItem <- addItem $ \uuid ->
          ElemArrow $ Arrow uuid target typ $
            LineShape (fromConnector, toConnector) mempty
        return $ Just newItem
      else return Nothing
  where
    target = entityId entity
    arrowLength = 50


drawCausalBox :: (
      AppearanceClass (CausalType v) v,
      Editable HadesRender v,
      EntityClass v,
      HasDiagrams HadesRender v,
      HasCausal v,
      HasEvidence v,
      GSN.HasGsn v,
      HasLookups v,
      HasReports v) =>
  Box (CausalType v) v
  -> HadesLayout ()
  -> Maybe Colour
  -> Delta (CausalNet v) (BoundBox -> HadesRender ())
drawCausalBox (Box _ modelId typ _ shape1) contents border = do
    ent <- liftBase $ lift $ goToEntity modelId >> current
    exports <- liftBase $ lift $ instanceExports modelId
    case typ of
      GoalBox -> return $ GSN.drawGsnBoxRaw
          GSN.GoalBox
          (ent ^? _Just . entityContents . GSN._Gsn . GSN._GsnNodeItem)
          contents
          border
          shape1
      EvidenceBox -> return $ drawEvidence ent contents border
      StateBox -> return $ drawCausalStateBox ent contents border shape1
      EventBox  -> return $ drawCausalEventBox ent contents border shape1
      LogicBox -> return $ drawCausalLogicBox ent border shape1
      InstanceBox -> return $ drawCausalInstanceBox exports ent border shape1
      HazardBox -> return $ \boundBox -> do  -- Obsolete type.
        let
          shape2 = shapeDefinition .~ boundBox $ shape1
          (border1, fill) = basicBox 4 (fromMaybe black border) hazardYellow
        local (\e -> e {boxBorderStyle = border1, boxFillStyle = fill}) $
          drawPolygon $ rectangleCorners $ Rectangle $ shape2 ^. shapeDefinition
        textBlock
            "Please Delete"
            ["Hazards are no longer permitted on causal diagrams."]
            (shapeInnerBox shape2)
  where
    hazardYellow = Colour $ C.sRGB24 255 255 200
    black = Colour C.black


drawCausalStateBox :: (BoxShapeClass shape, HasCausal v) =>
  Maybe (Entity v) -> HadesLayout () -> Maybe Colour -> shape -> BoundBox -> HadesRender ()
drawCausalStateBox ent contents border shape1 boundBox = do
    let shape2 = shapeDefinition .~ boundBox $ shape1
    maybe id withLineColour border $ drawOval $ Oval $ shape2 ^. shapeDefinition
    runLayout_ (shapeInnerBox shape2) $ do
      case ent ^? _Just . entityContents . _Causal . _CausalState of
        Just state -> do
          let titleFlag = if state ^. stateHidden then "" else "Ⓢ "
          layoutTitle Nothing $ titleFlag <> state ^. stateName . nameText
        Nothing -> layoutTitle Nothing "Ⓢ State"
      layoutParagraphGap
      contents


drawCausalEventBox :: (BoxShapeClass shape, HasCausal v) =>
  Maybe (Entity v) -> HadesLayout () -> Maybe Colour -> shape -> BoundBox -> HadesRender ()
drawCausalEventBox ent contents border shape1 boundBox = do
    let shape2 = shapeDefinition .~ boundBox $ shape1
    case ent ^? _Just . entityContents . _Causal . _CausalEvent of
      Just event -> withEventStyle (eventType event) $ do
        drawPolygon $ parallelogramCorners $ Parallelogram $ shape2 ^. shapeDefinition
        let titleFlag = if event ^. eventHidden then "" else "🄴 "
        runLayout_ (shapeInnerBox shape2) $ do
          layoutTitle Nothing $ titleFlag <> event ^. eventName . nameText
          layoutParagraphGap
          contents
        case eventType event of
          SimpleEvent -> return ()
          ControlEvent -> drawCornerLetter (shapeBounds shape2) 'C'
          MishapEvent -> drawCornerLetter (shapeBounds shape2) 'M'
      Nothing ->
        maybe id withLineColour border $
          drawPolygon $ parallelogramCorners $ Parallelogram $ shape2 ^. shapeDefinition
          -- Should never happen.
  where
    withEventStyle typ1 act =
      case typ1 of
        SimpleEvent -> act  -- Just draw normally.
        ControlEvent -> do
          let (border1, fill) = basicBox 2 (fromMaybe black border) controlGreen
          local (\e -> e {boxBorderStyle = border1, boxFillStyle = fill}) act
        MishapEvent -> do
          let (border1, fill) = basicBox 2 black mishapRed
          local (\e -> e {boxBorderStyle = border1, boxFillStyle = fill}) act
    controlGreen = Colour $ C.sRGB24 200 255 200
    mishapRed = Colour $ C.sRGB24 255 200 200
    black = Colour C.black


drawCausalLogicBox :: (BoxShapeClass shape, HasCausal v) =>
  Maybe (Entity v) -> Maybe Colour -> shape -> BoundBox -> HadesRender ()
drawCausalLogicBox ent border shape1 boundBox = do
    let shape2 = shapeDefinition .~ boundBox $ shape1
    maybe id withLineColour border $ drawPolygon $ diamondCorners $ shape2 ^. shapeDefinition
    case ent ^? _Just . entityContents . _Causal . _CausalLogic of
      Just logic -> do
        let titleFlag = if logic ^. logicHidden then "" else symbol $ CausalLogic logic
        textBlock
            (titleFlag <> (case logicType logic of
              LogicCountState n -> T.pack $ '≥' : show n
              LogicAndState -> "AND"
              LogicAndEvent -> "All"
              LogicOrEvent -> "Any"))
            [logic ^. logicName . nameText]
            (shapeInnerBox shape2)
      Nothing -> return ()


drawCausalInstanceBox :: (BoxShapeClass shape, EntityClass v, HasCausal v) =>
  [Entity v] -> Maybe (Entity v) -> Maybe Colour -> shape -> BoundBox -> HadesRender ()
drawCausalInstanceBox exports ent border shape1 boundBox = do
    let
      childs = mapMaybe (preview (entityContents . _Causal)) exports
      shape2 = shapeDefinition .~ boundBox $ shape1
    maybe id withLineColour border $
      drawPolygon $ rectangleCorners $ Rectangle $ shape2 ^. shapeDefinition
    void $ runLayout (shapeInnerBox shape2) $
      mapM_ (layoutTitle Nothing) $ ent ^? _Just . entityName . nameText
    zipWithM_ drawExport childs $ childBoxes $ shapeInnerBox shape2
  where
    drawExport causal box = runLayout box $ do
      layoutHLine
      layoutText [(`Pango.layoutSetEllipsize` Pango.EllipsizeModeEnd)] $
          symbol causal <> (causal ^. causalName . nameText)
      layoutHLine


symbol :: Causal v -> Text
symbol CausalEvent {} = "🄴 "  -- U+1F134 - Boxed E
symbol CausalState {} = "Ⓢ "  -- U+24C8 - Circled S
symbol (CausalLogic (Logic t _ _)) = case t of
  LogicCountState {} -> "Ⓢ "
  LogicAndState -> "Ⓢ "
  LogicAndEvent -> "🄴 "
  LogicOrEvent -> "🄴 "
symbol _ = ""


-- | Exported connection points for "ComponentBox" boxes. Returns @[]@ for anything else.
causalBoxChildren :: (
    AppearanceClass (CausalType v) v,
    Editable HadesRender v,
    EntityClass v,
    HasCausal v,
    HasDiagrams HadesRender v,
    HasEvidence v,
    GSN.HasGsn v,
    HasLookups v,
    HasReports v) =>
  Box (CausalType v) v -> Delta (CausalNet v) [(DiagramId, Point -> Bool)]
causalBoxChildren (Box _ modelId typ _ shape) =
    case typ of
      InstanceBox -> do
        childs <- liftBase $ lift $ instanceExports modelId
        return $ zipWith childConnector childs $ childBoxes $ shapeInnerBox shape
      _ -> return []
  where
    childConnector child box = (entityId child, (`inBox` box))


-- | When an arrow is connected to an exported item in a ComponentBox, what point should it
-- end at?
causalBoxChildConnection :: (
    AppearanceClass (CausalType v) v,
    Editable HadesRender v,
    EntityClass v,
    HasCausal v,
    HasDiagrams HadesRender v,
    HasEvidence v,
    GSN.HasGsn v,
    HasLookups v,
    HasReports v) =>
  Box (CausalType v) v -> DiagramId -> Delta (CausalNet v) (Point -> Point)
causalBoxChildConnection (Box _ modelId _ _ shape) childId = do
  childs <- liftBase $ lift $ map entityId <$> instanceExports modelId
  case elemIndex childId childs of
    Just n -> case childBoxes (shapeInnerBox shape) !! n of
      -- (!!) is safe because childBoxes returns an infinite list.
      childBox@(BoundBox p1 p2) -> do
        let
          Point midX midY = childBox ^. boxCentre
          pLeft = Point (p1 ^. pX) midY
          pRight = Point (p2 ^. pX) midY
        return $ \pt -> if pt ^. pX < midX then pLeft else pRight
      _ -> return $ shapeConnectionPoint shape
    Nothing -> cannotHappen
        "causalBoxChildConnection: cannot find chosen item in list."
        $ return $ shapeConnectionPoint shape


-- | Can the arrow connect to the box at the specified end.
causalArrowCanConnect :: (
    AppearanceClass (CausalType v) v,
    Editable HadesRender v,
    EntityClass v,
    HasCausal v,
    HasDiagrams HadesRender v,
    HasEvidence v,
    GSN.HasGsn v,
    HasLookups v,
    HasReports v) =>
  ConnectionDirection
  -> Arrow (CausalType v) v
  -> Box (CausalType v) v
  -> Maybe DiagramId  -- ^ Optional child of the target box.
  -> Delta (CausalNet v) Bool
causalArrowCanConnect
    direction
    (Arrow _ _ arrTyp arrShape)
    (Box _ boxEnt boxTyp _ _)
    mChild
  = do
    prsm <- liftBase $ lift $ usingTraversal return
    let
      otherEnd = case direction of
        ConnectionFrom -> arrShape ^. lineEnds . _2
        ConnectionTo -> arrShape ^. lineEnds . _1
    otherElement <- case otherEnd of
      Connected _ target _ -> use $ deltaDiagram . diagramContents . at target
      Unconnected {} -> return Nothing
    let otherBox = otherElement ^? _Just . _ElemBox
    thisEntity <- liftBase $ lift $ goToEntity boxEnt >> current
    let thisCausal = thisEntity ^? _Just . entityContents . prsm
    case arrTyp of
      TemporalArrow ->  -- Can connect State -> Event or Event -> State, but not Components.
        case boxType <$> otherBox of
          Just StateBox -> return $ boxTyp == EventBox
          Just EventBox -> return $
            boxTyp == StateBox &&
            isJust (thisCausal ^? _Just . _CausalState)
          Nothing -> return $
            boxTyp == EventBox ||
            (boxTyp == StateBox &&
              isJust (thisCausal ^? _Just . _CausalState))
          _ -> return False
      TriggerArrow ->  -- Can connect events or event logics, including exports of Components.
        case thisCausal of
          Just CausalEvent {} -> return True
          Just (CausalLogic l) -> return $ logicIsEvent $ logicType l
          Just CausalInstance {} -> case mChild of
            Just child -> do
              exports <- liftBase $ lift $ instanceExports boxEnt
              case find ((child ==) . entityId) exports of
                Just export -> case export ^? entityContents . prsm of
                  Just CausalEvent {} -> return True
                  Just (CausalLogic l) -> return $ logicIsEvent $ logicType l
                  _ -> return False
                _ -> return False
            Nothing -> return False
          _ -> return False
      GuardArrow ->  -- Can connect (State | State logic) -> (Event | State logic).
        case direction of
          ConnectionFrom ->
            case thisCausal of
              Just CausalState {} -> return True
              Just (CausalLogic l) -> return $ logicIsState $ logicType l
              Just CausalInstance {} -> case mChild of
                Just child -> do
                  exports <- liftBase $ lift $ instanceExports boxEnt
                  case find ((child ==) . entityId) exports of
                    Just export -> case export ^? entityContents . prsm of
                      Just CausalState {} -> return True
                      Just (CausalLogic l) -> return $ logicIsState $ logicType l
                      _ -> return False
                    _ -> return False
                Nothing -> return False
              _ -> return False
          ConnectionTo ->
            case thisCausal of
              Just CausalEvent {} -> return True
              Just (CausalLogic l) -> return $ logicIsState $ logicType l
              Just CausalInstance {} -> case mChild of
                Just child -> do
                  exports <- liftBase $ lift $ instanceExports boxEnt
                  case find ((child ==) . entityId) exports of
                    Just export -> case export ^? entityContents . prsm of
                      Just CausalEvent {} -> return True
                      Just (CausalLogic l) -> return $ logicIsState $ logicType l
                      _ -> return False
                    _ -> return False
                Nothing -> return False
              _ -> return False
      HazardArrow -> return False -- Obsolete type.
      TraceArrow ->
        case direction of
          ConnectionFrom ->
            case thisCausal of
              Just CausalState {} -> return True
              Just CausalEvent {} -> return True
              _ -> return False
          ConnectionTo -> return $ boxTyp `elem` [GoalBox, EvidenceBox]



-- | An infinite list of boxes, stacked vertically starting a line and a half below the top
-- left of the argument and carrying on down. The boxes are high enough for 1 line of text until
-- they reach the bottom of the argument, at which point they become of infinitesmial height and
-- all superimposed at the bottom of the argument. A box that goes over the bottom of the argument
-- will be truncated.
--
-- If the argument is "NoBox" then all the results will be "NoBox" too.
childBoxes :: BoundBox -> [BoundBox]
childBoxes NoBox = repeat NoBox
childBoxes (BoundBox p1 p2) = map crop $ zipWith BoundBox topLefts bottomRights
  where
    limit = min (p2 ^. pY)   -- Limit the argument to the bottom of the box.
    limit1 = min (p2 ^. pY + 0.001)  -- Infinitesimally below the bottom of the box.
    ys = iterate' (+(hadesFontSize*1.5+2)) (p1 ^. pY + hadesFontSize * 1.5)  -- Space for title.
    topLefts = map (Point (p1 ^. pX)) ys
    bottomRights = map (Point (p2 ^. pX)) $ tail ys  -- Safe because ys is infinite.
    crop (BoundBox p3 p4) = BoundBox (pY %~ limit $ p3) (pY %~ limit1 $ p4)
    crop NoBox = NoBox


-- | Context menu for causal entities in the model tree.
causalEntityMenu :: (
    EntityClass v,
    HasCausal v,
    HasDiagrams HadesRender v,
    HasEvidence v,
    GSN.HasGsn v,
    Queryable v) =>
  Entity v
  -> Causal v
  -> Menu (ModelScript HadesRender v (Causal v) (Maybe Text))
causalEntityMenu ent CausalComponent {} =
    Menu [[
        menuItem "Add Diagram" $ do
          v <- insertEntity
              (CausalDiagram (DiagramEntity (Name "Diagram ") mempty mempty) ^. re _Causal)
              (entityId ent)
          logInsertion v,
        subMenu "Add Element" $ Menu [[
            menuItem "Simple Event" $ insertEvent SimpleEvent "Event ",
            menuItem "Mishap Event" $ insertEvent MishapEvent "Mishap ",
            menuItem "Control Event" $ insertEvent ControlEvent "Control "
          ], [
            menuItem "State" $
              insertEntity
                  (CausalState (State (Name "State ") "" False) ^. re _Causal)
                  (entityId ent)
              >>= logInsertion
          ], [
            menuItem "Count Logic" $ insertLogic (LogicCountState 1) "Count ",
            menuItem "AND State" $ insertLogic LogicAndState "AND-",
            menuItem "AND Event" $ insertLogic LogicAndEvent "AND-",
            menuItem "OR Event" $ insertLogic LogicOrEvent "OR-"
          ]]
      ]] <> entityMenu ent
  where
    insertEvent typ nm = do
      v <- insertEntity
          (CausalEvent (Event typ (Name nm) "" False) ^. re _Causal)
          (entityId ent)
      logInsertion v
    insertLogic typ nm = do
      v <- insertEntity
          (CausalLogic (Logic typ (Name nm) False) ^. re _Causal)
          (entityId ent)
      logInsertion v
causalEntityMenu ent CausalDiagram {} = diagramEntityMenu $ entityId ent
causalEntityMenu ent _ = entityMenu ent


-- | Toolbar for Causal Component diagrams.
causalToolbar :: (
    AppearanceClass (CausalType v) v,
    Editable HadesRender v,
    EntityClass v,
    HasCausal v,
    HasDiagrams HadesRender v,
    HasEvidence v,
    GSN.HasGsn v,
    HasLookups v,
    HasReports v) =>
  ModelId  -- ^ Entity ID of this diagram.
  -> ModelId  -- ^ Entity ID of the parent of this diagram.
  -> DeltaToolbar (CausalNet v)
causalToolbar diagId owner = DeltaToolbar
    [[  -- Left hand end.
        DeltaTool "causal_event" "Event" $ DeltaToolAction $
          mkEventTool SimpleEvent "Event ",
        DeltaTool "causal_event_mishap" "Mishap" $ DeltaToolAction $
          mkEventTool MishapEvent "Mishap ",
        DeltaTool "causal_event_control" "Control" $ DeltaToolAction $
          mkEventTool ControlEvent "Control ",
        DeltaTool "causal_state" "State" $ DeltaToolAction $
          mkCausalBox (CausalState $ State (Name "State ") "" False) StateBox
      ], [
        DeltaTool "causal_logic_count" "State Count" $ DeltaToolAction $
          mkCausalBox (CausalLogic $ Logic (LogicCountState 1) (Name "Count ") False) LogicBox,
        DeltaTool "causal_logic_state_and" "State &" $ DeltaToolAction $
          mkCausalBox (CausalLogic $ Logic LogicAndState (Name "AND-") False) LogicBox,
        DeltaTool "causal_logic_event_or" "Event OR" $ DeltaToolAction $
          mkCausalBox (CausalLogic $ Logic LogicOrEvent (Name "OR-") False) LogicBox,
        DeltaTool "causal_logic_event_and" "Event AND" $ DeltaToolAction $
          mkCausalBox (CausalLogic $ Logic LogicAndEvent (Name "AND-") False) LogicBox
      ], [
        DeltaTool "causal_temporal" "Sequence" $ DeltaToolAction $
          mkArrow (CausalEdge $ TemporalEdge (Name "Sequence-")) TemporalArrow,
        DeltaTool "causal_trigger" "Trigger" $ DeltaToolAction $
          mkArrow (CausalEdge $ TriggerEdge (Name "Trigger-")) TriggerArrow,
        DeltaTool "causal_guard" "Guard" $ DeltaToolAction $
          mkArrow (CausalEdge $ GuardEdge (Name "Guard-") False) GuardArrow
      ], [
        DeltaTool "trace-arrow" "Trace" $ DeltaToolAction $
          mkScriptAction $ do
            parent <- liftBase $ lift $ goToEntity owner >> currentId
            newArrowEntityTool False parent (Trace (Name "Trace") ^. re _Trace) TraceArrow
      ], [
        DeltaTool "basic_comment" "Comment" $ DeltaToolAction newCommentTool,
        DeltaTool "emblem-symbolic-link" "Link" $ DeltaToolAction newLinkTool
      ]
    ]
    [
      alignmentTools,
      [viewControlTool
        diagId
        (_CausalDiagram . diagramEntityData)
        compile1
        (constantDialog $ diagramAppearanceDialog causalDiagramVariant)
      ]]  -- Right hand end.
  where
    compile1 m a ctx = fromRight ctx $
      evalModelEdit _Causal m $
      compileAppearance (diagramAppearanceClean a) ctx
    mkCausalBox item shape = mkScriptAction $ do
      parent <- liftBase $ lift $ goToEntity owner >> currentId
      newBoxEntityTool parent (item ^. re _Causal) shape
    mkArrow item shape = mkScriptAction $ do
      parent <- liftBase $ lift $ goToEntity owner >> currentId
      newArrowEntityTool False parent (item ^. re _Causal) shape
    mkEventTool typ nm = mkCausalBox (CausalEvent $ Event typ (Name nm) "" False) EventBox


-- | Context menu fragment for model packages allowing the user to add causal model entities.
addCausalEntityMenu :: (EntityClass v, HasCausal v) =>
  ModelId -> Menu (ModelScript HadesRender v (Causal v) (Maybe Text))
addCausalEntityMenu parent = Menu [[
    menuItem "Add Component" $
      insertEntity (CausalComponent (ComponentModel $ Name "Component ") ^. re _Causal) parent
      >>= logInsertion
  ]]


-- | Icon name for causal entities.
causalIcon :: Causal v -> Text
causalIcon (CausalEvent e) = case eventType e of
  SimpleEvent -> "causal_event"
  MishapEvent -> "causal_event_mishap"
  ControlEvent -> "causal_event_control"
causalIcon CausalState {} = "causal_state"
causalIcon (CausalLogic l) = case logicType l of
  LogicCountState _ -> "causal_logic_count"
  LogicAndState -> "causal_logic_state_and"
  LogicAndEvent -> "causal_logic_event_and"
  LogicOrEvent -> "causal_logic_event_or"
causalIcon CausalInstance {} = "causal_instance"
causalIcon (CausalEdge TemporalEdge {}) = "causal_temporal"
causalIcon (CausalEdge TriggerEdge {}) = "causal_trigger"
causalIcon (CausalEdge GuardEdge {}) = "causal_guard"
causalIcon CausalComponent {} = "causal_component"
causalIcon CausalDiagram {} = "causal_diagram"


-- | Activation script for causal entities.
causalActivation :: (
    EntityClass v,
    HasCausal v,
    HasDiagrams HadesRender v,
    HasEvidence v,
    GSN.HasGsn v,
    Queryable v) =>
  ActivationScript HadesRender v (Causal v)
causalActivation Nothing = return Nothing   -- Root does nothing on activation.
causalActivation (Just e) = do
  prsm <- lift $ usingTraversal return
  case e ^? entityContents . prsm  of
    Nothing -> return Nothing
    Just CausalDiagram {} -> do
      openDiagram $ entityId e
      return Nothing
    Just _ ->
      editEntityProperties $ entityId e
