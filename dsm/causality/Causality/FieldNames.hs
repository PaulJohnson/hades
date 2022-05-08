{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}


{- |

Constant strings used to identify variants, fields and references in GSN models.
-}

module Causality.FieldNames where

import Control.Lens
import Model.Reflection.NamedRelation (Relation)
import Model.Reflection.Types (FieldId, fieldsByName, builtInFields, fieldId)
import Model.Reflection.Values (Variant (..))
import Paths_dsm


temporalEdgeVariant :: Variant a
temporalEdgeVariant = Variant "Sequence Arrow"

triggerEdgeVariant :: Variant a
triggerEdgeVariant = Variant "Trigger Arrow"

guardEdgeVariant :: Variant a
guardEdgeVariant = Variant "Guard Arrow"

hazardEdgeVariant :: Variant a
hazardEdgeVariant = Variant "Hazard Arrow"

threatEdgeVariant :: Variant a
threatEdgeVariant = Variant "Threat Line"

causalStateVariant :: Variant a
causalStateVariant = Variant "State"

causalStateLogicVariant :: Variant a
causalStateLogicVariant = Variant "State Logic"

causalEventLogicVariant :: Variant a
causalEventLogicVariant = Variant "Event Logic"

causalInstanceVariant :: Variant a
causalInstanceVariant = Variant "Instance"

causalComponentVariant :: Variant a
causalComponentVariant = Variant "Component"

causalHazardVariant :: Variant a
causalHazardVariant = Variant "Hazard"

causalDiagramVariant :: Variant a
causalDiagramVariant = Variant "Causal Diagram"

causalControlVariant :: Variant a
causalControlVariant = Variant "Control"

bowTieDiagramVariant :: Variant a
bowTieDiagramVariant = Variant "Bow Tie Diagram"

bowTieRiskEventVariant :: Variant a
bowTieRiskEventVariant = Variant "Bow Tie Event"

hiddenField :: FieldId
hiddenField = fieldsByName builtInFields ^?! ix "Hidden" . to fieldId

inhibitField :: FieldId
inhibitField = fieldsByName builtInFields ^?! ix "Inhibit" . to fieldId

countField :: FieldId
countField = fieldsByName builtInFields ^?! ix "Count" . to fieldId

componentInstanceRelation :: Relation
componentInstanceRelation = "Component instance"

controlThreatRelation :: Relation
controlThreatRelation = "Threat control"

arrowHeadInstance :: Relation
arrowHeadInstance = "Instance input"

arrowTailInstance :: Relation
arrowTailInstance = "Instance output"

-- | Path for icons associated with causal and risk model types.
causalityIcons :: IO FilePath
causalityIcons = getDataDir
