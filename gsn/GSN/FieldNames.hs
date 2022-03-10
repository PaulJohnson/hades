{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}


{- |

Constant strings used to identify variants, fields and references in GSN models.
-}

module GSN.FieldNames where

import Control.Lens
import Model.Reflection.Types
import Model.Reflection.Values (Variant (..))
import Paths_gsn


goalVariant :: Variant a
goalVariant = Variant "Goal"

strategyVariant :: Variant a
strategyVariant = Variant "Strategy"

solutionVariant :: Variant a
solutionVariant = Variant "Solution"

contextVariant :: Variant a
contextVariant = Variant "Context"

assumptionVariant :: Variant a
assumptionVariant = Variant "Assumption"

justificationVariant :: Variant a
justificationVariant = Variant "Justification"

gsnOptionVariant :: Variant a
gsnOptionVariant = Variant "Option"

contextArrowVariant :: Variant a
contextArrowVariant = Variant "Context Arrow"

supportArrowVariant :: Variant a
supportArrowVariant = Variant "Support Arrow"

gsnDiagramVariant :: Variant a
gsnDiagramVariant = Variant "GSN Diagram"


instantiatedField :: FieldId
instantiatedField = fieldsByName builtInFields ^?! ix "Instantiated" . to fieldId

developedField :: FieldId
developedField = fieldsByName builtInFields ^?! ix "Developed" . to fieldId

cardinalityField :: FieldId
cardinalityField = fieldsByName builtInFields ^?! ix "Cardinality" . to fieldId


-- | Path for GTK icons associated with GSN entities.
gsnIcons :: IO FilePath
gsnIcons = getDataDir
