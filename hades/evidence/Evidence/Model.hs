{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.
-}

{- |
This module defines two entity types which can be included in models. It contains no
separate diagram type: it is for the various other diagrams to determine whether evidence
and traces can be included.

"Evidence" is an entity type for recording a piece of external evidence. This may be a document,
database or something else. The only built-in field is the name. It is for the end user to
define other extension fields for evidence depending on their procedures.

"Trace" is an arrow entity relating some other entity to associated evidence. The head end of
the arrow may connect to "Evidence", but it may also connect to other
-}
module Evidence.Model (
   -- * Models that include evidence
   HasEvidence (..),
   -- * Model entity definition.
   Evidence (Evidence),
   evidenceName,
   evidenceVariant,
   evidenceEntityMenu,
   addEvidenceEntityMenu,
   evidenceIconName,
   evidenceIconPixbuf,
   evidenceActivation,
   -- * HADES Graphical Representations
   drawEvidence,
   -- * Trace Arrows
   Trace (Trace),
   traceName,
   traceVariant,
   traceTailRelation,
   traceHeadRelation,
   traceArrowRelations,
   traceArrowDash,
   traceArrowHead,
   traceIconName,
   traceEntityMenu,
   traceActivation
) where

import Control.Lens hiding ((.=))
import Data.Aeson
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.GdkPixbuf as Gdk
import qualified GI.Gtk as Gtk
import Hades.Abstract
import Hades.GI
import Model.Abstract.DiagramType
import Model.Abstract.PackageTree
import Reactive.Banana.GI.DataIconTheme
import Model.Reflection.NamedRelation (Relation)
import Model.Reflection.References
import Model.Reflection.Reflective
import Model.Reflection.Values
import Reactive.Banana.Common
import Reactive.Banana.Menu
import System.IO.Unsafe


class (EntityClass v) => HasEvidence v where
   _Evidence :: Prism' v Evidence
   _Trace :: Prism' v Trace


-- | The only built-in data for evidence is its name. Its up to the user to add title, author,
-- publication date, status etc as extension data.
--
-- Likewise @Evidence@ has no built in references or arrows; its up to the application to add
-- reference links to entities that can have evidence associated with them.
newtype Evidence = Evidence {_evidenceName :: Name} deriving (Eq, Show)

instance ToJSON Evidence where
   toJSON evidence = object [
         "type" .= ("evidence" :: Text),
         "name" .= (evidence ^. evidenceName)
      ]

instance FromJSON Evidence where
   parseJSON = withObject "Evidence" $ \v -> do
      typ <- v .: "type"
      nm <- v .: "name"
      if typ /= ("evidence" :: Text)
         then fail $ "Not an evidence item: " <> show nm
         else return $ Evidence nm

instance Reflective Evidence where
   reflectiveName _ = evidenceVariant
   reflectiveDefaults = [Evidence $ Name ""]
   reflectiveBuiltIn _ = [nameField]
   reflectiveGet (Evidence (Name nm)) = M.singleton nameField $ ExtText nm
   reflectiveSet (Evidence (Name nm)) = Evidence . Name . T.strip <$> extract nm _ExtText nameField
   reflectiveBuiltInRefs = mempty
   reflectiveArrows = mempty


evidenceName :: Iso' Evidence Name
evidenceName = iso _evidenceName Evidence

evidenceVariant :: Variant a
evidenceVariant = Variant "Evidence"

-- | Context menu for evidence items.
evidenceEntityMenu :: (EntityClass v) => Entity v -> Menu (ModelScript p v Evidence (Maybe Text))
evidenceEntityMenu ent = Menu [[
      menuItem "Properties" $ editEntityProperties $ entityId ent,
      cloneEntityMenuItem $ entityId ent,
      menuItem "Delete" $ deleteEntity $ entityId ent
   ]]


-- | Menu for adding evidence entities.
addEvidenceEntityMenu :: (EntityClass v, HasEvidence v) =>
   ModelId -> Menu (ModelScript p v v (Maybe Text))
addEvidenceEntityMenu parent = Menu [[
      menuItem "Add Evidence" $ promoteScript _Evidence $
         insertEntity (Evidence (Name "Evidence-") ^. re _Evidence) parent >>= \case
            Nothing -> return Nothing
            Just (nm, _) -> return $ Just $ "Insert " <> nm ^. nameText,
      menuItem "Add Trace" $ promoteScript _Trace $
         insertEntity (Trace (Name "Trace-") ^. re _Trace) parent >>= \case
            Nothing -> return Nothing
            Just (nm, _) -> return $ Just $ "Insert " <> nm ^. nameText
   ]]

-- | Icon name for evidence items.
evidenceIconName :: Text
evidenceIconName = "evidence-doc"


-- | Should be @Just@ the evidence icon 16x16. Uses @unsafePerformIO@ to avoid reading the
-- icon file every time.
evidenceIconPixbuf :: Maybe Gdk.Pixbuf
evidenceIconPixbuf = unsafePerformIO $ do
   thm <- getDataIconTheme
   let sz = 16
   Gtk.iconThemeLookupIcon thm evidenceIconName sz [] >>= \case
      Nothing -> return Nothing
      Just info -> Just <$> Gtk.iconInfoLoadIcon info
{-# NOINLINE evidenceIconPixbuf #-}


evidenceActivation :: (EntityClass v) => ActivationScript p v Evidence
evidenceActivation Nothing = return Nothing
evidenceActivation (Just e) = editEntityProperties $ entityId e


-- | Draw the avatar as an evidence box. Evidence items can be included in many different
-- diagrams so there is no distinct @EvidenceBox@ type. It is for each instance of
-- "DiagramTypeClass" to define a diagram element subtype for evidence boxes and call this
-- function when it is drawn.
drawEvidence :: (EntityClass v) =>
   Maybe (Entity v)  -- ^ Entity to represent.
   -> HadesLayout ()  -- ^ Box contents.
   -> Maybe Colour   -- ^ Border colour if not default.
   -> BoundBox
   -> HadesRender ()
drawEvidence mEnt contents border box1 = do
      let box2 = shapeInnerBox $ Rectangle box1
      maybe id withLineColour border $ drawPolygon $ rectangleCorners $ Rectangle box1
      runLayout_ box2 $ do
            layoutTitle evidenceIconPixbuf $ case mEnt of
               Just ent -> ent ^. entityName . nameText
               Nothing -> "Evidence"
            layoutParagraphGap
            contents


-- | A trace represents a link from something asserted in the assurance model to a
-- supporting argument or evidence.
newtype Trace = Trace {_traceName :: Name} deriving (Eq, Show)

instance ToJSON Trace where
   toJSON (Trace nm) = object [
         "type" .= ("trace" :: Text),
         "name" .= nm
      ]

instance FromJSON Trace where
   parseJSON = withObject "Trace" $ \v -> do
      checkType v "trace"
      Trace <$> v .: "name"

instance Reflective Trace where
   reflectiveName _ = traceVariant
   reflectiveDefaults = [Trace $ Name ""]
   reflectiveBuiltIn _ = [nameField]
   reflectiveGet (Trace (Name nm)) = M.singleton nameField $ ExtText nm
   reflectiveSet (Trace (Name nm)) = Trace . Name <$> extract nm _ExtText nameField
   reflectiveBuiltInRefs = traceArrowRelations [] [evidenceVariant]
   reflectiveArrows = M.fromList [
         (traceVariant, (traceTailRelation, traceHeadRelation))
      ]


traceName :: Iso' Trace Name
traceName = iso _traceName Trace


traceVariant :: Variant a
traceVariant = Variant "Trace"


traceTailRelation :: Relation
traceTailRelation = "Trace Tail"

traceHeadRelation :: Relation
traceHeadRelation = "Trace Head"

-- | Construct a RefTypeTable for Trace arrows. Diagrams which include trace arrows should
-- use this to add to the types of entities that can be traced.
traceArrowRelations ::
   [Variant a]  -- ^ The list of things the tail can connect to.
   -> [Variant a]  -- ^ The list of things the head can connect to.
   -> RefTypeTable a
traceArrowRelations tailList headList =
   mkOneToMany traceTailRelation tailList [traceVariant] <>
   mkOneToMany traceHeadRelation headList [traceVariant]


-- | Dash pattern for trace arrows.
traceArrowDash :: [Double]
traceArrowDash = [10, 5]


-- | How to draw the head of a trace arrow.
traceArrowHead :: ArrowHead
traceArrowHead = openArrowHead 60 10


-- | Icon name for trace arrows.
traceIconName :: Text
traceIconName = "trace-arrow"


-- | Context menu for evidence items.
traceEntityMenu :: (EntityClass v) => ModelId -> Menu (ModelScript p v Trace (Maybe Text))
traceEntityMenu modelId = Menu [[
      menuItem "Properties" $ editEntityProperties modelId,
      menuItem "Delete" $ deleteEntity modelId
   ]]


traceActivation :: (EntityClass v) => ActivationScript p v Trace
traceActivation Nothing = return Nothing
traceActivation (Just e) = editEntityProperties $ entityId e
