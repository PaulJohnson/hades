{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE Arrows #-}

{-
Copyright © Paul Johnson 2020. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.
-}


{- |
This module provides a generic template for box-and-line diagrams where each diagram element
represents a corresponding model entity.

A typical client module will define the following:

* Some model entity types which are going to be represented by boxes and lines in a diagram type. I

* A unit type (i.e with one constructor and no data) which is an instance of "DiagramTypeClass".

The relationship between types of entity and types of diagram element may be simple 1:1, in which
case the diagram types can be used for both. Or it may be more complicated. The type-level
link between the two is in "avatarType" which takes a model entity and returns a tag for the
creation of a corresponding diagram element. Various diagram scripts can also create entities; it
is up to the programmer to ensure that diagram elements point at model entities of the appropriate
type.

"avatarType" can also accept entities defined in other modules. The usual approach is to have a
module for entities of type @Foo@ to export a @HasFoo@ typeclass which provides a prism from the
global entity type down to @Foo@

> class HasFoo v where
>    _Foo :: Prism' v Foo

Then any other diagram which needs to display avatars for @Foo@ entities can use @_Foo@ to see if
its got one. There will still need to be a distinct box or arrow type for the corresponding
diagram elements of course.

-}
module App.GenericDiagram (
  -- * The Class of Diagrams
  AvatarCreation (..),
  DiagramTypeClass (..),
  -- * Diagram Display
  AppearanceDef (..),
  compileAppearance,
  ViewContext (GenericContext, diagramId),
  diagramAppearanceDef,
  -- * Boxes and Arrows
  Box (..),
  Arrow (..),
  arrowShape,
  edgeLinks,
  Comment (Comment, commentId),
  commentShape,
  commentText,
  Link (linkId),
  linkTarget,
  linkShape,
  linkText,
  linkDialog,
  Element (..),
  _ElemBox,
  _ElemArrow,
  _ElemComment,
  elementMapAvatars,
  elementDialog,
  newBoxEntityTool,
  newArrowEntityTool,
  newCommentTool,
  newLinkTool
) where

import App.Appearance
import Control.Applicative
import Control.Arrow (returnA)
import Control.Exception (handle, SomeException)
import Control.Lens hiding ((.=))
import Control.Lens.Hades
import Control.Monad
import Control.Monad.State
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Base64.Lazy as LB64
import Data.Char
import Data.Default
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.UUID as U
import qualified Data.Vector as V
import qualified GI.GdkPixbuf as Gdk
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk
import qualified GI.Rsvg as SVG
import Hades.Abstract
import Hades.GI.BasicShapes
import Hades.GI.Saving
import Model.Abstract.DiagramType
import Model.Abstract.PackageTree
import Model.Abstract.Properties
import Model.Lookups.Base
import Model.Matrices.Base
import Model.Query.Base
import Model.Query.Diagram
import Model.Reflection.NamedRelation (Relation)
import Model.Report.Base
import qualified Model.Reflection.NamedRelation as NR
import Reactive.Banana.ArrowDialog
import Reactive.Banana.Common
import Reactive.Banana.GI.ErrorBox
import Reactive.Banana.Menu
import System.FilePath
import System.IO.Unsafe
import qualified Data.Colour.Names as C


-- | Avatar creation action. When an avatar of an existing entity is required, this specifies
-- what is to be created and how. The simple versions cover most cases, with the script option
-- available for anything else.
data AvatarCreation t v =
  NoAvatar  -- ^ The entity cannot be added to this diagram.
  | SimpleBox (BoxType t)  -- ^ A box of this type will be created referring to the entity.
  | SimpleArrow (ArrowType t)  -- ^ An arrow of this type will be created.
  | AvatarScript (Entity v -> Point -> Delta (Element t v) (Maybe (Element t v)))
      -- ^ The script will be executed. It should add the new avatar to the diagram and then
      -- return it, or do nothing and return @Nothing@.


{- | This class acts as a configuration parameter for types of diagrams. For the most part its
instances will not have any values; rather the type functions within it will return types
that have values.

The JSON instances for @Element@ use the @\"type\"@ field to distinguish between variants. It is
therefore important that the "BoxType" and "ArrowType" types have distinct JSON representations.

The relationship between arrows and edges is complicated. Most arrows just link directly to a box
at each end. However some can also link to \"children\" items within a box. These correspond to
entities "exported" by the underlying entity.

So for instance suppose that a @Foo@ entity can export some @Bar@ entities. These @Bar@ entities
appear somewhere else in the model tree and therefore have their own model IDs. In the diagram
this is represented by the avatar box for the @Foo@ having a list of \"children\", each of which
corresponds to one of the exported @Bar@ entities. The arrow "Connector" stores the diagram ID of
the @Foo@ avatar and also the ID of the child it is actually pointing to. This ID is actually
the model ID of the exported @Bar@ entity corresponding to that child on the diagram. Hence the
child regions in a box act as a kind of avatar for the corresponding exported entity.

The \"edge\" entities corresponding to the diagram arrows are related to the target entities
through relations. When an arrow head is connected to a @Foo@ box the following relations are
created:

* "arrowHeadRelation" between the edge and the @Foo@.

* If the arrow is connected to a child of the box, then an "arrowHeadExportRelation" betweeen
the edge and the @Bar@ entity corresponding to the selected child.

The logic for arrow tails is the same.
-}
class (ToJSON (BoxType t), ToJSON (ArrowType t), FromJSON (BoxType t),
      FromJSON (ArrowType t), Eq (BoxType t), Eq (ArrowType t), Eq (ModelSubtype t),
      HasDiagrams HadesRender v) =>
  DiagramTypeClass t v | t -> v where
    -- | Variant tag for different types of box that are avatars of model entities.
    data BoxType t :: *
    -- | Variant tag for different types of arrow that are avatars of model entities.
    data ArrowType t :: *
    -- | The subtype of the model for which this diagram has avatars.
    data ModelSubtype t :: *
    -- | Each variant of box has its own shape based on a BoundBox.
    mkBox :: DiagramId -> ModelId -> BoxType t -> Maybe Peg -> BoundBox -> Box t v
    -- | Each variant of box is drawn in its own way. The Delta monad gives it access to the
    -- model, but this may be called with the null ModelId, in which case it should assume
    -- sensible defaults. The @HadesLayout@ contains the contents of the box and the @Colour@
    -- contains the border colour according to the current diagram style.
    boxTypeDraw ::
      Box t v
      -> HadesLayout ()
      -> Maybe Colour
      -> Delta (Element t v) (BoundBox -> HadesRender ())
    -- | If this box has any application-specific connection points then list
    -- them here. The second component of each pair is the touch predicate for that area.
    boxChildren :: Box t v -> Delta (Element t v) [(DiagramId, Point -> Bool)]
    -- | When connecting a line to a child, where to attach it?
    boxChildConnection :: Box t v -> DiagramId -> Delta (Element t v) (Point -> Point)
    -- | Can this box be pegged to this arrow?
    -- If so, return the name of the relation in the model to represent this.
    boxCanPeg :: Box t v -> Arrow t v -> Delta (Element t v) (Maybe Relation)
    -- | How to represent an entity on the diagram.
    avatarType :: Traversal' v (ModelSubtype t) -> v -> AvatarCreation t v
    -- | Diagram type name, used to distinguish different types of diagram.
    -- The implementation MUST return a constant and MUST NOT inspect its argument.
    diagramTypeName :: t -> Text
    -- | Name of the relation between boxes and arrow tails.
    arrowTailRelation :: ArrowType t -> Relation
    -- | Name of the relation between arrow heads and boxes.
    arrowHeadRelation :: ArrowType t -> Relation
    -- | Name of the relation between arrow tails and items exported by the entity.
    arrowTailExportRelation :: ArrowType t -> Maybe Relation
    -- | Name of the relation between arrow heads and items exported by the entity pointed to.
    arrowHeadExportRelation :: ArrowType t -> Maybe Relation
    -- | How to draw the arrow head.
    arrowHeadDraw :: Maybe (ModelSubtype t) -> ArrowType t -> ArrowHead
    -- | Decorations along the arrow. The arrow name may be drawn at 0.5 so diagrams should
    -- avoid that area.
    arrowDecorations :: Maybe (ModelSubtype t) -> ArrowType t -> [ArrowDecoration]
    arrowDecorations _ _ = []
    -- | The line dash style for the arrow. @[]@ means solid line. A list of positive numbers
    -- provides alternate on and off lengths of stroke.
    arrowLineDashes :: ArrowType t -> [Double]
    -- | Can the arrow connect to the box? The "DiagramId" is the child of the box to which
    -- the connection will be made.
    arrowCanConnect ::
      ConnectionDirection -> Arrow t v -> Box t v -> Maybe DiagramId
          -> Delta (Element t v) Bool


-- | Appearance view settings for diagrams. This is the compiled version used to actually draw
-- the elements.
data AppearanceDef v d = AppearanceDef {
  appDefArrowLabels :: Maybe Bool,
  appDefEntityShowFields :: EntityShowFields v,
  appDefBorderColour :: d -> Maybe Colour}

instance Semigroup (AppearanceDef v d) where
  (AppearanceDef n1 e1 c1) <> (AppearanceDef n2 e2 c2) =
    AppearanceDef (n1 <|> n2) (e1 <> e2) $ \v -> c2 v <|> c1 v

instance Monoid (AppearanceDef v d) where
  mempty = AppearanceDef Nothing mempty $ const Nothing

-- | The cascade of diagram appearance values from this diagram and its parent packages is
-- evaluated.
--
--- Errors and warnings from the queries are ignored.
compileAppearance :: (
    d ~ Element t v,
    AppearanceClass t v,
    Avatar (Paint d) v w d,
    EntityClass v,
    Queryable v) =>
  DiagramAppearance t v   -- ^ The local appearance data.
  -> ViewContext d   -- ^ A base context to modify.
  -> ModelEdit v w (ViewContext d)
compileAppearance appear1 ctx = do
    goToRoot
    rootAppearance <- maybe mempty getDiagramAppearance <$> getMeta
    goToEntity $ diagramId ctx
    cascade <-
      mconcat .
      mapMaybe (preview $ entityContents . _Package . packageMeta . to getDiagramAppearance) <$>
      currentPath
    model <- getModel
    let
      appear2 = appear1 <> cascade <> rootAppearance
      -- funcs :: [d -> Maybe Colour]
      funcs = map (compilePair model) $ appear2 ^. highlights
      result = AppearanceDef {
          appDefArrowLabels = appear2 ^. arrowLabels,
          appDefEntityShowFields = appear2 ^. entityShowFields,
          appDefBorderColour = \e -> foldr ((<|>) . ($ e)) Nothing funcs
        }
    return $ diagramAppearanceDef .~ result $ ctx
  where
    -- compilePair :: (Avatar (Paint d) v w d, EntityClass v, Queryable v) =>
    --    Model v -> DiagramId -> (QueryId, Colour) -> d -> Maybe Colour
    compilePair model (queryId, colour) =
      case modelContents model ^? ix queryId . entityContents . _QueryDiagram . queryDiagram of
        Nothing -> const Nothing  -- Query not found. Ignore it.
        Just qd -> case compileQueryDiagram model qd of
          Left _ -> const Nothing  -- Query has errors. Ignore it.
          Right query ->
            let colourSet = fst $ runQuery query [] model $ S.singleton $ diagramId ctx
                -- Ignore warnings.
            in \e -> do  -- Maybe monad
              ent <- ascendedId e
              guard $ S.member ent colourSet
              return colour



data Box t v = forall s . (ToJSON (BoxType t), FromJSON (BoxType t), BoxShapeClass s) =>
  Box {
    boxId :: DiagramId,    -- ^ The ID of this box on the diagram.
    boxEntity :: ModelId,  -- ^ The ID of the model entity that this box represents.
    boxType :: BoxType t,  -- ^ The variant of the box.
    _boxPeg :: Maybe Peg,  -- ^ If the box is pegged to an arrow line.
    boxShape :: s          -- ^ The position and size of the box.  The name is required
                   -- by Haskell syntax but it can never be used due to leaking
                   -- existential type.
  }

instance (Eq (BoxType t)) => Eq (Box t v) where
  (Box id1 ent1 typ1 peg1 shape1) == (Box id2 ent2 typ2 peg2 shape2)  = and [
      id1 == id2,
      ent1 == ent2,
      typ1 == typ2,
      peg1 == peg2,
      shape1 ^. shapeDefinition == shape2 ^. shapeDefinition
        -- We are comparing reals here, but the use-case is about comparing a new value with
        -- an old to detect edits, so this is OK.
    ]

instance HasId (Box t v) where identifier = boxId

instance (DiagramTypeClass t v) => ToJSON (Box t v) where
  toJSON (Box diagId modelId typ peg shape) = object [
      "boxId" .= diagId,
      "entity" .= modelId,
      "type" .= typ,
      "peg" .= peg,
      "shape" .= shape
    ]

instance (DiagramTypeClass t v) => FromJSON (Box t v) where
  parseJSON = withObject "GSN diagram box" $ \v ->
    mkBox
      <$> v .: "boxId"
      <*> v .: "entity"
      <*> v .: "type"
      <*> v .:? "peg"
      <*> v .: "shape"


boxPeg :: Lens' (Box t v) (Maybe Peg)
boxPeg = lens _boxPeg $ \s p -> s {_boxPeg = p}


-- | Size handles for the box.
genericBoxHandles :: (
    d ~ Element t v,
    AppearanceClass t v,
    DiagramTypeClass t v,
    Editable HadesRender v,
    EntityClass v,
    HasDiagrams HadesRender v,
    HasLookups v,
    Queryable v,
    HasReports v) =>
  Box t v -> Delta d [View d]
genericBoxHandles (Box diagId modelId typ peg s) =
  forM (shapeHandles s) $ \(p, shapeF) ->
    simpleHandle "Resize " mempty squareHandle (p, ElemBox . Box diagId modelId typ peg . shapeF)


-- | View of a box, which is an avatar of model elmenents of type @w@, which is in turn a sub-type
-- of @v@.
boxView :: (
      d ~ Element t v,
      Avatar HadesRender v w d,
      DiagramTypeClass t v,
      EntityClass v,
      HasDiagrams HadesRender v) =>
  Box t v -> Delta d (View d)
boxView boxItem@(Box _ _ _ _ shape) = ascend (ElemBox boxItem) >>= \case
  Nothing -> do   -- Entity must have been deleted, so this avatar is too.
    deleteItem $ identifier boxItem
    return mempty
  Just ent -> do
    scale <- use deltaZoom
    border <- use $ deltaContext . diagramAppearanceDef . to appDefBorderColour
    contentDef <- use $ deltaContext . diagramAppearanceDef . to appDefEntityShowFields
    contents <- liftBase $ lift $ do
      goToEntity $ entityId ent
      diagramLayout contentDef
    drawFunc <- boxTypeDraw boxItem contents $ border $ ElemBox boxItem
    return View {
        viewDraw = drawFunc $ shape ^. shapeDefinition,
        viewTouched = shapeTouched shape scale,
        viewBox = shapeBounds shape,
        viewMenu = avatarShapeMenu (identifier boxItem),
        viewAction = handleDelegatedAction
          (ElemBox boxItem)
          (avatarProperties $ ElemBox boxItem)
          (const yieldViews)
      }


-- | Avatars that are represented as arrows on a diagram. The avatar implements connection
-- semantics in the underlying model when an arrow is connected to a box.
--
-- Currently there is only one type of line shape, so the type hackery used for "Box" is not
-- necessary. If different types of lines are ever introduced then this will need to be revisited.
data Arrow t v = Arrow {
    arrowId :: DiagramId,
    arrowEntity :: ModelId,
    arrowType :: ArrowType t,
    _arrowShape :: LineShape
  }

instance (Eq (ArrowType t)) => Eq (Arrow t v) where
  (Arrow id1 ent1 typ1 shape1) == (Arrow id2 ent2 typ2 shape2)  = and [
      id1 == id2,
      ent1 == ent2,
      typ1 == typ2,
      shape1 == shape2
    ]

instance HasId (Arrow t v) where identifier = arrowId

instance (DiagramTypeClass t v) => ToJSON (Arrow t v) where
  toJSON a = object [
      "arrowId" .= arrowId a,
      "entity" .= arrowEntity a,
      "type" .= arrowType a,
      "shape" .= (a ^. arrowShape)
    ]

instance (DiagramTypeClass t v) => FromJSON (Arrow t v) where
  parseJSON = withObject "GSN diagram arrow" $ \v ->
    Arrow
      <$> v .: "arrowId"
      <*> v .: "entity"
      <*> v .: "type"
      <*> v .: "shape"


arrowShape :: Lens' (Arrow t v) LineShape
arrowShape = lens _arrowShape $ \v s -> v {_arrowShape = s}


genericArrowHandles :: (
      AppearanceClass t v,
      DiagramTypeClass t v,
      d ~ Element t v,
      Editable HadesRender v,
      EntityClass v,
      Eq d,
      HasDiagrams HadesRender v,
      HasLookups v,
      Queryable v,
      HasReports v) =>
  Arrow t v -> Delta d [View d]
genericArrowHandles arrow =
    lineHandles (arrow ^. arrowShape) squareHandle promoteShape updateAssociations
  where
    promoteShape s = ElemArrow $ arrowShape .~ s $ arrow
    updateAssociations (ElemArrow newArrow) =
      updateArrowAssociations (Just arrow) newArrow
    updateAssociations _ = cannotHappen
        "genericArrowHandles: updating something that is not an arrow."
        $ return ()


-- Update the model Edge for which this arrow is an avatar with the new endpoints.
updateArrowAssociations :: (
      AppearanceClass t v,
      DiagramTypeClass t v,
      d ~ Element t v,
      Editable HadesRender v,
      EntityClass v,
      Eq d,
      HasDiagrams HadesRender v,
      HasLookups v,
      Queryable v,
      HasReports v) =>
  Maybe (Arrow t v)  -- ^ Previous state of this arrow. @Nothing@ for a newly created arrow.
  -> Arrow t v   -- ^ The arrow that has just been modified.
  -> Delta d ()
updateArrowAssociations oldArrow newArrow = do
    let
      edge = arrowEntity newArrow
      oldEnds = oldArrow ^? _Just . arrowShape . lineEnds
      (newFrom, newTo) = newArrow ^. arrowShape . lineEnds
    (newEdgeFrom, newEdgeFromExport) <- ascendedTarget newFrom
    (newEdgeTo, newEdgeToExport) <- ascendedTarget newTo
    liftBase $ lift $ do
      when (endMoved (fst <$> oldEnds) newFrom) $ do
        updateManyToOne (arrowTailRelation $ arrowType newArrow) edge newEdgeFrom
        case arrowTailExportRelation $ arrowType newArrow of
          Just er -> updateManyToOne er edge newEdgeFromExport
          Nothing -> return ()
      when (endMoved (snd <$> oldEnds) newTo) $ do
        updateManyToOne (arrowHeadRelation $ arrowType newArrow) edge newEdgeTo
        case arrowHeadExportRelation $ arrowType newArrow of
          Just er -> updateManyToOne er edge newEdgeToExport
          Nothing -> return ()
  where
    endMoved Nothing _ = True
    endMoved (Just oldEnd) newEnd = oldEnd /= newEnd
    ascendedTarget Unconnected {} = return (U.nil, U.nil)
    ascendedTarget (Connected _ targetId child) =
      withItem targetId (return . fromMaybe U.nil . ascendedId) >>= \case
        Nothing -> return (U.nil, U.nil)
        Just t -> case child of
          Just c -> return (c, t)
          Nothing -> return (t, U.nil)


-- | The entities linked by an edge. Returns two pairs for the tail and head respectively. Each
-- pair contains the entity the arrow should point to on the diagram and optionally a child
-- entity within the first. If the arrow is not linked at one end then it returns @Nothing@
-- for that end.
edgeLinks :: (DiagramTypeClass t v) =>
  ArrowType t   -- ^ The type of the arrow.
  -> ModelId    -- ^ The model ID of the edge entity linking two other entities.
  -> ModelEdit v (ModelSubtype t) (
      Maybe (ModelId, Maybe ModelId),
      Maybe (ModelId, Maybe ModelId))
edgeLinks typ modelId = do
    fromEnd <- getEnd (arrowTailRelation typ) (arrowTailExportRelation typ)
    toEnd <- getEnd (arrowHeadRelation typ) (arrowHeadExportRelation typ)
    return (fromEnd, toEnd)
  where
    getEnd modelRel diagramRel =
      queryRelations (NR.relation modelId modelRel) >>= \case
        (listToMaybe . S.toList -> Just target1) ->
          case diagramRel of
            Nothing -> return $ Just (target1, Nothing)
            Just rel ->
              queryRelations (NR.relation modelId rel) >>= \case
                (listToMaybe . S.toList -> Just target2) ->
                  return $ Just (target2, Just target1)
                _ ->
                  return $ Just (target1, Nothing)
        _ -> return Nothing


arrowView :: (
      AppearanceClass t v,
      DiagramTypeClass t v,
      d ~ Element t v,
      Editable HadesRender v,
      EntityClass v,
      Eq d,
      HasDiagrams HadesRender v,
      HasLookups v,
      Queryable v,
      HasReports v) =>
  Arrow t v -> Delta d (View d)
arrowView arrow = do
    scale <- use deltaZoom
    appearance <- use $ deltaContext . diagramAppearanceDef
    let border = appDefBorderColour appearance
    prsm  <- liftBase $ lift $ usingTraversal return
    if U.null $ arrowEntity arrow
      then return $ theView scale prsm Nothing (const Nothing) Nothing
          -- Null reference used for transient arrows.
      else ascend (ElemArrow arrow) >>= \case
          Nothing -> do   -- Entity has been deleted, so its avatar should be as well.
            deleteItem $ identifier arrow
            return mempty
          Just e ->
            return $ theView scale prsm (Just $ e ^. entityContents) border $
              case appDefArrowLabels appearance of
                Just True -> Just $ e ^. entityName . nameText
                _ -> Nothing
  where
    theView scale prsm ent border label =
      let
        shape = arrow ^. arrowShape
        (start, end) = shape ^. lineEnds
        highlight = maybe id withLineColour $ border $ ElemArrow arrow
      in View {
        viewDraw = highlight $ do
          drawPolyLine
            (arrowHeadDraw (ent ^? _Just . prsm) $ arrowType arrow)
            (arrowLineDashes $ arrowType arrow)
            (connectorPoint start)
            (V.toList $ shape ^. lineMiddle)
            (connectorPoint end)
            label
          mapM_ (drawArrowDecoration shape) $
            arrowDecorations (ent ^? _Just . prsm) $ arrowType arrow,
        viewTouched = shapeTouched shape scale,
        viewBox = growBox 12.0 $ shapeBounds shape,  -- Allow for arrow head.
        viewMenu = if isNothing ent
          then return mempty   -- No menu while still under construction.
          else lineMenu (\s -> ElemArrow $ arrowShape .~ s $ arrow) shape
              <> avatarShapeMenu (identifier arrow),
        viewAction = handleDelegatedAction
            (ElemArrow arrow)
            (avatarProperties $ ElemArrow arrow)
            (const yieldViews)
      }


-- | The type of a graphic displayed in a diagram. The string holds the data as stored
-- in a file, while the other part holds a parsed/decompressed version ready for rendering.
--
-- Equality is checked on the first part of the variants only.
data Graphic =
  VectorGraphic Text SVG.Handle
      -- ^ SVG files contain only unicode, so the JSON format is the escaped text.
  | PixelGraphic ByteString Gdk.Pixbuf
      -- ^ Pixel formats contain arbitrary binary. The JSON is encoded with Base64.
  | ErrorGraphic Object Text
      -- ^ If the graphic cannot be decoded then the raw JSON is preserved
      -- for saving. The text is the error message reported by the decoder.

instance Eq Graphic where
  VectorGraphic txt1 _ == VectorGraphic txt2 _  =  txt1 == txt2
  PixelGraphic b1 _ == PixelGraphic b2 _        =  b1 == b2
  ErrorGraphic obj1 _ == ErrorGraphic obj2 _    =  obj1 == obj2
  _ == _   = False

instance ToJSON Graphic where
  toJSON (VectorGraphic txt _) =
    object ["svg" .= txt]
  toJSON (PixelGraphic dat _) =
    object ["image" .= (LT.decodeLatin1 . LB64.encode . LB.fromStrict) dat]
  toJSON (ErrorGraphic v _) = Object v

instance FromJSON Graphic where
  parseJSON = withObject "graphic" $ \v -> parseVector v <|> parsePixel v
    where
      parseVector v = do
        txt <- v .: "svg"
        return $ unsafePerformIO $ handle (badGraphic v) $ do
          stream <- Gio.memoryInputStreamNewFromData (T.encodeUtf8 txt) Nothing
          mPic <- SVG.handleNewFromStreamSync
              stream
              (Nothing :: Maybe Gio.File)  -- Pic is internal, so no base file.
              [SVG.HandleFlagsFlagKeepImageData]
              (Nothing :: Maybe Gio.Cancellable)   -- Decoding is non-cancellable.
          case mPic of
            Just pic -> return $ VectorGraphic txt pic
            Nothing -> return $ ErrorGraphic v "Cannot decode SVG file."
      parsePixel v = do
          dat <- LB.toStrict . LB64.decodeLenient . LT.encodeUtf8 <$> v .: "image"
          return $ unsafePerformIO $ handle (badGraphic v) $ do
              stream <- Gio.memoryInputStreamNewFromData dat Nothing
              Gdk.pixbufNewFromStream stream (Nothing :: Maybe Gio.Cancellable) >>= \case
                Just pix -> return $ PixelGraphic dat pix
                Nothing -> return $ ErrorGraphic v "Cannot decode image file."
      -- Catching all exceptions here because anything except a user interupt
      -- indicates an unreadable file.
      badGraphic :: Object -> SomeException -> IO Graphic
      badGraphic v _ = return $ ErrorGraphic v "Error during image decoding."

      -- unsafePerformIO is safe here because the contents will not change over the life of
      -- the Graphic, so it doesn't matter if conversion is done now or some future time.


-- | Put a graphic in a text layout.
layoutGraphic :: Graphic -> HadesLayout ()
layoutGraphic (VectorGraphic _ svg) = layoutSvg (Just $ Colour C.white) svg
layoutGraphic (PixelGraphic _ pix) = layoutPixbuf True (Just $ Colour C.white) pix
layoutGraphic (ErrorGraphic _ msg) = do
  layoutHLine
  layoutParagraphs [] msg
  layoutHLine


data Comment = Comment {
    commentId :: DiagramId,
    _commentShape :: CommentBox,
    _commentGraphic :: Maybe Graphic,
    _commentText :: Text
  } deriving Eq

instance HasId Comment where identifier = commentId

instance Default Comment where
  def = Comment U.nil (CommentBox NoBox) Nothing ""

instance ToJSON Comment where
  toJSON c = object [
      "commentId" .= commentId c,
      "shape" .= (c ^. commentShape),
      "graphic" .= (c ^. commentGraphic),
      "text" .= (c ^. commentText)
    ]

instance FromJSON Comment where
  parseJSON = withObject "GSN diagram comment" $ \v ->
    Comment
      <$> v .: "commentId"
      <*> v .: "shape"
      <*> v .:? "graphic"
      <*> v .: "text"


commentShape :: Lens' Comment CommentBox
commentShape = lens _commentShape $ \c s -> c{_commentShape = s}

commentText :: Lens' Comment Text
commentText = lens _commentText $ \c t -> c{_commentText = t}

commentGraphic :: Lens' Comment (Maybe Graphic)
commentGraphic = lens _commentGraphic $ \c g -> c{_commentGraphic = g}


commentHandles :: (
    AppearanceClass t v,
    DiagramTypeClass t v,
    Editable HadesRender v,
    EntityClass v,
    w ~ ModelSubtype t,
    HasDiagrams HadesRender v,
    HasLookups v,
    Queryable v,
    HasReports v) =>
  Comment -> Delta (Element t v) [View (Element t v)]
commentHandles c =
  forM handles $ \(p, shapeF) ->
    simpleHandle "Resize " mempty squareHandle (p, ElemComment . shapeF)
  where
    handles = boxHandles (commentShape . commentBoxShape) c


commentView :: (
      AppearanceClass t v,
      DiagramTypeClass t v,
      Editable HadesRender v,
      EntityClass v,
      Eq (Element t v),
      HasDiagrams HadesRender v,
      HasLookups v,
      Queryable v,
      HasReports v) =>
  Comment -> Delta (Element t v) (View (Element t v))
commentView c = do
  let
    uuid = identifier c
    shape = c ^. commentShape
  return View {
      viewDraw = do
        drawCommentBox shape
        runLayout_ (shapeInnerBox shape) $ do
          mapM_ layoutGraphic $ c ^. commentGraphic
          layoutParagraphs [] $ c ^. commentText,
      viewTouched = insidePolygon $ commentBoxCorners shape,
      viewBox = shapeBounds shape,
      viewMenu = \p -> Menu [[
            MenuItem "Properties" $ Right $ mkScriptAction $
                use (deltaDiagram . diagramContents . at uuid) >>= \case
                  Nothing -> yieldViews
                  Just item -> avatarProperties item
          ]] <> basicShapeMenu uuid p,
      viewAction = handleDelegatedAction
        (ElemComment c)
        (yieldProperties $ ElemComment c)
        (const yieldViews)
    }


-- | Gadget for editing comments.
commentGadget :: Gadget' e w Comment
commentGadget = accum $ focusing l commentGadget1
  where
    l :: Lens' Comment (Maybe Graphic, Text)
    l = lens
      (\c -> (c ^. commentGraphic, c ^. commentText))
      (\c (g, t) -> (commentGraphic .~ g) . (commentText .~ t) $ c)


-- | Gadget for editing comment data that only looks at the editable fields.
commentGadget1 :: Gadget' e w (Maybe Graphic, Text)
commentGadget1 = proc (oldImage, oldText) -> do
    newImage <- simpleFrame "Image" (proc oldImage -> do
        newImage <- buttonIO "Import Image" (const $ const importImageFile) -< ()
        let img = join newImage <|> oldImage
        _ <- imageDisplay MemoLarge -< case img of
              Just (VectorGraphic txt _) -> Just $ T.encodeUtf8 txt
              Just (PixelGraphic bs _) -> Just bs
              _ -> Nothing
        returnA -< img
      ) -< oldImage
    newText <- memoBox MemoMedium True -< oldText
    returnA -< (newImage, newText)
  where
    importImageFile = do
      imageFilter <- Gtk.fileFilterNew
      Gtk.fileFilterSetName imageFilter $ Just "Image files"
      Gtk.fileFilterAddPixbufFormats imageFilter
      openUserFile noWidget [imageFilter] Nothing >>= \case
        Nothing -> return Nothing
        Just (fname, bytes) -> do
          let bytes2 = LB.toStrict bytes
          if map toLower (takeExtension fname) == ".svg"
            then  -- Vector SVG file.
              handle badGraphic $ tryDecodeXml bytes2 >>= \case
                Left msg -> do
                  errorBox noWidget $
                    "SVG file contents could not be decoded.\n\nTechnical details: "
                    <> msg
                  return Nothing
                Right txt -> do
                  stream <- Gio.memoryInputStreamNewFromData bytes2 Nothing
                  svg <- SVG.handleNewFromStreamSync
                      stream
                      (Nothing :: Maybe Gio.File)  -- Pic is internal, so no base file.
                      [SVG.HandleFlagsFlagKeepImageData]
                      (Nothing :: Maybe Gio.Cancellable)   -- Decoding is non-cancellable.
                  return $ VectorGraphic txt <$> svg
            else  -- Pixmap binary file.
              handle badGraphic $ do
                  stream <- Gio.memoryInputStreamNewFromData bytes2 Nothing
                  pix <- Gdk.pixbufNewFromStream stream (Nothing :: Maybe Gio.Cancellable)
                  return $ PixelGraphic bytes2 <$> pix
    badGraphic :: SomeException -> IO (Maybe Graphic)
    badGraphic e = do
      errorBox noWidget $
        "Image file could not be decoded.\n\nTechnical details: " <> T.pack (show e)
      return Nothing
    noWidget :: Maybe Gtk.Widget
    noWidget = Nothing


-- | A link from this diagram to another. The properties of a link will show the details below.
-- Activating the link will activate the target entity.
data Link = Link {
    linkId :: DiagramId,
    _linkTarget :: Maybe ModelId,
    _linkShape :: CommentBox,
    _linkText :: Text
  } deriving Eq

instance HasId Link where identifier = linkId

instance Default Link where def = Link U.nil Nothing (CommentBox NoBox) ""

instance Show Link where
  show (Link lId target _ txt) =
    "Link linkId=" <> show lId <> " _linkTarget=" <> show target <> " _linkText=" <> show txt

instance ToJSON Link where
  toJSON link = object [
      "linkId" .= linkId link,
      "linkTarget" .= (link ^. linkTarget),
      "linkShape" .= (link ^. linkShape),
      "linkText" .= (link ^. linkText)
    ]

instance FromJSON Link where
  parseJSON = withObject "Diagram link" $ \v ->
    Link <$> v .: "linkId" <*> v .: "linkTarget" <*> v .: "linkShape" <*> v .: "linkText"

linkTarget :: Lens' Link (Maybe ModelId)
linkTarget = lens _linkTarget $ \l i -> l{_linkTarget = i}

linkShape :: Lens' Link CommentBox
linkShape = lens _linkShape $ \l s -> l{_linkShape = s}

linkText :: Lens' Link Text
linkText = lens _linkText $ \l t -> l {_linkText = t}


-- | Can this entity have a link made to it?
isLinkable :: (HasDiagrams p v, HasLookups v, HasReports v) => Entity v -> Bool
isLinkable ent =
  let c = ent ^. entityContents
  in isDiagram c || isJust (c ^? _Matrix) || isJust (c ^? _LookupTable) || isJust (c ^? _Report)


linkHandles :: (
    AppearanceClass t v,
    DiagramTypeClass t v,
    Editable HadesRender v,
    EntityClass v,
    w ~ ModelSubtype t,
    HasDiagrams HadesRender v,
    HasLookups v,
    Queryable v,
    HasReports v) =>
  Link -> Delta (Element t v) [View (Element t v)]
linkHandles c =
  forM handles $ \(p, shapeF) ->
    simpleHandle "Resize " mempty squareHandle (p, ElemLink . shapeF)
  where
    handles = boxHandles (linkShape . commentBoxShape) c


linkView :: (
      AppearanceClass t v,
      DiagramTypeClass t v,
      Editable HadesRender v,
      EntityClass v,
      Eq (Element t v),
      HasDiagrams HadesRender v,
      HasLookups v,
      Queryable v,
      HasReports v) =>
  Link -> Delta (Element t v) (View (Element t v))
linkView link = do
  let
    uuid = identifier link
    shape = link ^. linkShape
  targetName <- case link ^. linkTarget of
    Just tgt -> liftBase $ lift $ try $ goToEntity tgt >> currentName
    Nothing -> return $ Right ""
  let
    targetText = case targetName of
      Left _ -> "✘ item deleted"  -- Heavy ballot X, U+2718
      Right txt -> "➔ " <> txt -- Heavy wide-headed rightwards arrow, U+2794
  return View {
      viewDraw = do
        drawCommentBox shape
        textBlock targetText (T.splitOn "\n\n" $ link ^. linkText) $ shapeInnerBox shape,
      viewTouched = insidePolygon $ commentBoxCorners shape,
      viewBox = shapeBounds shape,
      viewMenu = \p -> Menu [[
            menuItem "Properties" $ mkScriptAction $
                use (deltaDiagram . diagramContents . at uuid) >>= \case
                  Nothing -> yieldViews
                  Just item -> avatarProperties item
          ]] <> basicShapeMenu uuid p,
      viewAction = \act -> case actionCommand act of
        Activate -> do
          case link ^. linkTarget of
            Just modelId -> do
              ent <- liftBase $ lift $ withBaseType $ goToEntity modelId >> current
              liftBase (withBaseScript $ editableActivation ent) >>= mapM_ tellCheckpoint
            Nothing -> liftBase $ lift $ throwUser "Link has no target set."
          yieldViews
        _ -> handleDelegatedAction
            (ElemLink link)
            (yieldProperties $ ElemLink link)
            (const yieldViews)
            act
    }


linkDialog :: (EntityClass v, HasDiagrams HadesRender v, HasLookups v, HasReports v) =>
  Dialog' (Model v) () Link
linkDialog = Dialog "Diagram Link" OkApplyButton $
    accum $
    box Vertical [[
        form Vertical [
            ("Link Target:", focusing (linkTarget . from setMaybeIso) $
              validate validLink $ modelItemSpecifier (const isLinkable) (const Nothing))
          ],
        simpleFrame "Text" $ focusing linkText $ memoBox MemoMedium True
      ]]
  where
    validLink s = S.size s == 1


-- | Diagram element type.
data Element t v =
  ElemBox (Box t v)
  | ElemArrow (Arrow t v)
  | ElemComment Comment
  | ElemLink Link

instance (Eq (BoxType t), Eq (ArrowType t)) => Eq (Element t v) where
  ElemBox b1 == ElemBox b2          =  b1 == b2
  ElemArrow a1 == ElemArrow a2      =  a1 == a2
  ElemComment c1 == ElemComment c2  =  c1 == c2
  ElemLink l1 == ElemLink l2        =  l1 == l2
  _ == _                            =  False

instance HasId (Element t v) where
  identifier (ElemBox x) = identifier x
  identifier (ElemArrow x) = identifier x
  identifier (ElemComment x) = identifier x
  identifier (ElemLink x) = identifier x

instance (DiagramTypeClass t v) =>
  ToJSON (Element t v) where
    toJSON (ElemBox x) = toJSON x
    toJSON (ElemArrow x) = toJSON x
    toJSON (ElemComment x) = toJSON x
    toJSON (ElemLink x) = toJSON x

instance (DiagramTypeClass t v) =>
  FromJSON (Element t v) where
    parseJSON v =
      (ElemBox <$> parseJSON v) <|>
      (ElemArrow <$> parseJSON v) <|>
      (ElemComment <$> parseJSON v) <|>
      (ElemLink <$> parseJSON v)

instance (
    AppearanceClass t v,
    DiagramTypeClass t v,
    Editable HadesRender v,
    EntityClass v,
    Eq (Element t v),
    HasDiagrams HadesRender v,
    HasLookups v,
    Queryable v,
    HasReports v) =>
  Avatar HadesRender v (ModelSubtype t) (Element t v) where
    type DiagramMetadata (Element t v) = DiagramAppearance t v
    getDiagramModelId = diagramId <$> use deltaContext
    updateViewContext = compileAppearance
    entityAvatar = genericEntityAvatar
    ascendedId (ElemBox b) = Just $ boxEntity b
    ascendedId (ElemArrow line) = Just $ arrowEntity line
    ascendedId _ = Nothing
    avatarChildren (ElemBox b) = liftBase $ lift $ do
      goToEntity $ boxEntity b
      M.elems <$> currentChildren
    avatarChildren (ElemArrow a) = liftBase $ lift $ do
      goToEntity $ arrowEntity a
      M.elems <$> currentChildren
    avatarChildren _ = return []
    updateAvatar ElemComment {} = return False
    updateAvatar link@ElemLink {} = tellItem link >> return False  -- Just update on screen.
    updateAvatar (ElemBox b) = updateBox b
    updateAvatar (ElemArrow arrow) = updateArrow arrow

instance (
      AppearanceClass t v,
      DiagramTypeClass t v,
      Editable HadesRender v,
      EntityClass v,
      Eq (Element t v),
      HasDiagrams HadesRender v,
      HasLookups v,
      Queryable v,
      HasReports v) =>
  Viewable (Element t v) where
    type Base (Element t v) = ModelScript HadesRender v (ModelSubtype t)
    type Paint (Element t v) = HadesRender
    data ViewContext (Element t v) = GenericContext {
        diagramId :: DiagramId,
        _diagramAppearanceDef :: AppearanceDef v (Element t v)
          -- ^ The cumulative appearance from all the "DiagramAppearance" entries for this
          -- diagram type starting with the one in this diagram and working up through the
          -- package hierarchy.
      }
    itemHandles (ElemBox x) = genericBoxHandles x
    itemHandles (ElemArrow x) = genericArrowHandles x
    itemHandles (ElemComment x) = commentHandles x
    itemHandles (ElemLink x) = linkHandles x
    itemTouched (ElemBox (Box _ _ _ _ shape)) = shapeTouched shape
    itemTouched (ElemArrow a) = shapeTouched $ a ^. arrowShape
    itemTouched (ElemComment c) = shapeTouched $ c ^. commentShape
    itemTouched (ElemLink x) = shapeTouched $ x ^. linkShape
    itemBounds (ElemBox (Box _ _ _ _ shape)) = shapeBounds shape
    itemBounds (ElemArrow a) = shapeBounds $ a ^. arrowShape
    itemBounds (ElemComment c) = shapeBounds $ c ^. commentShape
    itemBounds (ElemLink x) = shapeBounds $ x ^. linkShape
    itemResize d (ElemBox (Box bId bEnt bTyp bPeg bShape)) = do
      -- Full pattern match due to escaped type variables in lens usage.
      let
        newShape = shapeResize d bShape
        newItem = ElemBox $ Box bId bEnt bTyp bPeg newShape
      updateItem newItem
      return newItem
    itemResize d (ElemComment c) = do
      let newItem = ElemComment $ commentShape %~ shapeResize d $ c
      updateItem newItem
      return newItem
    itemResize d (ElemLink c) = do
      let newItem = ElemLink $ linkShape %~ shapeResize d $ c
      updateItem newItem
      return newItem
    itemResize _ v = return v  -- Resize diagram arrow is a no-op.
    itemInBox (ElemBox (Box _ _ _ _ shape)) = (`containsBox` shapeBounds shape)
    itemInBox (ElemArrow a) = shapeInBox $ a ^. arrowShape
    itemInBox (ElemComment c) = shapeInBox $ c ^. commentShape
    itemInBox (ElemLink x) = shapeInBox $ x ^. linkShape
    itemMove d (ElemBox b) = boxMove d b
    itemMove d i = itemDrag d i
    itemDrag d (ElemBox b) = boxDrag d b
    itemDrag d (ElemArrow a) = do
      let newItem = ElemArrow $ arrowShape %~ shapeDrag d $ a
      updateItem newItem
      return newItem
    itemDrag d (ElemComment c) = do
      let newItem = ElemComment $ commentShape %~ shapeDrag d $ c
      updateItem newItem
      return newItem
    itemDrag d (ElemLink x) = do
      let newItem = ElemLink $ linkShape %~ shapeDrag d $ x
      updateItem newItem
      return newItem
    itemView (ElemBox b) = boxView b
    itemView (ElemArrow a) = arrowView a
    itemView (ElemComment c) = commentView c
    itemView (ElemLink x) = linkView x
    itemChildren (ElemBox b) = boxChildren b
    itemChildren _ = return []
    itemDescription ElemComment {} = return "comment"
    itemDescription ElemLink {} = return "link"
    itemDescription item = ascend item >>= \case
          Nothing -> liftBase $ lift $ throwInternal "Diagram element is avatar of nothing."
          Just {} -> liftBase $ lift currentName
    itemType = const $ diagramTypeName (undefined :: t)

instance (AppearanceClass t v,
      DiagramTypeClass t v,
      Editable HadesRender v,
      EntityClass v,
      HasDiagrams HadesRender v,
      HasLookups v,
      Queryable v,
      HasReports v) =>
  Connectable (Element t v) where
    itemCanConnect d (ElemArrow arrow) (ElemBox b) child = arrowCanConnect d arrow b child
    itemCanConnect _ _ _ _ = return False
    itemConnectors = _ElemArrow . arrowShape . lineEnds . both
    itemConnectionPoint (ElemBox (Box _ _ _ _ shape)) Nothing =
      return $ shapeConnectionPoint shape
    itemConnectionPoint (ElemBox b) (Just child) = boxChildConnection b child
    itemConnectionPoint (ElemArrow a) _ = return $ shapeConnectionPoint $ a ^. arrowShape
    itemConnectionPoint (ElemComment c) _ = return $ shapeConnectionPoint $ c ^. commentShape
    itemConnectionPoint (ElemLink x) _ = return $ shapeConnectionPoint $ x ^. linkShape
    itemAdjust (ElemArrow arrow) = do
      newShape <- lineUpdate $ arrow ^. arrowShape
      updateItem $ ElemArrow $ arrowShape .~ newShape $ arrow
    itemAdjust (ElemBox b) = boxAdjust b
    itemAdjust _ = return ()
    itemPegAt (ElemBox boxItem) arrId f = do
      contents <- use $ deltaDiagram . diagramContents
      case contents ^? ix arrId . _ElemArrow of
        Nothing ->
          return Nothing
        Just arrow ->
          boxCanPeg boxItem arrow >>= \case
            Nothing ->
              return Nothing
            Just relName -> do
              liftBase $ lift $ do
                modifyRelations $ NR.insert (boxEntity boxItem) (arrowEntity arrow) relName
                recordModified $ boxEntity boxItem   -- Update stamp for both sides of peg.
                recordModified $ arrowEntity arrow
              return $ Just $ ElemBox $ boxPeg ?~ (arrId, f) $ boxItem
    itemPegAt _ _ _ = return Nothing
    itemUnpeg (ElemBox boxItem) = case boxItem ^. boxPeg of
      Nothing -> return $ ElemBox boxItem  -- No-op because box is already unpegged.
      Just (arrId, _) -> do
        contents <- use $ deltaDiagram . diagramContents
        case contents ^? ix arrId . _ElemArrow of
          Nothing -> return () -- Box was pegged to something that no longer exists.
          Just arrow -> boxCanPeg boxItem arrow >>= \case
            Nothing -> cannotHappen "Box was pegged to something illegal." $ return ()
            Just relName -> liftBase $ lift $ do
              modifyRelations $ NR.delete (boxEntity boxItem) (arrowEntity arrow) relName
              recordModified $ boxEntity boxItem  -- Update stamp for both sides of the peg.
              recordModified $ arrowEntity arrow
        return $ ElemBox $ boxPeg .~ Nothing $ boxItem
    itemUnpeg v = return v
    itemPeg = _ElemBox . boxPeg . _Just
    itemPegPoint (arrId, f) = do
      contents <- use $ deltaDiagram . diagramContents
      case contents ^? ix arrId . _ElemArrow . arrowShape of
        Nothing -> return Nothing
        Just shape -> return $ Just $ lineFractionPoint f shape

-- | The cumulative appearance from all the "DiagramAppearance" entries for this
-- diagram type starting with the one in this diagram and working up through the
-- package hierarchy.
diagramAppearanceDef :: Lens' (ViewContext (Element t v)) (AppearanceDef v (Element t v))
diagramAppearanceDef = lens _diagramAppearanceDef $ \s a -> s{_diagramAppearanceDef = a}

_ElemBox :: Prism' (Element t v) (Box t v)
_ElemBox = prism ElemBox $ \case {ElemBox b -> Right b; v -> Left v}

_ElemArrow :: Prism' (Element t v) (Arrow t v)
_ElemArrow = prism ElemArrow $ \case {ElemArrow arrow -> Right arrow; v -> Left v}

_ElemComment :: Prism' (Element t v) Comment
_ElemComment = prism ElemComment $ \case {ElemComment c -> Right c; v -> Left v}

_ElemLink :: Prism' (Element t v) Link
_ElemLink = prism ElemLink $ \case {ElemLink x -> Right x; v -> Left v}


-- | If the element refers to any model IDs then replace them according to the supplied function.
elementMapAvatars :: (ModelId -> Maybe ModelId) -> Element t v -> Element t v
elementMapAvatars f x@(ElemBox b) = case f $ boxEntity b of
  Nothing -> x
  Just newId -> ElemBox $ b {boxEntity = newId}
elementMapAvatars f (ElemArrow a) =
    ElemArrow $
    arrowShape . lineEnds . both %~ fixTargetChild $
    a {arrowEntity = fromMaybe (arrowEntity a) $ f $ arrowEntity a}
  where
    fixTargetChild (Connected pt t (Just tc)) = Connected pt t $ Just $ fromMaybe tc $ f tc
    fixTargetChild u = u
elementMapAvatars _ x@ElemComment {} = x
elementMapAvatars f x@(ElemLink l) = case l ^. linkTarget >>= f of
  Nothing -> x
  Just newId -> ElemLink $ linkTarget . _Just .~ newId $ l


boxMove :: (
    AppearanceClass t v,
    DiagramTypeClass t v,
    Editable HadesRender v,
    HasLookups v,
    Queryable v,
    HasReports v) =>
  (Double, Double) -> Box t v -> Delta (Element t v) (Element t v)
boxMove d (Box dId mId typ peg shape) = do
  let newBox = ElemBox $ Box dId mId typ peg $ shapeDrag d shape
  updateItem newBox
  return newBox


boxDrag :: (
    AppearanceClass t v,
    DiagramTypeClass t v,
    Editable HadesRender v,
    HasLookups v,
    Queryable v,
    HasReports v) =>
  (Double, Double) -> Box t v -> Delta (Element t v) (Element t v)
boxDrag d boxItem@(Box dId mId typ oldPeg shape) =
    case shape ^. shapeDefinition of
      NoBox -> return $ ElemBox boxItem -- Shouldn't happen.
      _ -> do
        z <- use deltaZoom
        contents <- use $ deltaDiagram . diagramContents . to M.toList
        candidates <- filterM ((isJust <$>) . boxCanPeg boxItem . snd) $
          mapMaybe (\(uuid, v) -> (uuid,) <$> (v ^? _ElemArrow)) contents
        let
          newShape = shapeDrag d shape
          newBox = ElemBox $ Box dId mId typ oldPeg newShape
          centre = newShape ^. shapeDefinition . boxCentre
          check (uuid, arrow) =
            let s = arrow ^. arrowShape
            in (uuid,) <$> lineSnap (screenRange/z) s centre
          snaps :: [(DiagramId, (Double, Double, Point))]
          snaps = mapMaybe check candidates
        newBox1 <- case snaps of
          [] -> if isJust oldPeg
                then itemUnpeg newBox
                else return newBox

          _ -> do
            let
              (newLineId, (_, f, pt)) = minimumBy (comparing $ view $ _2 . _1) snaps
              snappedBox1 = ElemBox $ Box dId mId typ oldPeg $
                  shapeDefinition . boxCentre .~ pt $
                  newShape
            case oldPeg of
              Nothing -> fromMaybe newBox <$> itemPegAt snappedBox1 newLineId f
                -- If peg fails then go back to unsnapped box.
              Just (oldLineId, _) ->
                if oldLineId == newLineId
                  then return $ _ElemBox . boxPeg ?~ (oldLineId, f) $ snappedBox1
                  else do
                    snappedBox2 <- itemUnpeg snappedBox1
                    fromMaybe newBox <$> itemPegAt snappedBox2 newLineId f
        updateItem newBox1
        return newBox1
  where
    screenRange = 20  -- Screen distance within which snaps occur.


-- | If the box is pegged to a line then move it to the right point.
boxAdjust :: (
    AppearanceClass t v,
    DiagramTypeClass t v,
    d ~ Element t v,
    Editable HadesRender v,
    EntityClass v,
    HasDiagrams (Paint d) v,
    HasLookups v,
    Queryable v,
    HasReports v) =>
  Box t v -> Delta (Element t v) ()
boxAdjust (Box dId mId typ peg shape) =
  case peg of
    Nothing -> return ()
    Just (arrId, d) -> do
      contents <- use $ deltaDiagram . diagramContents
      case contents ^? ix arrId . _ElemArrow . arrowShape of
        Nothing -> return ()
        Just arrow -> do
          let
            pt = lineFractionPoint d arrow
            newShape = shapeDefinition . boxCentre .~ pt $ shape
          when (isJust $ shape ^? shapeDefinition . boxCentre) $
            updateItem $ ElemBox $ Box dId mId typ peg newShape


-- | Update the box avatar with information from the model.
updateBox :: (
      AppearanceClass t v,
      Editable HadesRender v,
      EntityClass v,
      DiagramTypeClass t v,
      HasDiagrams HadesRender v,
      HasLookups v,
      Queryable v,
      HasReports v) =>
  Box t v -> Delta (Element t v) Bool
updateBox boxItem@(Box diagId modelId typ peg shape) = do
    contents <- use $ deltaDiagram . diagramContents
    case peg of
      Nothing -> return False  -- Box is unpegged, so leave it alone.
      Just (arrId, f) -> do -- Check if relationship still there, if not then unpeg.
        let unpeg = do
            let newBox = ElemBox $ boxPeg .~ Nothing $ boxItem
            updateItem newBox
            tellItem newBox
            return False
        case contents ^? ix arrId . _ElemArrow of
          Nothing -> unpeg  -- Arrow no longer exists.
          Just arrow -> boxCanPeg boxItem arrow >>= \case
            Nothing -> cannotHappen "Box was pegged to something illegal." unpeg
            Just relName -> do
              stillPegged <- liftBase $ lift $ queryRelations $
                NR.member modelId (arrowEntity arrow) relName
              if stillPegged
                then return False  -- No change.
                else do  -- Clear peg and jump box sideways to show this.
                  let
                    (l, f1) = lineFractionSegment f (arrow ^. arrowShape)
                    lineLen = distance $ pointDiff (linePoint l 0) (linePoint l 1)
                    pt1 = linePoint l f1
                    pt2 = linePoint (lineNormal l pt1) (offset / lineLen)
                    newShape = shapeDrag (pointDiff pt2 pt1) shape
                    newBox = ElemBox $ Box diagId modelId typ Nothing newShape
                  updateItem newBox
                  tellItem newBox
                  return True
  where
    offset = 50  -- Distance a box will jump when unpegged from elsewhere.



-- | Update the arrow avatar with information from the model.
updateArrow :: (
      AppearanceClass t v,
      Editable HadesRender v,
      EntityClass v,
      DiagramTypeClass t v,
      HasDiagrams HadesRender v,
      HasLookups v,
      Queryable v,
      HasReports v) =>
  Arrow t v -> Delta (Element t v) Bool
updateArrow arrow = do
    let
      shape = arrow ^. arrowShape
      -- Points where start and end come from. "startPoint2" is the
      -- point on the line that connects to the start point. If there
      -- are no bends then this is the end point. And vice versa.
      (startPoint2, endPoint2) =
        if V.null $ shape ^. lineMiddle
          then (shape ^. lineEnds . _2 . to connectorPoint,
              shape ^. lineEnds . _1 . to connectorPoint)
          else (shape ^. lineMiddle . to V.head,
              shape ^. lineMiddle . to V.last)
    (eFrom, eTo) <- liftBase $ lift $ edgeLinks (arrowType arrow) (arrowEntity arrow)
    (flag1, newStart) <- updateConnector
        eFrom
        startPoint2
        (shape ^. lineEnds . _1)
    (flag2, newEnd) <- updateConnector
        eTo
        endPoint2
        (shape ^. lineEnds . _2)
    case (newStart, newEnd) of
      (Just newStart1, Just newEnd1) -> do
        newEnds <- if V.null $ shape ^. lineMiddle
          then optimiseEnds (newStart1, newEnd1)
          else return (newStart1, newEnd1)
        let
          newShape = lineEnds .~ newEnds $ shape
          newArrow = arrowShape .~ newShape $ arrow
        updateItem $ ElemArrow newArrow
        return $ flag1 || flag2
      _ -> do  -- End points cannot be updated, so delete arrow from diagram.
        deleteItem $ arrowId arrow
        return True
  where
    -- Update the connector to point to an avatar of modelId. If it returns Nothing then
    -- the arrow cannot be made consistent with the model and should be deleted.
    updateConnector :: (Avatar p v w d) =>
      Maybe (ModelId, Maybe ModelId) -> Point -> ConnectorEnd
      -> Delta d (Bool, Maybe ConnectorEnd)
    updateConnector Nothing _ old@Unconnected {} =
      return (False, Just old)  -- Nothing has changed.
    updateConnector (Just (modelId, exportId)) pt0 old@Unconnected {} = do
      newCon <- connectNearest modelId exportId pt0
      return (Just old /= newCon, newCon)
    updateConnector Nothing pt0 (Connected pt1 _ _) =
      -- Shorten line by 10% to show disconnection.
      return (True, Just $ Unconnected $ linePoint (lineFromPoints pt0 pt1) 0.9)
    updateConnector (Just (modelId, exportId)) pt0 old@(Connected _ oldTargetId _) = do
      r <- withItem oldTargetId $ \target -> do
        oldChildren <- avatarChildren target
        if ascendedId target == Just modelId || modelId `elem` oldChildren
          then return (False, Just old)
          else do
            newCon <- connectNearest modelId exportId pt0
            return (Just old /= newCon, newCon)
      case r of
        Nothing -> do
          cannotHappen
              "Generic DiagraM updateConnector: old target not found in diagram."
              $ return ()
          newCon <- connectNearest modelId exportId pt0
          return (Just old /= newCon, newCon)
        Just v -> return v
    connectNearest :: (Avatar p v w d) =>
      ModelId -> Maybe ModelId -> Point -> Delta d (Maybe ConnectorEnd)
    connectNearest modelId exportId pt =
      nearestAvatar modelId pt >>= \case
        Nothing -> return Nothing
        Just d -> do
          pt2 <- ($ pt) <$> itemConnectionPoint d exportId
          return $ Just $ Connected pt2 (identifier d) exportId


-- | If the argument is allowed on this diagram then add a new avatar to the diagram.
genericEntityAvatar :: (
    AppearanceClass t v,
    DiagramTypeClass t v,
    d ~ Element t v,
    Editable HadesRender v,
    EntityClass v,
    HasDiagrams (Paint d) v,
    HasLookups v,
    HasReports v,
    Queryable v) =>
  Entity v -> Point -> Delta d (Maybe d)
genericEntityAvatar entity basePoint = do
    let target = entityId entity
    prsm <- liftBase $ lift $ usingTraversal return
    case avatarType (cloneTraversal prsm) $ entity ^. entityContents of
      NoAvatar ->   -- If entity is linkable then create a link box.
        if isLinkable entity
          then do
            newItem <- addItem $ \uuid ->
              ElemLink $ Link uuid (Just target) (CommentBox defaultBox) ""
            return $ Just newItem
          else return Nothing
      SimpleBox typ -> do
        newItem <- addItem $ \uuid -> ElemBox $ mkBox uuid target typ Nothing defaultBox
        return $ Just newItem
      SimpleArrow typ -> do
        (eFrom, eTo) <- liftBase $ lift $ edgeLinks typ target
        fromConnector <- avatarConnector eFrom basePoint $
            basePoint `movePoint` (-arrowLength, 0)
        toConnector <- avatarConnector eTo basePoint $
            basePoint `movePoint` (arrowLength, 0)
        newItem <- addItem $ \uuid ->
          ElemArrow $ Arrow uuid target typ $
            LineShape (fromConnector, toConnector) V.empty
        return $ Just newItem
      AvatarScript script -> script entity basePoint
  where
    defaultBox = BoundBox
        (basePoint `movePoint` (-100,-50))
        (basePoint `movePoint` (100,50))
    arrowLength = 50  -- Pixels of arrow length when added to diagram.

-- | Dialog for diagram elements that are not avatars.
--
-- When a diagram element has its properties requested this is handled by "avatarProperties". If
-- the element is an avatar then the request is passed up to the model. Otherwise it is handled
-- by the element dialog selector. Hence this function does not need to handle avatar elements.
elementDialog :: (EntityClass v, HasDiagrams HadesRender v, HasLookups v, HasReports v) =>
  DialogSelector' (Model v) () (Element t v)
elementDialog _ ElemComment {} =
  Just $ promoteDialog (prismatic def _ElemComment) $ Dialog "Comment" OkApplyButton commentGadget
elementDialog _ ElemLink {} =
  Just $ promoteDialog (prismatic def _ElemLink) linkDialog
elementDialog _ _ =
  Nothing  -- Boxes and arrows are avatars so are handled elsewhere.


-- | Create a new entity in the model and add a corresponding box avatar to the diagram.
newBoxEntityTool :: (
    AppearanceClass t v,
    Editable HadesRender v,
    EntityClass v,
    DiagramTypeClass t v,
    HasDiagrams HadesRender v,
    HasLookups v,
    Queryable v,
    HasReports v) =>
  ModelId   -- ^ Parent for new entity.
  -> v      -- ^ Prototype entity value.
  -> BoxType t      -- ^ Representation on the diagram.
  -> Delta (Element t v) (Action (Element t v))
newBoxEntityTool parent newItem boxTyp = do
  liftBase $ lift $ goToEntity parent
  let
    newBox mId dId bounds = ElemBox $ mkBox dId mId boxTyp Nothing bounds
  drawFunc <- boxTypeDraw (mkBox U.nil U.nil boxTyp Nothing NoBox) (return ()) Nothing
  newBoxAvatarTool parent newBox drawFunc newItem


newArrowEntityTool :: (
    AppearanceClass t v,
    Editable HadesRender v,
    EntityClass v,
    DiagramTypeClass t v,
    HasDiagrams HadesRender v,
    HasLookups v,
    Queryable v,
    HasReports v) =>
  Bool  -- ^ If true then sequester arrows in a separate package called \"Arrows\"
  -> ModelId   -- ^ Parent for new entity.
  -> v         -- ^ Prototype entity value.
  -> ArrowType t      -- ^ Representation on the diagram.
  -> Delta (Element t v) (Action (Element t v))
newArrowEntityTool arrowFlag parent newItem arrowTyp = do
    arrows <- if arrowFlag
      then do  -- Create "Arrows" sub-package if it doesn't exist.
        (arrows, chg) <-
          liftBase $ lift $ goToEntity parent >> createPath [Name "Arrows"]
        when chg $ tellCheckpoint "Auto-added Arrows package"
        return arrows
      else return parent  -- Arrows are kept in parent along with everything else.
    liftBase $ lift $ goToEntity arrows
    nm <- liftBase $ lift $ modelNewName $ newItem ^. name
    let newArrow mId dId shape = ElemArrow $ Arrow dId mId arrowTyp shape
    newArrowAvatarTool
      arrows
      newArrow
      (name .~ nm $ newItem)
      updateAssociations
  where
    updateAssociations (ElemArrow arrow) = updateArrowAssociations Nothing arrow
    updateAssociations _ = cannotHappen
        "newArrowEntityTool arrow creation: something other than an arrow was created."
        $ return ()


-- | Add a new comment box to the diagram.
newCommentTool :: (
    AppearanceClass t v,
    Editable HadesRender v,
    EntityClass v,
    DiagramTypeClass t v,
    HasDiagrams HadesRender v,
    HasLookups v,
    Queryable v,
    HasReports v) =>
  Action (Element t v)
newCommentTool = mkScriptAction $ newDiagramBoxTool
    (Nothing, "")
    (constantDialog $ Dialog "Comment" OkButton commentGadget1)
    mkComment
    drawBox
    "Add comment to diagram"
  where
    mkComment uid s (graphic, txt) = ElemComment $ Comment uid (CommentBox s) graphic txt
    -- drawBox is only used for rubber band, so no text is required.
    drawBox (ElemComment (Comment _ s _ _)) = drawCommentBox s
    drawBox _ = cannotHappen
        "newCommentTool: something other than a comment was created."
        $ return ()


-- | Add a new link box to the diagram.
newLinkTool :: (
    AppearanceClass t v,
    Editable HadesRender v,
    EntityClass v,
    DiagramTypeClass t v,
    HasDiagrams HadesRender v,
    HasLookups v,
    Queryable v,
    HasReports v) =>
  Action (Element t v)
newLinkTool = mkScriptAction $ newDiagramBoxTool
    (Link U.nil Nothing (CommentBox NoBox) "")  -- Empty link. ID and box are dummies.
    (constantDialog linkDialog)
    mkLink
    drawBox
    "Add link to diagram"
  where
    mkLink uid p v = ElemLink $ Link uid (v ^. linkTarget) (CommentBox p) (v ^. linkText)
    drawBox (ElemLink (Link _ _ s _)) = drawCommentBox s
    drawBox _ = cannotHappen
        "newlinkTool: something other than a link was created."
        $ return ()
