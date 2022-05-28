{-# LANGUAGE Arrows #-}
{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

-- |

module Model.Report.Dialogs (
  -- * Tree Conversion
  reportFragmentToTree,
  reportFragmentsToForest,
  reportFragmentFromTree,
  reportFragmentsFromForest,
  reportTreeIso,
  -- * Editing Reports
  reportDesignGadget,
  reportDialogSelector,
  -- * Running Reports
  ReportFormat (..),
  ReportRunParameters (ReportRunParameters),
  reportTarget,
  reportFormat,
  reportRunDefault,
  reportRunDialog,
  reportWarningsDialog,
) where

import Control.Arrow
import Control.Lens
import Control.Lens.Hades
import Control.Monad
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree
import qualified Data.UUID as U
import Model.Abstract.PackageTree
import Model.Abstract.Properties
import Model.Lookups.Base
import Model.Matrices.Base
import Model.Matrices.Design (sortOrderGadget)
import Model.Query.Diagram
import Model.Reflection.Reflective
import Model.Reflection.Types
import Model.Reflection.Values
import Model.Report.Base
import Reactive.Banana.ArrowDialog
import Reactive.Banana.Menu
import Reactive.Banana.Table

-- | To avoid duplicating almost the exact design of "ReportFragment" this module just stuffs
-- reports into a forest. The only difference is that "SubReport" items have their subreport lists
-- stored as children instead of in the internal field.
reportFragmentToTree :: ReportFragment v -> Tree (ReportFragment v)
reportFragmentToTree (SubReport query order subreport) =
  Node (SubReport query order []) $ reportFragmentsToForest subreport
reportFragmentToTree sr = Node sr []


reportFragmentsToForest :: [ReportFragment v] -> Forest (ReportFragment v)
reportFragmentsToForest = map reportFragmentToTree


reportFragmentFromTree :: Tree (ReportFragment v) -> ReportFragment v
reportFragmentFromTree (Node (SubReport query order _) childs) =
  SubReport query order $ reportFragmentsFromForest childs
reportFragmentFromTree (Node sr _) = sr


reportFragmentsFromForest :: Forest (ReportFragment v) -> [ReportFragment v]
reportFragmentsFromForest = map reportFragmentFromTree

-- | ISO for the conversion between report fragments and trees. Not quite a true ISO because the
-- reverse will ignore children of a node that is not a @SubReport@. However the forward followed
-- by the reverse will be an identity.
reportTreeIso :: Iso' (ReportFragment v) (Tree (ReportFragment v))
reportTreeIso = iso reportFragmentToTree reportFragmentFromTree


-- | Gadget for designing reports. Emits "ModelEdit" actions which carry undo information.
reportDesignGadget :: (EntityClass v, HasLookups v, HasMatrices v) =>
  ModelId -> Gadget' (Model v) (ModelEdit v (Report v) (Maybe Text)) ((), Report v)
reportDesignGadget reportId = getInitial $ \((), report1) ->
    let
      nm1 = report1 ^. reportName . nameText
    in proc ((), report) -> do
        report' <- accum $ box Vertical [[
            form Vertical [
                ("Name:", focusing (reportName . nameText) simpleTextBox),
                ("Description:", focusing reportDescription $ memoBox MemoSmall False),
                ("Default Input:", focusing (reportDefaultInput . from setMaybeIso) $
                    modelItemSpecifier (const $ const True) (const Nothing)),
                ("Default Template:", focusing (reportDefaultTemplate . defaultIso "") $
                  filePathSelector
                      "Report Template Document"
                      FilePathOpen
                      [("Word documents", [FilePathGlob "*.docx"])]
                      False)
              ],
            focusing (reportSpec . mapping reportTreeIso) $ simpleFrame "Structure" specEdit
          ]] -< report
        model <- getEnv -< ()
        _ <- send (reportDelta nm1) -< (model, report, report')
        returnA -< ((), report')
  where
    specEdit = forestEditor
      (fragmentDescription 1000)
      treeMenu
      (isJust . preview _SubReport)
      (Just reportDialogSelector)
    treeMenu (Just SubReport {}) = Menu [[
        subMenu "Add before" $
          Menu [map (\(txt, v) -> menuItem txt $ TreeAddBefore v) addOptions],
        subMenu "Add inside" $
          Menu [map (\(txt, v) -> menuItem txt $ TreeAddIn v) addOptions],
        subMenu "Add after" $
          Menu [map (\(txt, v) -> menuItem txt $ TreeAddAfter v) addOptions],
        menuItem "Delete" TreeDelete
      ]]
    treeMenu Nothing = Menu [map (\(txt, v) -> menuItem txt $ TreeAddAfter v) addOptions]
    treeMenu _ = Menu [[
        subMenu "Add before" $
          Menu [map (\(txt, v) -> menuItem txt $ TreeAddBefore v) addOptions],
        subMenu "Add after" $
          Menu [map (\(txt, v) -> menuItem txt $ TreeAddAfter v) addOptions],
        menuItem "Delete" TreeDelete
      ]]
    addOptions = [
        ("Literal", ReportLiteral ""),
        ("Field", ReportField nameField),
        ("Field List", ReportFieldList [nameField]),
        ("Heading", ReportHeading 1 "" $ Just (nameField, "")),
        ("Diagram", ReportDiagram),
        ("Matrix", ReportMatrix Nothing),
        ("Lookup Table", ReportLookup Nothing),
        ("Section", SubReport Nothing (SortOrder [(nameField, Ascending)]) [])
      ]
    reportDelta :: (EntityClass v) =>
      Text -> (Model v, Report v, Report v) -> Maybe (ModelEdit v (Report v) (Maybe Text))
    reportDelta oldName (model, r1, r2)
      | r1 ^. reportName /= r2 ^. reportName  = Just $ do
        goToEntity reportId
        void $ setValue r2
        return $ Just $ "Renamed " <> oldName
      | r1 ^. reportDescription /= r2 ^. reportDescription  = Just $ do
        goToEntity reportId
        void $ setValue r2
        return $ Just $ "Edited " <> oldName <> " description"
      | r1 ^. reportDefaultInput /= r2 ^. reportDefaultInput  = Just $ do
        goToEntity reportId
        void $ setValue r2
        return $ Just $ "Edited " <> oldName <> " default input"
      | r1 ^. reportDefaultTemplate /= r2 ^. reportDefaultTemplate  = Just $ do
        goToEntity reportId
        void $ setValue r2
        return $ Just $ "Edited " <> oldName <> " default template"
      | r1 ^. reportSpec /= r2 ^. reportSpec  = Just $ do
        let
          ft1 = concatMap flatten $ r1 ^. reportSpec . mapping reportTreeIso
          ft2 = concatMap flatten $ r2 ^. reportSpec . mapping reportTreeIso
          diff = find (uncurry (/=)) $ zip ft1 ft2
          l1 = length ft1
          l2 = length ft2
          changed
            | l1 > l2  =  -- Somthing deleted from ft1.
              "Deleted " <> fragmentDescription 40 model (maybe (last ft1) fst diff)
                -- "last" is safe because length ft1 > 0
            | l1 < l2  =  -- Something inserted in ft2.
              "Inserted " <> fragmentDescription 40 model (maybe (last ft2) snd diff)
            | otherwise = case diff of
              Just (frag, _) -> "Changed " <> fragmentDescription 40 model frag
              Nothing -> "Changed something"  -- Should never happen.
        goToEntity reportId
        void $ setValue r2
        return $ Just $ "Edit " <> oldName <> ": " <> changed
      | otherwise  = Nothing


reportDialogSelector :: (EntityClass v, HasLookups v, HasMatrices v) =>
  DialogSelector' (Model v) w (ReportFragment v)
reportDialogSelector _ frag = case frag of
  ReportLiteral {} -> Just reportLiteralDialog
  ReportField {} -> Just reportFieldDialog
  ReportFieldList {} -> Just reportFieldListDialog
  ReportHeading {} -> Just reportHeadingDialog
  ReportDiagram {} -> Nothing
  ReportMatrix {} -> Just reportMatrixDialog
  ReportLookup {} -> Just reportLookupDialog
  SubReport {} -> Just subReportDialog


-- | Dialog for the text in a literal fragment.
reportLiteralDialog :: Dialog' (Model v) w (ReportFragment v)
reportLiteralDialog = Dialog "Literal text" OkButton $
  prismatic "" _ReportLiteral $ memoBox MemoMedium True


-- | Dialog for a single field selection.
reportFieldDialog :: (EntityClass v) => Dialog' (Model v) w (ReportFragment v)
reportFieldDialog = Dialog "Field contents" OkButton $
    prismatic nameField _ReportField $ validate (not . U.null) $ comboBox fields
  where
    fields =
      map (\f -> ComboItem (f ^. fieldName) Nothing Nothing (fieldId f)) .
      sortOn (view fieldName) . M.elems . withStampFields . withBuiltIn . modelFields

-- | Dialog for a list of field selections.
reportFieldListDialog :: (EntityClass v) => Dialog' (Model v) w (ReportFragment v)
reportFieldListDialog = Dialog "Field definition list" OkButton $
    prismatic [nameField] _ReportFieldList $ getInitialEnv $ \model ->
      let
        fields = withStampFields $ withBuiltIn $ modelFields model
        fieldNames = sort $ map (view fieldName) $ M.elems fields
        fieldTable = table
          [TableDelete, TableShuffle]
          [mkField "Field" id Nothing (EditCombo fieldNames) Nothing]
          Nothing
      in validate validFields $ accum $ focusing (fieldNameIso fields . extraIso) fieldTable
  where
    validFields [] = False
    validFields xs = not $ any U.null xs
    extraIso :: Iso' [FieldName] [FieldName]
    extraIso = iso (++ [" -?- "]) (filter $ (/= "-?-") . T.strip)


reportHeadingDialog :: (EntityClass v) => Dialog' (Model v) w (ReportFragment v)
reportHeadingDialog = Dialog "Heading" OkButton $ prismatic (1, "", Nothing) _ReportHeading $
    accum $ box Vertical [[
        form Vertical [
            ("Level:", focusing _1 $ rangeCombo (1,5)),
            ("Text 1:", focusing _2 simpleTextBox)
          ],
        focusingOver _3 $ optionalOver (nameField, "") $ form Vertical [
            ("Field:", focusing _1 $ validate (not . U.null) $ comboBox fields),
            ("Text 2:", focusing _2 simpleTextBox)
          ]
      ]]
  where
    fields =
      map (\f -> ComboItem (f ^. fieldName) Nothing Nothing (fieldId f)) .
      M.elems . withStampFields . withBuiltIn . modelFields


reportMatrixDialog :: (EntityClass v, HasMatrices v) => Dialog' (Model v) w (ReportFragment v)
reportMatrixDialog = Dialog "Select a matrix" OkButton $
  prismatic mempty (_ReportMatrix . from setMaybeIso) $
  validate ((> 0) . S.size) $
  treeSelector $ modelFilter $ isJust .  preview (entityContents . _Matrix)


reportLookupDialog :: (EntityClass v, HasLookups v) => Dialog' (Model v) w (ReportFragment v)
reportLookupDialog = Dialog "Select a lookup table" OkButton $
  prismatic mempty (_ReportLookup . from setMaybeIso) $
  validate ((> 0) . S.size) $
  treeSelector $ modelFilter $ isJust .  preview (entityContents . _LookupTable)


subReportDialog :: (EntityClass v, Queryable v) => Dialog' (Model v) w (ReportFragment v)
subReportDialog = Dialog "Report Section" OkButton $
  prismatic (Nothing, SortOrder [(nameField, Ascending)], []) _SubReport $ accum $ proc frag -> do
    r <- box Vertical [[
          simpleFrame "Section query" $
            focusing (_1 . from setMaybeIso) $
            validate ((1 ==) . S.size) $
            treeSelector $ modelFilter $ isJust .  preview (entityContents . _QueryDiagram),
          simpleFrame "Section ordering" $ focusingOver _2 sortOrderGadget
        ]]
      -< frag
    _ <- message1 "The children of this item will be added to the report once for \
            \each result of the query."
      -< ()
    returnA -< r


-- | Format the diagram is to be generated in.
data ReportFormat =
  ReportHtml FilePath    -- ^ Destination file name.
  | ReportDocx ReportPageLayout FilePath  -- ^ Page layout and destination file name.
  | ReportTemplate FilePath FilePath   -- ^ Template and destination file names.
  deriving (Eq, Show)

_ReportHtml :: Prism' ReportFormat FilePath
_ReportHtml = prism ReportHtml $ \case {ReportHtml p -> Right p; v -> Left v}

_ReportDocx :: Prism' ReportFormat (ReportPageLayout, FilePath)
_ReportDocx = prism (uncurry ReportDocx) $ \case {ReportDocx l p -> Right (l, p); v -> Left v}

_ReportTemplate :: Prism' ReportFormat (FilePath, FilePath)
_ReportTemplate = prism
    (uncurry ReportTemplate) $
    \case {ReportTemplate t p -> Right (t, p); v -> Left v}


data ReportRunParameters = ReportRunParameters {
    _reportTarget :: Set ModelId,
    _reportFormat :: ReportFormat
  } deriving (Show)

reportTarget :: Lens' ReportRunParameters (Set ModelId)
reportTarget = lens _reportTarget $ \s x -> s {_reportTarget = x}

reportFormat :: Lens' ReportRunParameters ReportFormat
reportFormat = lens _reportFormat $ \s f -> s {_reportFormat = f}


-- | Default value for "ReportRunParameters" in dialogs.
reportRunDefault :: Report v -> ReportRunParameters
reportRunDefault rpt =
  ReportRunParameters (rpt ^. reportDefaultInput . from setMaybeIso) $
      case rpt ^. reportDefaultTemplate of
        Nothing -> ReportDocx reportPageDefault ""
        Just t -> ReportTemplate t ""


-- | Dialog for running a report.
reportRunDialog :: (EntityClass v, HasMatrices v) =>
  Bool  -- ^ True means report is preview only.
  -> Report v
  -> Dialog' (Model v) () ReportRunParameters
reportRunDialog previewFlag report =
    Dialog "Run Report" OkButton $ accum $
      if previewFlag
        then contentItems
        else box Vertical [[
              simpleFrame "Report Target" contentItems,
              simpleFrame "Output Format" outputItems
            ]]
  where
    nullLens :: Lens' a ()
    nullLens = lens (const ()) const
    contentItems = form Vertical [
        ("Report name:",
          focusing nullLens $
          readOnlyText $ const $ const $ report ^. reportName . nameText),
        ("Description:",
          focusing nullLens $
          readOnlyMemo MemoSmall True $ const $ const $ report ^. reportDescription),
        ("Input:",
          focusing reportTarget $
          validate (not . S.null) $
          modelItemSpecifier inputFilter $ const Nothing)
      ]
    -- outputItems :: GadgetF' e w ReportRunParameters
    outputItems = styled1 "reportParameters" $ focusing reportFormat $ unionTab [
        ("New Word document", PrismaticGadget (reportPageDefault, "") _ReportDocx $
          accum $ form Vertical [
            ("Paper", focusing (_1 . reportPagePaper) $
              simpleCombo [Letter, A3, A4]),
            ("Orientation", focusing (_1 . reportPageOrientation) $
              simpleCombo [Portrait, Landscape]),
            ("Output document", focusing _2 outputDocx)
          ]),
        ("From Word template", PrismaticGadget ("", "") _ReportTemplate $
          accum $ form Vertical [
            ("Template document", focusing _1 templateDocx),
            ("Output document", focusing _2 outputDocx)
          ]),
        ("HTML for Web", PrismaticGadget "" _ReportHtml $
          accum $ form Vertical [
              ("Output document", focusing id outputHtml)
          ])
      ]
    -- Target word file selector
    templateDocx =
      validate (/= "") $
      filePathSelector "Word document for template" FilePathOpen docxFilter False
    outputDocx =
      validate (/= "") $ filePathSelector "Word document to create" FilePathSave docxFilter True
    outputHtml =
      validate (/= "") $ filePathSelector "HTML document to create" FilePathSave htmlFilter True
    docxFilter = [("Word documents", [FilePathGlob "*.docx"])]
    htmlFilter = [("HTML documents", [FilePathGlob "*.html", FilePathGlob "*.htm"])]
    -- The input set for the report is the union of the input sets of the top level section
    -- queries and any matrices which appear in the top level.
    topQueryIds :: [ModelId]
    topQueryIds = mapMaybe (preview $ _SubReport . _1 . _Just) $ report ^. reportSpec
    topQueries :: (EntityClass v, Queryable v) => Model v -> [QueryDiagram v]
    topQueries model = mapMaybe
        (\mId -> modelContents model ^? ix mId . entityContents . _QueryDiagram)
        topQueryIds
    queryInputSet :: (EntityClass v, Queryable v) => Model v -> Set (Variant v)
    queryInputSet = foldr (S.union . view queryInputs) mempty . topQueries
    topMatrixIds :: [ModelId]
    topMatrixIds = mapMaybe (preview $ _ReportMatrix . _Just) $ report ^. reportSpec
    topMatrices :: (EntityClass v, HasMatrices v) => Model v -> [Matrix]
    topMatrices model = mapMaybe
        (\mId -> modelContents model ^? ix mId . entityContents . _Matrix)
        topMatrixIds
    matrixInputs :: (EntityClass v, HasMatrices v) => Model v -> Set (Variant v)
    matrixInputs model = foldr (S.union . flip matrixInputSet model) mempty $ topMatrices model
    inputFilter model =
      let inputSet = matrixInputs model `S.union` queryInputSet model
      in if S.null inputSet
          then const True
          else \e -> S.member (e ^. entityContents . to reflectiveName) inputSet


-- | Dialog for report warnings. Displays a non-editable table of warnings.
reportWarningsDialog :: (EntityClass v) => [(ModelId, Text)] -> Dialog' (Model v) () ()
reportWarningsDialog warnings = Dialog "Report Warnings" OkButton $ proc () -> do
    model <- getEnv -< ()
    let tblDat =
        map (_1 %~ (\k -> modelContents model ^? ix k . entityName . nameText)) warnings
    _ <- table [] tbl Nothing -< tblDat
    returnA -< ()
  where
    tbl = [
        mkField "Entity" _1 Nothing (Opt $ EditFixed id) Nothing,
        mkField "Message" _2 Nothing (EditFixed id) Nothing
      ]
