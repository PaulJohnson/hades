{-
Copyright © Paul Johnson 2021. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.
-}

{- |
Implements "Reactive.Banana.Table" in Webits using the <https://appendgrid.apphb.com AppendGrid>
JavaScript plugin.
-}
module Network.Webits.Table where

import Control.Concurrent.STM
import Control.Concurrent.TState
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Types
import Data.IORef
import Data.List (intersperse)
import Data.Maybe
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Network.JavaScript
import Network.Webits
import Network.Webits.Icons
import Reactive.Banana.Table
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Reactive.Banana.Common
import Text.Read
import Data.Char


-- @AppendGrid@ JavaScript objects.
data WebitTable a = WebitTable {
    wtRoot :: WebitContainer,
    wtGrid :: RemoteValue (WebitTable a)
  }


data TableIconClick = TableIconClick {
    tableIconSrc :: Text,  -- ^ Full URL of clicked icon.
    tableIconColumn :: Text,
    tableIconIndex :: Int
  } deriving Show

instance FromJSON TableIconClick where
  parseJSON = withObject "Table icon click" $ \v ->
    TableIconClick <$>
        (iconFromUrl <$>  v .: "src") <*>
        v .: "col" <*>
        v .: "idx"


-- | The JSON value is a record with @op@ being @insert@, @delete@ or @swap@, and @n@ being
-- the index value.
data TableRowOp =
  TableRowInsert Int   -- ^ The index of the new row.
  | TableRowDelete Int  -- ^ The index of the deleted row.
  | TableRowSwap Int  -- ^ The index of the first row, swapped with it's successor.

instance FromJSON TableRowOp where
  parseJSON = withObject "Table row op" $ \v -> do
    op1 <- v .: "op"
    n <- v .:? "n" .!= 0
    case op1 of
      ("insert" :: String) -> return $ TableRowInsert n
      "delete" -> return $ TableRowDelete n
      "swap" -> return $ TableRowSwap n
      _ -> fail $ "Unrecognised table op: " <> show op1


-- | Apply the table row operation to the list. The first argument is the value to insert. If
-- it is @Nothing@ then insertion is ignored.
applyTableRowOp :: Maybe a -> TableRowOp -> [a] -> [a]
applyTableRowOp (Just d) (TableRowInsert n) xs = let (xs1, xs2) = splitAt n xs in xs1 ++ d : xs2
applyTableRowOp Nothing (TableRowInsert _) xs = xs
applyTableRowOp _ (TableRowDelete n) xs = let (xs1, xs2) = splitAt n xs in xs1 ++ drop 1 xs2
applyTableRowOp _ (TableRowSwap n) xs = let (xs1, xs2) = splitAt n xs in xs1 ++ case xs2 of
  x1:x2:xs3 -> x2:x1:xs3
  _ -> xs2



-- | Convert a table field to a triple of:
--
-- * A JSON object representing the AppendGrid column type.
--
-- * A getter for extracting the JSON for this column from a Haskell @row@. This needs to be
-- inserted into an 'Object' along with the rest of the columns to generate a JSON row record.
--
-- * A setter for updating the associated Haskell value from the JSON row record.
fieldToColumn ::
  WebitId   -- ^ If the field is an icon then this webit will get the IconClick events. It should
        -- react by putting up the 'iconMenu'.
  -> Bool   -- ^ When @True@, add a @click@ event to the column that sends the column and index.
  -> RemoteValue (WebitTable row)
  -> Int   -- ^ Column number (used for identification)
  -> TableField row a
  -> (JavaScript, row -> Value, Value -> row -> row)
fieldToColumn wId clickFlag grid colNum tf =
    (column,
    \row -> fromMaybe Null $ row ^? fieldValue tf . re (clonePrism fPrsm),
    \j -> maybe id (set (fieldValue tf)) $ j ^? clonePrism fPrsm
    )
  where
    fPrsm = fieldPrism $ fieldEditor tf
    column = "{\
          \name:" <> value (columnId colNum) <> ",\
          \display:" <> value (columnName tf) <> ",\
          \type:\"custom\",\
          \customBuilder:" <> customBuilder <> ",\
          \customGetter:" <> customGetter <> ",\
          \customSetter:" <> customSetter <> "\
        \}"
    hasIcon = isJust $ fieldIcon tf
    customBuilder = if clickFlag
      then
        "function (parent, idPrefix, name, uniqueIndex) {\
            \let controlId=idPrefix+\"_\"+name+\"_\"+uniqueIndex;\
            \parent.onclick=function (e) {" <>
                webitEventData wId (var grid <> ".getRowIndex(uniqueIndex)") <>
              "};\
            \parent.innerHTML=" <> htmlValue customHtml <> ";\
            \let n=parent.childNodes[" <> value fieldIdx <> "];\
            \n.id=controlId;\
            \n.dsmUnique=uniqueIndex;\
          \}"
      else
        "function (parent, idPrefix, name, uniqueIndex) {\
            \let controlId=idPrefix+\"_\"+name+\"_\"+uniqueIndex;\
            \parent.innerHTML=" <> htmlValue customHtml <> ";\
            \let n=parent.childNodes[" <> value fieldIdx <> "];\
            \n.id=controlId;\
            \n.dsmUnique=uniqueIndex;\
          \}"
    customGetter =
      "function (idPrefix, name, uniqueIndex) {\
          \let controlId=idPrefix+\"_\"+name+\"_\"+uniqueIndex;\
          \return document.getElementById(controlId)." <> fieldJS (fieldEditor tf) <>";\
        \}"
    customSetter =
      "function (idPrefix, name, uniqueIndex, v) {\
          \let controlId=idPrefix+\"_\"+name+\"_\"+uniqueIndex;\
          \document.getElementById(controlId)." <> fieldJS (fieldEditor tf) <> "=v;\
        \}"
    fieldIdx = if hasIcon then 1 else 0 :: Int
    customHtml = do
      when hasIcon $ H.img ! HA.class_ "icon-inline" ! HA.src ""
      fieldHtml $ fieldEditor tf
    fieldHtml EditBool = if clickFlag
      then H.input
        ! HA.type_ "checkBox"
        ! HA.onchange (jsValue "this.checked=!this.checked;")  -- Simulate read-only
        ! HA.readonly "1"
      else H.input
        ! HA.type_ "checkbox"
        ! HA.onchange (jsValue $ jsSendChange wId grid)
    fieldHtml EditEntry {} = H.input
      ! HA.type_ "text"
      ! HA.onchange (jsValue $ jsSendChange wId grid)
      ! if clickFlag then HA.readonly "1" else mempty
    fieldHtml EditNote = H.textarea
      ! HA.rows "3"
      ! HA.cols "20"
      ! HA.onchange (jsValue $ jsSendChange wId grid)
      ! (if clickFlag then HA.readonly "1" else mempty)
      $ ""
    fieldHtml (EditEnum f xs) = H.select
      ! HA.onchange (jsValue $ jsSendChange wId grid)
      ! (if clickFlag then HA.readonly "1" else mempty)
      $ zipWithM_ (\n x -> H.option ! HA.value (H.toValue n) $ H.text (f x)) ([0..] :: [Int]) xs
    fieldHtml EditDate {} = H.input
      ! HA.type_ "date"
      ! HA.onchange (jsValue $ jsSendChange wId grid)
      ! if clickFlag then HA.readonly "1" else mempty
    fieldHtml EditColour = H.input
      ! HA.type_ "color"
      ! HA.onchange (jsValue $ jsSendChange wId grid)
      ! if clickFlag then HA.readonly "1" else mempty
    fieldHtml EditIcon {} = tableIcon clickFlag wId grid colNum
    fieldHtml (EditCombo opts) = H.select
      ! HA.onchange (jsValue $ jsSendChange wId grid)
      ! (if clickFlag then HA.readonly "1" else mempty)
      $ forM_ opts $ \str -> H.option ! HA.value (H.textValue str) $ H.text str
    fieldHtml EditFixed {} = H.input
      ! HA.type_ "text"
      ! HA.readonly "1"
      ! HA.onchange (jsValue $ jsSendChange wId grid)
    fieldHtml (Opt EditBool) = H.select
      ! HA.onchange (jsValue $ jsSendChange wId grid)
      ! (if clickFlag then HA.readonly "1" else mempty)
      $ do
        H.option ! HA.value "null" $ H.text ""
        H.option ! HA.value "true" $ H.text "Yes"
        H.option ! HA.value "false" $ H.text "No"
    fieldHtml (Opt EditEntry {}) =  H.input
      ! HA.type_ "text"
      ! HA.onchange (jsValue $ jsSendChange wId grid)
      ! if clickFlag then HA.readonly "1" else mempty
    fieldHtml (Opt EditNote) = H.textarea
      ! HA.rows "3" ! HA.cols "20"
      ! HA.onchange (jsValue $ jsSendChange wId grid)
      ! (if clickFlag then HA.readonly "1" else mempty)
      $ ""
    fieldHtml (Opt (EditEnum f xs)) = fieldHtml $ EditEnum (maybe "" f) $ Nothing : map Just xs
    fieldHtml (Opt EditDate {}) =  H.input
      ! HA.type_ "date"
      ! HA.onchange (jsValue $ jsSendChange wId grid)
      ! if clickFlag then HA.readonly "1" else mempty
    fieldHtml (Opt EditColour) = H.input
      ! HA.type_ "color"
      ! HA.onchange (jsValue $ jsSendChange wId grid)
      ! if clickFlag then HA.readonly "1" else mempty
    fieldHtml (Opt EditIcon {}) = tableIcon clickFlag wId grid colNum
    fieldHtml (Opt (EditCombo opts)) = H.select
      ! HA.onchange (jsValue $ jsSendChange wId grid)
      ! (if clickFlag then HA.readonly "1" else mempty)
      $ do
        H.option ! HA.value "null" $ H.text ""
        forM_ opts $ \str -> H.option ! HA.value (H.textValue str) $ H.text str
    fieldHtml (Opt EditFixed {}) = H.input ! HA.type_ "text" ! HA.readonly "1"
    fieldHtml (Opt (Opt _)) =
      error "fieldToColumn: Nested Opt not supported in FieldEditor type."
    fieldPrism :: FieldEditor a -> Prism' Value a
    fieldPrism EditBool = _Bool
    fieldPrism (EditEntry prsm) = _String . prsm
    fieldPrism EditNote = _String
    fieldPrism (EditEnum _ xs) =
      let
        fwd = V.fromList xs
        bwd = M.fromList $ zip xs [0..]
        prsm = prism' (fromMaybe 0 . (`M.lookup` bwd)) (fwd V.!?)
      in _Integral . prsm
    fieldPrism EditDate {} = _String . datePrism "%Y-%m-%d"
    fieldPrism EditColour = _String . colourPrism
    fieldPrism EditIcon {} = _String . iconIso
    fieldPrism EditCombo {} = _String
    fieldPrism (EditFixed f) = _String . prism' f (const Nothing)
    fieldPrism (Opt (EditEntry prsm)) = _String . prismToMaybe prsm
    fieldPrism (Opt EditNote) = _String . prismToMaybe id
    fieldPrism (Opt field) = nullIsNothing $ fieldPrism field
    fieldJS EditBool = "checked"
    fieldJS EditIcon {} = "attributes.src.value"
    fieldJS (Opt EditIcon {}) = "attributes.src.value"
    fieldJS _ = "value"


-- | Javasript to send an updated copy of the table to the server.
jsSendChange :: WebitId -> RemoteValue (WebitTable row) -> JavaScript
jsSendChange wId grid = "if (" <> var grid <> ") {" <>
    webitEventData wId (var grid <> ".getAllValue()") <>
  "}"


-- | Creates a Webit for editing a list of values in a table.
--
-- The obvious way to implement this would be to make @a@ an instance of 'ToJSON' and 'FromJSON'.
-- However 'TableField' type does not include an internal fieldname that could be matched to the
-- JSON object fields. The solution is to make each cell update its row, starting from a default
-- value. As a result of this, any parts of @a@ that are not listed in the 'Table' specification
-- will have old values. Where shuffling is enabled these may be values from other rows.
--
-- The result includes a command to be executed once the table has been inserted into the DOM.
mkTableWebit :: (Eq row) =>
  [TableEditing row] -> Table row -> [row] -> Elmer (Webit [row], RemoteMonad ())
mkTableWebit editFlags spec initial = do
    outerBox <- makeWebitContainer ""
    tableBox <- makeWebitContainer1 "table" []
    showWebit outerBox tableBox
    editChan <- addWebitChannel tableBox  -- Full copy of contents when an edit occurs.
    opChan <- addWebitChannel tableBox  -- Insert, delete, shuffle.
    current <- liftIO $ newTVarIO initial
    inputChan <- liftIO newTChanIO
    events <- liftIO newBroadcastTChanIO
    eventsOut <- liftIO $ atomically $ dupTChan events
    let output = TState {
          tStateBehaviour = readTVar current,
          tStateEvent = readTChan eventsOut,
          tStateDup = do
            newEvents <- dupTChan events
            return $ TState (tStateBehaviour output) (readTChan newEvents) (tStateDup output)
        }
    elmer <- ask
    let
      iconMenuNeeded = any isIconEditor spec
      tableId = webitId tableBox
      eng = elmerEngine elmer
      defList = maybe [] repeat $ addButtons ^? _head . _2
    grid <- liftIO $ send eng $ constructor "null"  -- Null constructor to avoid mdo.
    let
      (columnSpecs, getters, setters) =
        unzip3 $ zipWith (\n (TableColumn s) -> fieldToColumn tableId False grid n s) [0..] spec
      rowToJson row = object $ zip (map columnId [0..]) (map ($ row) getters)
      -- Load new values into the grid, being careful to set and unset the callback disable flag.
      -- Loading new data triggers a multiple row append operation, resulting in an endless
      -- ping-pong between client and server if the callback is not blocked during data load.
      loadNewValues vs = "{" <>
          var grid <> ".dsmRowDisable=true;" <>
          (if null vs
            then   -- Bodge because AppendGrid won't accept empty data.
              var grid <> ".load([{col0:null}]);" <>
              var grid <> ".removeRow(0);"
            else
              call (var grid <> ".load") [value $ map rowToJson vs] <> ";") <>
          updateDecorations spec grid vs <> ";" <>
          var grid <> ".dsmRowDisable=false;" <>
        "}"
      parseRow = withObject "Table row" $ \v ->
        foldr (.) id <$> forM (zip [0..] setters) (\(n, s) -> s <$> v .:? columnId n .!= Null)
      domCode1 = do
        command $ "{\
          \let sendOp=function(o1,n1){\
              \if (" <> var grid <> " && !(" <> var grid  <> ".dsmRowDisable)){\
                \" <> webitEventData tableId "{op:o1,n:n1}" <>
            "}};" <>
          var grid <> "=new AppendGrid({\
            \element:" <> var (webitHandle tableBox) <> ",\
            \columns:[" <> mconcat (intersperse "," columnSpecs) <> "],\
            \hideButtons:{" <> mconcat (intersperse "," hideButtons) <> "},\
            \afterRowInserted:function(c,p,ns){sendOp('insert',p===null?0:p+1);},\
            \afterRowSwapped:function(c,n1,n2){sendOp('swap',Math.min(n1,n2));},\
            \afterRowRemoved:function(c,n){\
                \sendOp('delete',n===null?" <> var grid <> ".getRowCount():n);},\
            \afterRowAppended:function(c,p,ns){sendOp('insert',p===null?0:p+1);},\
            \hideRowNumColumn:true});\
          \}"
        command $ loadNewValues initial
        command $ updateDecorations spec grid initial
    -- If there are EditIcon columns then run the icon menu.
    when iconMenuNeeded $ do
      iconClicks <- addWebitChannel tableBox  -- TableIconClick record.
      elmerDaemon tableBox $ do
        click <- liftIO $ atomically $ readTChan iconClicks  -- Icon was clicked; pop up menu.
        getIconChoice (iconFromUrl $ tableIconSrc click) >>= \case
          Nothing -> return ()
          Just newIcon -> do
            liftIO $ atomically $ do
              oldValue <- readTVar current
              let
                colNum = fromMaybe 0 $ readMaybe $
                    filter isDigit $ T.unpack $ tableIconColumn click
                newValue = case drop colNum spec of
                  (TableColumn (TableField _ trv _ EditIcon {} _) : _) ->
                    ix colNum . trv .~ newIcon $ oldValue
                  _ -> oldValue
              writeTVar current newValue
              writeTChan events ((), newValue)
            elmerSend $ "{" <>
              var grid <> ".setCtrlValue(" <>
                value (tableIconColumn click) <> "," <>
                var grid <> ".getRowIndex(" <> value (tableIconIndex click) <> ")," <>
                value (iconUrl newIcon) <>");" <>
              jsSendChange tableId grid <>
              "}"
    elmerDaemon tableBox $ join $ liftIO $ atomically $
        (do
          -- Process input from the rest of the application.
          newValue <- readTChan inputChan
          oldValue <- readTVar current
          writeTVar current newValue
          return $ when (newValue /= oldValue) $ elmerSend $ loadNewValues newValue
        ) `orElse` (do
          -- Process user edits to the table data.
          v <- readTChan editChan
          case parseEither (withArray "Table values" $ mapM parseRow . V.toList) v of
            Right updates -> do
              oldValue <- readTVar current
              let newValue = zipWith ($) updates $ oldValue ++ defList
              writeTVar current newValue
              writeTChan events ((), newValue)
              return $ elmerSend $ updateDecorations spec grid newValue
            Left _ ->
              return $ return ()
        ) `orElse` (do
          -- Process row operations.
          op1 <- readTChan opChan
          oldValue <- readTVar current
          let newValue = applyTableRowOp (snd <$> listToMaybe addButtons) op1 oldValue
          writeTVar current newValue
          writeTChan events ((), newValue)
          return $ elmerSend $ loadNewValues newValue
        )
    parent <- liftIO $ newTVarIO Nothing
    final <- liftIO $ newIORef $ elmerSend $ "delete " <> var grid
    return (Webit {
          webitId1 = webitId outerBox,
          webitHandle1 = webitHandle outerBox,
          webitIn = writeTChan inputChan,
          webitState = output,
          _webitCurrent = current,
          webitParent1 = parent,
          webitFinal = final
        },
        domCode1)
  where
    hideButtons :: [JavaScript]
    hideButtons =
      [x | TableShuffle `notElem` editFlags, x <- ["moveUp: true", "moveDown: true"]] <>
      [x | TableDelete `notElem` editFlags, x <- ["remove: true", "removeLast: true"]] <>
      [x | null addButtons, x <- ["append: true"]]
    addButtons = mapMaybe isAdd editFlags
    isAdd (TableAdd txt v) = Just (txt, v)
    isAdd _ = Nothing
    isIconEditor (TableColumn fld) = case fieldEditor fld of
      EditIcon {} -> True
      _ -> False


-- | Like 'mkTableWebit', but when the table is clicked an event is sent with the
-- row number. The channel that receives these clicks is returned as the third part of the result.
--
-- The webit may also update itself when row add, delete or shuffle buttons are clicked.
mkTableWebitWithClicks :: (Eq row) =>
  [TableEditing row]
  -> Table row
  -> [row]
  -> Elmer (Webit [row], RemoteMonad (), TChan Int)
mkTableWebitWithClicks editFlags spec initial = do
    tableBox <- makeWebitContainer1 "table" []
    current <- liftIO $ newTVarIO initial
    inputChan <- liftIO newTChanIO  -- Inputs from the application.
    opChan <- addWebitChannel tableBox  -- Insert, delete, shuffle.
    events <- liftIO newBroadcastTChanIO  -- Events to be sent on the output channel.
    eventsOut <- liftIO $ atomically $ dupTChan events
    let output = TState {
          tStateBehaviour = readTVar current,
          tStateEvent = readTChan eventsOut,
          tStateDup = do
            newEvents <- dupTChan events
            return $ TState (tStateBehaviour output) (readTChan newEvents) (tStateDup output)
        }
    elmer <- ask
    clickRows <- addWebitChannel tableBox
    let
      tableId = webitId tableBox
      eng = elmerEngine elmer
    grid <- liftIO $ send eng $ constructor "null"  -- Null constructor to avoid mdo.
    let
      (columnSpecs, getters, _) =
        unzip3 $ zipWith (\n (TableColumn s) -> fieldToColumn tableId True grid n s) [0..] spec
      rowToJson row = object $ zip (map columnId [0..]) (map ($ row) getters)
      -- Load new values into the grid, being careful to set and unset the callback disable flag.
      loadNewValues vs = "{" <>
          var grid <> ".dsmRowDisable=true;" <>
          (if null vs
            then   -- Bodge because AppendGrid won't accept empty data.
              var grid <> ".load([{col0:null}]);" <>
              var grid <> ".removeRow(0);"
            else
              call (var grid <> ".load") [value $ map rowToJson vs] <> ";") <>
          updateDecorations spec grid vs <> ";" <>
          var grid <> ".dsmRowDisable=false;" <>
        "}"
      domCode = do
        command $ "{\
          \let sendOp=function(o1,n1){\
              \if (" <> var grid <> " && !(" <> var grid  <> ".dsmRowDisable)){\
                \" <> webitEventData tableId "{op:o1,n:n1}" <>
            "}};" <>
          var grid <> "=new AppendGrid({\
            \element:" <> var (webitHandle tableBox) <> ",\
            \columns:[" <> mconcat (intersperse "," columnSpecs) <> "],\
            \hideButtons:{" <> mconcat (intersperse "," hideButtons) <> "},\
            \afterRowInserted:function(c,p,ns){sendOp('insert',p===null?0:p+1);},\
            \afterRowSwapped:function(c,n1,n2){sendOp('swap',Math.min(n1,n2));},\
            \beforeRowRemoved:function(c,n)\
              \{sendOp('delete',n===null?$(this).getRowCount():n);},\
            \afterRowAppended:function(c,p,ns){sendOp('insert',p===null?0:p+1);},\
            \hideRowNumColumn:true});\
          \}"
        command $ loadNewValues initial
        command $ updateDecorations spec grid initial
    elmerDaemon tableBox $ join $ liftIO $ atomically $
      (do
        -- Process input from the rest of the application.
        newValue <- readTChan inputChan
        oldValue <- readTVar current
        writeTVar current newValue
        return $ when (newValue /= oldValue) $ elmerSend $ loadNewValues newValue
      ) `orElse` (do
        -- Process row operations.
        op1 <- readTChan opChan
        oldValue <- readTVar current
        let newValue = applyTableRowOp (snd <$> listToMaybe addButtons) op1 oldValue
        writeTVar current newValue
        writeTChan events ((), newValue)
        return $ when (newValue /= oldValue) $ elmerSend $ loadNewValues newValue
      )
    parent <- liftIO $ newTVarIO Nothing
    final <- liftIO $ newIORef $ elmerSend $ "delete " <> var grid
    return (Webit {
          webitHandle1 = webitHandle tableBox,
          webitIn = writeTChan inputChan,
          webitState = output,
          _webitCurrent = current,
          webitId1 = webitId tableBox,
          webitParent1 = parent,
          webitFinal = final
        },
        domCode,
        clickRows)
  where
    hideButtons :: [JavaScript]
    hideButtons =
      [x | TableShuffle `notElem` editFlags, x <- ["moveUp: true", "moveDown: true"]] <>
      [x | TableDelete `notElem` editFlags, x <- ["remove: true", "removeLast: true"]] <>
      [x | null addButtons, x <- ["append: true"]]
    addButtons = mapMaybe isAdd editFlags
    isAdd (TableAdd txt v) = Just (txt, v)
    isAdd _ = Nothing


-- | JavaScript command to update any icons and colours in the table, based on the new row values.
updateDecorations :: Table row -> RemoteValue (WebitTable row) -> [row] -> JavaScript
updateDecorations spec grid newValue = mconcat $ concat $
  zipWith (\v row -> concat $
      zipWith (\(TableColumn tf) col -> catMaybes [
        do  -- Maybe monad
          iconF <- fieldIcon tf
          newIcon <- v ^? fieldValue tf . to iconF
          return $ call
              (var grid <> ".getCellCtrl")
              [value (columnId col), value row] <>
            ".previousSibling.src=" <> value (iconUrl newIcon) <> ";",
        do  -- Maybe monad
          colF <- fieldColour tf
          newColour <- v ^? fieldValue tf . to colF . re colourPrism
          return $ call
              (var grid <> ".getCellCtrl")
              [value (columnId col), value row] <>
            ".parentNode.style.backgroundColor=" <> value newColour <> ";"
        ]) spec [0..]
    ) newValue [0 :: Int ..]



deleteTableWebit :: WebitTable a -> Elmer ()
deleteTableWebit (WebitTable r g) = do
  deleteWebit r
  eng <- asks elmerEngine
  liftIO $ send eng $ delete g


-- | Generate the column identifier from a serial number.
columnId :: Int -> Text
columnId colNum = T.pack $ "col" <> show colNum


-- Treat JSON @Null@ as @Nothing@. This has been submitted to the lens-aeson project, so this
-- code can be deleted if it clashes with a future version.
nullIsNothing :: APrism' Value a -> Prism' Value (Maybe a)
nullIsNothing p = withPrism p $ \bk fw ->
  let
    bk1 Nothing = Null
    bk1 (Just x) = bk x
    fw1 Null = Right Nothing
    fw1 x = Just <$> fw x
  in prism bk1 fw1


-- | Icon in a table.
tableIcon :: Bool -> WebitId -> RemoteValue (WebitTable row) -> Int -> H.Html
tableIcon clickFlag wId grid colNum =
  let c = columnId colNum
  in H.img
    ! HA.alt (H.textValue c)
    ! HA.src (H.textValue $ iconUrl "no-icon")  -- Value set later.
    ! HA.class_ "icon-inline"
    ! HA.onclick (jsValue $ webitEventData wId $ if clickFlag
        then "{" <> webitEventData wId (var grid <> ".getRowIndex(this.dsmUnique)") <> "}"
        else "{src:this.src,idx:this.dsmUnique,col:" <> value c <> "}"
      )
