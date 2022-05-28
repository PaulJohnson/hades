{-
Copyright © Paul Johnson 2021. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

{- | = Arrows for Web GUIs

Copyright © Paul Johnson 2021. See LICENSE file for details.

This is a web implementation of the arrow-based GUI language defined in
"Reactive.Banana.ArrowDialog".

The Gadgets are rendered using web sockets. The following assumptions are made about the
environment that the GUI is running in:

* Icons are in a static folder @/icon@. E.g. an icon named @tick@ will be in @/icon/tick@.
The web server may provide either PNG or SVG files, but with no suffix.  The icons must include:
  [object-select-symbolic]  A green tick.
  [list-remove-symbolic]  A red cross.
  [blank-icon] An empty icon.

* The contents of 'webitJsLibrary' are included.

The following CSS classes are used:

[active]  A tab button which is currently selected.

[dialog]  A container used as a modal dialog. These are created and destroyed by the server on
open and close, so there is no need for visibility settings in the CSS.

[disabled]  Should be grayed out. The CSS is not expected to handle the unresponsive semantics.

[grid]  A table of gadgets with row and column headers.

[horizontal]  The contents should be displayed in a horizontal row.

[icon-inline]  An icon displayed in line with another gadget. Typically 16x16 pixels.

[invalid]  The element contains invalid data.

[memo-small]  A memo-box sized to accept a short paragraph.

[memo-medium] A memo-box sized to accept a couple of short paragraphs.

[memo-large]  A memo-box sized to accept several paragraphs.

[no-box]  A container used to organise content but which has no visible representation.

[popup]  An element which contains a popup-widget.

[popup-widget] An element which is positioned over another element and appears when given the
additional class @show@.

[show]  A visible popup-widget.

[tab-body]  The container for tab currently displayed.

[tab-form]  A tabbed element with a tab-header and one or more tab-body elements.

[tab-header]  The container for the list of tabs.

[vertical]  The contents should be displayed in a vertical column.

[window-title]  A paragraph used as the header of a dialog pop-up.

[vl]  A @div@ used as a vertical line.

-}

module Network.Webits.ArrowDialog (
  arrowDialogPrelude,
  FancyTreeIcon (..),
  FancyTree (..),
  ftDefault,
  mkWebitPopupSelect,
  createDialog,
  GadgetOut (..),
  renderGadget,
  showOpr
) where

import Codec.ImageType
import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.TState
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.Colour.CIE as C
import Data.Either
import qualified Data.ForestModel as FM
import Data.IORef
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Data.Tree
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Conc
import GHC.Stack (HasCallStack)
import Paths_webits
import Reactive.Banana.Common hiding (IconName)
import Reactive.Banana.ArrowDialog as G
import Reactive.Banana.GadgetPrimitives as G
import Network.JavaScript as JS
import Network.Webits
import Network.Webits.Icons
import Network.Webits.Menu
import Network.Webits.Table
import System.IO.Unsafe
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Text.Blaze.Html.Renderer.Text
import Text.Read (readMaybe)


data FancyTreeIcon =
  FtIconNone   -- ^ No icon.
  | FtIconDefault -- ^ Use default FancyTree theme icon.
  | FtIcon Text  -- ^ Either an image URL or a custom JQuery-UI icon tag.

instance ToJSON FancyTreeIcon where
  toJSON FtIconNone = toJSON False
  toJSON FtIconDefault = toJSON True
  toJSON (FtIcon txt) = toJSON txt

-- | Fancy Tree display data.
--
-- The ToJSON instance leaves JSON fields undefined where blank text or @Nothing@ makes that
-- appropriate according to the Fancy Tree API.
data FancyTree = FancyTree {
    ftTitle :: Text,  -- ^ Display name.
    ftKey :: Text,  -- ^ Key value.
    ftToolTip :: Maybe Text,  -- ^ Hover tooltip.
    ftIcon :: Maybe FancyTreeIcon,
      -- ^ @Nothing@ = global tree icon. Use @Just FtIconNone@ for no icon.
    ftCheckBox :: Maybe Bool,  -- ^ Display a checkbox. @Nothing@ = use global.
    ftExpanded :: Bool,    -- ^ Initial expanded state.
    ftExtraClasses :: Text,   -- ^ CSS classes for the node.
    ftLazy :: Bool,   -- Are the children of this node to be loaded on demand?
    ftSelected :: Bool,  -- ^ Initial selection state.
    ftType :: Text,   -- ^ Node type data for icons, tooltips etc.
    ftUnselectable :: Bool,  -- ^ Node cannot be selected.
    ftChildren :: Maybe [FancyTree]  -- ^ @Nothing@ for not yet loaded.
  }

instance ToJSON FancyTree where
  toJSON v = object $ ["title" .= ftTitle v, "key" .= ftKey v] <> catMaybes [
      ("tooltip" .=) <$> ftToolTip v,
      ("icon" .=) <$> ftIcon v,
      ("checkbox" .=) <$> ftCheckBox v,
      if ftExpanded v then Just $ "expanded" .= True else Nothing,
      if ftExtraClasses v == "" then Nothing else Just $ "extraClasses" .= ftExtraClasses v,
      if ftLazy v then Just $ "lazy" .= True else Nothing,
      if ftSelected v then Just $ "selected" .= True else Nothing,
      if ftType v == "" then Nothing else Just $ "type" .= ftType v,
      if ftUnselectable v then Just $ "unselectable" .= True else Nothing,
      ("children" .=) <$> ftChildren v
    ]


data TreeMoveMode = MoveBefore | MoveAfter | MoveOver

instance FromJSON TreeMoveMode where
  parseJSON (String "before") = return MoveBefore
  parseJSON (String "after") = return MoveAfter
  parseJSON (String "over") = return MoveOver
  parseJSON (String str) = fail $ "Unrecognised move mode: " <> show str
  parseJSON x = fail $ "Expected move mode: " <> show x


-- | Must be included as a part of every web page using these gadgets.
arrowDialogPrelude :: H.Html
arrowDialogPrelude = H.script ! HA.type_ "text/javascript" $ H.text $ unsafePerformIO $ do
  fn <- getDataFileName "arrowDialog.js"
  T.readFile fn


-- | Default values for FancyTree, with only key and title supplied. Use field syntax on this rather
-- than building @FancyTree@ values from scratch to avoid undefined fields in future versions.
--
-- Fields are set blank, @False@ or @Nothing@.
ftDefault ::
  Text    -- ^ Key.
  -> Text   -- ^ Title
  -> FancyTree
ftDefault k txt = FancyTree {
    ftTitle = txt,
    ftKey = k,
    ftToolTip = Nothing,
    ftIcon = Nothing,
    ftCheckBox = Nothing,
    ftExpanded = False,
    ftExtraClasses = "",
    ftLazy = False,
    ftSelected = False,
    ftType = "",
    ftUnselectable = False,
    ftChildren = Nothing
  }

-- | Raw selection events received from FancyTree.
data FTSelection = FTSelection {
    ftsKey :: Int,
    ftsSelected :: Bool
  } deriving Show

instance FromJSON FTSelection where
  parseJSON = withObject "Fancytree Selection event" $ \v ->
    FTSelection <$> (read <$> (v .: "k")) <*> v .: "s"


-- | Associate a dialog selector with events on a channel. Each event causes a new dialog
-- to pop up. The @k@ values are used to correlate the input events with the outputs. The output
-- value is @Just@ for clicks on @OK@ and @Apply@, and @Nothing@ for clicks on @Cancel@.
mkWebitPopupSelect :: HasCallStack =>
  TState Bool e  -- ^ Environment value.
  -> DialogSelector' e w a
  -> STM (k, a)  -- ^ An event channel. A new popup is created every time this succeeds.
  -> Elmer (TChan (k, Maybe a))
mkWebitPopupSelect env selector trigger = do
  result <- liftIO newTChanIO
  outer <- asks elmerTop  -- Only lasts as long as the current parent.
  elmerDaemon outer $ do
    ((k, v), e) <- liftIO $ atomically $ (,) <$> trigger <*> tStateBehaviour env
    forM_ (selector e v) $ \dialog -> do
      (dialogBox, dialogOut) <- createDialog env (k, v, dialog)
      elmerDaemon dialogBox $ forever $ liftIO $ atomically $
        readTChan dialogOut >>= writeTChan result
  return result


-- | Create a dialog box and a channel of outputs from clicks to the \"Close\",
-- \"Apply\" and \"Ok\" buttons.
--
-- The @WebitContainer@ in the result is created as a child of the @elmerTop@ value. It is
-- automatically deleted when @OK@ or @Cancel@ are clicked.
createDialog :: HasCallStack =>
  TState Bool e  -- ^ The gadget environment.
  -> (k, a, Dialog' e w a)
    -- ^ The dialog to open. @k@ is a key to identify the value being edited. @a@
    -- value is the initial value to display.
  -> Elmer (WebitContainer, TChan (k, Maybe a))
createDialog env (key, initial, d) = do
    form1 <- makeWebitContainer1 "form" [("title", dialogTitle d)]
    buttonChan <- addWebitChannel form1
    (loopStore, loopWriter) <- newTStateIO initial
    gadget <- renderGadget env (GadgetData True <$> loopStore) $ dialogGadget d
    mapM_ (showWebit form1) $ gadgetView gadget
    outChan <- liftIO newBroadcastTChanIO
    initialValid <- liftIO $ atomically $ view gdOk <$> tStateBehaviour (gadgetOutput gadget)
    updateButtons (webitId form1) initialValid
    eng <- asks elmerEngine
    currentValid <- liftIO $ newIORef initialValid  -- Avoid sending nugatory updates to buttons.
    elmerDaemon form1 $ do
      newValid <- liftIO $ atomically $ do
        (userFlag, st) <- tStateEvent $ gadgetOutput gadget
        when (st ^. gdOk && userFlag) $ loopWriter False $ st ^. gdValue
        return $ st ^. gdOk
      oldValid <- liftIO $ readIORef currentValid
      when (newValid /= oldValid) $ do
        liftIO $ writeIORef currentValid newValid
        updateButtons (webitId form1) newValid
    elmerDaemon form1 $ liftIO $ atomically $ do
      btn <- readTChan buttonChan
      if btn
        then do
          v <- tStateBehaviour (gadgetOutput gadget)
          writeTChan outChan (key, Just $ v ^. gdValue)
        else
          writeTChan outChan (key, Nothing)
    top <- asks elmerTop
    showWebit top form1
    liftIO $ JS.send eng $ gadgetDomCode gadget
    liftIO $ JS.send eng $ initJS $ webitId form1
    out <- liftIO $ atomically $ dupTChan outChan
    return (form1, out)
  where
    okText:: Text
    okText = "OK"
    applyText :: Text
    applyText = "Apply"
    -- Close function sends False *unless* the close was triggered by the OK button.
    initJS wId = command $
      "$(" <> webitSelector wId <> ").dialog({\
          \autoOpen:true,\
          \modal:true,\
          \buttons:[" <> buttonsJS wId <> "],\
          \height:'auto',\
          \width:'auto',\
          \close:function(){\
              \if ($(" <> webitSelector wId <> ").data('okPressed')) { \
                  \$(" <> webitSelector wId <> ").data('okPressed',false);\
                \} else {\
                  \" <> webitEvent wId False <> ";\
                \}\
              \$(" <> webitSelector wId <> ").dialog(\"close\");\
            \}\
        \});\
      \"
    updateButtons wId True = elmerSend $ "{\
        \$(" <> webitSelector1 wId "_ok" <> ")\
          \.prop('disabled',false).removeClass( 'ui-state-disabled' );\
        \$(" <> webitSelector1 wId "_apply" <> ")\
          \.prop('disabled',false).removeClass( 'ui-state-disabled' );\
      \}"
    updateButtons wId False = elmerSend $ "{\
        \$(" <> webitSelector1 wId "_ok" <> ")\
          \.prop('disabled',true).addClass( 'ui-state-disabled' );\
        \$(" <> webitSelector1 wId "_apply" <> ")\
          \.prop('disabled',true).addClass( 'ui-state-disabled' );\
      \}"
    buttonsJS wId = mconcat $ intersperse "," $ case dialogButtons d of
        OkButton -> [okButton, closeButton "Cancel"]
        OkApplyButton -> [okButton, applyButton, closeButton "Cancel"]
        CloseButton txt -> [closeButton txt]
      where
        okButton = "{\
            \id:" <> value wId <> "+'_ok',\
            \text:" <> value okText <>",\
            \click:function(ev,ui){\
                \console.log('OK click event ID =' + ev.target.id);\
                \" <> webitEvent wId True <> ";\
                \$(" <> webitSelector wId <> ").data('okPressed',true);\
                \$(" <> webitSelector wId <> ").dialog('close');\
              \}\
          \}"
        applyButton =
          "{\
              \id:" <> value wId <> "+'_apply',\
              \text:" <> value applyText <>",\
              \click:function(){" <> webitEvent wId True <> ";}\
            \}"
        closeButton :: Text -> JavaScript
        closeButton txt =
          "{\
              \id:" <> value wId <> "+'_close',\
              \text:" <> value txt <>",\
              \click:function(){$(" <> webitSelector wId <> ").dialog(\"close\");}\
            \}"


-- | Data representing a Gadget instantiated as a Webit. The output Boolean flags that this event
-- was triggered by the user, and therefore needs to be fed back into the gadget with the flag
-- set to false.
--
-- Some gadgets rely on third-party code that can only be initialised after the container is
-- inserted in the DOM (typically they rely in @document.getElementById@). The @gadgetDomCode@
-- field carries the necessary JavaScript to the outermost gadget where it will be executed
-- at the right time.
data GadgetOut w o = GadgetOut {
    gadgetView :: [WebitItem],   -- ^ Webits at the top level.
    gadgetDomCode :: RemoteMonad (),
      -- ^ Code to be executed after the view is inserted in the DOM.
    gadgetEvents :: STM w,       -- ^ Data sent by the side channel. Retries until event occurs.
    gadgetOutput :: TState Bool (GadgetData o)     -- ^ Gadget output.
  } deriving (Functor)

instance Applicative (GadgetOut w) where
  pure v = GadgetOut [] (return ()) retry (pure $ pure v)
  f <*> v = GadgetOut {
      gadgetView = gadgetView f ++ gadgetView v,
      gadgetDomCode = gadgetDomCode f >> gadgetDomCode v,
      gadgetEvents = gadgetEvents f `orElse` gadgetEvents v,
      gadgetOutput = (<*>) <$> gadgetOutput f <*> gadgetOutput v
    }


-- | Duplicate  the argument while filtering out invalid events. The initial behaviour may be
-- invalid.
cleanOutput :: TState a (GadgetData b) -> Elmer (TState a (GadgetData b))
cleanOutput st = liftIO $ do
  (r, st1, writer) <- atomically $ do
    st1 <- tStateDup st
    v1 <- tStateBehaviour st
    (r, writer) <- newTState v1
    return (r,  st1, writer)
  threadId <- forkIO $ forever $ atomically $ do
    v <- tStateEvent st1
    when (v ^. _2 . gdOk) $ uncurry writer v
  labelThread threadId "cleanOutput"
  return r


-- | Make the output validity conditional on an external condition. If the @Bool@ is false then
-- all the gadget values will be flagged as invalid.
qualifyOutput :: Bool -> GadgetOut w o -> GadgetOut w o
qualifyOutput ok o = o { gadgetOutput = (gdOk %~ (ok &&)) <$> gadgetOutput o}


-- | Merges two streams of GadgetData, typically the gadget input and user actions.
--
-- Gadgets which receive invalid inputs from upstream should present the data to the user without
-- flagging the invalidity. The output validity should be @AND@ the input validity and the
-- validity of the user data. The initial value of the output is the value of the first argument.
--
-- The input events are only propogated to the output if they change the value.
mergeGadgetData :: (MonadIO m, Eq a) =>
  TState Bool (GadgetData a) -> TState Bool (GadgetData a) -> m (TState Bool (GadgetData a))
mergeGadgetData ts1 ts2 = do
  ts1a <- tStateDupIO ts1
  ts2a <- tStateDupIO ts2
  initial <- liftIO $ atomically $ do
    i1 <- tStateBehaviour ts1a
    i2 <- tStateBehaviour ts2a
    return $ GadgetData (i1 ^. gdOk && i2 ^. gdOk) $ i1 ^. gdValue
  (out, writer) <- newTStateIO initial
  threadId <- liftIO $ forkIO $ forever $ atomically $ (do
      (userflag, v1) <- tStateEvent ts1a
      oldValue <- tStateBehaviour out
      when (v1 /= oldValue) $ do
        v2 <- tStateBehaviour ts2a
        writer userflag $ GadgetData (v1 ^. gdOk && v2 ^. gdOk) $ v1 ^. gdValue
    ) `orElse` (do
      (userflag, v2) <- tStateEvent ts2a
      oldValue <- tStateBehaviour out
      when (v2 /= oldValue) $ do
        v1 <- tStateBehaviour ts1a
        writer userflag $ GadgetData (v1 ^. gdOk && v2 ^. gdOk) $ v2 ^. gdValue
    )
  liftIO $ labelThread threadId "mergeGadgetData"
  return out



-- | Render a GUI Gadget as Webits.
--
-- See 'GadgetOut' for the @Bool@ flags on the environment and input.
--
-- The loop construct employs @mdo@, which may have odd effects on processes. If stuff hangs for
-- no apparent reason, suspect this first.
renderGadget :: HasCallStack =>
  TState Bool e   -- ^ The environment.
  -> TState Bool (GadgetData i)  -- ^ The input.
  -> Gadget e w i o
  -> Elmer (GadgetOut w o)

renderGadget _ i G.Null = {-# SCC renderGadget_Null #-} return $ GadgetOut {
    gadgetView = [],
    gadgetDomCode = return (),
    gadgetEvents = retry,
    gadgetOutput = i
  }

renderGadget _ i (Pure f) = {-# SCC renderGadget_Pure  #-} return $ GadgetOut {
    gadgetView = [],
    gadgetDomCode = return (),
    gadgetEvents = retry,
    gadgetOutput = fmap f <$> i
  }

renderGadget env i (Dot g1 g2) = {-# SCC renderGadget_Dot  #-} do
  r2 <- renderGadget env i g2
  r1 <- renderGadget env (gadgetOutput r2) g1
  return $ GadgetOut {
      gadgetView = gadgetView r2 ++ gadgetView r1,
      gadgetDomCode = gadgetDomCode r2 >> gadgetDomCode r1,
      gadgetEvents = gadgetEvents r2 `orElse` gadgetEvents r1,
      gadgetOutput = gadgetOutput r1
    }

renderGadget env i (Prod g1 g2) = {-# SCC renderGadget_Prod #-} do
  r1 <- renderGadget env (fmap fst <$> i) g1
  r2 <- renderGadget env (fmap snd <$> i) g2
  return $ (,) <$> r1 <*> r2

-- Loop :: Gadget (i, s) (o, s) -> Gadget i o
renderGadget env i (Loop g) = {-# SCC renderGadget_Loop  #-} mdo
  let
    loopInput = liftA2 (,) <$> i <*> (fmap snd <$> gadgetOutput result)
  result <- renderGadget env loopInput g
  return $ fst <$> result

renderGadget env i (Focusing lns g) = {-# SCC renderGadget_Focusing  #-} do
  let extract x = x ^. getting lns
  out <- renderGadget env (fmap extract <$> i) g
  return $ set lns <$> out

renderGadget env i (Prismatic d prsm g) = {-# SCC renderGadget_Prismatic  #-}
  withPrism prsm $ \inject extract -> do
    let extract1 = fromRight d . extract
    out <- renderGadget env (fmap extract1 <$> i) g
    return $ inject <$> out

renderGadget env i (PrismaticOver d prsm g) = {-# SCC renderGadget_PrismaticOver  #-} do
  let extract x = fromMaybe d $ x ^? getting prsm
  out <- renderGadget env (fmap extract <$> i) g
  return $ (\f v -> view (re prsm) $ f $ extract v) <$> out

renderGadget env i (Traversing d trv g) = {-# SCC renderGadget_Traversing  #-} do
  let extract x = fromMaybe d $ x ^? getting trv
  out <- renderGadget env (fmap extract <$> i) g
  return $ set trv <$> out

renderGadget env i (TraversingOver d trv g) = {-# SCC renderGadget_TraversingOver  #-} do
  let extract x = fromMaybe d $ x ^? getting trv
  out <- renderGadget env (fmap extract <$> i) g
  return $ over trv <$> out

-- Accum :: Gadget e w a (a -> a) -> Gadget' e w a
renderGadget env i (Accum g) = {-# SCC renderGadget_Accum  #-} do
    inner <- renderGadget env i g
    i1 <- tStateDupIO i
    (output, writer) <- liftIO $ atomically $ tStateBehaviour i >>= newTState
    threadId <- liftIO $ forkIO $ forever $ atomically $
      -- No need to duplicate @gadgetOutput inner@ because this is the only consumer.
      (do
        (userflag, f) <- tStateEvent (gadgetOutput inner)
        new <- applyUpdate f <$> tStateBehaviour output
        writer userflag new
      ) `orElse` (do
        (userflag, v) <- tStateEvent i1
        writer userflag v
      )
    liftIO $ labelThread threadId "renderGadget_Accum"
    return inner {gadgetOutput = output}
  where
    applyUpdate (GadgetData ok newF) (GadgetData _ old) = GadgetData ok $ newF old

renderGadget _ i (Initially v) = {-# SCC renderGadget_Initially  #-} do
  initial <- liftIO $ newTMVarIO (GadgetData True v)
  return GadgetOut {
      gadgetView = [],
      gadgetDomCode = return (),
      gadgetEvents = retry,
      gadgetOutput = i {tStateBehaviour = readTMVar initial `orElse` tStateBehaviour i}
    }

renderGadget env i (GetInitial f) = {-# SCC renderGadget_GetInitial  #-} do
  g <- liftIO $ atomically $ f . view gdValue <$> tStateBehaviour i
  renderGadget env i g

renderGadget env _ GetEnv = {-# SCC renderGadget_GetEnv  #-} return GadgetOut {
    gadgetView = [],
    gadgetDomCode = return (),
    gadgetEvents = retry,
    gadgetOutput = GadgetData True <$> env
  }

renderGadget env i (GetInitialEnv f) = {-# SCC renderGadget_GetInitialEnv  #-} do
  g <- liftIO $ atomically $ f <$> tStateBehaviour env
  renderGadget env i g

renderGadget env i (WithEnv engG g) = {-# SCC renderGadget_WithEnv  #-} do
  envOut1 <- renderGadget env i engG
  envState <- cleanOutput $ gadgetOutput envOut1
  renderGadget (view gdValue <$> envState) i g

renderGadget _ i (Send f) = {-# SCC renderGadget_Send  #-} do
  chan <- liftIO newTQueueIO
  i2 <- liftIO $ atomically $ tStateDup i
  threadId <- liftIO $ forkIO $ forever $ atomically $ do
    v <- tStateEvent i2
    when (v ^. _2 . gdOk && v ^. _1) $ forM_ (v ^. _2 . gdValue . to f) $ writeTQueue chan
  liftIO $ labelThread threadId "renderGadget_Send"
  return $ GadgetOut {
    gadgetView = [],
    gadgetDomCode = return (),
    gadgetEvents = readTQueue chan,
    gadgetOutput = i
  }

renderGadget env i (SendMap f g) = {-# SCC renderGadget_SendMap  #-} do
  inner <- renderGadget env i g
  chan <- liftIO newTQueueIO
  threadId <- liftIO $ forkIO $ forever $ atomically $
    gadgetEvents inner >>= mapM_ (writeTQueue chan) . f
  liftIO $ labelThread threadId "renderGadget_SendMap"
  return inner {gadgetEvents = readTQueue chan}

renderGadget env i (Exec gadgetF) = {-# SCC renderGadget_Exec  #-} do
    elmer <- ask
    i1 <- tStateDupIO i
    parent <- makeWebitContainer "no-box"
    initial <- liftIO $ atomically $ tStateBehaviour i1
    initialContent <- doExec parent $ gadgetF . fst <$> initial
    liftIO $ do
      initialOutput <- atomically $ tStateBehaviour $ gadgetOutput initialContent
      currentArg <- newTVarIO $ fst <$> initial
      currentContent <- newTVarIO initialContent
      (output, outputWriter) <- newTStateIO initialOutput
      -- Update the gadget whenever the first part of the input changes.
      threadId1 <- forkIO $ forever $ join $ do
        atomically $ do
          (userFlag, newInput) <- tStateEvent i1
          let newArg = fmap fst newInput
          oldArg <- readTVar currentArg
          if newArg /= oldArg
            then do
              writeTVar currentArg newArg
              return $ do
                newOut <- runElmer elmer $ doExec parent $ gadgetF <$> newArg
                JS.send (elmerEngine elmer) $ gadgetDomCode newOut
                atomically $ do
                  writeTVar currentContent newOut
                  tStateBehaviour (gadgetOutput newOut) >>= outputWriter userFlag
            else
              return $ return ()
      -- Write output from the current content to the unified output stream.
      threadId2 <- forkIO $ forever $ atomically $ do
        c <- readTVar currentContent
        v <- tStateEvent $ gadgetOutput c
        uncurry outputWriter v
      liftIO $ labelThread threadId1 "renderGadget_Exec1"
      liftIO $ labelThread threadId2 "renderGadget_Exec2"
      return GadgetOut {
          gadgetView = [WebitItem parent],
          gadgetDomCode = gadgetDomCode initialContent,
          gadgetEvents = readTVar currentContent >>= gadgetEvents,
          gadgetOutput = output
        }
  where
    doExec parent v = do
      o <- renderGadget env (fmap snd <$> i) $ v ^. gdValue
      childs <- liftIO $ readTVarIO $ webitContainerChildren parent
      mapM_ deleteWebit childs
      mapM_ (showWebit parent) $ gadgetView o
      return $ qualifyOutput (v ^. gdOk) o

renderGadget env i (Cond opts) = {-# SCC renderGadget_Cond  #-} do
    -- outputs :: [(predicate, (gadget, index))]
    (outputs :: [(i -> Bool, (GadgetOut w o, Int))]) <-
      forM (zip [1..] opts) $ \(n, PrismaticGadget d p g) ->
        withPrism p $ \setter getter -> do
          let
            getter1 (GadgetData ok v) = case getter v of
              Left _ -> Nothing
              Right v2 -> Just $ GadgetData ok v2
          i1 <- filterTStateMaybe (GadgetData True d) $ getter1 <$> i
          o <- renderGadget env i1 $ g >>> arr setter
          return (isRight . getter, (o, n))
    parent <- makeWebitContainer "no-box"
    let
      defaultOutput = case outputs of
        (_, g) : _ -> g
        [] -> error "Gadget cond with empty option list."
      chooseOutput :: GadgetData i -> (GadgetOut w o, Int)
      chooseOutput v = maybe defaultOutput snd $ find (($ _gdValue v) . fst) outputs
    -- Put all the gadgets into the DOM with the hidden flag set.
    forM_ outputs $ \(_, (g, _)) -> do
      hideGadget g
      mapM_ (showWebit parent) $ gadgetView g
    (startOutput, i1) <- liftIO $ atomically $ (,) <$>
      (chooseOutput <$> tStateBehaviour i) <*>
      tStateDup i
    displayGadget $ fst startOutput
    -- Collate the output as it changes.
    (o, writer) <- liftIO $ atomically $ do
      initOut <- tStateBehaviour $ gadgetOutput $ fst startOutput
      newTState initOut
    currentChoice <- liftIO $ newTVarIO startOutput
    -- Update the displayed Webit when the input changes.
    elmerDaemon parent $ do
      (prev, nxt) <- liftIO $ atomically $ do
        (userflag, nxt) <- second chooseOutput <$> tStateEvent i1
        cur <- readTVar currentChoice
        when (snd nxt /= snd cur) $ do
          writeTVar currentChoice nxt
          tStateBehaviour (gadgetOutput $ fst nxt) >>= writer userflag
        return (fst cur, fst nxt)
      hideGadget prev
      displayGadget nxt
    elmerDaemon parent $ liftIO $ atomically $ do
      cur <- readTVar currentChoice
      v <- tStateEvent $ gadgetOutput $ fst cur
      uncurry writer v
    return $ GadgetOut {
        gadgetView = [WebitItem parent],
        gadgetDomCode = mapM_ (gadgetDomCode . fst . snd) outputs,
        gadgetEvents = readTVar currentChoice >>= gadgetEvents . fst,
        gadgetOutput = o
      }
  where
    hideGadget g =
      forM_ (gadgetView g) $ \w -> elmerSend $ var (webitHandle w) <> ".hidden=true"
    displayGadget g =
      forM_ (gadgetView g) $ \w -> elmerSend $ var (webitHandle w) <> ".hidden=false"

renderGadget _ _ (UnionTab []) = error "renderGadget UnionTab: empty list."

renderGadget env i (UnionTab tabs) = {-# SCC renderGadget_UnionTab  #-} do
    elmer <- ask
    i1 <- liftIO $ atomically $ tStateDup i
    tabHeader <- makeWebitContainer "tab-header"
    -- tabWebits contains list of ((button, body output), selection predicate)
    tabWebits <- forM tabs $ \(lbl, PrismaticGadget d p g) ->
      withPrism p $ \setter getter -> do
        btn <- makeWebit (btnHtml lbl) (setButton tabHeader) ()
        let
          getter1 (GadgetData ok v) = case getter v of
            Left _ -> Nothing
            Right v2 -> Just $ GadgetData ok v2
        i2 <- filterTStateMaybe (GadgetData True d) $ getter1 <$> i
        page <- renderGadget env i2 $ g >>> arr setter
        return ((btn, page), isJust . getter1)
    -- Construct the Webit
    mapM_ (showWebit tabHeader . fst . fst) tabWebits
    pageParent <- makeWebitContainer "tab-body"
    forM_ tabWebits $ \((btn, page), _) -> do
      hideGadget page
      showWebit tabHeader btn
      mapM_ (showWebit pageParent) $ gadgetView page
    outer <- makeWebitContainer "no-box"
    showWebit outer tabHeader
    showWebit outer pageParent
    initialInput <- liftIO $ atomically $ tStateBehaviour i
    let
      initialSelection = fst $ fromMaybe (head tabWebits) $
          find (($ initialInput) . snd) tabWebits
    (output, writer) <- liftIO $ atomically $ do
      webitIn (fst initialSelection) ()
      initialOut <- tStateBehaviour $ gadgetOutput $  snd initialSelection
      newTState initialOut
    displayGadget $ snd initialSelection
    currentSelection <- liftIO $ newTVarIO initialSelection
    -- Update display when a new input changes the selection or a tab is clicked.
    elmerDaemon outer $ liftIO $ do
      (old, selected) <- atomically $ do -- foldr1 is safe because tabs is not empty.
        selected <-
          foldr1 orElse (map (receiveClick . fst) tabWebits) `orElse` (do
            (userflag, v) <- tStateEvent i1
            return $ (userflag, ) . fst <$> find (($ v) . snd) tabWebits)
        old <- readTVar currentSelection
        when (Just (webitId $ fst old) /= (webitId . fst . snd <$> selected)) $ do
          forM_ selected $ \s@(userflag, (_, page)) -> do
            writeTVar currentSelection $ snd s
            newOut <- tStateBehaviour $ gadgetOutput page
            writer userflag newOut
          mapM_ (writeTVar currentSelection . snd) selected
        return (old, selected)
      forM_ selected $ \(_, (btn, page)) -> runElmer elmer $ do
          hideGadget $ snd old
          displayGadget page
          liftIO $ atomically $ webitIn btn ()
    -- Update the output when the output of the currently selected page changes.
    elmerDaemon outer $ liftIO $ atomically $ do
      cs <- readTVar currentSelection
      v <- tStateEvent $ gadgetOutput $ snd cs
      uncurry writer v
    return $ GadgetOut {
        gadgetView = [WebitItem outer],
        gadgetDomCode = mapM_ (gadgetDomCode . snd . fst) tabWebits,
        gadgetEvents = readTVar currentSelection >>= gadgetEvents . snd,
        gadgetOutput = output
      }
  where
    receiveClick tab = do
      void $ tStateEvent $ webitState $ fst tab
      return $ Just (True, tab)  -- True for user action.
    btnHtml lbl wId = H.button
      ! HA.type_ "button"
      ! HA.onclick (jsValue $ webitEvent wId ())
      $ H.toHtml lbl
    -- Javascript to set the button to "active" and remove active status from the other buttons.
    setButton container () button _ =
      "for(\
          \var c=" <> var (webitHandle container) <> ".firstChild;\
          \c!==null;\
          \c=c.nextSibling) {\
        \c.className=c.className.replace(\" active\", \"\");\
      \}" <>
      var button <> ".className += \" active\";"
    hideGadget g =
      forM_ (gadgetView g) $ \w -> elmerSend $ var (webitHandle w) <> ".hidden=true"
    displayGadget g =
      forM_ (gadgetView g) $ \w -> elmerSend $ var (webitHandle w) <> ".hidden=false"

renderGadget env i (Enabled g) = {-# SCC renderGadget_Enabled  #-} do
  elmer <- ask
  i1 <- tStateDupIO i
  result <- renderGadget env (fmap fst <$> i) g
  initEnabled <- liftIO $ atomically $ snd . view gdValue <$> tStateBehaviour i
  parent <- makeWebitContainer1 "fieldset" [("disabled", "disabled") | not initEnabled]
  mapM_ (showWebit parent) $ gadgetView result
  -- Add or remove the "disabled" attribute as the input changes.
  void $ liftIO $ forkIO $ forever $ atomically $ do
    enabledFlag <- snd . view (_2 . gdValue) <$> tStateEvent i1
    elmerSendJS elmer $ var (webitHandle parent) <> "." <>
      if enabledFlag
        then call "removeAttribute" ["disabled"]
        else call "setAttribute" ["disabled", "disabled"]
  return result {gadgetView = [WebitItem parent]}

renderGadget env i (Optional _ (TextBox p)) =
  renderGadget env i $ TextBox $ prismToMaybe. p

renderGadget env i (Optional _ DisplayText) =
  renderGadget env i $
      arr (\t -> if T.null t then Nothing else Just t) <<< DisplayText <<< arr (fromMaybe "")

renderGadget env i (Optional _ (MemoBox expand size)) =
    fmap outF <$> renderGadget env (fmap (fromMaybe "") <$> i) (MemoBox expand size)
  where
    outF :: Text -> Maybe Text
    outF "" = Nothing
    outF txt = Just txt

renderGadget env i (Optional _ (Combo itemF)) =
    renderGadget env i $ Combo $ (nothingItem :) . map (fmap Just) . itemF
  where
    nothingItem = ComboItem "" Nothing Nothing Nothing

renderGadget env i (Optional _ TickBox) =
  renderGadget env i $ Combo $ const [
      ComboItem "" Nothing Nothing Nothing,
      ComboItem "Yes" (Just "object-select-symbolic") Nothing (Just True),  -- Green tick
      ComboItem "No" (Just "list-remove-symbolic") Nothing (Just False)  -- Red cross
    ]

renderGadget env i (Optional _ (FixedText textF Nothing)) =
    renderGadget env i $ FixedText (maybe "" . textF) Nothing

renderGadget env i (Optional d (FixedText textF (Just selector1))) =
    renderGadget env i $ FixedText (maybe "" . textF) $ Just selector2
  where
    selector2 = promoteDialogSelector d _Just selector1

renderGadget env i (Optional _ (FixedMemo expand size textF Nothing)) =
    renderGadget env i $ FixedMemo expand size (maybe "" . textF) Nothing

renderGadget env i (Optional d (FixedMemo expand size textF (Just selector1))) =
    renderGadget env i $ FixedMemo expand size (maybe "" . textF) $ Just selector2
  where
    selector2 = promoteDialogSelector d _Just selector1

renderGadget _ i (Optional _ DateBox{}) = {-# SCC renderGadget_Opt_DateBox #-} do
    -- This ignores the date format in the argument because HTML delegates that to the browser.
    initVal <- liftIO $ atomically $ tStateBehaviour i
    let prsm = prismToMaybe $ datePrism dateFmt
    i1 <- tStateDupIO i
    entry <- makeWebit dateHtml updateValue (initVal ^. gdValue . re (clonePrism prsm))
    (output, writer) <- newTStateIO initVal
    elmerDaemon entry $ liftIO $ atomically $
      (do
        (userflag, newVal) <- tStateEvent i1
        oldVal <- tStateBehaviour output
        when (oldVal ^. gdValue /= newVal ^. gdValue) $ do
          webitIn entry $ newVal ^. gdValue . re (clonePrism prsm)
          writer userflag newVal
      ) `orElse` (do
        ((), newText) <- tStateEvent $ webitState entry
        oldVal <- tStateBehaviour output
        let newVal = newText ^? clonePrism prsm
        when (Just (oldVal ^. gdValue) /= newVal) $ mapM_ (writer True . GadgetData True) newVal
      )
    return GadgetOut {
        gadgetView = [WebitItem entry],
        gadgetDomCode = return (),
        gadgetEvents = retry,
        gadgetOutput = output
      }
  where
    dateFmt = "%Y-%m-%d"  -- Standard browser date format.
    dateHtml wId = H.input
      ! HA.type_ "date"
      ! HA.onchange (jsValue $ "() => {" <>
        call "jsb.event" ["{d:" <> value wId <> ",v:this.value}"] <> ";}")
    updateValue txt ref _ = var ref <> "." <> call "setAttribute" ["value", value txt]

-- ToDo: Fill in the other Optional special cases.

renderGadget env i (Optional d g) = {-# SCC renderGadget_Optional #-}
  renderGadget env i $ proc v -> do
    -- The general case for Optional decorates the inner widget with a check button. If there
    -- are no inner widgets then it passes its input straight through.
    ticked <- TickBox -< isJust v
    innerVal <- g -< fromMaybe d v
    returnA -< if ticked then Just innerVal else Nothing

renderGadget env i (Intercept dblClick dSel g) = {-# SCC renderGadget_Intercept #-} mdo
  (innerInput, writeInner) <- liftIO $ atomically $ newTState =<< tStateBehaviour i
  i1 <- liftIO $ atomically $ tStateDup i
  inner <- renderGadget env innerInput g
  let
    prop = if dblClick then "ondblclick" else "onclick"
    JavaScript script = webitEvent (webitId box) ()
  box <- makeWebitContainer1 "div" [(prop, LT.toStrict script)]
  clickChan <- addWebitChannel box :: Elmer (TChan ())
  let
    vEvent = ((),) . view gdValue <$>
      (tStateBehaviour (gadgetOutput inner) <* readTChan clickChan)
  clickResult <- mkWebitPopupSelect env dSel vEvent
  elmerDaemon box $ liftIO $ atomically $ do
    v <- (Just <$> tStateEvent i1) `orElse` (
        readTChan clickResult >>= \case
          ((), Just v1) -> return $ Just (True, GadgetData True v1)
          ((), Nothing) -> return Nothing)
    mapM_ (uncurry writeInner) v
  return inner

renderGadget env i (Icon f g) = {-# SCC renderGadget_Icon #-} do
    inner <- renderGadget env i g
    i1 <- liftIO $ atomically $ tStateDup i
    startIcon <- liftIO $ atomically $ f . view gdValue <$> tStateBehaviour i
    icon1 <- makeWebit (const $ img startIcon) updateIcon startIcon
    elmerDaemon icon1 $ liftIO $ atomically $
      webitIn icon1 . f . view (_2 . gdValue) =<< tStateEvent i1
    hbox <- makeWebitContainer "horizontal"
    showWebit hbox icon1
    mapM_ (showWebit hbox) $ gadgetView inner
    return inner {gadgetView = [WebitItem hbox]}
  where
    img nm = H.img ! HA.src (H.textValue $ iconUrl nm) ! HA.class_ "icon-inline"
    updateIcon nm ref _ = var ref <> ".innerHTML=" <> value (renderHtml $ img nm)

renderGadget env i (Coloured f g) = {-# SCC renderGadget_Coloured #-} do
    inner <- renderGadget env i g
    i1 <- liftIO $ atomically $ tStateDup i
    s <- ask
    liftIO $ atomically $ do
      c <- f . view gdValue <$> tStateBehaviour i
      elmerSendJS s $ mconcat $ map (setColours c) $ gadgetView inner
    -- Thread to update colours is owned by the first webit in "inner".
    -- If no inner webits then thread is not needed.
    case gadgetView inner of
      [] -> return ()
      view1 : _ -> elmerDaemon view1 $ liftIO $ atomically $ do
        c <- f . view (_2 . gdValue) <$> tStateEvent i1
        elmerSendJS s $ mconcat $ map (setColours c) $ gadgetView inner
    return inner
  where
    setColours :: Maybe Colour -> WebitItem -> JavaScript
    setColours Nothing w =
      let
        h = var $ webitHandle w
      in h <> ".style.color=null;" <> h <> ".style.backgroundColor=null;"
    setColours (Just bg) w =
      let
        bg1 = bg ^. re colourPrism
        fg1 = if C.luminance (getColour bg) > 0.5 then "Black" else "White" :: Text
        h = var $ webitHandle w
      in h <> ".style.color=" <> value fg1 <> ";"
        <> h <> ".style.backgroundColor=" <> value bg1 <> ";"

renderGadget env i (Linked f g) = {-# SCC renderGadget_Linked #-} do
  s <- ask
  i1 <- liftIO $ atomically $ tStateDup i
  inner <- renderGadget env i g
  initial <- liftIO $ atomically $ f . view gdValue <$> tStateBehaviour i1
  let
    attrs = case initial of
      Nothing -> [("target", "_blank")]
      Just url -> [("target", "_blank"), ("href", url)]
  link <- makeWebitContainer1 "a" attrs
  mapM_ (showWebit link) $ gadgetView inner
  elmerDaemon link $ liftIO $ atomically $ do
    v <- f . view (_2 . gdValue) <$> tStateEvent i1
    elmerSendJS s $ case v of
      Just url -> setAttribute link "href" $ LT.fromStrict url
      Nothing -> deleteAttribute link "href"
  return inner {gadgetView = [WebitItem link]}

renderGadget env i (Styled f g) = {-# SCC renderGadget_Styled #-} do
  s <- ask
  inner <- renderGadget env i g
  (i1, initial) <- liftIO $ atomically $ do
    i1 <- tStateDup i
    initial <- f . view gdValue <$> tStateBehaviour i1
    forM_ initial $ \sty -> elmerSendJS s $ mconcat $ map (`addClass` sty) $ gadgetView inner
    return (i1, initial)
  let
    styleUpdate oldStyle = do
      newStyle <- atomically $ do
        newStyle <- f . view (_2 . gdValue) <$> tStateEvent i1
        when (oldStyle /= newStyle) $ do
          let
            js1 = flip (maybe "") oldStyle $ \sty ->
              mconcat $ map (`removeClass` sty) $ gadgetView inner
            js2 = flip (maybe "") newStyle $ \sty ->
              mconcat $ map (`addClass` sty) $ gadgetView inner
          elmerSendJS s $ js1 <> js2
        return newStyle
      styleUpdate newStyle
  case gadgetView inner of
    [] -> return ()
    view1 : _ -> elmerDaemon view1 $ liftIO $ styleUpdate initial
  return inner

renderGadget env i (Frame f g) = {-# SCC renderGadget_Frame #-} do
  s <- ask
  inner <- renderGadget env i g
  (i1, initial) <- liftIO $ atomically $ (,) <$> tStateDup i <*> tStateBehaviour i
  let
    initialText = initial ^. gdValue . to f
    legend = case initialText of
      Nothing -> H.legend ! HA.hidden "1" $ H.text ""
      Just t -> H.legend $ H.text t
  current <- liftIO $ newTVarIO initialText
  frameBox <- makeWebitContainer1 "fieldset" []
  liftIO $ JS.send (elmerEngine s) $ command $ setWebitHtml legend $ webitHandle frameBox
    -- Can't wrap legend in div, so can't make it a webit.
    -- Must use send here because showWebit uses it, leading to a race condition with elmerSend.
  mapM_ (showWebit frameBox) $ gadgetView inner
  elmerDaemon frameBox $ liftIO $ atomically $ do
    newText <- f . view (_2 . gdValue) <$> tStateEvent i1
    oldText <- readTVar current
    when (oldText /= newText) $ elmerSendJS s $ case newText of
      Nothing -> var (webitHandle frameBox) <> ".childNodes[0].setAttribute(\"hidden\", \"1\");"
      Just t -> "{" <>
          var (webitHandle frameBox) <> "childNodes[0].innerText=" <> value t <> ";" <>
          var (webitHandle frameBox) <> "childNodes[0].removeAttribute(\"hidden\");" <>
        "}"
  return inner {gadgetView = [WebitItem frameBox]}

renderGadget env i (Form orient labelled) = {-# SCC renderGadget_Form #-} do
    tbl <- makeWebitContainer1 "table" []
    rowOuts <- mkRows orient tbl
    let result = foldr (liftA2 (.)) (pure id) rowOuts
    return result {gadgetView = [WebitItem tbl]}
  where
    mkRows Vertical tbl = forM labelled $ \(txt, field) -> do
      row <- makeWebitContainer1 "tr" []
      cell1 <- makeWebitContainer1 "td" []
      lbl <- makeWebStatic $ H.text txt
      cell2 <- makeWebitContainer1 "td" []
      inner <- renderGadget env i field
      mapM_ (showWebit cell2) $ gadgetView inner
      showWebit cell1 lbl
      showWebit row cell1
      showWebit row cell2
      showWebit tbl row
      return inner
    mkRows Horizontal tbl = do
      let (lbls, gs) = unzip labelled
      r1 <- makeWebitContainer1 "tr" []
      forM_ lbls $ \txt -> do
        c1 <- makeWebitContainer1 "td" []
        t1 <- makeWebStatic (H.text txt)
        showWebit c1 t1
        showWebit r1 c1
      r2 <- makeWebitContainer1 "tr" []
      inners <- forM gs $ \g -> do
        cell <- makeWebitContainer1 "td" []
        inner <- renderGadget env i g
        mapM_ (showWebit cell) $ gadgetView inner
        showWebit r2 cell
        return inner
      showWebit tbl r1
      showWebit tbl r2
      return inners

renderGadget env i (TabForm tabs) = {-# SCC renderGadget_TabForm #-} do
  tabContainer <- makeWebitContainer "tab-form"
  tabHeader <- makeWebitContainer "tab-header"
  showWebit tabContainer tabHeader
  tabOuts <- mapM (makeTab env i tabHeader tabContainer) tabs
  unless (null tabOuts) $
      elmerSend $ var (webitHandle tabHeader) <> ".firstElementChild.click()"
  return $ foldr (liftA2 (.)) (pure id) tabOuts

renderGadget env i (Box orient gadgetss) = {-# SCC renderGadget_Box #-} do
  container <- makeWebitContainer $ case orient of
    Horizontal -> "horizontal"
    Vertical -> "vertical"
  let
    separator = makeWebStatic $ case orient of
      Horizontal -> H.hr
      Vertical -> H.div ! HA.class_ "vr" $ ""
  webits <- sequence (intercalate [Left <$> separator] $
      map (map (fmap Right . renderGadget env i)) gadgetss)
  let webitOuts = rights webits
  forM_ webits $ \case
    Left w -> showWebit container w
    Right g -> mapM_ (showWebit container) $ gadgetView g
  return $ foldr (liftA2 (.)) (pure id) webitOuts

renderGadget env i (Grid columnHeads rowHeads gss) = {-# SCC renderGadget_Grid #-} do
    gridBox <- makeWebitContainer1 "table" [("style", "gridBox")]
    hRow <- makeWebStatic $ H.tr $ mconcat $ H.th mempty : map (H.th . H.text) columnHeads
    showWebit gridBox hRow
    (rows, rowOuts) <- unzip <$> zipWithM makeRow rowHeads gss
    mapM_ (showWebit gridBox) rows
    return $ foldr (liftA2 (.)) (pure id) $ concat rowOuts
  where
    makeRow rowHead gs = do
      row <- makeWebitContainer1 "tr" []
      headWebit <- makeWebStatic $ H.th $ H.text rowHead
      showWebit row headWebit
      outs <- forM gs $ \g -> do
        out <- renderGadget env i g
        cell <- makeWebitContainer1 "tc" []
        mapM_ (showWebit cell) $ gadgetView out
        return out
      return (row, outs)

renderGadget env i (Validate f g) = {-# SCC renderGadget_Validate #-} do
    inner <- renderGadget env i g
    let
      checkOk = f <$> env <*> (view gdValue <$> i) <*> (view gdValue <$> gadgetOutput inner)
      output = (\ok -> gdOk %~ (ok &&)) <$> checkOk <*> gadgetOutput inner
    initialOk <- liftIO $ atomically $ tStateBehaviour checkOk
    setStyle inner initialOk
    case gadgetView inner of
      [] -> return ()
      view1 : _ -> elmerDaemon view1 $ do
        okFlag <- liftIO $ atomically $ snd <$> tStateEvent checkOk
        setStyle inner okFlag
    return inner {gadgetOutput = output}
  where
    setStyle g1 b = elmerSend $ mconcat $ map (setJS b) $ gadgetView g1
    setJS True w = var (webitHandle w) <> ".classList.add(\"invalid\");"
    setJS False w = var (webitHandle w) <> ".classList.remove(\"invalid\")"

renderGadget env i (ValidateText f g) = {-# SCC renderGadget_ValidateText #-} do
    box1 <- makeWebitContainer "vertical"
    inner <- renderGadget env i g
    warning <- makeWebStatic $
      H.p ! HA.style "backgroundColor:LightCoral;text-align:center;" $ mempty
    mapM_ (showWebit box1) $ gadgetView inner
    showWebit box1 warning
    let
      checkOk = f <$> env <*> (view gdValue <$> i) <*> (view gdValue <$> gadgetOutput inner)
      output = (\ok -> gdOk %~ (isNothing ok &&)) <$> checkOk <*> gadgetOutput inner
    initialOk <- liftIO $ atomically $ isNothing <$> tStateBehaviour checkOk
    setStyle warning initialOk
    elmerDaemon box1 $ do
      okFlag <- liftIO $ atomically $ isNothing . snd <$> tStateEvent checkOk
      setStyle warning okFlag
    return inner {gadgetOutput = output}
  where
    setStyle w True = elmerSend $ var (webitHandle w) <> ".style.display=\"none\";"
    setStyle w False = elmerSend $ var (webitHandle w) <> ".style.display=\"block\";"

renderGadget env i (TextBox prismFunc) = {-# SCC renderGadget_TextBox #-} do
    (prism1, initial) <- liftIO $ atomically $ do
      prism1 <- prismFunc <$> tStateBehaviour env
      initial1 <- tStateBehaviour i
      return (prism1, initial1)
    webit <- makeWebit boxHtml updateJS (initial ^. gdValue . re (clonePrism prism1))
    i1 <- tStateDupIO $ (gdValue %~ view (re $ clonePrism prism1)) <$> i
    out1 <- mergeGadgetData i1 $ mapTState1 (const True) $ GadgetData True <$> webitState webit
    let out2 = processOutput initial (clonePrism prism1) <$> out1
    out3 <- tStateDupIO out2
    elmerDaemon webit $ do
      ok <- view (_2 . gdOk) <$> liftIO (atomically $ tStateEvent out3)
      let h = var $ webitHandle webit
      elmerSend $ "{" <>
        h <> ".childNodes[0].setCustomValidity(" <> value (msg ok) <> ");" <>
        h <> ".childNodes[0].classList." <> if ok
        then "remove(\"invalid\");}"
        else "add(\"invalid\");}"
    elmerDaemon webit $ liftIO $ atomically $ do
      (_, v) <- tStateEvent i1
      webitIn webit $ v ^. gdValue
    return GadgetOut {
        gadgetView = [WebitItem webit],
        gadgetDomCode = return (),
        gadgetEvents = retry,
        gadgetOutput = out2
      }
  where
    boxHtml wId = H.input
        ! HA.type_ "text"
        ! HA.onchange (jsValue $ call "jsb.event" ["{\"d\":" <> value wId <> ",v:this.value}"])
    processOutput :: GadgetData a -> Prism' Text a -> GadgetData Text -> GadgetData a
    processOutput v0 p txt = case txt ^? gdValue . p of
      Just v ->  gdValue .~ v $ txt
      Nothing -> gdOk .~ False $ v0
    updateJS v h _ = var h <> ".childNodes[0].value=" <> value v <> ";"
    msg :: Bool -> Text
    msg b = if b then "" else "Invalid"
        -- See https://stackoverflow.com/questions/7609130/set-the-value-of-an-input-field

renderGadget _ i DisplayText = {-# SCC renderGadget_DisplayText #-} do
    initial <- liftIO $ atomically $ tStateBehaviour i
    display <- makeWebit (displayHtml $ initial ^. gdValue) updateDisplay (initial ^. gdValue)
    i1 <- liftIO $ atomically $ tStateDup i
    elmerDaemon display $ liftIO $ atomically $ do
      (_, v) <-tStateEvent i1
      webitIn display $ v ^. gdValue
    out <- mergeGadgetData i1 $ mapTState1 (const True) $ GadgetData True <$> webitState display
    return GadgetOut {
        gadgetView = [WebitItem display],
        gadgetDomCode = command $ initJS $ webitId display,
        gadgetEvents = retry,
        gadgetOutput = out
      }
  where
    displayId wId = LT.pack $ show wId <> "_dsp"
    popupId wId = LT.pack $ show wId <> "_pop"
    displayHtml txt wId = do
      H.p ! HA.class_ "no-box" ! HA.id (H.lazyTextValue $ displayId wId) $ H.text txt
      H.input
        ! HA.id (H.lazyTextValue $ popupId wId)
        ! HA.type_ "text"
        ! HA.value (H.textValue txt)
    updateDisplay v _ wId = "{\
        \$(" <> webitSelector1 wId "_dsp" <> ").text(" <> value v <> ");\
        \$(" <> webitSelector1 wId "_pop" <> ").val(" <> value v <> ");\
      \}"
    initJS wId = let sel = "\"#" <> JavaScript (popupId wId) <> "\"" in
      "$(" <> sel <> ").dialog({\
          \title:'Enter text:',\
          \position:{of:(" <> webitSelector1 wId "_dsp" <> ")},\
          \height:'auto',\
          \width:'auto',\
          \buttons:[{text:\"OK\",click:function () {" <>
              webitEventData wId "this.value" <>
              "$(" <> webitSelector1 wId "_dsp" <> ").text(this.value);\
              \$(" <> webitSelector1 wId "_pop" <> ").dialog('close');\
            \}}],\
          \modal:true,\
          \autoOpen:false\
        \});\
      \$(" <> webitSelector wId <> ").on(\"click\", function () {\
        \$(" <> sel <> ").dialog(\"open\");})"

renderGadget _ i (MemoBox size _) = {-# SCC renderGadget_MemoBox #-} do
    initial <- liftIO $ atomically $ tStateBehaviour i
    input <- makeWebit boxHtml updateJS (initial ^. gdValue)
    i1 <- liftIO $ atomically $ tStateDup i
    elmerDaemon input $ liftIO $ atomically $ do
      v <- tStateEvent i1
      webitIn input $ v ^. _2 . gdValue
    out <- mergeGadgetData i $ mapTState1 (const True) $ GadgetData True <$> webitState input
    return GadgetOut {
        gadgetView = [WebitItem input],
        gadgetDomCode = return (),
        gadgetEvents = retry,
        gadgetOutput = out
      }
  where
    boxHtml wId = H.textarea
      ! HA.onchange (jsValue $ call "jsb.event" ["{\"d\":" <> value wId <> ",v:this.value}"])
      ! HA.class_ boxClass
      $ ""
    updateJS v h _ = var h <> ".childNodes[0].value=" <> value v <> ";"
    boxClass = case size of
      MemoSmall -> "memo-small"
      MemoMedium -> "memo-medium"
      MemoLarge -> "memo-large"

renderGadget _ i DisplayMemo = {-# SCC renderGadget_DisplayMemo #-} do
    initial <- liftIO $ atomically $ tStateBehaviour i
    display <- makeWebit (displayHtml $ initial ^. gdValue) updateDisplay (initial ^. gdValue)
    i1 <- liftIO $ atomically $ tStateDup i
    elmerDaemon display $ liftIO $ atomically $ do
      (_, v) <-tStateEvent i1
      webitIn display $ v ^. gdValue
    out <- mergeGadgetData i $ mapTState1 (const True) $ GadgetData True <$> webitState display
    return GadgetOut {
        gadgetView = [WebitItem display],
        gadgetDomCode = command $ initJS $ webitId display,
        gadgetEvents = retry,
        gadgetOutput = out
      }
  where
    displayId wId = LT.pack $ show wId <> "_dsp"
    popupId wId = LT.pack $ show wId <> "_pop"
    displayHtml txt wId = do
      H.p
        ! HA.class_ "no-box"
        ! HA.id (H.lazyTextValue $ displayId wId)
        ! HA.style "white-space:pre-wrap"
        $ H.text txt
      H.textarea
        ! HA.id (H.lazyTextValue $ popupId wId)
        ! HA.style "resize:none;"
        ! HA.rows "5"
        ! HA.cols "50"
        $ H.text txt
    updateDisplay v _ wId = "{\
        \$(" <> webitSelector1 wId "_dsp" <> ").text(" <> value v <> ");\
        \$(" <> webitSelector1 wId "_pop" <> ").val(" <> value v <> ");\
      \}"
    initJS wId = let sel = "\"#" <> JavaScript (popupId wId) <> "\"" in
      "$(" <> sel <> ").dialog({\
          \title:'Enter text:',\
          \position:{of:(" <> webitSelector1 wId "_dsp" <> ")},\
          \height:'auto',\
          \width:'auto',\
          \buttons:[{text:\"OK\",click:function () {" <>
              webitEventData wId "this.value" <>
              "$(" <> webitSelector1 wId "_dsp" <> ").text(this.value);\
              \$(" <> webitSelector1 wId "_pop" <> ").dialog('close');\
            \}}],\
          \modal:true,\
          \autoOpen:false\
        \});\
      \$(" <> webitSelector wId <> ").on(\"click\", function () {\
        \$(" <> sel <> ").dialog(\"open\");})"


renderGadget e i (Combo choiceF) = {-# SCC renderGadget_Combo #-} do
    (env, initial) <- liftIO $ atomically $ (,) <$> tStateBehaviour e <*> tStateBehaviour i
    let
      choices = V.fromList $ choiceF env
      valueIndex v = fromMaybe (-1) $ V.findIndex (((v ^. gdValue) ==) . menuItemValue) choices
      initialN = valueIndex initial
    input <- makeWebit (comboHtml choices) updateJS initialN
    i1 <- liftIO $ atomically $ tStateDup i
    elmerDaemon input $ liftIO $ atomically $
        webitIn input . valueIndex . snd =<< tStateEvent i1
    out <- mergeGadgetData i $ mapTState1 (const True) $
      maybe initial (GadgetData True . menuItemValue) . (choices V.!?) <$> webitState input
    return GadgetOut {
        gadgetView = [WebitItem input],
        gadgetDomCode = return (),
        gadgetEvents = retry,
        gadgetOutput = out
      }
  where
    comboHtml choices wId = H.select
      ! HA.oninput (jsValue $
          call "jsb.event" ["{\"d\":" <> value wId <> ",\"v\":this.selectedIndex}"])
      $ sequence_ $ V.imap optionHtml choices
    optionHtml n item = H.option
      ! HA.value (H.stringValue $ show n)
      ! (case menuItemColour item of
          Just c -> HA.style (H.textValue $ "background-color:" <> c ^. re colourPrism <> ";")
          Nothing -> mempty)
      $ do
        forM_ (menuItemIcon item) $ \icon1 -> do
          H.img ! HA.src (H.textValue $ iconUrl icon1) ! HA.height "16" ! HA.width "16"
          " "
        H.text $ menuItemLabel item
    updateJS v h _ = var h <> ".childNodes[0].value=" <> value v <> ";"

renderGadget e i (Radio choiceF) = {-# SCC renderGadget_Radio #-} do
    (env, initial) <- liftIO $ atomically $ (,) <$> tStateBehaviour e <*> tStateBehaviour i
    let
      choices = V.fromList $ choiceF env
      valueIndex v = fromMaybe (-1) $ V.findIndex (((v ^. gdValue) ==) . snd) choices
      initialN = valueIndex initial
    input <- makeWebit (radioHtml choices) updateJS initialN
    i1 <- liftIO $ atomically $ tStateDup i
    elmerDaemon input $ liftIO $ atomically $
        webitIn input . valueIndex . snd =<< tStateEvent i1
    out <- mergeGadgetData i $ mapTState1 (const True) $
        maybe initial (GadgetData True . snd) . (choices V.!?) <$> webitState input
    return GadgetOut {
        gadgetView = [WebitItem input],
        gadgetDomCode = return (),
        gadgetEvents = retry,
        gadgetOutput = out
      }
  where
    radioHtml choices wId = sequence_ $ V.imap (optionHtml wId) choices
    optionHtml wId n (txt, _) = do
      H.input
        ! HA.type_ "radio"
        ! HA.id (H.stringValue $ show n)
        ! HA.name (H.stringValue $ show wId)
        ! HA.onclick (jsValue $
          call "jsb.event" ["{\"d\":" <> value wId <> ",\"v\":" <> value n <> "}"])
      H.label
        ! HA.for (H.stringValue $ show n)
        $ H.text txt
    updateJS v h _ = var h <> ".childNodes[" <> value (v*2) <>"].checked=true;"
      -- The "v*2" refers to the alternating input and label elements. The first input is
      -- childNodes[0], the second is childNodes[2], and so on.

renderGadget _ i TickBox = {-# SCC renderGadget_TickBox #-} do
    initial <- liftIO $ atomically $ tStateBehaviour i
    input <- makeWebit tickHtml updateJS $ initial ^. gdValue
    i1 <- liftIO $ atomically $ tStateDup i
    elmerDaemon input $ liftIO $ atomically $
        webitIn input . view gdValue . snd =<< tStateEvent i1
    out <- mergeGadgetData i $ mapTState1 (const True) $ GadgetData True <$> webitState input
    return GadgetOut {
        gadgetView = [WebitItem input],
        gadgetDomCode = return (),
        gadgetEvents = retry,
        gadgetOutput = out
      }
  where
    tickHtml wId = H.input
      ! HA.type_ "checkbox"
      ! HA.oninput (jsValue $
        call "jsb.event" ["{\"d\":" <> value wId <> ",\"v\":this.checked}"])
    updateJS v h _ = var h <> ".childNodes[0].checked=" <> if v then "true;" else "false;"

renderGadget e i (Message messageF) = {-# SCC renderGadget_Message #-} do
    (env, initial) <- liftIO $ atomically $ (,) <$> tStateBehaviour e <*> tStateBehaviour i
    webit <- makeWebit msgHtml updateJS $ messageF env $ initial ^. gdValue
    i1 <- liftIO $ atomically $ tStateDup i
    o1 <- liftIO $ atomically $ tStateDup i
    msg <- liftIO $ atomically $ tStateDup $ messageF <$> e <*> (view gdValue <$> i1)
    elmerDaemon webit $ liftIO $ atomically $ do
      (_, txt) <- tStateEvent msg
      webitIn webit txt
      -- Pass through the behaviour, but no user-initiated events.
    return GadgetOut {
        gadgetView = [WebitItem webit],
        gadgetDomCode = return (),
        gadgetEvents = retry,
        gadgetOutput = o1
      }
  where
    msgHtml _ = H.p ""
    updateJS v h _ = var h <> ".childNodes[0].innerText=" <> value v <> ";"

renderGadget e i (FixedText textFunc mSel) = {-# SCC renderGadget_FixedText #-} do
    (env, initial) <- liftIO $ atomically $ (,) <$> tStateBehaviour e <*> tStateBehaviour i
    webit <- makeWebit boxHtml updateJS $ textFunc env $ initial ^. gdValue
    i1 <- liftIO $ atomically $ tStateDup i
    case mSel of
      Just dSel -> do
        let
          vEvent = ((),) . view gdValue <$>
            (tStateBehaviour i <* tStateEvent (webitState webit))
        clickResult <- mkWebitPopupSelect e dSel vEvent
        (out, outWriter) <- newTStateIO initial
        elmerDaemon webit $ liftIO $ atomically $ do
          r <- (Just <$> tStateEvent i1) `orElse` (
              readTChan clickResult >>= \case
                ((), Just v1) -> return $ Just (True, GadgetData True v1)
                ((), Nothing) -> return Nothing)
          forM_ r $ \(userflag, v) -> do
            env1 <- tStateBehaviour e
            webitIn webit $ textFunc env1 $ v ^. gdValue
            outWriter userflag v
        return GadgetOut {
            gadgetView = [WebitItem webit],
            gadgetDomCode = return (),
            gadgetEvents = retry,
            gadgetOutput = out
          }
      Nothing -> return GadgetOut {
          gadgetView = [WebitItem webit],
          gadgetDomCode = return (),
          gadgetEvents = retry,
          gadgetOutput = i1
        }
  where
    boxHtml wId = H.input
      ! HA.type_ "text"
      ! HA.readonly "true"
      ! HA.onclick (jsValue $ webitEvent wId ("" :: Text))
      -- The event has to send text but we only want the click, so just send "".
    updateJS v h _ = var h <> ".childNodes[0].value=" <> value v <> ";"

renderGadget e i (FixedMemo size _ textFunc mSel) = {-# SCC renderGadget_FixedMemo #-} do
    (env, initial) <- liftIO $ atomically $ (,) <$> tStateBehaviour e <*> tStateBehaviour i
    webit <- makeWebit boxHtml updateJS $ textFunc env $ initial ^. gdValue
    i1 <- liftIO $ atomically $ tStateDup i
    case mSel of
      Just dSel -> do
        let
          vEvent = ((),) . view gdValue <$>
            (tStateBehaviour i <* tStateEvent (webitState webit))
        clickResult <- mkWebitPopupSelect e dSel vEvent
        (out, outWriter) <- newTStateIO initial
        elmerDaemon webit $ liftIO $ atomically $ do
          r <- (Just <$> tStateEvent i1) `orElse` (
              readTChan clickResult >>= \case
                ((), Just v1) -> return $ Just (True, GadgetData True v1)
                ((), Nothing) -> return Nothing)
          forM_ r $ \(userflag, v) -> do
            env1 <- tStateBehaviour e
            webitIn webit $ textFunc env1 $ v ^. gdValue
            outWriter userflag v
        return GadgetOut {
            gadgetView = [WebitItem webit],
            gadgetDomCode = return (),
            gadgetEvents = retry,
            gadgetOutput = out
          }
      Nothing -> return GadgetOut {
          gadgetView = [WebitItem webit],
          gadgetDomCode = return (),
          gadgetEvents = retry,
          gadgetOutput = i1
        }
  where
    boxHtml wId = H.textarea
      ! HA.class_ boxClass
      ! HA.readonly "true"
      ! HA.onclick (jsValue $ webitEvent wId ("" :: Text))
      $ ""
      -- The event has to send text but we only want the click, so just send "".
    updateJS v h _ = var h <> ".childNodes[0].value=" <> value v <> ";"
    boxClass = case size of
      MemoSmall -> "memo-small"
      MemoMedium -> "memo-medium"
      MemoLarge -> "memo-large"

renderGadget _ i ClickableList = {-# SCC renderGadget_ClickableList #-} do
    initial <- liftIO $ atomically $ V.fromList . view gdValue <$> tStateBehaviour i
    store <- liftIO $ newTVarIO initial
    webit <- makeWebit (`bodyHtml` itemList initial) updateJS (itemList initial)
    i1 <- tStateDupIO i
    (out, writeOut) <- newTStateIO $ GadgetData True Nothing
    elmerDaemon webit $ liftIO $ atomically $
        procInput webit store i1 `orElse` procClick webit store writeOut
    return GadgetOut {
        gadgetView = [WebitItem webit],
        gadgetDomCode = return (),
        gadgetEvents = retry,
        gadgetOutput = out
      }
  where
    itemList :: Vector (ClickableItem a) -> [(Text, Int)]
    itemList = V.toList . V.imap (\n c -> (clickText c, n))
    bodyHtml wId items = sequence_ $ intersperse (H.text ", ") $ map itemHtml items
      where
        itemHtml (txt, n) = H.span
          ! HA.onclick (jsValue $ webitEvent wId [("1" :: Text, n)])
          ! HA.ondblclick (jsValue $ webitEvent wId [("2" :: Text, n)])
          $ H.text txt  -- Have to send input type, so use first field to flag click count.
    updateJS items h wId = var h <> ".innerHTML=" <> value (renderHtml $ bodyHtml wId items)
    procInput webit store i1 = do
      newVec <- V.fromList . view gdValue . snd <$> tStateEvent i1
      oldVec <- readTVar store
      let
        newList = itemList newVec
      when (newList /= itemList oldVec) $ do
        writeTVar store newVec
        webitIn webit newList
    procClick webit store write = do
      tStateEvent (webitState webit) >>= \case
        ((), [(t, n)]) -> do
          s <- readTVar store
          let c = s V.!? n
          when (t == "1") $ write True $ GadgetData True $ clickSingle =<< c
          when (t == "2") $ write True $ GadgetData True $ clickDouble =<< c
        _ -> return ()

renderGadget _ i DateBox {} = {-# SCC renderGadget_DateBox #-} do
    -- This ignores the date format in the argument because HTML delegates that to the browser.
    initVal <- liftIO $ atomically $ tStateBehaviour i
    sendJS <- asks elmerSendJS
    i1 <- tStateDupIO i
    entry <- makeWebit dateHtml updateValue (initVal ^. gdValue . re (clonePrism prsm))
    let h = var $ webitHandle entry
    (output, writer) <- newTStateIO initVal
    elmerDaemon entry $ liftIO $ atomically $
      (do
        (userflag, newVal) <- tStateEvent i1
        oldVal <- tStateBehaviour output
        when (oldVal /= newVal) $ do
          webitIn entry $ newVal ^. gdValue . re (clonePrism prsm)
          writer userflag newVal
      ) `orElse` (do
        ((), newVal) <- tStateEvent $ webitState entry
        case newVal ^? clonePrism prsm of
          Just d -> do
            writer True $ GadgetData True d
            sendJS $ "{" <>
              h <> ".childNodes[0].setCustomValidity(\"\");" <>
              h <> ".childNodes[0].classList.remove(\"invalid\");}"
          Nothing -> do
            writer True $ gdOk .~ False $ initVal
            sendJS $ "{" <>
              h <> ".childNodes[0].setCustomValidity(\"Invalid date\");" <>
              h <> ".childNodes[0].classList.add(\"invalid\");}"
      )
    return GadgetOut {
        gadgetView = [WebitItem entry],
        gadgetDomCode = return (),
        gadgetEvents = retry,
        gadgetOutput = output
      }
  where
    prsm = datePrism "%Y-%m-%d"  -- Standard browser date format.
    dateHtml wId = H.input
      ! HA.type_ "date"
      ! HA.oninput (jsValue $ call "jsb.event" ["{d:" <> value wId <> ",v:this.value}"]
        )
    updateValue txt ref _ = var ref <> ".childNodes[0].value=" <> value txt

renderGadget _ i (IconBox _) = {-# SCC renderGadget_IconBox #-} do
    -- Ignore the group predicate: its not needed on the Webit version.
    initVal <- liftIO $ atomically $ tStateBehaviour i
    box1 <- makeWebitContainer "no-box"
    webit <- makeWebit
        (iconHtml "icon-inline" (initVal ^. gdValue) . Just)
        updateValue $
        initVal ^. gdValue
    showWebit box1 webit
    i1 <- tStateDupIO i
    (output, outputWriter) <- newTStateIO initVal
    elmerDaemon box1 $ do
      oldIcon <- liftIO $ atomically $ do
        void $ tStateEvent $ webitState webit  -- Icon clicked: pop up menu.
        tStateBehaviour i
      getIconChoice (oldIcon ^. gdValue) >>= \case
        Nothing -> return ()
        Just newIcon -> liftIO $ atomically $ do
          webitIn webit newIcon
          outputWriter True $ GadgetData True newIcon
    elmerDaemon box1 $ liftIO $ atomically $ do
      (userflag, newVal) <- tStateEvent i1
      oldVal <- tStateBehaviour output
      when (oldVal /= newVal) $ do
        webitIn webit $ newVal ^. gdValue
        outputWriter userflag newVal
    return GadgetOut {
        gadgetView = [WebitItem box1],
        gadgetDomCode = return (),
        gadgetEvents = retry,
        gadgetOutput = output
      }
  where
    updateValue txt ref _ =
      var ref <> call ".childNodes[0].setAttribute" [value ("src" :: Text), value $ iconUrl txt]

renderGadget _ i ColourBox = {-# SCC renderGadget_ColourBox #-} do
    initVal <- liftIO $ atomically $ tStateBehaviour i
    sendJS <- asks elmerSendJS
    i1 <- tStateDupIO i
    entry <- makeWebit colourHtml updateValue (initVal ^. gdValue . re colourPrism)
    let h = var $ webitHandle entry
    (output, writer) <- newTStateIO initVal
    void $ liftIO $ forkIO $ forever $ atomically $
      (do
        (userflag, newVal) <- tStateEvent i1
        oldVal <- tStateBehaviour output
        when (oldVal /= newVal) $ do
          webitIn entry $ newVal ^. gdValue . re colourPrism
          writer userflag newVal
      ) `orElse` (do
        ((), newVal) <- tStateEvent $ webitState entry
        case newVal ^? colourPrism of
          Just d -> do
            writer True $ GadgetData True d
            sendJS $ "{" <>
              h <> ".childNodes[0].setCustomValidity(\"\");" <>
              h <> ".childNodes[0].classList.remove(\"invalid\");}"
          Nothing -> do
            writer True $ gdOk .~ False $ initVal
            sendJS $ "{" <>
              h <> ".childNodes[0].setCustomValidity(\"Invalid colour\");" <>
              h <> ".childNodes[0].classList.add(\"invalid\");}"
      )
    return GadgetOut {
        gadgetView = [WebitItem entry],
        gadgetDomCode = return (),
        gadgetEvents = retry,
        gadgetOutput = output
      }
  where
    colourHtml wId = H.input
      ! HA.type_ "color"
      ! HA.onchange (jsValue $ call "jsb.event" ["{d:" <> value wId <> ",v:this.value}"])
    updateValue txt ref _ = var ref <> ".childNodes[0].value=" <> value txt

renderGadget _ i (ImageDisplay size) = {-# SCC renderGadget_ImageDisplay #-} do
    initVal <- liftIO $ atomically $ tStateBehaviour i
    box1 <- makeWebitContainer "no-box"
    elmerSend $ addClass box1 $ case size of
      MemoSmall -> "image-small"
      MemoMedium -> "image-medium"
      MemoLarge -> "image-large"
    displayImage box1 $ initVal ^. gdValue
    i1 <- tStateDupIO i
    elmerDaemon box1 $ do
      (_, newBytes) <- liftIO $ atomically $ tStateEvent i1
      displayImage box1 $ newBytes ^. gdValue
    output <- tStateDupIO i
    return GadgetOut {
        gadgetView = [WebitItem box1],
        gadgetDomCode = return (),
        gadgetEvents = retry,
        gadgetOutput = output
      }
  where
    displayImage box1 Nothing =
      elmerSend $ var (webitHandle box1) <> ".innerHTML=\"\""
    displayImage box1 (Just bytes) = elmerSend $
      var (webitHandle box1) <> ".innerHTML=" <> value ("<img src=\"" <> encoded bytes <> "\">")
    encoded bytes =
      "data:image/" <> fromMaybe "*" (mimeSubtype bytes) <> ";base64," <>
      LT.decodeLatin1 (B64.encode $ LB.fromStrict bytes)
    mimeSubtype bytes = foldr ((<|>) . fmap LT.pack . ($ bytes)) Nothing
      [testJpeg, testPng, testSvg, testGif, testTiff, testBmp]
    testSvg bytes = if "<?xml" `B.isPrefixOf` bytes then Just "svg+xml" else Nothing
      -- Assume that any XML image file is an SVG. The browser can sort it out.

renderGadget e i (TreeSelector forestFunc) = {-# SCC renderGadget_TreeSelector #-} do
    trees1 <- forestFunc <$> liftIO (atomically $ tStateBehaviour e)
    -- Build indexes for the tree values, because type "a" is not "ToJSON".
    let
      trees2 = evalState (mapM tag $ trimForest trees1) 1  -- Node zero is root.
      tagTable = M.fromList $ concatMap flatten trees2
      idTable = M.fromList $ catMaybes $ concatMap (flatten . fmap invertId) trees2
    initial <- liftIO $ atomically $ tStateBehaviour i
    webit <- makeWebitContainer ""
    let
      wId = webitId webit
      h = webitHandle webit
    elmerSend $
      "$(" <> var h <> ").fancytree({\
          \extensions: [\"multi\"],\
          \expanded: true,\
          \select: function(event,data){\
              \jsb.event({\
                  \d:" <> value wId <> ",\
                  \v:{k:data.node.key,s:data.node.isSelected()}\
                \});\
            \}," <>
          "source:" <> JavaScript (LT.decodeLatin1 $ encode $ map treeConvert trees2) <>
        "})"
    i1 <- tStateDupIO i
    (out, outWriter) <- newTStateIO initial
    selectClicks <- addWebitChannel webit
    elmerDaemon webit $ do
      v <- liftIO $ atomically $ do
        (userFlag, v) <- tStateEvent i1
        oldVal <- tStateBehaviour out
        when (oldVal /= v) $ outWriter userFlag v
        return v
      setSelected h idTable $ v ^. gdValue
    elmerDaemon webit $ liftIO $ atomically $ do
        ev <- readTChan selectClicks
        forM_ (tagTable ^? ix (ftsKey ev) . _3 . _Just) $ \key -> do
          oldVal <- tStateBehaviour out
          outWriter True $ gdValue . contains key .~ ftsSelected ev $ oldVal
    setSelected h idTable $ initial ^. gdValue
    return GadgetOut {
        gadgetView = [WebitItem webit],
        gadgetDomCode = return (),
        gadgetEvents = retry,
        gadgetOutput = out
      }
  where
    trimForest :: Forest (Text, Maybe Text, Maybe a) -> Forest (Text, Maybe Text, Maybe a)
    trimForest = mapMaybe trimTree
    trimTree :: Tree (Text, Maybe Text, Maybe a) -> Maybe (Tree (Text, Maybe Text, Maybe a))
    trimTree (Node v sub) = case trimForest sub of
        [] -> if isJust $ v ^. _3 then Just $ Node v [] else Nothing
        trees -> Just $ Node v trees
    tag = mapM $ \v -> do
      n <- get
      modify succ
      return (n, v)
    invertId (n, (_, _, idx)) = (, T.pack $ show n) <$> idx
    treeConvert :: Tree (Int, (Text, Maybe Text, Maybe a)) -> FancyTree
    treeConvert (Node (n, (nm, tip, k)) cs) =
      (ftDefault (T.pack $ show n) nm) {
          ftToolTip = tip,
          ftUnselectable = isNothing k,
          ftChildren = Just $ map treeConvert cs
        }
    setSelected h idTable selection = do
      let remoteSelection = mapMaybe (`M.lookup` idTable) $ S.toList selection
      elmerSend $ "{\
          \let selSet=new " <> call "Set" [value remoteSelection] <> ";" <>
          "let tree=" <> call "$.ui.fancytree.getTree" [var h] <> ";" <>
          "tree.visit(function(node){node.setSelected(\
              \selSet.has(node.key),\
              \{\"noEvents\":true}\
            \);});\
        \}"

renderGadget _ i (Table editing spec Nothing) = {-# SCC renderGadget_Table #-} do
  initial <- liftIO $ atomically $ tStateBehaviour i
  (webit, domJS) <- mkTableWebit editing spec $ initial ^. gdValue
  i1 <- tStateDupIO i
  (out, outWriter) <- newTStateIO initial
  elmerDaemon webit $ liftIO $ atomically $
    (do
      (userFlag, v) <- tStateEvent i1
      oldVal <- tStateBehaviour out
      when (oldVal /= v) $ do
        webitIn webit $ v ^. gdValue
        outWriter userFlag v
    ) `orElse` (do
      ((), newVal) <- tStateEvent $ webitState webit
      outWriter True $ GadgetData True newVal
    )
  return GadgetOut {
      gadgetView = [WebitItem webit],
      gadgetDomCode = domJS,
      gadgetEvents = retry,
      gadgetOutput = out
    }

renderGadget e i (Table editing spec (Just selector)) = {-# SCC renderGadget_Table_Just #-} do
  initial <- liftIO $ atomically $ tStateBehaviour i
  (webit, domJS, clickRows) <- mkTableWebitWithClicks editing spec $ initial ^. gdValue
  rowUpdates <- mkWebitPopupSelect e selector $ do
    row <- readTChan clickRows
    v <- tStateBehaviour $ webitState webit
    return (row, v !! row)
  i1 <- tStateDupIO i
  (out, outWriter) <- newTStateIO initial
  elmerDaemon webit $ liftIO $ atomically $ (do
      (userFlag, v) <- tStateEvent i1
      oldVal <- tStateBehaviour out
      when (v /= oldVal) $ do
        webitIn webit $ v ^. gdValue
        outWriter userFlag v
    ) `orElse` ( do
      oldVal <- tStateBehaviour $ webitState webit
      (row, v) <- readTChan rowUpdates
      forM_ v $ \v1 -> do
        let newVal = ix row .~ v1 $ oldVal
        webitIn webit newVal
        outWriter True $ GadgetData True newVal
    )
  return GadgetOut {
      gadgetView = [WebitItem webit],
      gadgetDomCode = domJS,
      gadgetEvents = retry,
      gadgetOutput = out
    }

renderGadget e i (ForestTable headerGroups lensForest) = {-# SCC renderGadget_ForestTable #-} do
    eng <- asks elmerEngine
    -- Create the table and header.
    table1 <- makeWebitContainer1 "table" [("class", "matrix")]
    initialEnv <- liftIO $ atomically $ tStateBehaviour e
    let headerBlock = renderHtml $ H.thead $ headerRows headerGroups
    liftIO $ JS.send eng $ command $
      var (webitHandle table1) <> ".innerHTML=" <> value headerBlock <> ";"
    -- Create the body.
    gadgets <- gadgetForest (map (map snd . snd) headerGroups) (lensForest initialEnv)
    bodyBlock <- makeWebitContainer1 "tbody" []
    forM_ (concatMap assembleTreeRows gadgets) $ \cells -> do
      row <- makeWebitContainer1 "tr" []
      mapM_ (mapM_ (showWebit row) . gadgetView) cells
      -- Each GadgetOut in 'cells' has exactly one <td> webit created by gadgetTree1
      showWebit bodyBlock row
    showWebit table1 bodyBlock
    -- Assemble the resulting GadgetOut.
    let result = foldr (liftA2 (.)) (pure id) $ concatMap (concat . flatten) gadgets
    return result {gadgetView = [WebitItem table1]}
  where
    headerRows :: [(Text, [(Text, a)])] -> H.Html
    headerRows [] = mempty
    headerRows [(_,columns)] = H.tr $ mapM_ (H.th . H.text . fst) columns
    headerRows ((_, group1) : groups) = do
      H.tr $ do
        mapM_ ((H.th ! HA.rowspan "2") . H.text . fst) group1
        mapM_ headerSpan groups
      H.tr $ mapM_ (H.th . H.text . fst) $ concatMap snd groups
    headerSpan (_, []) = mempty
    headerSpan (grpHead, items) = H.th ! HA.colspan (H.toValue $ length items) $ H.text grpHead
    -- webitTree1 :: [GadgetF' e w a] -> [[GadgetF' e w a]] -> Tree (k, Lens1 s a)
    --    -> Elmer (Tree [GadgetOut (Either w1 (k, a -> a)) s])
    gadgetTree1 gs gss (Node (k, lns1) forest) = do
      subRows <- gadgetForest gss forest
      let
        n = sum $ map leafCount subRows
        attribs = [("rowspan", T.pack $ show n) | n > 1]
      cells <- forM gs $ \g -> do
        cell <- makeWebitContainer1 "td" attribs
        let
          g2 = focusingOver (getLens lns1) $
            G.send (Just . Right . (k,)) <<< sendMap (Just . Left) g
        contents <- renderGadget e i g2
        mapM_ (showWebit cell) $ gadgetView contents
        return contents {gadgetView = [WebitItem cell]}
      return $ Node cells subRows
    -- webitForest :: [[GadgetF' e w a]] -> Forest (k, Lens1 s a) ->
    --    Elmer (Forest [GadgetOut (Either w1 (k, a -> a)) s])
    gadgetForest [] _ = return []
    gadgetForest (gs:gss) nodes = mapM (gadgetTree1 gs gss) nodes
    assembleTreeRows :: Tree [a] -> [[a]]
    assembleTreeRows (Node gs cs) = case concatMap assembleTreeRows cs of
      [] -> [gs]
      r:rs -> gs <> r : rs
    leafCount :: Tree a -> Int
    leafCount (Node _ []) = 1
    leafCount (Node _ cs) = sum $ map leafCount cs

renderGadget e i (ForestEditor labelFunc menuFunc legalParent dSel) =
  {-# SCC "renderGadget_ForestEditor" #-} do
    env <- liftIO $ atomically $ tStateBehaviour e
    initial <- liftIO $ atomically $ tStateBehaviour i
    webit <- makeWebitContainer "forest-editor"
    bgMenu <- mkWebitMenu $ (FM.rootIndex,) <$> menuFunc Nothing
    let
      initialModel = FM.makeModel $ initial ^. gdValue
      wId = webitId webit
      h = webitHandle webit
      bgH = webitHandle bgMenu
    i1 <- tStateDupIO i
    dnd <- addWebitChannel webit  -- Gets the whole updated tree.
    (initialJS, ms) <- jsonModel env initialModel
    currentModel <- liftIO $ newTVarIO initialModel
    currentMenus <- liftIO $ newTVarIO ms
    (output, outWriter) <- newTStateIO initial
    elmerSend $ jsSetup h wId bgH initialJS
    -- If a DialogSelector is provided then process double-click events.
    forM_ dSel $ \selector -> do
      dblClicks <- addWebitChannel webit  -- Gets the ModelIndex of the double-clicked item.
      let
        getClick = do
          click <- readTChan dblClicks
          case readMaybe click of
            Just click2 -> do
              mdl <- readTVar currentModel
              case mdl ^? ix click2 of
                Just v -> return (click2, v)
                Nothing -> getClick
            Nothing -> getClick
      dialogOutputs <- mkWebitPopupSelect e selector getClick
      elmerDaemon webit $ do
        (k, v1) <- liftIO $ atomically $ readTChan dialogOutputs
        forM_ v1 $ \v2 -> do
          oldModel <- liftIO $ readTVarIO currentModel
          let newModel = ix k .~ v2 $ oldModel
          elmerSend $ "{\
              \let n = $.ui.fancytree.getTree(" <> webitSelector wId <> ").\
                    \getNodeByKey(" <> value (show k) <>");\
              \if (n) {n.setTitle(" <> value (labelFunc env v2) <> ")};\
            \}"
          liftIO $ atomically $ do
            writeTVar currentModel newModel
            outWriter True $ GadgetData True $ newModel ^. from FM.modelIso
    -- Process context menu clicks.
    elmerDaemon webit $ do
      (idx, opr) <- liftIO $ atomically $ do
        oldMenus <- readTVar currentMenus
        foldr (orElse . readTChan . webitMenuOutput) retry $ bgMenu : oldMenus
      let newJS = JavaScript . LT.decodeLatin1 . encode
      oldModel <- liftIO $ readTVarIO currentModel
      case opr of
        TreeAddBefore newVal -> do
          let (mIdx, newModel) = FM.runEdit (FM.insert FM.InsertBefore idx newVal) oldModel
          forM_ mIdx $ \newIdx -> do
            (newNode, newMenu) <- jsonTree env $ Node (newIdx, newVal) []
            elmerSend $ if idx == FM.rootIndex
              then "{\
                  \let n = $.ui.fancytree.getTree(" <> webitSelector wId <> ").\
                      \getRootNode();\
                  \if (n.hasChildren()) {\
                      \n.addChildren(" <> newJS newNode <> ",0);\
                    \} else {\
                      \n.addChildren(" <> newJS newNode <> ");\
                    \};\
                \}"
              else "{\
                  \let n=$.ui.fancytree.getTree(" <> webitSelector wId <> ").\
                        \getNodeByKey(" <> value (show idx) <> ");\
                  \if (n) {n.parent.addChildren(" <> newJS newNode <> ",n);};\
                \}"
            liftIO $ atomically $ do
              writeTVar currentModel newModel
              modifyTVar currentMenus (newMenu ++)
              outWriter True $ GadgetData True $ newModel ^. from FM.modelIso
        TreeAddAfter newVal -> do
          let (mIdx, newModel) = FM.runEdit (FM.insert FM.InsertAfter idx newVal) oldModel
          forM_ mIdx $ \newIdx -> do
            (newNode, newMenu) <- jsonTree env $ Node (newIdx, newVal) []
            elmerSend $ if idx == FM.rootIndex
              then "{\
                  \$.ui.fancytree.getTree(" <> webitSelector wId <> ").\
                    \getRootNode().addChildren(" <> newJS newNode <> ");};\
                \}"
              else "{\
                \let n=$.ui.fancytree.getTree(" <> webitSelector wId <> ").\
                      \getNodeByKey(" <> value (show idx) <>");\
                \if (n) {\
                  \let s=n.getNextSibling();\
                  \if (s) {s.parent.addChildren(" <> newJS newNode <> ", s);}\
                    \else {n.parent.addChildren(" <> newJS newNode <> ");};\
                \};\
              \}"
            liftIO $ atomically $ do
              writeTVar currentModel newModel
              modifyTVar currentMenus (newMenu ++)
              outWriter True $ GadgetData True $ newModel ^. from FM.modelIso
        TreeAddIn newVal -> do
          let (mIdx, newModel) = FM.runEdit (FM.insert FM.AppendChild idx newVal) oldModel
          forM_ mIdx $ \newIdx -> do
            (newNode, newMenu) <- jsonTree env $ Node (newIdx, newVal) []
            elmerSend $ if idx == FM.rootIndex
              then "$.ui.fancytree.getTree(" <> webitSelector wId <> ").\
                    \getRootNode().addChildren(" <> newJS newNode <> ");"
              else "{\
                  \let n=$.ui.fancytree.getTree(" <> webitSelector wId <> ").\
                        \getNodeByKey(" <> value (show idx) <>");\
                  \if (n) {n.addChildren(" <> newJS newNode <> ");};\
                \}"
            liftIO $ atomically $ do
              writeTVar currentModel newModel
              modifyTVar currentMenus (newMenu ++)
              outWriter True $ GadgetData True $ newModel ^. from FM.modelIso
        TreeDelete -> do
          elmerSend $ "{\
              \let n=$.ui.fancytree.getTree(" <> webitSelector wId <> ").\
                    \getNodeByKey(" <> value (show idx) <>");\
              \if (n) {n.remove();};\
            \}"
          let
            newModel = FM.execEdit (FM.delete idx) oldModel
            newForest = newModel ^. from FM.modelIso
          liftIO $ atomically $ do
            writeTVar currentModel newModel
            outWriter True $ GadgetData True newForest
    -- Process D&D updates.
    elmerDaemon webit $ liftIO $ atomically $ do
      newIdxForest <- forestCatMaybe . map (fmap readMaybe) <$> readTChan dnd
      oldModel <- readTVar currentModel
      let
        newModel = FM.stableUpdate newIdxForest oldModel
        newForest = newModel ^. from FM.modelIso
      writeTVar currentModel newModel
      outWriter True $ GadgetData True newForest
    -- Process inputs from application.
    elmerDaemon webit $ do
      (newForest, oldModel) <- liftIO $ atomically $
        (,) <$> (snd <$> tStateEvent i1) <*> readTVar currentModel
      let changed = newForest ^. gdValue /= oldModel ^. from FM.modelIso
      when changed $ do
        oldMenus <- liftIO $ readTVarIO currentMenus
        mapM_ (deleteWebit . webitMenuDisplay) oldMenus
        let newModel = FM.makeModel $ newForest ^. gdValue
        (newJs, newMenus) <- jsonModel env newModel
        elmerSend $ "(" <> webitSelector wId <> ").\
            \reload(" <> JavaScript (LT.decodeLatin1 $ encode newJs) <> ")"
        liftIO $ atomically $ do
          writeTVar currentModel newModel
          writeTVar currentMenus newMenus
    return GadgetOut {
        gadgetView = [WebitItem webit],
        gadgetDomCode = return (),
        gadgetEvents = retry,
        gadgetOutput = output
      }
  where
    jsonForest env nodes = second concat . unzip <$> mapM (jsonTree env) nodes
    jsonTree env (Node (k, v) cs) = do
      menu <- mkWebitMenu $ fmap (k,) $ menuFunc $ Just v
      case cs of
        [] -> return (
            object [
                "title" .= labelFunc env v,
                "key" .= show k,
                "data" .= object [
                    "legalParent" .= legalParent v,
                    "menu" .= webitId (webitMenuDisplay menu)
                  ]
              ],
            [menu]
          )
        _ -> do
          (cs1, menus) <- jsonForest env cs
          return (object [
                "title" .= labelFunc env v,
                "key" .= show k,
                "data" .= object [
                    "legalParent" .= True,
                    "menu" .= webitId (webitMenuDisplay menu)
                  ],
                "folder" .= True,
                "children" .= cs1
              ],
              menu : menus
            )
    jsonModel env = jsonForest env . FM.evalEdit FM.getForest . FM.mapWithIndex (,)
    jsSetup h wId bgH initialJS = "{\
        \$(" <> var h <> ").fancytree({\
            \extensions: ['dnd5'],\
            \expanded: true,\
            \nodata: false,\
            \dnd5: {\
                \effectAllowed: 'move',\
                \preventForeignNodes: true,\
                \preventNoneNodes: true,\
                \dragStart: function (){return true;},\
                \dragEnter: function (node,data){return(legalDrop (node, data));},\
                \dragDrop: function(node,data){\
                    \if (legalDrop (node, data)) {\
                        \data.otherNode.moveTo(node, data.hitMode);" <>
                        webitEventData wId
                            "fancyToForest(data.tree.rootNode.getChildren())" <>
                      "} else {return(false);}\
                  \}\
              \},\
            \source:" <> JavaScript (LT.decodeLatin1 $ encode initialJS) <> ",\
            \create: function (ev,data){\
                \this.addEventListener('contextmenu',function(event){\
                    \event.preventDefault();\
                    \let z=ancestorZ(" <> value h <> ");\
                    \$(" <> value bgH <> ").finish().toggle(100).css({\
                        \top:event.pageY+'px',\
                        \left:event.pageX+'px',\
                        \zIndex: z+1\
                      \});\
                  \});\
              \},\
            \createNode: function(ev,data){" <>
                (if isNothing dSel then mempty else
                  "data.node.span.addEventListener('dblclick',function(clickEvent){\
                      \clickEvent.preventDefault();" <>
                      webitEventData wId "data.node.key" <> ";\
                    \});"
                ) <>
                "data.node.span.addEventListener('contextmenu',function(event){\
                    \event.preventDefault();\
                    \let z=ancestorZ(data.node.span);\
                    \$('#'+data.node.data.menu).finish().toggle(100).css({\
                        \top:event.pageY+'px',\
                        \left:event.pageX+'px',\
                        \zIndex: z+1\
                      \});\
                  \});\
              \}\
          \});\
      \}"

renderGadget _ _ FilePathSelector {} = error "Unimplemented FilePathSelector"

renderGadget env i (Scrolled g) = {-# SCC renderGadget_Scrolled #-} do
  result <- renderGadget env i g
  case gadgetView result of
    [] -> return result
    [item] -> do
      elmerSend $ addClass item "scrolled"
      return result
    items -> do
      r <- makeWebitContainer "scrolled"
      mapM_ (showWebit r) items
      return result {gadgetView = [WebitItem r]}

renderGadget _ _ (ButtonBar []) =
  let st = TState (pure $ GadgetData True id) retry $ return st
  in return GadgetOut {
      gadgetView = [],
      gadgetDomCode = return (),
      gadgetEvents = retry,
      gadgetOutput = st
    }

renderGadget _ _ (ButtonBar buttons) = {-# SCC renderGadget_ButtonBar #-} do
    webit <- makeWebitContainer "horizontal"
    outChan <- liftIO newBroadcastTChanIO
    clicks <- forM buttons $ \(lbl, updateF) -> do
      btn <- makeWebit (btnHtml lbl) (\_ _ _ -> mempty) ()
      showWebit webit btn
      return $ updateF <$ tStateEvent (webitState btn)
    elmerDaemon webit $ liftIO $ atomically $ do
      clicks2 <- foldr1 orElse clicks   -- Safe because buttons /= [], so clicks /= []
      writeTChan outChan (True, GadgetData True clicks2)
    outChan1 <- liftIO $ atomically $ dupTChan outChan
    let
      dup = do
        newE <- dupTChan outChan
        return st {tStateEvent = readTChan newE}
      st = TState (pure $ GadgetData True id) (readTChan outChan1) dup
    return GadgetOut {
        gadgetView = [WebitItem webit],
        gadgetDomCode = return (),
        gadgetEvents = retry,
        gadgetOutput = st
      }
  where
    btnHtml lbl wId = H.button
      ! HA.type_ "button"
      ! HA.onclick (jsValue $ webitEvent wId ())
      $ H.toHtml lbl

renderGadget e i (ButtonIO lbl actionF) = do
    webit <- makeWebit btnHtml (\_ _ _ -> mempty) ()
    (output, outWriter) <- newTStateIO $ GadgetData True Nothing
    elmerDaemon webit $ liftIO $ do
      act <- atomically $ do
        _ <- tStateEvent (webitState webit)
        env <- tStateBehaviour e
        inp <- tStateBehaviour i
        return $ actionF env (inp ^. gdValue)
      r <- act
      atomically $ outWriter True $ GadgetData True $ Just r
    return GadgetOut {
        gadgetView = [WebitItem webit],
        gadgetDomCode = return (),
        gadgetEvents = retry,
        gadgetOutput = output
      }
  where
    btnHtml wId = H.button
      ! HA.type_ "button"
      ! HA.onclick (jsValue
      $ webitEvent wId ()) $ H.toHtml lbl

renderGadget _ i (Image path) = do
    i1 <- tStateDupIO i
    webit <- makeWebit imgHtml (\_ _ _ -> mempty) (0 :: Double, 0 :: Double)
    elmerSend $ var (webitHandle webit) <> ".addEventListener('click', function(ev){" <>
        webitEventData (webitId webit) "[ev.offsetX, ev.offsetY]" <>
      "})"
    (output, outWriter) <- newTStateIO $ GadgetData True (0,0)
    elmerDaemon webit $ liftIO $ atomically $ (do
        (dx,dy) <- snd <$> tStateEvent (webitState webit)
        -- Can't be sure web client will send Ints, so allow for decimals.
        outWriter True $ GadgetData True (round dx, round dy)
      ) `orElse` (do
        (userFlag, v) <- tStateEvent i1
        outWriter userFlag v
      )
    return GadgetOut {
        gadgetView = [WebitItem webit],
        gadgetDomCode = return (),
        gadgetEvents = retry,
        gadgetOutput = output
      }
  where
    imgHtml = const $ H.img ! HA.src (H.stringValue path)

renderGadget e i (Trace msg inF outF g) = do
    inner <- renderGadget e i g
    i1 <- liftIO $ atomically $ tStateDup i
    o1 <- liftIO $ atomically $ tStateDup $ gadgetOutput inner
    top <- asks elmerTop
    elmerDaemon top $ liftIO $ do
      dat <- atomically $
        (Left <$> tStateEvent i1) `orElse`
        (Right <$> ((,) <$> tStateBehaviour i1 <*> tStateEvent o1))
      case dat of
        Left (b, v) ->
          putStrLn $ msg <> " input = " <> show (b, v ^. gdValue . to inF)
        Right (iv, (b, ov)) ->
          putStrLn $ msg <> " output = " <> show (b, outF (iv ^. gdValue) (ov ^. gdValue))
    return inner



-- | Create a tab within a tabbed form and add it to the argument containers.
makeTab ::
  TState Bool e      -- ^ Environment
  -> TState Bool (GadgetData i)  -- ^ Input
  -> WebitContainer   -- ^ Header container.
  -> WebitContainer   -- ^ Tab body container.
  -> (Text, Gadget e w i o) -- ^ Tab label and gadget pair.
  -> Elmer (GadgetOut w o)
makeTab env i tabHeader tabContainer (lbl, g) = do
  tab <- renderGadget env i g
  tabWebit <- case gadgetView tab of
    [] -> WebitItem <$> makeWebStatic (H.text "")
      -- Empty tab; should not happen, but play it straight.
    [w] -> return w
    ws -> do
      c <- makeWebitContainer "tab-body"
      mapM_ (showWebit c) ws
      return $ WebitItem c
  btn <- makeWebStatic $ H.button
    ! HA.type_ "button"
    ! HA.onclick (jsValue $ "openTab(event, " <>
        value (webitId tabWebit) <> "," <>
        var (webitHandle tabHeader) <> "," <>
        var (webitHandle tabContainer) <> ")")
    $ H.text lbl
  showWebit tabHeader btn
  showWebit tabContainer tabWebit
  return tab


-- | Debugging function.
showOpr :: (a -> Text) -> TreeOper a -> String
showOpr f (TreeAddBefore v) = "TreeAddBefore " <> show (f v)
showOpr f (TreeAddAfter v) = "TreeAddAfter " <> show (f v)
showOpr f (TreeAddIn v) = "TreeAddIn " <> show (f v)
showOpr _ TreeDelete = "TreeDelete"
