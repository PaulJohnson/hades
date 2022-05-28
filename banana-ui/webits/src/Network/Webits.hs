{-
Copyright © Paul Johnson 2021. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.
-}

{-
Webits are a composable dynamic GUI for web applications. Javascript is pushed to the
client, from where it returns events via web sockets. Each Webit has a UUID which is used to route
event data to the appropriate TChan.

When a new client connects, use "Network.JavsScript.start" to get an "Engine". Pass this to
"makeElmerEnv" to get a @TVar@ holding the local state for that client. Then call "runElmer" with
an action to create the initial state of the GUI on the client.

__WARNING:__ Do not use recursive @do@ notation (@mdo@) with Webits; it seems to cause threading
to fail.
-}
module Network.Webits (
  -- * Elmer Monad
  WebitId,
  ElmerEnv (elmerEngine, elmerTop, elmerBase, elmerSendJS, elmerIconMenu, elmerLogQ, elmerZ),
  elmerSend,
  makeElmerEnv,
  Elmer,
  runElmer,
  elmerLog,
  elmerLogSTM,
  elmerLogIO,
  Priority (..),
  logLine,
  logToStderr,
  forkElmer,
  elmerDaemon,
  elmerStatic,
  elmerScotty,
  newWebitId,
  -- * Webits
  WebitClass (..),
  WebitItem (..),
  webitTree,
  WebitContainer (..),
  makeWebitContainer,
  makeWebitContainer1,
  addWebitChannel,
  addWebitChannelWith,
  flushTChan,
  WebStatic (..),
  makeWebStatic,
  Webit (..),
  makeWebit,
  setWebitHtml,
  webitEvent,
  deleteWebit,
  showWebit,
  hideWebit,
  -- * Icons in Webits
  IconName,
  iconPic,
  iconHtml,
  getIconChoice,
  -- * Webit JavaScript Utilities
  jsNoOp,
  htmlValue,
  jsValue,
  setAttribute,
  deleteAttribute,
  addClass,
  removeClass,
  webitSelector,
  webitSelector1,
  webitEventData,
  -- * Re-export
  liftIO
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.TState
import Control.Exception
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Function
import Data.IORef
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Tree
import qualified Data.UUID as U
import Data.UUID.Generate
import GHC.Conc
import GHC.Stack
import Network.JavaScript as JS
import Network.Wai
import Network.Webits.Icons
import Network.Webits.StyleSheets
import Paths_webits
import System.Directory
import System.FilePath
import System.IO
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Web.Scotty

import Debug.Trace


type WebitId = U.UUID


-- | Private type: wrap a value up with a UUID
data WebitEvent a = WebitEvent WebitId a

instance (ToJSON a) => ToJSON (WebitEvent a) where
  toJSON (WebitEvent d v) = object ["d" .= d, "v" .= v]

instance (FromJSON a) => FromJSON (WebitEvent a) where
  parseJSON = withObject "Webit Event" $ \v -> WebitEvent <$> v .: "d" <*> v .: "v"


-- | Logging priority.
data Priority =
  DEBUG                   -- ^ Debug messages
  | INFO                    -- ^ Information
  | NOTICE                  -- ^ Normal runtime conditions
  | WARNING                 -- ^ General Warnings
  | ERROR                   -- ^ General Errors
  | CRITICAL                -- ^ Severe situations
  | ALERT                   -- ^ Take immediate action
  | EMERGENCY               -- ^ System is unusable
  deriving (Eq, Ord, Enum, Bounded, Show, Read)


-- | Log message format.
data LogItem = LogItem {
    logPriority :: Priority,
    logLocation :: CallStack,
    logMessage :: Text
  } deriving (Show)


-- | One-line representation of a log item.
logLine :: LogItem -> Text
logLine item =
    T.pack (show $ logPriority item) <> " " <> locStr <> ": " <> logMessage item
  where
    locStr = case getCallStack $ logLocation item of
      (nm, loc) : _ ->
        T.pack $ srcLocModule loc <> "." <> nm <> ":" <> show (srcLocStartLine loc)
      _ -> "(unknown location)"


-- | Channel the log to Standard Error.
logToStderr :: Elmer ()
logToStderr = forkElmer $ forever $ do
  q <- asks elmerLogQ
  liftIO $ do
    msg <- atomically $ readTQueue q
    T.hPutStrLn stderr $ logLine msg


-- | Exceptions inside Elmer actions.
data ElmerException =
  ElmerDecodeError Value String  -- ^ JSON received was not what was expected.
  | ElmerUnknownWebit WebitId     -- ^ Received a @WebitId@ that does not exist.
  | ElmerExhaustedIds    -- ^ The supposedly infinite list of WebitIds was exhausted.

instance Show ElmerException where
  show (ElmerDecodeError v msg) = "Could not decode JSON from client: " <> msg <> " in " <> show v
  show (ElmerUnknownWebit w) = "Event from unknown Webit: " <> show w
  show ElmerExhaustedIds = "Ran out of unique IDs. This is supposed to be impossible."

instance Exception ElmerException


{- | The internal environment of the Webit GUI shown on a web browser.

When a Webit is updated the resulting Javascript command is sent via the @elmerSend@ function.
This avoids the risk of race conditions between updates to the internal state of the Webit and the
update to the visible state in the web browser.

There is no problem using @send@ in other contexts.
-}
data ElmerEnv = ElmerEnv {
    elmerEngine :: Engine,
    elmerTop :: WebitContainer,
      -- ^ Top level container into which GUI components should be inserted. Always part of
      -- the DOM.
    elmerBase :: WebitContainer,
      -- ^ Bottom of the visibility stack, representing the HTML page where this is happening.
    elmerSendJS :: JavaScript -> STM (),
      -- ^ Javascript commands which are sent to the @Engine@ from within a transaction.
    elmerIds :: TVar [WebitId],
      -- ^ An infinite list of random UUIDs available from inside transactions.
    elmerRoutes :: TVar (Map WebitId [Value -> STM Bool]),
      -- ^ Actions to take when receiving events. The value is passed to each function until
      -- one of them returns True.
    elmerLogQ :: TQueue LogItem,
      -- ^ Logging queue.
    elmerIconMenu :: (Webit IconName, TChan Bool),
      -- ^ Global icon menu. This is a fixed menu, and building it is expensive. So it makes
      -- sense to do it once and make it available here. The @TChan@ carries clicks from the
      -- dialog buttons: @True@ for OK, @False@ for cancel.
    elmerZ :: Int
      -- ^ Z index of current container.
  }
-- Design Note: elmerIconMenu MUST remain non-strict. See makeElmerEnv.


elmerSend :: JavaScript -> Elmer ()
elmerSend js = do
  s <- asks elmerSendJS
  liftIO $ atomically $ s js


-- | Create an initial state for the Elmer monad, including a thread which serialises Webit
-- updates via "elmerSend".
makeElmerEnv :: HasCallStack =>
  LT.Text    -- ^ Identity of the top-level container in the HTML document shown on the browser.
  -> [IconGroup]
  -> Engine
  -> IO ElmerEnv
makeElmerEnv topName iconData eng = do
    pipe <- newTQueueIO
    logQ <- newTQueueIO
    ids <- uuidStreamIO >>= newTVarIO
    top <- send eng $ constructor $ "document" <> "." <> call "getElementById" [JS.string topName]
    topParent <- newTVarIO Nothing
      -- Delete the top element on the client once the server forgets it.
    topChildren <- newTVarIO []
    final <- newIORef $ return ()
    -- Construct a temporary ElmerEnv and bootstrap the Icon menu webit with it.
    routes1 <- newTVarIO mempty
    let
      baseContainer = WebitContainer top U.nil topParent topChildren final
      env1 = ElmerEnv
        eng
        baseContainer
        baseContainer
        (writeTQueue pipe)
        ids
        routes1
        logQ
        undefined  -- Safe because this is the value we are going to construct.
        0
    (iconMenuWebit, iconMenuClicks, routes) <- runElmer env1 $ do
        (w, c) <- createIconMenu iconData
        showWebit baseContainer w
        r <- asks elmerRoutes  -- Need to capture the iconMenu routing.
        return (w, c, r)
    threadId <- forkIO $ forever $ join $ atomically $
        sendElmerCommand pipe `orElse` receiveElmerEvent routes
    labelThread threadId "makeElmerEnv command pipe"
    send eng $ command $ iconDialogJS $ webitId iconMenuWebit
    return $ ElmerEnv
        eng
        baseContainer
        baseContainer
        (writeTQueue pipe)
        ids
        routes
        logQ
        (iconMenuWebit, iconMenuClicks)
        0
  where
    sendElmerCommand pipe = do
      js <- readTQueue pipe
      return $ send eng $ command js  -- Returns an IO action
    receiveElmerEvent routes = do
      (ev, _) <- readEventChan eng
      traceM $ "Elmer event received: " <> show ev
      case parseEither parseJSON ev of
        Left msg -> throwSTM $ ElmerDecodeError ev msg
        Right (WebitEvent d v) -> do
          rt <- readTVar routes
          case M.lookup d rt of
            Nothing -> throwSTM $ ElmerUnknownWebit d
            Just fs -> offerList fs v
      return (return ())
    offerList [] v = throwSTM $ ElmerDecodeError v "Client value not accepted"
    offerList (f:fs) v = f v >>= \case
      True -> return ()
      False -> offerList fs v
    iconDialogJS ::  WebitId -> JavaScript
    iconDialogJS mId = "$("<> webitSelector mId <> ")\
      \.dialog({\
          \modal:true,\
          \autoOpen:false,\
          \width:700,\
          \buttons:{'Ok':function (){\
              \" <> webitEventData mId "true" <> ";\
              \$(this).dialog('close');\
            \}},\
          \close:function () {\
              \" <> webitEventData mId "false" <> ";\
              \$(this).dialog('close');\
            \}\
        \})\
      \.tabs();"


-- | Create a menu of icons in a tabbed UI, with one tab for each icon group.
--
-- This should not be called in user code. Every invocation reads the icons from disk, and also
-- causes the client browser to download the full set. Instead you should access the pre-loaded
-- value stored in 'elmerIconMenu'.
createIconMenu :: [IconGroup] -> Elmer (Webit IconName, TChan Bool)
createIconMenu iconsData = do
    groups <- do
      case iconsData of
        [] -> return [IconGroup "No icons available" mempty]
        _ -> return iconsData
      -- Ensure 'groups' is not empty.
    let (names, contents1) = unzip $ map groupHtml groups
    result <- makeWebit
        (\wId -> tabs names (map ($ wId) contents1) wId)
        (updateIcon groups)
        "no-icon"
    selection <- tStateDupIO $ webitState result
    elmerDaemon result $ do
      clickedIcon <- liftIO $ atomically $ snd <$> tStateEvent selection
      elmerSend $ updateIcon groups clickedIcon (webitHandle result) (webitId result)
    clickChan <- addWebitChannel result
    return (result, clickChan)
  where
    locateIcon grps icon = do  -- Maybe monad
      gn <- findIndex (M.member icon . iconGroupContents) grps
      let g = M.keys $ iconGroupContents $ grps !! gn
      n <- elemIndex icon g
      return (gn, n)
    updateIcon groups icon _ wId1 =
      let unhighlight =
          "$(" <> webitSelector wId1 <> " + ' img.icon-display').removeAttr('style');"
      in case locateIcon groups icon of
        Nothing -> unhighlight
        Just (gn, n) -> "{" <> unhighlight <>
            "$(" <> webitSelector1 wId1 (T.pack $ "_" <> show gn) <> " + ' img')\
              \.eq(" <> JavaScript (LT.pack $ show n) <> ")\
              \.css('border-color','#cc6600');\
            \$(" <> webitSelector wId1 <> ").tabs ('option', 'active', " <> value gn <> ");\
          \}"
    tabs names contents1 wId = do
      H.ul $ zipWithM_ (\txt n -> H.li $ H.a
        ! HA.href (H.stringValue $ "#" <> show wId <> "_" <> show n)
        $ H.text txt) names [0 :: Int ..]
      zipWithM_ (\c n -> H.div
        ! HA.id (H.stringValue $ show wId <> "_" <> show n)
        $ c ) contents1 [0 :: Int ..]
    groupHtml :: IconGroup -> (Text, WebitId -> H.Html)
    groupHtml grp =
      (iconGroupName grp, \wId -> mconcat $ map (iconPic wId) $ M.keys $ iconGroupContents grp)


-- | Monad for managing and updating Webbits.
--
-- Elmer actions can be run in multiple threads and associated with multiple different clients.
-- The environment associated with a single client is the @ElmerEnv@, which is made
-- available via this monad.
--
-- <https://en.wikipedia.org/wiki/Elmer_Fudd>
newtype Elmer a = Elmer (ReaderT ElmerEnv IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadReader ElmerEnv)


runElmer :: HasCallStack => ElmerEnv -> Elmer a -> IO a
runElmer env (Elmer action) = runReaderT action env


-- | Similar to "forkIO", but for @Elmer@.
forkElmer :: HasCallStack => Elmer () -> Elmer ()
forkElmer act = do
  s <- ask
  void $ liftIO $ forkIO $ runElmer s act


elmerLog :: HasCallStack => Priority -> Text -> Elmer ()
elmerLog pri msg = do
  logQ <- asks elmerLogQ
  liftIO $ atomically $ writeTQueue logQ $ LogItem pri callStack msg


elmerLogSTM :: ElmerEnv -> Priority -> Text -> STM ()
elmerLogSTM s pri msg = writeTQueue (elmerLogQ s) $ LogItem pri callStack msg


elmerLogIO :: ElmerEnv -> Priority -> Text -> IO ()
elmerLogIO s pri msg = atomically $ elmerLogSTM s pri msg


-- | Run an Elmer action 'forever' in a separate thread. If the action throws an exception
-- then the exception is logged (currently just to stderr). If the exception was an 'ErrorCall' or
-- an 'ElmerException' then the action is restarted after waiting for 1 second.
--
-- Each daemon is tied to a webit. When the webit is finalised the daemon will be killed.
elmerDaemon :: (WebitClass w) => w -> Elmer () -> Elmer ()
elmerDaemon owner action = do
    s <- ask
    let
      hError :: ErrorCall -> IO ()
      hError (ErrorCallWithLocation msg loc) = do
        atomically $ writeTQueue (elmerLogQ s) $
          LogItem ERROR callStack (T.pack $ loc <> ": " <> msg)
        threadDelay sec
      hElmer :: ElmerException -> IO ()
      hElmer e = do
        elmerLogIO s ERROR $ T.pack $ show e
        threadDelay sec
      sec = 1000000  -- 1 second in microseconds.
      reportExit :: SomeException -> IO a
      reportExit e = do
        elmerLogIO s INFO $ T.pack $ "Daemon thread exited: " <> show e
        throwIO e
    thread <- liftIO $ forkIO $ forever $
      handle reportExit $
      handle hError $
      handle hElmer $
      runElmer s action
    liftIO $ labelThread thread $ show $ webitId owner
    webitAddFinaliser owner $ liftIO $ killThread thread
      -- Kill the thread once the owner is finalised.


-- | Serve the static files. The application may serve several
-- 'elmerScotty' instances, but it should have exactly one @elmerStatic@.
--
-- The files served are:
--
--    * The FavIcon.
--
--    * The icons specified in the argument, under the @/icon@ folder.
--
--    * Project data files (using 'getDataFileName') under the @/assets@ folder. The content type
--      is set using the file suffix for Text, JavaScript, CSS, HTML, PDF, PNG, SVG and JPEG files.
elmerStatic :: HasCallStack => [IconGroup] -> ScottyM ()
elmerStatic iconGroups = do
    scottyIcons iconGroups
    get (function assets) $ do
      assetPath <- param "assetPath"
      filePath <- liftIO $ getDataFileName assetPath >>= checkSuffixes ["", ".js"]
      case takeExtension filePath of
        ".js" ->   setHeader "Content-Type" "text/javascript"
        ".css" ->  setHeader "Content-Type" "text/css"
        ".html" -> setHeader "Content-Type" "text/html"
        ".htm" ->  setHeader "Content-Type" "text/html"
        ".png" ->  setHeader "Content-Type" "image/png"
        ".svg" ->  setHeader "Content-Type" "image/svg+xml"
        ".jpg" ->  setHeader "Content-Type" "image/jpeg"
        ".jpeg" -> setHeader "Content-Type" "image/jpeg"
        ".txt" ->  setHeader "Content-Type" "text/plain"
        ".pdf" ->  setHeader "Content-Type" "application/pdf"
        _ -> do

          return ()  -- As per RFC-7231
      file filePath
  where
    pathSep = T.singleton pathSeparator
    assets :: Request -> Maybe [Param]
    assets rq = case pathInfo rq of
        "assets" : rest ->
          return [("assetPath", LT.fromStrict $ T.intercalate pathSep rest)]
        _ -> Nothing
    -- Some JS files are requested without a suffix. Hence we check both.
    checkSuffixes [] base = return base
    checkSuffixes (x:xs) base = do
      let fn = base <> x
      doesFileExist fn >>= \case
        True -> return fn
        False -> checkSuffixes xs base



-- | Run an Elmer action as a Scotty web page, and then wait for events. Events are routed to
-- the Webits that trigger them.
--
-- Typically the return value will be a "TVar", "TChan" or similar STM communication primitive.
elmerScotty :: HasCallStack =>
  RoutePattern  -- ^ The name of the page that will load this application.
  -> H.Html    -- ^ HTML to go in the @<head>@ tag of the page.
  -> [IconGroup]  -- ^ Icons required by the GUI.
  -> Elmer ()   -- ^ The initial configuration of the GUI.
  -> ScottyM ()
elmerScotty route htmlHead iconData action = do
    middleware $ start app
    baseServer
  where
    mainName = "main"
    app eng = do
      env <- makeElmerEnv mainName iconData eng
      runElmer env action
    baseServer = get route $ html $ renderHtml $ H.html $ do
      H.head $ do
        htmlHead
        -- Import bundled Fancytree core, jQuery and associated CSS.
        H.link
        ! HA.href "//cdn.jsdelivr.net/npm/jquery.fancytree@2.38/dist/skin-win8/\
                    \ui.fancytree.min.css"
        ! HA.rel "stylesheet"
        H.link ! HA.href "assets/JQuery/jquery-ui.min.css" ! HA.rel "stylesheet"
        H.script ! HA.src "assets/JQuery/external/jquery/jquery.js" $ ""
        H.script ! HA.src "assets/JQuery/jquery-ui.min.js" $ ""
        H.script ! HA.src "//cdn.jsdelivr.net/npm/jquery.fancytree@2.38/dist/\
                      \jquery.fancytree-all.min.js" $ ""
        -- H.script ! HA.src "/assets/JS/AppendGrid.js" $ ""
        H.script ! HA.type_ "module" ! HA.src "/assets/JS/AppendGrid-dev.js" $ ""
        H.script ! HA.src "assets/JS/arrowDialog.js" $ ""
        standardHeader
      H.body $ do
        H.script ! HA.type_ "module" $ "\
          \import AppendGrid from '/assets/JS/AppendGrid-dev.js';\
          \window.jsb = {ws: new WebSocket('ws://' + location.host)};\
          \jsb.debug=true;\
          \jsb.ws.onmessage = (evt) => eval(evt.data);"
        H.noscript "Sorry, but this application requires JavaScript to be enabled."
        H.div ! HA.id (lazyTextValue mainName) $ return ()


-- | Get the next ID.
newWebitId :: Elmer WebitId
newWebitId = do
  tv <- asks elmerIds
  liftIO $ atomically $ stateTVar tv $ \case
    i:rest -> (i, rest)
    [] -> throw ElmerExhaustedIds  -- Can't happen.


-- | Things that can appear inside WebitContainers.
class WebitClass a where
  webitHandle :: a -> RemoteValue Html
    -- ^ The remote representation of this Webit in the browser.
  webitId :: a -> WebitId
    -- ^ The unique ID of this Webit.
  webitParent :: a -> TVar (Maybe WebitContainer)
    -- ^ The current parent (if any) of this Webit.
  webitChildren :: a -> STM [WebitItem]
    -- ^ The children (if any) of this Webit.
  webitFinalise :: a -> Elmer ()
    -- ^ Run any finalisers.
  webitAddFinaliser :: a -> Elmer () -> Elmer ()
    -- ^ Add an extra action to be run on deletion of this Webit.


-- | Webits and containers stored in a single supertype.
data WebitItem = forall w . WebitClass w => WebitItem w

instance WebitClass WebitItem where
  webitHandle (WebitItem w) = webitHandle w
  webitId (WebitItem w) = webitId w
  webitParent (WebitItem w) = webitParent w
  webitChildren (WebitItem w) = webitChildren w
  webitFinalise (WebitItem w) = webitFinalise w
  webitAddFinaliser (WebitItem w) = webitAddFinaliser w


-- | The tree of Webits currently below the argument.
webitTree :: WebitItem -> STM (Tree WebitItem)
webitTree w = do
  cs <- mapM webitTree =<< webitChildren w
  return $ Node w cs


-- | A container in the remote DOM into which Webits can be inserted. The @Eq@ instance only
-- compares IDs.
data WebitContainer = WebitContainer {
    webitContainerHandle :: RemoteValue Html,
    webitContainerId :: WebitId,
    webitContainerParent :: TVar (Maybe WebitContainer),
    webitContainerChildren :: TVar [WebitItem],
    webitContainerFinal :: IORef (Elmer ())
  }

instance WebitClass WebitContainer where
  webitHandle = webitContainerHandle
  webitId = webitContainerId
  webitParent = webitContainerParent
  webitChildren = readTVar . webitContainerChildren
  webitFinalise = join . liftIO . readIORef . webitContainerFinal
  webitAddFinaliser w act = liftIO $ modifyIORef (webitContainerFinal w) (>> act)



-- | Create a new container for Webits. The container is initially empty and has no parents.
makeWebitContainer ::
  Text   -- ^ CSS class name for @div@
  -> Elmer WebitContainer
makeWebitContainer className = do
  eng <- asks elmerEngine
  uuid <- newWebitId
  final <- liftIO $ newIORef $ return ()
  liftIO $ do
    parent <- newTVarIO Nothing
    childs <- newTVarIO []
    rv <- send eng $ do
      rv <- constructor $ call "document.createElement" [value ("div" :: Text)]
      command $ var rv <> "." <> call "setAttribute"
          [value ("id" :: Text), value $ U.toText uuid]
      command $ var rv <> "." <> call "setAttribute"
          [value ("class" :: Text), value className]
      return rv
    return $ WebitContainer rv uuid parent childs final


-- | Create a new container with arbitrary HTML. The container is initially empty and has no
-- parents.
makeWebitContainer1 ::
  Text  -- ^ HTML tag.
  -> [(Text, Text)]  -- ^ Attributes and values. @id@ will be ignored.
  -> Elmer WebitContainer
makeWebitContainer1 tagName attrs = do
  eng <- asks elmerEngine
  uuid <- newWebitId
  final <- liftIO $ newIORef $ return ()
  liftIO $ do
    parent <- newTVarIO Nothing
    childs <- newTVarIO []
    rv <- send eng $ do
      rv <- constructor $ call "document.createElement" [value tagName]
      forM_ attrs $ \(attr, val) ->
        command $ var rv <> "." <> call "setAttribute" [value attr, value val]
      command $ var rv <> "." <> call "setAttribute"
        [value ("id" :: Text), value $ U.toText uuid]
      return rv
    return $ WebitContainer rv uuid parent childs final


-- | Add a new channel for the Webbit to send events through. Events which do not
-- match the @TVChan@ are silently ignored. Multiple channels can receive each event.
addWebitChannel :: (FromJSON a, WebitClass w) => w -> Elmer (TChan a)
addWebitChannel = addWebitChannelWith parseJSON


-- | Add a new channel for the Webbit, where the output type does not have a 'FromJSON' instance.
addWebitChannelWith :: (WebitClass w) => (Value -> Parser a) -> w -> Elmer (TChan a)
addWebitChannelWith p c = do
  s <- ask
  liftIO $ do
    events <- newBroadcastTChanIO
    let
      eventAction x = case parse p x of
        Success v -> do
          writeTChan events v
          return True
        Error _ -> return False
    atomically $ do
      modifyTVar (elmerRoutes s) $ M.insertWith (<>) (webitId c) [eventAction]
      dupTChan events


-- | Return any pending items on the channel. Never retries.
flushTChan :: TChan a -> STM [a]
flushTChan c = tryReadTChan c >>= \case
        Just x -> (x:) <$> flushTChan c
        Nothing -> return []


-- | A GUI element constructed from static HTML.
data WebStatic = WebStatic {
    webStaticHandle :: ! (RemoteValue Html),
    webStaticId :: ! WebitId,
    webStaticParent :: ! (TVar (Maybe WebitContainer)),
    webStaticFinal :: ! (IORef (Elmer ()))
  }

instance WebitClass WebStatic where
  webitHandle = webStaticHandle
  webitId = webStaticId
  webitParent = webStaticParent
  webitChildren = const $ return []
  webitFinalise = join . liftIO . readIORef . webStaticFinal
  webitAddFinaliser w act = liftIO $ modifyIORef (webStaticFinal w) (>> act)


-- | Construct a static fragment of a Webit inside a @div@.
makeWebStatic :: H.Html -> Elmer WebStatic
makeWebStatic fragment = do
  eng <- asks elmerEngine
  uuid <- newWebitId
  final <- liftIO $ newIORef $ return ()
  rv <- liftIO $ do
    send eng $ do
      rv <- constructor $ call "document.createElement" [value ("div" :: Text)]
      command $ var rv <> "." <> call "setAttribute"
          [value ("id" :: Text), value $ U.toText uuid]
      command $ var rv <> ".innerHTML=" <> value (renderHtml fragment)
      return rv
  parent <- liftIO $ newTVarIO Nothing
  return $ WebStatic rv uuid parent final


-- | A GUI element that can appear in a web page.
data Webit a = Webit {
    webitHandle1 :: ! (RemoteValue Html),
      -- ^ The remote representation of this Webit in the browser. Points to a @<div>@ which
      -- has the @id@ and the HTML as its contents.
    webitIn :: ! ((Eq a) => a -> STM ()),
      -- ^ Call this to update the Webit with new data. No-op if the new data equals the current.
      -- This will not trigger an event on the @webitState@ but will update the behaviour.
    webitState :: ! (TState () a),
      -- ^ Client event data from the Webit will appear on this channel. Data from @webitIn@
      -- will update the behaviour but not trigger an event.
    _webitCurrent :: TVar a,
      -- ^ The current input value of this Webit.
    webitId1 :: ! WebitId,
      -- ^ The unique ID of this Webit.
    webitParent1 :: ! (TVar (Maybe WebitContainer)),
      -- ^ The parent (if any) of this Webit.
    webitFinal :: ! (IORef (Elmer ()))
  }

instance WebitClass (Webit a) where
  webitHandle = webitHandle1
  webitId = webitId1
  webitParent = webitParent1
  webitChildren = const $ return []
  webitFinalise = join . liftIO . readIORef . webitFinal
  webitAddFinaliser w act = liftIO $ modifyIORef (webitFinal w) (>> act)


{- | Send the HTML to the engine and prepare to receive inputs and outputs. The HTML will
automatically be wrapped in a @<div>@ identified by the @WebitId@.

Many webits can just ignore their ID unless there is other JavaScript that is going to refer to
them.

Events sent from within the HTML must be wrapped in an outer JSON object with the fields @id@ for
the @WebitId@ and @value@ for the event data. This allows the event to be routed to the correct
TChan. The "webitEvent" function does this automatically.
-}
makeWebit :: (Eq a, ToJSON a, FromJSON a) =>
  (WebitId -> Html)    -- ^ The HTML to display this webit.
  -> (a -> RemoteValue Html -> WebitId -> JavaScript)
      -- ^ The remote update action for new values. This must not trigger an output event.
  -> a    -- ^ The initial value to display.
  -> Elmer (Webit a)
makeWebit fragment inputF initial = do
  s <- ask
  uuid <- newWebitId
  eng <- asks elmerEngine
  sendJS <- asks elmerSendJS
  liftIO $ do
    final <- newIORef $ return ()
    currentVal <- newTVarIO initial  -- The latest value sent to the Webit.
    events <- newBroadcastTChanIO   -- Event values sent by the Webit.
    eventsOut <- atomically $ dupTChan events
    rv <- send eng $ constructor $ call "document.createElement" [value ("div" :: Text)]
    send eng $ do
      command $ var rv <> "." <> call "setAttribute"
          [value ("id" :: Text), value $ U.toText uuid]
      let v = renderHtml $ fragment uuid
      command $ var rv <> ".innerHTML=" <> value v
      command $ inputF initial rv uuid
    let
      sendAction x = do
        old <- readTVar currentVal
        when (old /= x) $ do
          writeTVar currentVal x
          sendJS $ inputF x rv uuid
      eventAction x = case fromJSON x of
        Success v -> do
          writeTChan events ((), v)
          writeTVar currentVal v
          return True
        Error _ -> return False
      tState = TState {
          tStateBehaviour = readTVar currentVal,
          tStateEvent = readTChan eventsOut,
          tStateDup = do
            newEvents <- dupTChan events
            return $ TState (tStateBehaviour tState) (readTChan newEvents) (tStateDup tState)
        }
    parent <- newTVarIO Nothing
    atomically $ modifyTVar (elmerRoutes s) $ M.insert uuid [eventAction]
      -- uuid is fresh and unique, so we know that elmerRoutes does not contain it.
    return $ Webit rv sendAction tState currentVal uuid parent final


-- | Display a clickable icon.
iconPic :: WebitId -> IconName -> H.Html
iconPic wId v = iconHtml "icon-display" v $ Just wId


-- | HTML for an icon.
iconHtml ::
  Text   -- ^ Stye name. Typically @icon-inline@ or @icon-display@.
  -> IconName   -- ^ Icon name to display.
  -> Maybe WebitId    -- ^ On click an 'IconClick' will be sent to this Webit.
  -> H.Html
iconHtml sty nm wId = H.img
  ! HA.alt (H.textValue nm)
  ! HA.src (H.textValue $ iconUrl nm)
  ! HA.onclick (jsValue $ case wId of
    Just v -> webitEventData v "this.src.substring(this.src.lastIndexOf('/')+1)"
    Nothing -> jsNoOp)
  ! HA.class_ (H.textValue sty)


-- | Display the icon menu and return the icon selected by the user, or @Nothing@ if
-- the user clicks @Cancel@. The argument is the icon initially shown as selected.
getIconChoice :: IconName -> Elmer (Maybe IconName)
getIconChoice initial = do
  (menu, menuClosed) <- asks elmerIconMenu
  liftIO $ atomically $ do
    void $ flushTChan menuClosed  -- OK click causes multiple events, so flush stale ones.
    webitIn menu initial
  elmerSend $ "$(" <> webitSelector (webitId menu) <> ").dialog('open')"
  liftIO $ atomically $ do
    okFlag <- readTChan menuClosed
    if okFlag then Just <$> tStateBehaviour (webitState menu) else return Nothing


-- | Send an event to the Webbit from the remote client.
webitEvent :: (ToJSON v) => WebitId -> v -> JavaScript
webitEvent w v = event $ WebitEvent w v


-- | Replace the HTML in the webit.
setWebitHtml :: Html -> RemoteValue Html -> JavaScript
setWebitHtml html1 w = var w <> ".innerHTML=" <> value (renderHtml html1)


-- | Remove a Webit from the remote client and delete any event handlers. Recursively applied
-- to all current children. Using a Webit after deletion is an error.
deleteWebit :: (WebitClass a) => a -> Elmer ()
deleteWebit w = do
  e <- ask
  webitFinalise w
  liftIO $ send (elmerEngine e) $ command $ var (webitHandle w) <> ".remove()"
  targets <- liftIO $ atomically $ do
    targets <- flatten <$> webitTree (WebitItem w)
    forM_ targets $ \w1 -> do
      modifyTVar (elmerRoutes e) $ M.delete (webitId w1)
    return targets
  forM_ targets $ \w1 -> do
    liftIO $ send (elmerEngine e) $ JS.delete $ webitHandle w1
    webitFinalise w1


-- | Append a Webit to the container.
--
-- This does not let the same Webit appear in multiple places. If you add a Webit twice it will
-- disappear from the first location.
showWebit :: (WebitClass a) => WebitContainer -> a -> Elmer ()
showWebit c w = do
  e <- ask
  liftIO $ do
    send (elmerEngine e) $ command $
      var (webitHandle c) <> "." <> call "appendChild" [var $ webitHandle w]
    atomically $ do
      unparent w
      modifyTVar (webitContainerChildren c) (++ [WebitItem w])
      writeTVar (webitParent w) $ Just c


-- | Remove a Webit from the remote DOM.
hideWebit :: (WebitClass a) => a -> Elmer ()
hideWebit w = do
  e <- ask
  liftIO $ atomically $ do
    elmerSendJS e $ var (webitHandle w) <> ".remove()"
    unparent w


-- | Set the value of the named attribute of the webit.
setAttribute :: (WebitClass a, ToJSON b) => a -> T.Text -> b -> JavaScript
setAttribute item attr val =
  call (var (webitHandle item) <> ".setAttribute") [value attr, value val]

-- | Delete the named attribute from the webit.
deleteAttribute :: (WebitClass a) => a -> T.Text -> JavaScript
deleteAttribute item attr =
  call (var (webitHandle item) <> ".removeAttribute" ) [value attr] <> ";"


-- | Add a CSS class to a webit. No-op if the webit already has this class. The
-- text must be a legal CSS class name. This is not checked.
addClass :: (WebitClass a) => a -> T.Text -> JavaScript
addClass item nm =
  call (var (webitHandle item) <> ".classList.add") [value nm] <> ";"


-- | Remove a CSS class from a webit. No-op if the webit does not have the class.
removeClass :: (WebitClass a) => a -> T.Text -> JavaScript
removeClass item nm =
  call (var (webitHandle item) <> ".classList.remove") [value nm] <> ";"


jsNoOp :: JavaScript
jsNoOp = JavaScript ""


htmlValue :: H.Html -> JavaScript
htmlValue = value . renderHtml


-- | Useful for events in 'Html' values.
jsValue :: JavaScript -> AttributeValue
jsValue (JavaScript txt) = lazyTextValue txt


-- | Private function to remove a Webit from the STM tree.
unparent :: (WebitClass a) => a -> STM ()
unparent w = do
  readTVar (webitParent w) >>= \case
    Just c -> modifyTVar (webitContainerChildren c) $ deleteBy ((==) `on` webitId) (WebitItem w)
    Nothing -> return ()
  writeTVar (webitParent w) Nothing


-- | Generates a JQuery selector for the @WebitId@.
webitSelector :: WebitId -> JavaScript
webitSelector wId = JavaScript $ "\"#" <> LT.pack (show wId) <> "\""


-- | Generates a JQuery selector for the WebitId with a suffix.
webitSelector1 :: WebitId -> Text -> JavaScript
webitSelector1 wId suf = JavaScript $ "\"#" <> LT.pack (show wId) <> LT.fromStrict suf <> "\""


-- | Assemble a Webit event. The JavaScript argument will be the value sent to the Webit.
webitEventData :: WebitId -> JavaScript -> JavaScript
webitEventData wId js = call "jsb.event" ["{d:" <> value wId <> ",v:" <> js <> "}"] <> ";"
