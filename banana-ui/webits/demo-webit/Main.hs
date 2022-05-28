{-# LANGUAGE Arrows #-}

{-
Copyright © Paul Johnson 2021. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

module Main (main) where

import Control.Concurrent.STM
import Control.Concurrent.TState
import Control.Lens
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree
import Network.HTTP.Types.Status
import Network.Webits
import Network.Webits.ArrowDialog
import Network.Webits.Icons
import Reactive.Banana.ArrowDialog
import Reactive.Banana.Menu
import Test.QuickCheck
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Web.Scotty

import Dialog
import Matrix
import Menu
import Types
import Paths_webits

import Debug.Trace
import Data.Maybe
import Reactive.Banana.Common

-- | Run a test web server
main :: IO ()
main = do
  putStrLn "http://localhost:3000/"
  icons1 <- getIconData "/home/paj/.local/share/hades/Diametric/scalable/"
  icons2 <- getIconData "/home/paj/.local/share/hades/Diametric/48x48"
  (initialModel, matrixKeys) <- generate $ arbitraryMatrix 10
  (modelState, modelWriter) <- newTStateIO initialModel
  let icons = mergeIconGroups $ icons1 ++ icons2
  scotty 3000 $ do
    elmerStatic icons
    elmerScotty "/" appHeader icons $ choicePage modelState modelWriter matrixKeys
    get "/image.jpg" $ do
      fn <- liftIO $ getDataFileName "Testcard_F.jpg"
      liftIO $ putStrLn $ "GET image: fn = " <> fn
      setHeader "Content-Type" "image/jpeg"
      file fn
    notFound $ do
      rq <- request
      liftIO $ putStrLn $ "Scotty 404: " <> show rq
      status $ mkStatus 404 "Scotty can't find that."


appHeader :: H.Html
appHeader = H.head $ do
  H.title "Demo Webits"
  H.link ! HA.rel "shortcut icon" ! HA.type_ "image/x-icon" ! HA.href "/favicon.ico"


choicePage :: TState Bool Model -> (Bool -> Model -> STM ()) -> MatrixItems -> Elmer ()
choicePage modelState modelWriter matrix = do
  logToStderr
  top <- asks elmerTop
  btnBox <- makeWebitContainer ""
  dialogBtn <- choiceButton "Dialog Demo"
  showWebit btnBox dialogBtn
  matrixBtn <- choiceButton "Matrix Demo"
  showWebit btnBox matrixBtn
  menuBtn <- demoMenuButton  -- Has its own event handler; not needed below.
  showWebit btnBox menuBtn
  forestBtn <- choiceButton "Forest Editor Demo"
  showWebit btnBox forestBtn
  imageBtn <- choiceButton "Clickable Image Demo"
  showWebit btnBox imageBtn
  showWebit top btnBox
  -- Set up dialog popup demo.
  dialogTrigger <- liftIO newTChanIO
  demoDialogOut <- mkWebitPopupSelect (pure demoForest) (constantDialog demoDialog) $
      readTChan dialogTrigger
  d1 <- liftIO $ atomically $ dupTChan demoDialogOut
  -- Set up forest editing popup demo.
  forestTrigger <- liftIO newTChanIO
  demoForestData <- liftIO $ generate $ resize 5 arbitrary
  demoForestOut <- mkWebitPopupSelect (pure demoForest) (constantDialog demoForestEditor) $
      readTChan forestTrigger
  f1 <- liftIO $ atomically $ dupTChan demoForestOut
  -- Respond to buttons
  join $ liftIO $ atomically $ (do
      ((), txt) <- tStateEvent $ webitState dialogBtn
      traceM $ "Dialog button clicked: " <> show txt
      writeTChan dialogTrigger ((), demoDefault)
      return $ deleteWebit btnBox
    ) `orElse` (do
      ((), txt) <- tStateEvent $ webitState matrixBtn
      traceM $ "Matrix button clicked: " <> show txt
      return $ do
        deleteWebit btnBox
        demoMatrixAction modelState modelWriter matrix
    ) `orElse` (do
      ((), txt) <- tStateEvent $ webitState forestBtn
      traceM $ "Forest button clicked: " <> show txt
      writeTChan forestTrigger ((), demoForestData)
      return $ deleteWebit btnBox
    ) `orElse` (do
      ((), txt) <- tStateEvent $ webitState imageBtn
      traceM $ "Got click on " <> show txt
      return demoImageAction
    )
  elmerDaemon top $ do
      ((), r) <- liftIO $ atomically $ readTChan d1
      elmerLog NOTICE $ T.pack $ "Dialog output = " <> maybe "Nothing" prettyDemo r
  void $ forever $ do
      ((), r) <- liftIO $ atomically $ readTChan f1
      let tr = drawForest . map (fmap $ T.unpack . view demoText) <$> r
      elmerLog NOTICE $ "Forest edit output = \n" <> T.pack (fromMaybe "Nothing" tr)


choiceButton ::
  T.Text    -- ^ Text to display.
  -> Elmer (Webit T.Text)
choiceButton display =
    makeWebit buttonContents (const $ const $ const jsNoOp) display
  where
    buttonContents wId = H.button
      ! HA.type_ "button"
      ! HA.onclick (jsValue $ webitEvent wId display)
      $ H.text display


demoForestEditor :: Dialog' (Forest (Text, Maybe Text, Maybe Int)) w (Forest DemoData)
demoForestEditor = Dialog "Demo Dialog" OkApplyButton $
    forestEditor demoLabel demoMenus (view demoBool) $ Just $ constantDialog demoDialog
  where
    demoLabel _ x = x ^. demoText <> " (" <> if x ^. demoBool then "Y" else "N" <> ")"
    demoMenus Nothing =  Menu [[
            menuItem "Add leaf" $ TreeAddIn demoDefault,
            menuItem "Add node" $ TreeAddIn $ demoBool .~ True $ demoDefault
          ]]
    demoMenus (Just x) = if x ^. demoBool
      then Menu [[
            menuItem "Add leaf" $ TreeAddIn demoDefault,
            menuItem "Add node" $ TreeAddIn $ demoBool .~ True $ demoDefault
          ],[
            menuItem "New node before" $ TreeAddBefore $ demoBool .~ True $ demoDefault,
            menuItem "New node after" $ TreeAddAfter $ demoBool .~ True $ demoDefault
          ],[
            menuItem "Delete" TreeDelete
          ]]
      else Menu [[
            menuItem "New node before" $ TreeAddBefore $ demoBool .~ True $ demoDefault,
            menuItem "New node after" $ TreeAddAfter $ demoBool .~ True $ demoDefault
          ],[
            menuItem "Delete" TreeDelete
          ]]


demoImageAction :: Elmer ()
demoImageAction = do
  liftIO $ putStrLn "Got Image Demo click."
  top <- asks elmerTop
  gOut <- renderGadget (pure ()) (pure $ GadgetData True (0,0)) $ image "image.jpg"
  mapM_ (showWebit top) $ gadgetView gOut
  elmerDaemon (head $ gadgetView gOut) $ liftIO $ do
    click <- atomically $ tStateEvent $ gadgetOutput gOut
    putStrLn $ "Image clicked at " <> show click
