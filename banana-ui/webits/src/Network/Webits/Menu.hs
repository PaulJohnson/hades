{-
Copyright © Paul Johnson 2021. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

module Network.Webits.Menu (
  WebitMenu,
  webitMenuDisplay,
  webitMenuOutput,
  mkWebitMenu,
  PopupTrigger (..),
  popupMenuAt,
  popupMenuOn
) where

import Control.Lens
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Network.JavaScript as JS
import Network.Webits
import Reactive.Banana.Menu
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA



-- | Webit Menu type.
data WebitMenu a = WebitMenu {
    webitMenuDisplay :: WebitContainer,
    webitMenuOutput :: TChan a
  }

instance WebitClass (WebitMenu a) where
  webitHandle (WebitMenu m _) = webitHandle m
  webitId (WebitMenu m _) = webitId m
  webitParent (WebitMenu m _) = webitParent m
  webitChildren _ = return []
  webitFinalise (WebitMenu m _) = webitFinalise m
  webitAddFinaliser (WebitMenu m _) = webitAddFinaliser m


-- | Create a menu on the client browser. Add the @WebitItem@ to whatever point in the DOM is
-- appropriate.
--
-- The menu is attached to the 'elmerTop' and will remain there until deleted. Webits that
-- dynamically create menus should reattach the menu to themselves using 'showWebit'.
mkWebitMenu :: Menu a -> Elmer (WebitMenu a)
mkWebitMenu menu = do
  result <- makeWebitContainer1 "ul" [("class", "popup-menu"), ("style","display:none;")]
  let (hMenu, tbl) = evalState (menuHtml (webitId result) menu) 0
  elmerSend $ var (webitHandle result) <> ".innerHTML=" <> value (renderHtml hMenu)
  clickChan <- addWebitChannel result
  outChan <- liftIO newBroadcastTChanIO
  elmerDaemon result $ liftIO $ atomically $ do
    n <- readTChan clickChan
    forM_ (tbl ^? ix n) $ \v -> writeTChan outChan v
  outChan1 <- liftIO $ atomically $ dupTChan outChan
  top <- asks elmerTop
  showWebit top result
  elmerSend $ "$(" <> webitSelector (webitId result) <> ").menu()"
  return (WebitMenu result outChan1)


-- | Generate the HTML for a menu. The menu items emit ints when clicked, which are keys
-- to the map returned alongside the result.
menuHtml :: WebitId -> Menu a -> State Int (H.Html, Map Int a)
menuHtml wId (Menu xss) = do
    (rs, ms) <- unzip <$> mapM menuListHtml xss
    return (sequence_ $ intersperse sepItem rs, mconcat ms)
  where
    sepItem = H.li "-"
    menuListHtml xs = mconcat <$> mapM menuItemHtml xs
    menuItemHtml i = do
      case menuValue i of
        Left sub -> do
          (r, m) <- menuHtml wId sub
          let
            h = do
              H.div $ H.toMarkup $ menuLabel i
              H.ul r
          return (H.li h, m)
        Right v -> do
          n <- get
          put $ n+1
          return (H.li $ H.div
              ! HA.onclick (jsValue $ "{" <>
                webitEvent wId n <> ";\
                \$('.popup-menu').hide(100);}")
              $ H.toMarkup $ menuLabel i,
            M.singleton n v)


-- | Pop up the menu at the given location on the web page. This does not set the Z index
-- of the menu to ensure it is on top of anything else.
popupMenuAt :: (Int, Int) -> WebitMenu a -> Elmer ()
popupMenuAt (x,y) menu = do
  elmerSend $ "$(" <> webitSelector (webitId menu) <> ").finish().toggle(100).css({\
      \top:" <> value y <> " + 'px',\
      \left:" <> value x <> " + 'px'\
    \});"


-- | Browser Event types on which a menu could pop up.
data PopupTrigger = OnLeftClick | OnRightClick


-- | Attach a popup menu to a Webit. On the specified event the menu will pop up with a Z index
-- just above the event target.
popupMenuOn :: (WebitClass w) => w -> PopupTrigger -> WebitMenu a -> Elmer ()
popupMenuOn webit trigger menu = do
  let
    wId = webitId webit
    mId = webitId menu
    evtName = case trigger of
      OnLeftClick -> "click" :: Text
      OnRightClick -> "contextmenu"
  elmerSend $ "$(" <> webitSelector wId <> ").on(" <> value evtName <> ", function (event) {\
      \event.preventDefault();\
      \let z=ancestorZ(event.target);\
      \console.log('target z index = ' + z);\
      \$(" <> webitSelector mId <> ").finish().toggle(100).css({\
        \top: event.pageY + 'px',\
        \left: event.pageX + 'px',\
        \zIndex: z+1\
      \});\
    \});"
  return ()
