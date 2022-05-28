{-# LANGUAGE Arrows #-}

{-
Copyright © Paul Johnson 2021. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

module Menu (
  demoMenu,
  demoMenuButton
) where

import Control.Concurrent.STM
import Data.Text (Text)
import Network.Webits
import Network.Webits.Menu
import Reactive.Banana.Menu
import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

demoMenu :: Menu Int
demoMenu = Menu [[
    menuItem "Ten" 10,
    subMenu "Tens" $ Menu [[
        menuItem "Eleven" 11,
        menuItem "Twelve" 12,
        menuItem "Thirteen" 13,
        menuItem "Fourteen" 14
      ]],
    menuItem "Twenty" 20
  ], [
    menuItem "Thirty" 30,
    menuItem "Fourty" 40
  ]]


demoMenuButton :: Elmer WebitItem
demoMenuButton = do
  menu <- mkWebitMenu demoMenu
  elmerDaemon menu $ liftIO $ do
    v <- atomically $ readTChan $ webitMenuOutput menu
    putStrLn $ "Menu option " <> show v <> " clicked."
  btn <- makeWebit buttonContents (const $ const $ const jsNoOp) ("Menu" :: Text)
  popupMenuOn btn OnRightClick menu
  return $ WebitItem btn
  where
    buttonContents _ = H.button ! HA.type_ "button" $ "Right-click Menu"
