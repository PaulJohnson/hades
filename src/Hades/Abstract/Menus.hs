{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

-- |
--
-- Utility module containing functions to produce some common menus.
--
-- Menus in Hades diagrams are of type @Point -> Menu (Action v)@, so all of these functions
-- take a @Point@ even when they don't need it.
module Hades.Abstract.Menus (
   addItemsMenu,
   basicShapeMenu,
   selectionOrTarget
) where

import Control.Lens
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)
import Data.UUID
import Hades.Abstract.Connections
import Hades.Abstract.Geometry
import Hades.Abstract.Delta
import Hades.Abstract.Diagram
import Hades.Abstract.Primitives
import Reactive.Banana.Menu


-- | A menu for adding new items to the diagram. The "UUID" is the new UUID for the item, and
-- the "Point" is the location for the new item (i.e. where the right click bought up the menu).
addItemsMenu :: (Viewable v) =>
   [(Text, Point -> UUID -> v)] -> Point -> Menu (Action v)
addItemsMenu generators p = Menu [map add generators]
   where
      add (str, f) = MenuItem str $ Right $ mkScriptAction $ do
         v <- addItem $ f p
         desc <- itemDescription v
         tellCheckpoint $ "Add " <> desc
         yieldViews



-- | Basic menu for shapes in the diagram. The argument is the UUID of the shape. The @Point@ is
-- ignored.
basicShapeMenu :: (Connectable v) =>
   UUID -> Point -> Menu (Action v)
basicShapeMenu uuid = const $ Menu [[
         MenuItem "To top" $ Right $ selectionOrTarget
               (fmap (("Move " <>) . (<> " to top")) . itemsDescription)
               moveTop
               uuid,
         MenuItem "To bottom" $ Right $ selectionOrTarget
               (fmap (("Move " <>) . (<> " to bottom")) . itemsDescription)
               moveBottom
               uuid
      ],[
         MenuItem "Delete" $ Right $
            selectionOrTarget (fmap ("Delete " <>) . itemsDescription) deleteItem uuid
      ]
   ]


-- | If the target is a member of the selected set then apply the action to all of the current
-- selection. If the target is not a member of the selected set then only apply it to the target.
selectionOrTarget :: (Viewable v) =>
   ([v] -> Delta v Text)
      -- ^ Message for the undo log, based on the items the action is applied to.
   -> (UUID -> Delta v ())
      -- ^ Action for each item.
   -> UUID
      -- ^ Item that has been clicked on.
   -> Action v
selectionOrTarget strF action uuid = mkScriptAction $ do
   sel <- use deltaSelection
   contents <- use $ deltaDiagram . diagramContents
   if uuid `S.member` sel
      then do
         msg <- strF $ mapMaybe (`M.lookup` contents) $ S.toList sel
         mapM_ action sel
         tellCheckpoint msg
      else case uuid `M.lookup` contents of
         Nothing -> return ()
         Just target -> do
            msg <- strF [target]
            action uuid
            tellCheckpoint msg
   yieldViews
