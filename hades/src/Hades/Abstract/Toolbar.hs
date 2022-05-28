{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

{- |

This provides the abstract command toolbar for Hades diagrams. Each tool has an associated
Delta command run when the tool is activated.

The following type variables are used:

[@v@] The type of drawing.

[@m@] The underlying monad for the drawing.
-}
module Hades.Abstract.Toolbar (
  DeltaToolClick (..),
  DeltaTool (..),
  DeltaToolbar (..),
  alignmentTools
) where

import Data.Text (Text)
import Hades.Abstract.Commands
import Hades.Abstract.Connections
import Hades.Abstract.Delta
import Reactive.Banana.Menu


-- | When a toolbutton is clicked it either does something or provides a menu of actions.
data DeltaToolClick v =
  DeltaToolAction (Action v)
  | DeltaToolMenu (Menu (Action v))


data DeltaTool v = DeltaTool {
    toolIcon :: Text,  -- ^ The back end is expected to know what to do with this.
    toolTip :: Text,  -- ^ Description of the tool.
    toolAction :: DeltaToolClick v
  }


-- | An abstract toolbar with slots for icons on the left and the right. Sublists are rendered
-- with separators between them. The monoid instance is concatenation with a separator.
data DeltaToolbar v = DeltaToolbar {toolbarLeft, toolbarRight :: [[DeltaTool v]]}

instance Semigroup (DeltaToolbar v) where
  DeltaToolbar t1Left t1Right <> DeltaToolbar t2Left t2Right =
    DeltaToolbar (t1Left <> t2Left) (t1Right <> t2Right)

instance Monoid (DeltaToolbar v) where
  mempty = DeltaToolbar [] []


-- | Common menu buttons for box alignment.
alignmentTools :: (Connectable v) => [DeltaTool v]
alignmentTools = [
    DeltaTool "align" "Align positions" $ DeltaToolMenu alignMenu,
    DeltaTool "align-size" "Align sizes" $ DeltaToolMenu sizeMenu,
    DeltaTool "align-distribute" "Distribute selection" $ DeltaToolMenu distributeMenu
  ]


alignMenu :: (Connectable v) => Menu (Action v)
alignMenu = Menu [[
    menuItem "Left" alignLeft,
    menuItem "Right" alignRight,
    menuItem "Top" alignTop,
    menuItem "Bottom" alignBottom,
    menuItem "Vertical" alignVertical,
    menuItem "Horizontal" alignHorizontal
  ]]


sizeMenu :: (Connectable v) => Menu (Action v)
sizeMenu = Menu [[
    menuItem "Widest" sizeWidest,
    menuItem "Narrowest" sizeNarrowest,
    menuItem "Tallest" sizeTallest,
    menuItem "Shortest" sizeShortest
  ]]


distributeMenu :: (Connectable v) => Menu (Action v)
distributeMenu = Menu [[
    menuItem "Distribute horizontally" distributeH,
    menuItem "Distribute vertically" distributeV,
    menuItem "Space horizontally" spaceH,
    menuItem "Space vertically" spaceV
  ]]
