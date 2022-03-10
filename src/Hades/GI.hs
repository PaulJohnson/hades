{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

{- |

The implementation of the abstract HADES libary on the GI-GTK and Cairo APIs.

These libraries import the "Hades.Abstract" component modules.
-}

module Hades.GI (
   module Hades.GI.BasicShapes,
   module Hades.GI.MouseMachine,
   module Hades.GI.Rendering,
   module Hades.GI.Saving,
   module Hades.GI.Toolbar
) where

import Hades.GI.BasicShapes
import Hades.GI.MouseMachine
import Hades.GI.Rendering
import Hades.GI.Saving
import Hades.GI.Toolbar
