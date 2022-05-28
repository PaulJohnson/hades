{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

{- |

The Haskell Diagram Editing System (HADES) provides a concept of a diagram abstracted from
the GUI toolkit used to implement it.

The Hades.Abstract modules are at the bottom of the dependency graph. The modules imported
here depend only on external libraries.

-}

module Hades.Abstract (
  module Hades.Abstract.AutoMonad,
  module Hades.Abstract.BasicShapes,
  module Hades.Abstract.Connections,
  module Hades.Abstract.Commands,
  module Hades.Abstract.Delta,
  module Hades.Abstract.Diagram,
  module Hades.Abstract.Geometry,
  module Hades.Abstract.Menus,
  module Hades.Abstract.Primitives,
  module Hades.Abstract.Toolbar
) where

import Hades.Abstract.AutoMonad
import Hades.Abstract.BasicShapes
import Hades.Abstract.Connections
import Hades.Abstract.Commands
import Hades.Abstract.Delta
import Hades.Abstract.Diagram
import Hades.Abstract.Geometry
import Hades.Abstract.Menus
import Hades.Abstract.Primitives
import Hades.Abstract.Toolbar
