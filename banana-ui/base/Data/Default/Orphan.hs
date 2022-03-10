{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Orphan "Default" instances
--
-- > instance Default Bool where def = False
--
-- > instance Default Text where def = ""
--
-- "Day" instance returns current date through "unsafePerformIO".

module Data.Default.Orphan where

import Data.Default
import Data.Text (Text)
import Data.Time
import System.IO.Unsafe

instance Default Bool where def = False

instance Default Text where def = ""

instance Default Day where def = utctDay $ unsafePerformIO getCurrentTime
