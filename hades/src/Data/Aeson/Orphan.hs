{-# OPTIONS_GHC -fno-warn-orphans #-}
{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}



{- |

This package is a home for the following orphan instances:

> instance (RealFrac a, Floating a) => ToJSON (Colour a)
> instance (RealFrac a, Floating a) => FromJSON (Colour a)

"Colour" is represented as a hex string in HTML style.
-}

module Data.Aeson.Orphan where

import Data.Aeson
import Data.Char
import Data.Colour
import Data.Colour.SRGB
import qualified Data.Text as T

instance (RealFrac a, Floating a) => ToJSON (Colour a) where
  toJSON = toJSON . sRGB24show

instance (RealFrac a, Floating a) => FromJSON (Colour a) where
  parseJSON = withText "Colour" $ \str ->
    case sRGB24reads $ T.unpack $ T.dropAround isSpace str of
      [(c, "")] -> return c
      _ -> fail $ "Colour not parsed: " ++ show str
