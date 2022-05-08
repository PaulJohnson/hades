{-
Copyright © Paul Johnson 2021. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

{- |

Utility functions for manipulating XML files.
-}


module Model.Report.Docx.Utils where

import qualified Text.XML.Light as X
import qualified Text.XML.Light.Cursor as XC


-- | Create a qualified name, The arguments are the name space and the node name.
qual :: String -> String -> X.QName
qual ns nm = X.QName nm Nothing $ Just ns


-- | Go to the root, and then as far left as possible.
goStart :: XC.Cursor -> XC.Cursor
goStart c = repeatWhile XC.left $ XC.root c
   where
      repeatWhile f v = case f v of
         Just v1 -> repeatWhile f v1
         Nothing -> v


-- Apply all the functions in order, ignoring any failures.
applyAll :: [a -> Maybe a] -> a -> a
applyAll [] v = v
applyAll (f:fs) v = case f v of
   Nothing -> applyAll fs v
   Just v2 -> applyAll fs v2


-- | Find the next content (in depth-first order) satisfying the predicate
findDF :: (X.Content -> Bool) -> XC.Cursor -> Maybe XC.Cursor
findDF f c = if f $ XC.current c then Just c else XC.nextDF c >>= findDF f


-- | Find the next element with this name.
findNameDF :: X.QName -> XC.Cursor -> Maybe XC.Cursor
findNameDF qn = findDF p
   where
      p (X.Elem e) = isNamed qn e
      p _ = False


-- | True if the element has the given name. Ignores the URI and focuses on the namespace, unlike
-- the Eq instance.
isNamed :: X.QName -> X.Element -> Bool
isNamed qn e = X.elName e `nameEq` qn


-- | True if the two qualified names share the same namespace and name. Ignores the URI.
nameEq :: X.QName -> X.QName -> Bool
nameEq (X.QName x1 _ x2) (X.QName y1 _ y2) = x1 == y1 && x2 == y2


-- | If this is an element then set its content to the string. Otherwise do nothing.
setElemText :: String -> X.Content -> X.Content
setElemText str (X.Elem e) = X.Elem $ e {X.elContent = [X.Text $ X.CData X.CDataText str Nothing]}
setElemText _ v = v
