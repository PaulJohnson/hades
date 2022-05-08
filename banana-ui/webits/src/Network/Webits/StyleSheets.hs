{-
Copyright © Paul Johnson 2021. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

{- | = Stylesheets for Arrowised Web GUIs

The "Network.Webits.ArrowDialog" module uses CSS classes. This module defines a default
stylesheet, along with a function that puts the stylesheet in the appropriate URL using Scotty.
-}

module Network.Webits.StyleSheets where

import Clay
import Prelude hiding ((**))
import qualified Text.Blaze.Html5 as H

-- | A standard CSS file. Applications can override this as they see fit.
webitStdCss :: Css
webitStdCss = do
   star ? do
      fontFamily ["Roboto"] [sansSerif]
   ".dialog" ? do
      overflow scroll
      backgroundColor $ grayish 0xfe
      border solid (px 1) (grayish 0x80)
      position fixed
      left $ pct 20
      maxWidth $ pct 60
      top $ pct 20
      maxHeight $ pct 60
   ".disabled" ** star ? do
      color $ grayish 0x40
   ul # ".fancytree-container" ? do
      minHeight $ px 10
      paddingBottom $ px 10
   ".grid" ** th ? do
      fontWeight bold
      alignContent center
   ".horizontal" |> star ? do
      display inline
   ".icon-menu" ? do
      position fixed
      left $ pct 25
      right $ pct 15
      top $ pct 30
      bottom $ pct 15
      backgroundColor white
      border solid (px 1) black
      overflow scroll
   ".icon-display" ? do
      width $ px 48
      height $ px 48
      let m = px 5
      border solid m white
   ".icon-inline" ? do
      width $ px 16
      height $ px 16
   ".image-small" ? do
      maxWidth $ px 150
      maxHeight $ px 100
   ".image-medium" ? do
      maxWidth $ px 300
      maxHeight $ px 200
   ".image-large" ? do
      maxWidth $ px 600
      maxHeight $ px 400
   ".invalid" ? do
      backgroundColor lightcoral
   ".invalid" ** star ? do
      backgroundColor lightcoral
   table # ".matrix" ? do
      borderCollapse collapse
   ".matrix" ** td ? do
      border solid (px 1) black
   ".matrix" ** th ? do
      border solid (px 1) black
   ".memo-small" ? do
      width $ em 20
      height $ Clay.rem 3
   ".memo-medium" ? do
      width $ em 40
      height $ Clay.rem 8
   ".memo-large" ? do
      width $ em 40
      height $ Clay.rem 15
   ".no-box" ? do
      padding (px 0) (px 0) (px 0) (px 0)
      borderWidth $ px 0
      padding (px 0) (px 0) (px 0) (px 0)
      margin (px 0) (px 0) (px 0) (px 0)
   ".popup" ? do
      position relative
      display inlineBlock
   ".popup" ** ".popup-widget" ? do
      visibility hidden
      width $ em 40
      backgroundColor $ grayish 0x50
      color white
      padding (px 8) (px 0) (px 8) (px 0)
      position absolute
      top $ px (-10)
      left $ px 10
   ".popup" ** ".popup-widget" # after ? do
      content none
      position absolute
      top $ pct 100
      left $ pct 50
      marginLeft $ px (-5)
      borderWidth $ px 5
      borderStyle solid
      borderColor4 (grayish 0x50) transparent transparent transparent
   ".popup" ** ".show" ? do
      visibility visible
      animation "fadeIn" (sec 0) linear (sec 1) infinite normal forwards
      overflow scroll
   ".popup-menu" ? do
      position absolute
   ".scrolled" ? do
      overflow scroll
   ".tab-form" ? do
      padding (px 6) (px 12) (px 6) (px 12)
      -- borderTop none none none
   ".tab-header" ? do
      overflow hidden
      border solid (px 1) (grayish 0xc0)
      backgroundColor $ grayish 0xf1
   ".tab-header" ** button ? do
      backgroundColor inherit
      float floatLeft
      borderWidth $ px 0
      outlineWidth $ px 0
      cursor pointer
      padding (px 7) (px 8) (px 7) (px 8)
      transitionDuration $ sec 0.3
   ".tab-header" ** button # hover ? do
      backgroundColor $ grayish 0xd0
   ".tab-header" ** ".active" ? do
      backgroundColor $ grayish 0xc0
   ".vertical" |> star ? do
      display block
   ".window-title" ? do
      backgroundColor $ grayish 0x30
      color white
      fontWeight bold
      alignContent center
   ".vl" ? do
      borderLeft solid (px 1) $ grayish 0x20
      height $ pct 100


-- | The stylesheet as a HTML style tag.
stylesheetInternal :: Css -> H.Html
stylesheetInternal = H.style . H.lazyText . renderWith pretty []


-- | The standard CSS plus the sebitScriptLibrary, ready to go in the @head@ of a web page.
standardHeader :: H.Html
standardHeader = do
   stylesheetInternal webitStdCss
