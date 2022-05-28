{-# LANGUAGE Arrows #-}

{-
Copyright © Paul Johnson 2021. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.
-}


module Dialog where

import Control.Arrow
import Control.Lens
import Data.Colour.Names
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree
import Network.Webits
import Network.Webits.StyleSheets
import Reactive.Banana.ArrowDialog
import Reactive.Banana.Common hiding (IconName)
import Reactive.Banana.Table
import System.Random
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

import Types


demoDialogPage :: H.Html
demoDialogPage = do
  H.head $ do
    H.title "Demo Webits"
    H.link ! HA.rel "shortcut icon" ! HA.type_ "image/x-icon" ! HA.href "/favicon.ico"
    standardHeader
  H.body (H.p (H.text loremIpsum))


demoTableColumns :: Table DemoTable
demoTableColumns = [
    mkField "Boolean" boolField (Just boolIcon) EditBool (Just boolColour),
    mkField "Number" numberField Nothing (EditEntry textPrism) Nothing,
    mkField "Icon" iconField Nothing (EditIcon (const True)) Nothing
  ]


boolIcon :: Bool -> IconName
boolIcon True = "computer-add"
boolIcon False = "computer-application-exit"


boolColour :: Bool -> Colour
boolColour True = Colour darkgreen
boolColour False = Colour darkred


demoTableForm :: Dialog' e w DemoTable
demoTableForm = Dialog "Edit table row" OkApplyButton $ accum $ form Vertical [
    ("Boolean", focusing boolField $ coloured (Just . boolColour) $ icon boolIcon tickBox),
    ("Number", focusing numberField typedTextBox),
    ("Icon", focusing iconField $ iconBox $ const True)
  ]


demoDialog :: Dialog' (Forest (Text, Maybe Text, Maybe Int)) w DemoData
demoDialog = Dialog "Demo Dialog" OkApplyButton $
  proc dat -> do
    out <- accum $ box Vertical [[
        form Vertical [
            ("Demo Text:", focusing demoText displayMemo),
            ("Demo Number:", focusing demoNum $
                accum (buttonBar [("Increment", (+1)), ("Decrement", subtract 1)])
                <<< typedTextBox
              ),
            ("Demo Choice:", focusing demoEnum boundedRadio),
            ("Demo Boolean:", focusing demoBool tickBox),
            ("Demo Date:", focusing demoDate $ dateBox shortDate),
            ("Demo Icon:", focusing demoIcon $ iconBox (const True)),
            ("Demo Colour:", focusing demoColour colourBox),
            ("Demo Set:", focusing demoSet demoSetPopup)
          ],
        focusing demoTable $ table
          [TableAdd "new row" $ DemoTable False 0 noIconName, TableDelete, TableShuffle]
          demoTableColumns
          Nothing
          --  (Just $ constantDialog demoTableForm)
      ]]  -< dat
    _ <- simpleFrame "Clickable" $
          clickableSingle (T.pack . show) >>>
          message (\_ v -> "Clicked = " <> T.pack (show v))
        -< [1..10 :: Int]
    _ <- displayMemo <<< arr (\v -> T.pack $
        "The secret code is: " <> show (v ^. demoEnum) <> show (v ^. demoNum)) -< out
    _ <- displayMemo
        <<< arr (\(v :: Maybe Int) -> T.pack $ "The random value is: " <> maybe "" show v)
        <<< buttonIO "Randomise" (const $ const $ randomRIO (0,100)) -< ()
    returnA -< out



demoSetPopup :: Gadget' (Forest (Text, Maybe Text, Maybe Int)) w (Set Int)
demoSetPopup = textPopup (const showSet) $ const $ const $ Just demoForestSelection
  where showSet = T.intercalate ", " . map (T.pack . show) . S.toList

demoForestSelection :: Dialog' (Forest (Text, Maybe Text, Maybe Int)) w (Set Int)
demoForestSelection = Dialog "Demo Tree Selection" OkApplyButton $ treeSelector id

loremIpsum :: Text
loremIpsum = "\
\Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, \
\totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae \
\dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, \
\sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam \
\est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius \
\modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima \
\veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea \
\commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam \
\nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?"
