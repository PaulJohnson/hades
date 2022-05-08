{-
Copyright © Paul Johnson 2021. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.
-}


module Types where

import Control.Lens
import Data.Colour.Names
import qualified Data.Colour.SRGB as C
import Data.List
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Calendar
import Data.Tree
import Network.Webits
import Network.Webits.Icons
import Reactive.Banana.Common hiding (IconName)
import System.IO.Unsafe
import Test.QuickCheck as QC


data DemoEnum = Foo | Bar | Baz | Wibble | Woo deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance Arbitrary DemoEnum where
   arbitrary = QC.elements [Foo .. Woo]

data DemoTable = DemoTable {
      _boolField :: Bool,
      _numberField :: Double,
      _iconField :: IconName
   } deriving (Eq, Show)

instance Arbitrary DemoTable where
   arbitrary = DemoTable <$> arbitrary <*> arbitrary <*> arbitraryIcon

boolField :: Lens' DemoTable Bool
boolField = lens _boolField $ \s v -> s {_boolField = v}

numberField :: Lens' DemoTable Double
numberField = lens _numberField $ \s v -> s {_numberField = v}

iconField :: Lens' DemoTable IconName
iconField = lens _iconField $ \s v -> s {_iconField = v}


data DemoData = DemoData {
      _demoText :: Text,
      _demoNum :: Integer,
      _demoEnum :: DemoEnum,
      _demoBool :: Bool,
      _demoDate :: Day,
      _demoIcon :: Text,
      _demoColour :: Colour,
      _demoSet :: Set Int,
      _demoTable :: [DemoTable]
   } deriving (Eq, Show)

instance Arbitrary DemoData where
   arbitrary = DemoData <$> arbitraryText 5 <*> arbitrary <*> arbitrary <*> arbitrary <*>
         arbitraryDay <*> arbitraryIcon <*> arbitraryColour <*> arbitrary <*> arbitrary


demoDefault :: DemoData
demoDefault = DemoData {
      _demoText = "Foo! This is a ridiculously long line to put in a text box.\
            \ It ought to be a paragraph!" ,
      _demoNum = 5,
      _demoEnum = Bar,
      _demoBool = False,
      _demoDate = fromGregorian 2021 8 1,
      _demoIcon = "basic_checked",
      _demoColour = Colour aqua,
      _demoSet = S.fromList [1,21,13],
      _demoTable = [
            DemoTable True 3.14159 "computer-about",
            DemoTable False 4 "general-car",
            DemoTable True 5 "emoji-happy-1"
         ]
   }

demoText :: Lens' DemoData Text
demoText = lens _demoText $ \s v -> s {_demoText = v}

demoNum :: Lens' DemoData Integer
demoNum = lens _demoNum $ \s v -> s {_demoNum = v}

demoEnum :: Lens' DemoData DemoEnum
demoEnum = lens _demoEnum $ \s v -> s {_demoEnum = v}

demoBool :: Lens' DemoData Bool
demoBool = lens _demoBool $ \s v -> s {_demoBool = v}

demoDate :: Lens' DemoData Day
demoDate = lens _demoDate $ \s v -> s {_demoDate = v}

demoIcon :: Lens' DemoData Text
demoIcon = lens _demoIcon $ \s v -> s {_demoIcon = v}

demoColour :: Lens' DemoData Colour
demoColour = lens _demoColour $ \s v -> s {_demoColour = v}

demoSet :: Lens' DemoData (Set Int)
demoSet = lens _demoSet $ \s v -> s {_demoSet = v}

demoTable :: Lens' DemoData [DemoTable]
demoTable = lens _demoTable $ \s v -> s {_demoTable = v}



prettyDemo :: DemoData -> String
prettyDemo d =
   "DemoData {\n\
   \   demoText: " <> show (d ^. demoText) <> "\n\
   \   demoNum: " <> show (d ^. demoNum) <> "\n\
   \   demoEnum: " <> show (d ^. demoEnum) <> "\n\
   \   demoBool: " <> show (d ^. demoBool) <> "\n\
   \   demoDate: " <> show (d ^. demoDate) <> "\n\
   \   demoIcon: " <> show (d ^. demoIcon) <> "\n\
   \   demoColour: " <> show (d ^. demoColour) <> "\n\
   \   demoSet: {" <> intercalate ", " (map show $ S.toList $ d ^. demoSet) <> "}\n\
   \   demoTable: [\n" <> concatMap (("       " <>) . (<> "\n") . show) (d ^. demoTable) <> "   ]\n\
   \}"

demoForest :: Forest (Text, Maybe Text, Maybe Int)
demoForest = [
      Node ("Item 1", Just "tooltip one", Nothing) [
            Node ("Item 1.1", Just "tooltip one point one", Just 11) [],
            Node ("Thingy 1.2", Just "thingy is like an item", Just 12) [],
            Node ("Itemised 1.3", Nothing, Just 13) [
                  Node ("Final 1.3.1", Just "some tips", Just 131) []
               ]
         ],
      Node ("Item 2", Just "tooltip two", Nothing) [
            Node ("Item 2.1", Just  "this is not item 1.2", Just 21) [],
            Node ("Item 2.2", Just "invisible", Nothing) []
         ]
   ]


-- | A random RGB colour.
arbitraryColour :: Gen Colour
arbitraryColour = Colour <$> (C.sRGB24 <$> arbitrary <*> arbitrary <*> arbitrary)


-- | A random day in 2021.
arbitraryDay :: Gen Day
arbitraryDay = do
      d <- choose (0,364)
      return $ addDays d b
   where
      b = fromGregorian 2021 1 1


-- | An arbitrary icon name.
arbitraryIcon :: Gen IconName
arbitraryIcon = QC.elements iconList


-- | A string of @n@ arbitrary common words.
arbitraryText :: Int -> Gen Text
arbitraryText n = T.unwords <$> vectorOf n (QC.elements wordsList)


-- | Known icon names. Uses @unsafePerformIO@.
iconList :: [IconName]
iconList = unsafePerformIO $ do
   icons1 <- getIconData "/home/paj/.local/share/hades/Diametric/scalable/"
   icons2 <- getIconData "/home/paj/.local/share/hades/Diametric/48x48"
   return $ M.keys $ allIcons $ mergeIconGroups $ icons1 ++ icons2
{-# NOINLINE iconList #-}


-- | Most common 1000 words in English. Uses @unsafePerfromIO@.
wordsList :: [Text]
wordsList = unsafePerformIO $ T.lines <$> T.readFile "/home/paj/.local/share/words.txt"
{-# NOINLINE wordsList #-}
