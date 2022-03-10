{-
Copyright © Paul Johnson 2021. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.
-}

{- | Utiltities for managing icons.
-}

module Network.Webits.Icons (
   -- * Managing Icons
   IconName,
   IconGroup (..),
   IconData (..),
   getIconData,
   mergeIconGroups,
   allIcons,
   -- * Icons via HTTP
   scottyIcons,
   iconUrl,
   iconFromUrl,
   iconIso
) where

import Control.Lens hiding ((<.>))
import Control.Monad
import Control.Monad.IO.Class
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import GHC.Stack
import Network.HTTP.Types
import Paths_webits
import Reactive.Banana.Common (IconName)
import System.Directory
import System.FilePath
import Web.Scotty



-- | Icon catalog data
data IconGroup = IconGroup {
      iconGroupName :: Text,
      iconGroupContents :: Map IconName IconData
   }


data IconData = IconData {
      iconPath :: FilePath,  -- Path on local file system.
      iconMime :: IconMime  -- MIME type for the icon file.
   }


data IconMime = IconSvg | IconPng deriving (Eq)

instance Show IconMime where
   show IconSvg = "image/svg+xml"
   show IconPng = "image/png"


-- | Generate the icon group data by reading a directory. The directory should contain
-- one sub-directory for each icon group. The names of the subdirectories will be capitalised
-- to give the group names. The sub-directories should contain icon files with @svg@
-- or @png@ suffixes. The base name is used as the icon name and the suffix is used to
-- determine the MIME type. Files not suffixed @svg@ or @png@ will be ignored.
--
-- The returned icon groups are sorted into alphabetical order.
getIconData :: (HasCallStack, MonadIO m) => FilePath -> m [IconGroup]
getIconData path = liftIO $ do
      pathContents <- listDirectory path
      forM pathContents $ \groupDir -> do
         let
            groupName = T.toTitle $ T.pack groupDir
            groupPath = path </> groupDir
         groupContents <- listDirectory groupPath
         let
            groupFiles = foldr
                  (\fp -> M.insertWith (++) (takeBaseName fp) [fp])
                  M.empty
                  groupContents
            icons = M.mapMaybeWithKey (mkIconData groupPath) groupFiles
         return $ IconGroup groupName $ M.mapKeysMonotonic T.pack icons
   where
      mkIconData p icon iconFiles =
         let
            pngFile = icon <.> "png"
            svgFile = icon <.> "svg"
         in if svgFile `elem` iconFiles then Just $ IconData (p </> svgFile) IconSvg
            else if pngFile `elem` iconFiles then Just $ IconData (p </> pngFile) IconPng
            else Nothing


-- | Merge icon groups with the same name. Prefers SVG over PNG. Left-biased.
mergeIconGroups :: [IconGroup] -> [IconGroup]
mergeIconGroups = map mergeGroups . groupBy ((==) `on` iconGroupName) . sortOn iconGroupName
   where
      mergeGroups gs1 = IconGroup (iconGroupName $ head gs1) $
         foldr1 mergeIcons $ map iconGroupContents gs1
      -- "head" is safe because groupBy never produces empty sub-lists.

mergeIcons :: Map IconName IconData -> Map IconName IconData -> Map IconName IconData
mergeIcons = M.unionWith mergeIcon


-- | Prefer SVG over PNG. If both have the same MIME type then take the left.
mergeIcon :: IconData -> IconData -> IconData
mergeIcon d1 d2 = case iconMime d1 of
   IconSvg -> d1
   IconPng -> case iconMime d2 of
      IconSvg -> d2
      IconPng -> d1


-- | Merge the icon data.
allIcons :: [IconGroup] -> Map IconName IconData
allIcons = M.unions . map iconGroupContents


-- | Serve icons in a web server.
--
-- The icons from the argument are to be found in @/icons/:icon@. Also serves @/favicon.ico@
-- from the bundled data file @JS/favicon.ico@.
scottyIcons :: [IconGroup] -> ScottyM ()
scottyIcons iconGroups = do
      get "/favicon.ico" $ do
         setHeader "Content-Type" "image/x-icon"
         filePath <- liftIO $ getDataFileName $ "JS" </> "favicon.ico"
         file filePath
      get "/icons/:icon" $ do
         iconName <- param "icon"
         case M.lookup iconName tbl of
            Nothing ->
               raiseStatus notFound404 $ "Unknown icon " <> LT.fromStrict iconName
            Just icon -> do
               setHeader "Content-Type" $ LT.pack $ show $ iconMime icon
               file $ iconPath icon
   where
      tbl = allIcons iconGroups


-- | The local URL for the icon image file.
iconUrl :: IconName -> Text
iconUrl = ("/icons/" <>)

-- | Find the icon referenced in the URL. Actually just returns the last component of the path.
iconFromUrl :: Text -> IconName
iconFromUrl = last . T.splitOn "/"  -- Safe because splitOn never returns an empty list.

-- | ISO from URL to icon name. Not a true ISO because it ignores the everything except the last
-- component of the path.
iconIso :: Iso' Text IconName
iconIso = iso iconFromUrl iconUrl
