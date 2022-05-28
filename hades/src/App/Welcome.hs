{-# LANGUAGE CPP #-}
{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

-- |
module App.Welcome (
  WelcomeResult (WelcomeResult),
  welcomeDon'tShow,
  welcomeExample,
  welcomeScreen,
  manualFile
) where

import Control.Arrow
import Control.Exception
import Control.Lens
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import Reactive.Banana.ArrowDialog
import Reactive.Banana.GI.ArrowDialog
import System.Directory
import System.FilePath


data WelcomeResult = WelcomeResult {
    _welcomeDon'tShow :: Bool,
    _welcomeExample :: Maybe FilePath
  } deriving Show

welcomeDon'tShow :: Lens' WelcomeResult Bool
welcomeDon'tShow = lens _welcomeDon'tShow $ \r b -> r{_welcomeDon'tShow = b}

welcomeExample :: Lens' WelcomeResult (Maybe FilePath)
welcomeExample = lens _welcomeExample $ \r p -> r{_welcomeExample = p}


-- | Display a welcome screen unless the Boolean @force@ argument is @False@ and user has
-- previously clicked the \"Don't show me this again\" box. Returns an initial example file to load,
-- or @Nothing@ for a blank file.
welcomeScreen :: (Gtk.IsWidget parent) =>
  parent
  -> Bool   -- ^ Force showing the welcome screen.
  -> FilePath  -- ^ Documentation folder, including manual file and @welcome.svg@.
  -> Maybe FilePath  -- ^ Folder holding example files, if any.
  -> IO (Maybe FilePath)
welcomeScreen parent force docFolder sampleFolder = do
  iconTheme <- Gtk.iconThemeGetDefault
  let
    imageFile = docFolder </> "welcome.svg"
    docFile = docFolder </> manualFile
  uri <- GLib.filenameToUri docFile Nothing
  examplePaths <- case sampleFolder of
    Just f -> catch (sort <$> listDirectory f) $ \(_ :: IOException) -> return []
    Nothing -> return []
  let examples = map (\p -> (T.pack $ takeBaseName p, p)) examplePaths
  flagPath <- welcomeFlagFile
  don'tShow <- doesFileExist flagPath
  let
    defaultWelcome = WelcomeResult don'tShow Nothing
    d = welcomeDialog imageFile uri examples
  if force || not don'tShow
    then
      runDialog parent iconTheme d () defaultWelcome >>= \case
        Nothing -> return Nothing
        Just r -> do
          catch
            (if r ^. welcomeDon'tShow then writeFile flagPath "" else removeFile flagPath)
            (\(_ :: IOException) -> return ())  -- Ignore file-not-found errors.
          return $ (</>) <$> sampleFolder <*> (r ^. welcomeExample)
    else
      return Nothing


welcomeDialog ::
  FilePath   -- ^ Image file for left hand side of dialog.
  -> Text  -- ^ Manual URI.
  -> [(Text, FilePath)] -- ^ Title / pathname pairs for examples.
  -> Dialog' e w WelcomeResult
welcomeDialog imageFile docFile examples =
    Dialog "Welcome to the Diametric Safety Case Manager" (CloseButton "_Continue") $ accum $
      styled1 "welcome" $ box Vertical [[
          box Horizontal [[
              arr (const id) <<< image imageFile <<< arr (const (-1,-1)),
              grid [] [] [
                [ arr (const id) <<< message1 "\n" ],  -- Kludge for spacing.
                [
                  arr (const id) <<< styled1 "welcome-doc-button"
                    (buttonIO "Click here for the User Manual"
                      (const $ const $ Gtk.showUriOnWindow
                          (Nothing :: Maybe Gtk.Window)
                          docFile
                          0)), -- Zero is magic timestamp for "now".
                  spacer
                ],[
                  spacer
                ],[
                  if null examples
                    then focusing welcomeExample $ arr id
                    else styled1 "welcome-gadget" $ form Horizontal
                      [("Open a worked example", focusing welcomeExample pickExample)]
                ]]
            ]],
          styled1 "welcome-gadget" $
            form Vertical [("Don't show this again", focusing welcomeDon'tShow tickBox)]
        ]]
  where
    pickExample :: Gadget' e w (Maybe FilePath)
    pickExample = radio $ const $ ("Empty model", Nothing) : map (_2 %~ Just) examples
    -- pickExample = optional "" $ comboBox $ const exampleItems
    -- exampleItems = map (\(nm, p) -> ComboItem nm Nothing Nothing p) examples
    spacer = arr (const id) <<< message1 "\n\n"  -- Kludge for spacing.


-- | Get the pathname for the flag file. If this file exists then the Welcome screen is not shown.
-- This creates the directory if it doesn't already exist.
welcomeFlagFile :: IO FilePath
welcomeFlagFile = do
  base <- getXdgDirectory XdgConfig $ "DiametricSoftware" </> "DSM"
  createDirectoryIfMissing True base
  return $ base </> "welcome-seen"


-- | On Windows GTK can't open a PDF file in Acrobat, due to non-standard brain damage in Acrobat.
-- Hence we open a HTML file which redirects to the PDF. On Linux we don't have this problem so
-- we just open the PDF.
manualFile :: FilePath
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
manualFile = "Manual.html"
#else
manualFile = "Manual.pdf"
#endif
