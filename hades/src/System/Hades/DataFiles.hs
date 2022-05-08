{-# LANGUAGE CPP #-}

{-
Copyright © Paul Johnson 2020. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.
-}

module System.Hades.DataFiles (
   getProgramDataFolder,
   getEvidenceDataFolder,
   getUserName
) where

import Paths_hades
import System.FilePath


-- Import and re-export the OS function for finding the users name, depending on the platform.
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import System.Win32.Info (getUserName)
#else
import System.Posix.User (getRealUserID, getUserEntryForID, userName)
-- | The name of the user running the program as provided by the operating system.
getUserName :: IO String
getUserName = do
   -- getLoginName does not work with xrdp due to utmp not set. And this is more secure anyway.
   euid <- getRealUserID
   pw <- getUserEntryForID euid
   return (userName pw)
#endif


-- | The folder where the data files are stored.
getProgramDataFolder :: IO FilePath
getProgramDataFolder = (</> "share" </> "hades") <$> getDataDir

-- | The folder where the evidence data files (currently just icons) are stored.
getEvidenceDataFolder :: IO FilePath
getEvidenceDataFolder = (</> "evidence") <$> getDataDir
