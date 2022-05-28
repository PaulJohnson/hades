{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}


{- |

Lock files for editing.

Editing is a long transaction, so there are no blocking or \"with\" functions

This works by creating a temporary lock file and getting an exclusive lock on that file. The
lock file contains the name of the user with the lock so that attempting to get the lock on
a file locked by someone else lets the application give a useful message to the user.

There is a very narrow race condition when a lock is released: it is possible for another
process to see that the file already exists but then attempt to get the exclusive lock after
it has been deleted, leading to a "File not found" error.
-}

module System.Hades.EditLock (
  EditLock,
  EditLockResult (..),
  tryEditLock,
  dropEditLock
) where

import Control.Exception
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.FileLock
import System.FilePath
import System.IO.Error

data EditLock = EditLock FilePath FileLock


data EditLockResult =
  EditLockSuccessful EditLock
  | EditLockFailed Text  -- ^ Message explaining why the lock acquisition failed.
  | EditLockError IOException


{- |
To gain an edit lock on a file \"foo.txt\" this will:

1. Check that \"foo.txt\" is writable.

2. Create a lockfile \"$foo.txt$\" if it does not already exist.

3. Get an exclusive lock on that file. If no lock is available then the attempt fails.

4. Write the user name in it.

If the lock is obtained then it returns an "EditLock" value. If the lock is not obtained then
it returns the name of the user holding the lock. If it cannot create the lock file (e.g. because
the directory is read-only) then it returns an "IOException".
-}
tryEditLock :: FilePath -> IO EditLockResult
tryEditLock p = flip catchIOError (return . EditLockError) $ do
  perm <- getPermissions p
  if writable perm
    then do
      let p1 = lockFileName p
      tryLockFile p1 Shared >>= \case
        Nothing ->  -- File is already locked.
          return $ EditLockFailed $ T.pack p <> " is currently being edited."
        Just l ->
          return $ EditLockSuccessful $ EditLock p1 l
    else return $ EditLockFailed $ T.pack p <> " is not writeable by you"


-- | Drop the lock on the file and delete the lockfile.
dropEditLock :: EditLock -> IO ()
dropEditLock (EditLock p l) = flip catchIOError (const $ return ()) $ do
  -- Ignore any errors here; probably the file doesn't exist, or permissions have changed
  -- or something. The worst that can happen is that a stale lock file is left hanging around.
  unlockFile l
  removeFile p


-- | The name of the lock file.
lockFileName :: FilePath -> FilePath
lockFileName p =
  let (d, f) = splitFileName p
  in d </> "$" <> f <> "$"
