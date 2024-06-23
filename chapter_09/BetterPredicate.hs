import Control.Exception (bracket, handle)
import Control.Monad (filterM)
-- the function we wrote earlier
import RecursiveContents (getRecursiveContents)
import System.Directory (Permissions (..), getModificationTime, getPermissions)
import System.FilePath (takeExtension)
import System.IO (IOMode (..), hClose, hFileSize, openFile)
import System.Time (ClockTime (..))

type Predicate =
  FilePath -> -- path to directory entry
  Permissions -> -- permissions
  Maybe Integer -> -- file size (Nothing if not file)
  ClockTime -> -- last modified
  Bool

getFileSize :: FilePath -> IO (Maybe Integer)
betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
  where
    check name = do
      perms <- getPermissions name
      size <- getFileSize name
      modified <- getModificationTime name
      return (p name perms size modified)