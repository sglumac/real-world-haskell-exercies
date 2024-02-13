import Control.Exception (SomeException, handle)
import Control.Monad (forM)
import Exercise1_02 (matchesGlob)
import Glob (listPlain)
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    getCurrentDirectory,
    getDirectoryContents,
  )
import System.FilePath (dropTrailingPathSeparator, isPathSeparator, splitFileName, (</>))

isWindows :: Bool
isWindows = isPathSeparator '\\'

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

namesMatching pat
  | not (isPattern pat) = do
      exists <- doesNameExist pat
      return ([pat | exists])
  | otherwise = do
      case splitFileName pat of
        ("", baseName) -> do
          curDir <- getCurrentDirectory
          listMatches curDir baseName
        (dirName, baseName) -> do
          dirs <-
            if isPattern dirName
              then namesMatching (dropTrailingPathSeparator dirName)
              else return [dirName]
          let listDir =
                if isPattern baseName
                  then listMatches
                  else listPlain
          pathNames <- forM dirs $ \dir -> do
            baseNames <- listDir dir baseName
            return (map (dir </>) baseNames)
          return (concat pathNames)

doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
  fileExists <- doesFileExist name
  if fileExists
    then return True
    else doesDirectoryExist name

returnEmpty :: SomeException -> IO [String]
returnEmpty _ = return []

listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
  dirName' <-
    if null dirName
      then getCurrentDirectory
      else return dirName
  handle returnEmpty $ do
    names <- getDirectoryContents dirName'
    let names' =
          if isHidden pat
            then filter isHidden names
            else filter (not . isHidden) names
    return (filter (matchesGlob (not isWindows) pat) names')

isHidden ('.' : _) = True
isHidden _ = False
