import Control.Exception (SomeException, handle)
import Control.Monad (forM)
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    getCurrentDirectory,
    getDirectoryContents,
  )
import System.FilePath (dropTrailingPathSeparator, isPathSeparator, splitFileName, (</>))
import Text.Regex.Posix ((=~))

type GlobError = String

globToRegex :: String -> Either GlobError String
globToRegex cs = fmap ('^' :) $ (++) <$> globToRegex' cs <*> Right "$"

globToRegex' :: String -> Either GlobError String
globToRegex' "" = Right ""
globToRegex' ('*' : cs) = (++) <$> Right ".*" <*> globToRegex' cs
globToRegex' ('?' : cs) = ('.' :) <$> globToRegex' cs
globToRegex' ('[' : '!' : c : cs) = (("[^" ++ [c]) ++) <$> charClass cs
globToRegex' ('[' : c : cs) = (('[' : [c]) ++) <$> charClass cs
globToRegex' ('[' : _) = Left "unterminated character class"
globToRegex' (c : cs) = (++) <$> escape c <*> globToRegex' cs

escape :: Char -> Either GlobError String
escape c
  | c `elem` regexChars = Right ('\\' : [c])
  | otherwise = Right [c]
  where
    regexChars = "\\+()^$.{}]|"

charClass :: String -> Either GlobError String
charClass (']' : cs) = (']' :) <$> globToRegex' cs
charClass (c : cs) = (c :) <$> charClass cs
charClass [] = Left "unterminated character class"

matchesGlob :: FilePath -> String -> Bool
name `matchesGlob` pat = matchesRegex' name (globToRegex pat)
  where
    matchesRegex' _ (Left _) = False
    matchesRegex' name (Right reg) = name =~ reg

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

namesMatching :: String -> IO [String]
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
    return (filter (matchesGlob pat) names')

listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
  exists <-
    if null baseName
      then doesDirectoryExist dirName
      else doesNameExist (dirName </> baseName)
  return ([baseName | exists])

isHidden ('.' : _) = True
isHidden _ = False
