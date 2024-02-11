import Data.Char (toLower)
import GlobToRegex qualified
import Text.Regex.Posix ((=~))

globToRegex :: Bool -> String -> String
globToRegex caseSensitive
  | caseSensitive = GlobToRegex.globToRegex
  | otherwise = GlobToRegex.globToRegex . map toLower

matchesGlob :: Bool -> FilePath -> String -> Bool
matchesGlob caseSensitive name pat = map toLower name =~ globToRegex caseSensitive pat
