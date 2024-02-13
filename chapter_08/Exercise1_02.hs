module Exercise1_02 (matchesGlob) where

import Data.Char (toLower)
import GlobRegex qualified
import Text.Regex.Posix ((=~))

globToRegex :: Bool -> String -> String
globToRegex caseSensitive
  | caseSensitive = GlobRegex.globToRegex
  | otherwise = GlobRegex.globToRegex . map toLower

matchesGlob :: Bool -> FilePath -> String -> Bool
matchesGlob caseSensitive name pat = map toLower name =~ globToRegex caseSensitive pat
