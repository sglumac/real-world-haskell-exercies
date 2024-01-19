{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
import Data.Char (digitToInt, isDigit)

foldChars :: Either ErrorMessage Int -> Char -> Either ErrorMessage Int
foldChars (Left acc) _ = Left acc
foldChars (Right acc) c
  | isDigit c = Right (10 * acc + digitToInt c)
  | otherwise = Left "not a digit"

type ErrorMessage = String

asInt_either :: String -> Either ErrorMessage Int
asInt_either ('-' : cs) = process result
  where
    process (Left err) = Left err
    process (Right converted) = Right (converted * (-1))
    result = foldl foldChars (Right 0) cs
asInt_either cs = foldl foldChars (Right 0) cs
