{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
import Data.Char (digitToInt)

foldChars :: Int -> Char -> Int
foldChars acc c = 10 * acc + digitToInt c

asInt_fold :: String -> Int
asInt_fold = foldl foldChars 0
