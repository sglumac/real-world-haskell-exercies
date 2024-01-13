{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}
myLength :: [a] -> Int
myLength [] = 0
myLength (x : xs) = 1 + myLength xs
