{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}
mySum [] = 0
mySum (x : xs) = x + mySum xs

myLength [] = 0
myLength (x : xs) = 1 + myLength xs

myMean :: (Fractional a, Integral b) => [b] -> a
myMean xs = fromIntegral (mySum xs) / fromIntegral (myLength xs)
