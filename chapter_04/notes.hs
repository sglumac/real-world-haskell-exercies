{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use sum" #-}
{-# HLINT ignore "Avoid lambda" #-}

myListCopy10L :: [Integer]
myListCopy10L = take 10 (foldl (\ys x -> ys ++ [x]) [] [1 .. 100000000])

myListCopy10R :: [Integer]
myListCopy10R = take 10 (foldr (:) [] [1 .. 100000000])

myListReverse10L :: [Integer]
myListReverse10L = take 10 (foldl (\ys x -> x : ys) [] [1 .. 100000000])

myListReverse10R :: [Integer]
myListReverse10R = take 10 (foldr (:) [] [1 .. 100000000])

mySumL :: Integer
mySumL = foldl (+) 0 [1 .. 100000000]

mySumR :: Integer
mySumR = foldr (+) 0 [1 .. 100000000]
