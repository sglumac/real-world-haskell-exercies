import Data.Foldable (foldl')
import Data.List (groupBy)

recursiveGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
recursiveGroupBy _ [] = []
recursiveGroupBy p (x : xs) = (x : takeWhile (p x) xs) : recursiveGroupBy p (dropWhile (p x) xs)

foldlGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
foldlGroupBy p = foldl' step []
  where
    step [] y = [[y]]
    step xss y
      | p (head (last xss)) y = init xss ++ [last xss ++ [y]]
      | otherwise = xss ++ [[y]]

foldrGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
foldrGroupBy p xs = yss
  where
    yss = foldr step [] xs
    step x [] = [[x]]
    step x ([] : yss) = [x] : yss
    step x ((y : ys) : yss)
      | p x y = (x : y : ys) : yss
      | otherwise = [x] : (y : ys) : yss

sample1Input = [1, 2, 2, 3, 1, 2, 0, 4, 5, 2]

sample1 = groupBy (<=) sample1Input

test1 = foldrGroupBy (<=) sample1Input

sample2 = take 3 (groupBy (>=) [1 .. 5])

test2 = take 3 (foldrGroupBy (>=) [1 .. 5])