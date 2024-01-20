recursiveGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
recursiveGroupBy _ [] = []
recursiveGroupBy p (x : xs) = (x : takeWhile (p x) xs) : foldlGroupBy p (dropWhile (p x) xs)

foldlGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
foldlGroupBy p = foldl step []
  where
    step [] y = [[y]]
    step xss y
      | p (head (last xss)) y = init xss ++ [last xss ++ [y]]
      | otherwise = xss ++ [[y]]
