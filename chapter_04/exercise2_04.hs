recursiveTakeWhile :: (a -> Bool) -> [a] -> [a]
recursiveTakeWhile p (x : xs)
  | p x = x : recursiveTakeWhile p xs
  | otherwise = []

foldingTakeWhile :: (a -> Bool) -> [a] -> [a]
foldingTakeWhile p xs = foldr step [] xs
  where
    step x xs
      | p x = x : xs
      | otherwise = []
