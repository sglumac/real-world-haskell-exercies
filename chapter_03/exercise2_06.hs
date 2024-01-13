import Data.List (sortBy)

compareByLength :: [a] -> [a] -> Ordering
compareByLength xs ys = compare (length xs) (length ys)

sortAccordingToLength :: [[a]] -> [[a]]
sortAccordingToLength = sortBy compareByLength