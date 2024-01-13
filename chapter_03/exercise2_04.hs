myReverse :: [a] -> [a]
myReverse xs = reverseHelper xs []
  where
    reverseHelper [] ys = ys
    reverseHelper (x : xs) ys = reverseHelper xs (x : ys)

myMerge :: [a] -> [a] -> [a]
myMerge (x : xs) ys = x : myMerge xs ys
myMerge [] (y : ys) = y : myMerge [] ys
myMerge [] [] = []

palindrome :: [a] -> [a]
palindrome xs = myMerge xs (myReverse xs)
