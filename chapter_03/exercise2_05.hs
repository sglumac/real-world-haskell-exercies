myReverse :: [a] -> [a]
myReverse xs = reverseHelper xs []
  where
    reverseHelper [] ys = ys
    reverseHelper (x : xs) ys = reverseHelper xs (x : ys)

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == myReverse xs