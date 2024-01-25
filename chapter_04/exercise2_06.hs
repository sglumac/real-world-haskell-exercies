import Data.Char (isSpace)

myAny :: (a -> Bool) -> [a] -> Bool
myAny p = foldr step False
  where
    step x result = p x || result

myCycle :: [a] -> [a]
myCycle xs = foldr (:) (myCycle xs) xs

myWords :: String -> [String]
myWords = foldr step []
  where
    step x []
      | isSpace x = []
      | otherwise = [[x]]
    step x (ys : yss)
      | isSpace x = [] : ys : yss
      | otherwise = (x : ys) : yss

myUnlines :: [String] -> String
myUnlines = foldr step []
  where
    step xs [] = xs
    step xs ys = foldr (:) ('\n' : ys) xs