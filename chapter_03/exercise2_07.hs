intersperse :: a -> [[a]] -> [a]
intersperse x [] = []
intersperse x [xs] = xs
intersperse x (xs : xss) = xs ++ x : intersperse x xss
