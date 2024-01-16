splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith splitOn xs = map reverse allReversed
  where
    splitHelper [] ys = [ys]
    splitHelper (z : zs) ys
      | splitOn z = ys : splitHelper zs []
      | otherwise = splitHelper zs (z : ys)
    allReversed = splitHelper xs []
