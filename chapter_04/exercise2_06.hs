myAny :: (a -> Bool) -> [a] -> Bool
myAny p = foldr step False
  where
    step x result = p x || result
