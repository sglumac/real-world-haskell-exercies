data List a = Cons a (List a) | Nil deriving (Show)

fromList :: [a] -> List a
fromList = foldr Cons Nil

toList :: List a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

durian :: List Char
durian = fromList "durian"

maybeBooleans :: List (Maybe Bool)
maybeBooleans = fromList [Just True, Nothing, Just False]