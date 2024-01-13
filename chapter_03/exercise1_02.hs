data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a)) deriving (Show)

simpleTree :: Tree String
simpleTree =
  Node
    "parent"
    (Just (Node "left child" Nothing Nothing))
    (Just (Node "right child" Nothing Nothing))
