import GHC.Base (leftSection)

data Tree a
  = Node a (Tree a) (Tree a)
  | Empty
  deriving (Show)

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ leftTree rightTree) = 1 + max (treeHeight leftTree) (treeHeight rightTree)
