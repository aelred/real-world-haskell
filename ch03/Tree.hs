data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

simpleTree = Node "parent" (Node "left child" Empty Empty)
                           (Node "right child" Empty Empty)

data Tree2 a = Tree2 a (Maybe (Tree2 a)) (Maybe (Tree2 a))
               deriving (Show)

height (Node _ left right) = max (height left) (height right) + 1
height Empty               = 0
