data Tree a
  = Empty
  | Node a (Tree a) (Tree a)
  deriving (Eq, Show)

sumTree :: (Num a) => Tree a -> a
sumTree tree =
  case tree of
    Empty -> 0
    Node n a b -> n + sumTree a + sumTree b

tree =
  Node
    1
    ( Node
        2
        Empty
        Empty
    )
    ( Node
        3
        ( Node
            4
            Empty
            Empty
        )
        Empty
    )

main = print . sumTree $ tree
