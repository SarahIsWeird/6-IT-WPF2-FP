data Tree a
  = Empty
  | Node a (Tree a) (Tree a)
  deriving (Show)

tInsert :: (Ord a) => a -> Tree a -> Tree a
tInsert val tree =
  case tree of
    Empty -> Node val Empty Empty
    Node a tree1 tree2 ->
      if val < a
        then Node a (tInsert val tree1) tree2
        else Node a tree1 (tInsert val tree2)

tElem :: (Eq a) => a -> Tree a -> Bool
tElem a tree =
  case tree of
    Empty -> False
    Node val tree1 tree2 ->
      a == val || tElem a tree1 || tElem a tree2

tSum :: (Num a) => Tree a -> a
tSum tree =
  case tree of
    Empty -> 0
    Node a tree1 tree2 ->
      a + tSum tree1 + tSum tree2

tFlatten :: Tree a -> [a]
tFlatten tree =
  case tree of
    Empty -> []
    Node val tree1 tree2 ->
      tFlatten tree1 ++ [val] ++ tFlatten tree2

tDepth :: Tree a -> Int
tDepth tree =
  case tree of
    Empty -> 0
    Node _ tree1 tree2 ->
      1 + max (tDepth tree1) (tDepth tree2)

tCount :: Tree a -> Int
tCount tree =
  case tree of
    Empty -> 0
    Node _ tree1 tree2 ->
      1 + tCount tree1 + tCount tree2

tMap :: (a -> b) -> Tree a -> Tree b
tMap f tree =
  case tree of
    Empty -> Empty
    Node a tree1 tree2 ->
      Node (f a) (tMap f tree1) (tMap f tree2)

tFold :: (a -> b -> b) -> b -> Tree a -> b
tFold f init tree =
  case tree of
    Empty -> init
    Node a tree1 tree2 ->
      let rFolded = tFold f init tree2
          lFolded = tFold f rFolded tree1
       in f a lFolded

main = do
  let tree = tInsert 2 $ tInsert 5 $ tInsert 4 $ tInsert 3 $ tInsert 1 Empty
  print tree
  putStrLn $ "3 is in the tree: " <> show (tElem 3 tree)
  putStrLn $ "6 is in the tree: " <> show (tElem 6 tree)
  putStrLn $ "The sum of the tree is " <> show (tSum tree)
  putStrLn $ "The flattened tree is " <> show (tFlatten tree)
  putStrLn $ "The depth of the three is " <> show (tDepth tree)
  putStrLn $ "The tree has " <> show (tCount tree) <> " elements"
  putStrLn $ "The tree times 2 is " <> show (tMap (2 *) tree)
  putStrLn $ "The tree summed with fold is " <> show (tFold (+) 0 tree)
  putStrLn $ "The tree flattened with fold is " <> show (tFold (:) [] tree)
