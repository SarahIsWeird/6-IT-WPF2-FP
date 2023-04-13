head2 :: [a] -> a
head2 [] = error "no head in empty list"
head2 (x : _) = x

tail2 :: [a] -> [a]
tail2 [] = []
tail2 (_ : xs) = xs

last2 :: [a] -> a
last2 [] = error "no last element in empty list"
last2 [x] = x
last2 (_ : xs) = last2 xs

last3 = head . reverse

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [x] = True
-- sorted [x, y] = x < y
sorted (x : y : xs) = (x <= y) && sorted (y : xs)

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) = (quicksort [s | s <- xs, s <= x]) ++ [x] ++ (quicksort [l | l <- xs, x < l])

quicksort2 :: (Ord a) => [a] -> [a]
quicksort2 [] = []
quicksort2 (x : xs) = quicksort smaller ++ [x] ++ quicksort larger
  where
    smaller = [s | s <- xs, s <= x]
    larger = [l | l <- xs, x < l]

merge2 :: Ord a => [a] -> [a] -> [a]
merge2 a b = quicksort (a ++ b)

compose2 :: (b -> c) -> (a -> b) -> (a -> c)
compose2 g f val = g (f val)

compose3 :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
compose3 g f x y = g (f x y)

merge4 = compose3 quicksort (++)

main = do
  print (head2 [1 .. 10])
  print (tail2 [1 .. 10])
  print (last2 [1 .. 10])
  print (last3 [1 .. 10])
  print (sorted [1 .. 10])
  print (sorted [2, 3, 1])
  print (quicksort [3, 6, 2, 5, 1, 4])
  print (quicksort2 [3, 6, 2, 5, 1, 4])
  print (merge2 [1, 4, 2, 7, 4, 6] [32, 89, 1, 6, 1, -1, 7, 3])
  print (merge4 [1, 4, 2, 7, 4, 6] [32, 89, 1, 6, 1, -1, 7, 3])
  print (compose2 (\x -> x * x) (2 +) 0)
