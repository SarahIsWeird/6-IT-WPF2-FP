map2 :: (a -> b) -> [a] -> [b]
map2 _ [] = []
map2 f (x : xs) = f x : map2 f xs

map3 :: (a -> b) -> [a] -> [b]
map3 f xs = [f x | x <- xs]

curry2 :: ((a, b) -> c) -> a -> b -> c
curry2 f a b = f (a, b)

add5 :: Int -> Int -> Int -> Int -> Int -> Int
add5' :: Int -> (Int -> Int -> Int -> Int -> Int)
add5'' :: Int -> (Int -> (Int -> (Int -> (Int -> Int))))

addAndMultiply :: Int -> Int -> (Int, Int)
addAndMultiply a b = (a + b, a * b)

add :: Int -> (Int -> Int)
add a b = a + b

add2 :: Int -> Int
add2 b = 2 + b

add3 :: (Int, Int) -> Int
add3 (a, b) = a + b

compose2 :: (b -> c) -> (a -> b) -> (a -> c)
compose2 g f a = g (f a)

compose3 :: (b -> c) -> (a -> b) -> a -> c
compose3 = (.)

-- (add 2) == add2
-- (add 2 n) == add2 n

main = do
  print $ map2 (1 +) [1 .. 10]
  print $ map3 (1 +) [1 .. 10]
  print $ add3 (1, 2)
