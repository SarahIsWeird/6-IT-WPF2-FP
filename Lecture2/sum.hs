sum2 :: (Num a) => [a] -> a
sum2 [] = 0
sum2 (x : xs) = x + sum2 xs

sum3 :: Num a => [a] -> a
sum3 xs = sum' xs 0
  where
    sum' [] y = y
    sum' (x : xs) y = sum' xs $! (x + y)

sum4 :: Num a => [a] -> a
sum4 xs = sum' xs 0
  where
    sum' xs y = case xs of
      [] -> y
      (x : xs) -> sum' xs $! (x + y)

main = do
  print (sum2 [1 .. 10])
  print (sum3 [1 .. 10])
