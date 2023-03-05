sumList :: [Int] -> Int
sumList [] = 0
sumList (x : xs) = x + sumList xs

main = do
  putStrLn ("The sum of [1..5] is " ++ show (sum [1 .. 5]))
  putStrLn ("The sum of [1..100] is " ++ show (sum [1 .. 100]))
