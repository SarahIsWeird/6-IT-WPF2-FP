myTake :: [a] -> Int -> [a]
myTake ls 0 = []
myTake [] n = []
myTake (x : xs) 1 = [x]
myTake (x : xs) n = x : myTake xs (n - 1)

main = do
  putStrLn ("The first five elements of [1..10] are " ++ show (myTake [1 .. 10] 5))
  putStrLn ("The first ten elements of [1..69] are " ++ show (myTake [1 .. 69] 10))
  putStrLn ("The first two elements of \"Hello!\" are " ++ show (myTake "Hello!" 2))
