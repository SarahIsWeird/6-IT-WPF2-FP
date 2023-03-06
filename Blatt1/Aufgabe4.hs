myTake :: [a] -> Int -> [a]
myTake ls 0 = []
myTake [] n = []
myTake (x : xs) 1 = [x]
myTake (x : xs) n = x : myTake xs (n - 1)

{-
Aufgabe 1.4
-}
nimm :: [a] -> Int -> [a]
nimm [] n = []
nimm xs 0 = []
nimm (x:xs) n =  x : nimm xs (n-1) 

main = do
  putStrLn ("The first five elements of [1..10] are " ++ show (myTake [1 .. 10] 5))
  putStrLn ("The first ten elements of [1..69] are " ++ show (myTake [1 .. 69] 10))
  putStrLn ("The first two elements of \"Hello!\" are " ++ show (myTake "Hello!" 2))
  putStrLn ("nimm [1, 2, 3, 4, 5, 6, 7, 8] 5 )" ++ show(nimm [1, 2, 3, 4, 5, 6, 7, 8] 5 ))
