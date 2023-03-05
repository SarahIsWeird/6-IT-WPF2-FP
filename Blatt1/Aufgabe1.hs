isLeapYear :: Int -> Bool
isLeapYear y
  | mod y 400 == 0 = True
  | mod y 100 == 0 = False
  | mod y 4 == 0 = True
  | otherwise = False

main = do
  putStrLn ("1997 is a leap year: " ++ show (isLeapYear 1997))
  putStrLn ("1996 is a leap year: " ++ show (isLeapYear 1996))
  putStrLn ("2000 is a leap year: " ++ show (isLeapYear 2000))
  putStrLn ("1900 is a leap year: " ++ show (isLeapYear 1900))
