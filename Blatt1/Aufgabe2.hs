contains :: Eq a => [a] -> a -> Bool
contains ls el = length (filter ((==) el) ls) > 0

main = do
  putStrLn ("[1, 2, 3] contains 1: " ++ show (contains [1, 2, 3] 1))
  putStrLn ("[] contains 1: " ++ show (contains [] 1))
  putStrLn ("[\"uwu\", \"owo\"] contains \"uwu\": " ++ show (contains ["uwu", "owo"] "uwu"))
