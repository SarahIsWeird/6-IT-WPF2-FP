contains :: Eq a => [a] -> a -> Bool
contains ls el = length (filter ((==) el) ls) > 0

{-
Aufgabe 1.2

Exakt nach Aufgabenstellung (nur für Integer):
isElementInList :: [Int] -> Int -> [Char]
isElementInList [] a = "False"
isElementInList (x:xs) a
    | x == a = "True"
    | otherwise = isElementInList xs a

Ansonsten muss in Typdefinition mit 'Eq a =>' angegeben werden, dass wir eine Typklasse erwarten, die auf (Un-)Gleichheit geprüft werden kann
-}
isElementInList :: Eq a => [a] -> a -> [Char]
isElementInList [] a = "False"
isElementInList (x:xs) a
    | x == a = "True"
    | otherwise = isElementInList xs a

main = do
  putStrLn ("[1, 2, 3] contains 1: " ++ show (contains [1, 2, 3] 1))
  putStrLn ("[] contains 1: " ++ show (contains [] 1))
  putStrLn ("[\"uwu\", \"owo\"] contains \"uwu\": " ++ show (contains ["uwu", "owo"] "uwu"))
  putStrLn ("isElementInList ['a', 'b', 'c'] 'a'" ++ isElementInList ['a', 'b', 'c'] 'a' )
