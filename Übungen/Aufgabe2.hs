doubleAll :: (Num a) => [a] -> [a]
-- doubleAll ls = map (2 *) ls
-- doubleAll = map ((*) 2)
doubleAll = map (2 *)

doubleAllRecursive :: (Num a) => [a] -> [a]
doubleAllRecursive [] = []
doubleAllRecursive (x : xs) =
  x * 2 : doubleAllRecursive xs

doubleAllComprehension :: (Num a) => [a] -> [a]
doubleAllComprehension ls = [x * 2 | x <- ls]

main = do
  print (doubleAll [1 .. 10])
  print (doubleAllRecursive [1 .. 10])
  print (doubleAllComprehension [1 .. 10])
