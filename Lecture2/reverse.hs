reverseInt :: [Int] -> [Int]
reverseInt [] = []
reverseInt (x : xs) = reverseInt xs ++ [x]

reverse2 :: [a] -> [a]
reverse2 [] = []
reverse2 (x : xs) = reverse2 xs ++ [x]

reverse3 :: [a] -> [a]
reverse3 xs = reverse' xs []
  where
    reverse' [] ys = ys
    reverse' (x : xs) ys = reverse' xs $! (x : ys)

main = do
  print (reverseInt [1 .. 10])
  print (reverse2 "Hello, world! uwu")
  print (reverse3 "bababooey")
