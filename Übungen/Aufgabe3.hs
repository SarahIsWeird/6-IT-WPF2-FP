myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x : xs) = f x : myMap f xs

main =
  print (myMap (2 *) [1 .. 10])
