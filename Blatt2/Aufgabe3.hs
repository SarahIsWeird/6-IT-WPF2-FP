myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a acc -> f a : acc) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr maybeInclude []
  where
    maybeInclude a acc =
      if f a
        then a : acc
        else acc

main = do
  print $ myMap (2 *) [1 .. 10]
  print $ myFilter (< 5) [1 .. 10]
