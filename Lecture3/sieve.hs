primes = sieve [2 ..]
  where
    sieve (p : l) = p : sieve [y | y <- l, mod y p /= 0]

nthPrime n = head (drop (n - 1) primes)

sum2 :: [Int] -> Int
sum2 [] = 0
sum2 (x : xs) = x + sum2 xs

foldRight :: (a -> b -> b) -> b -> [a] -> b
foldRight _ acc [] = acc
foldRight f acc (x : xs) = foldRight f (f x acc) xs

sum3 :: [Int] -> Int
sum3 = foldRight (+) 0

forall :: (a -> Bool) -> [a] -> Bool
forall p = foldRight (\a b -> p a && b) True

main = print $ sum3 [1 .. 10]
