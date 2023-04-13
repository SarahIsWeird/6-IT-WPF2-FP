cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct lsA lsB = [(a, b) | a <- lsA, b <- lsB]

main = print $ cartesianProduct [1 .. 4] ['a' .. 'd']
