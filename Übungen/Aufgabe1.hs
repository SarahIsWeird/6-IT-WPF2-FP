foo 0 = 1
foo 1 = 1
foo n = foo (n - 1) + foo (n - 2)

main = do
  print (map foo [0 .. 10])
