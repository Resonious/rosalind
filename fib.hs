fib 1 _ = 1
fib 2 _ = 1
fib n k = (fib (n - 1) k) + ((fib (n - 2) k) * k)

main = do
  print (fib 33 4)
