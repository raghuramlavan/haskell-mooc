fibAux n result previous
    | n == 0 = result
    | otherwise = fibAux (n-1) ( result + previous) result
fibTail n
    |   n == 0 = 0
    | otherwise = fibAux n 1 0


fib 0 = 1

fib 1 = 1

fib n = fib (n-1) + fib (n-2)