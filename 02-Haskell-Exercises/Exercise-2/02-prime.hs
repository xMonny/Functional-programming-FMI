--Искаме да проверим дали число е просто.

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

isPrime :: Int -> Int ->  Bool
isPrime start number
    | number < 0              = isPrime start (number - 1)
    | start > isqrt number    = True
    | number `mod` start == 0 = False
    | otherwise               = isPrime(start + 1) number

prime :: Int -> Bool
prime number
    | number == 0 = False
    | number == 1 = False
    | otherwise   = isPrime 2 number