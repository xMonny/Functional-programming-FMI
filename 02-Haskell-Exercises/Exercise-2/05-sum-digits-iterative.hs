--Търсим сумата от цифрите на дадено число.
--Процедурата да генерира итеративен процес.

sumDigitsHelp :: Int -> Int -> Int
sumDigitsHelp number ans
    | number == 0 = ans
    | otherwise   = sumDigitsHelp (number `div` 10) (ans + (number `mod` 10))

sumDigitsIterative :: Int -> Int
sumDigitsIterative number = sumDigitsHelp number 0

sumDigitsRecursive :: Int -> Int
sumDigitsRecursive 0 = 0
sumDigitsRecursive n = (n `mod` 10) + sumDigitsRecursive(n `div` 10)
