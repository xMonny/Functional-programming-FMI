--Търсим процедура, която намира броя цифри на дадено число.
--Трябва да работи и за отрицателни числа.

countDigits :: Int -> Int
countDigits n
    | n < 0     = countDigits (- n)
    | n < 10    = 1
    | otherwise = 1 + countDigits (n `div` 10)
