--Търсим процедура, която проверява дали едно число е палиндром.
--Трябва да работи и за отрицателни числа.

countDigits :: Int -> Int
countDigits number
    | number < 0 = countDigits(- number)
    | number < 10 = 1
    | otherwise = 1 + countDigits(number `div` 10)

extent :: Int -> Int
extent n = countDigits n - 1

power :: Int -> Int -> Int
power a b = a ^ b

reverseDigits :: Int -> Int
reverseDigits number
    | number < 0  = reverseDigits (- number)
    | number < 10 = number
    | otherwise   = (number `mod` 10)*(power 10 (extent number)) + reverseDigits(number `div` 10)

palindrome :: Int -> Bool
palindrome number
    | number < 0                     = palindrome (- number)
    | number < 10                    = True
    | number == reverseDigits number = True
    | otherwise                      = False
