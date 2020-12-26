--Съчинете процедура, която по дадени три числа, намира сумата от квадратите на по-големите две от тях.
--За по-удобно, може да разбиете задачата на по-малки такива.

sumSquares :: Int -> Int -> Int
sumSquares a b = a*a + b*b

findMinimum :: Int -> Int -> Int -> Int
findMinimum a b c
 | a < b && a < c = a
 | b < a && b < c = b
 | c < a && c < b = c

sumSquaresBiggerTwo :: Int -> Int -> Int -> Int
sumSquaresBiggerTwo a b c
 | findMinimum a b c == a = sumSquares b c --a < b < c
 | findMinimum a b c == b = sumSquares a c --b < a < c
 | otherwise              = sumSquares a b