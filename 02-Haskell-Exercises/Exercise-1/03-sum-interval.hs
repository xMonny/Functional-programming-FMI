--Съчинете процедура, която намира сумата на числата в даден затворен интервал.

sumInterval :: Int -> Int -> Int
sumInterval start end
    | start == end = start
    | start < end  = start + sumInterval (start + 1) end
    | otherwise    = sumInterval end start
