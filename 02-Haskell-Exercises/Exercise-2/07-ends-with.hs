--Търсим процедура, която проверява дали дадено число завършва на дадено друго

len :: Int -> Int
len num
    | num < 0   = len(- num)
    | num < 10  = 1
    | otherwise = 1 + len(num `div` 10)

endsWith :: Int -> Int -> Bool
endsWith num test = num `mod` 10^(len test) == test