--Искаме да обърнем число в двоична бройна система

convertToBinary :: Int -> Int -> Int -> Int
convertToBinary pos num ans
    | num == 0  = ans
    | otherwise = convertToBinary (pos + 1) (num `div` 2) (ans + (num `mod` 2)*10^pos)

toBinary :: Int -> Int
toBinary number = convertToBinary 0 number 0
