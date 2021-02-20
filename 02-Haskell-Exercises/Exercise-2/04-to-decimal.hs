--Искаме да обърнем число в десетична бройна система

convertToDecimal :: Int -> Int -> Int -> Int
convertToDecimal pos num ans
    | num == 0  = ans
    | otherwise = convertToDecimal (pos + 1) (num `div` 10) (ans + (num `mod` 10)*2^pos)

toDecimal :: Int -> Int
toDecimal number = convertToDecimal 0 number 0