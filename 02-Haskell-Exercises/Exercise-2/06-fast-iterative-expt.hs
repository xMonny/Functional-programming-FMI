--Стъпвайки на дефиницията за бързо повдигане на степен,
--търсим такава, която генерира итеративен процес

exptHelp :: Int -> Int -> Int -> Int
exptHelp num degree ans
    | degree == 0 = ans
    | otherwise   = exptHelp num (degree - 1) (ans*num)

exptIterative :: Int -> Int -> Int
exptIterative num degree = exptHelp num degree 1

exptRecursive :: Int -> Int -> Int
exptRecursive num 0      = 1
exptRecursive num degree = num * (exptRecursive num (degree - 1))