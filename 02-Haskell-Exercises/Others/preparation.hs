-- sumLast init n - безкраен поток, който започва с init, а всеки следващ елемент е сума на последните n от потока
-- sumLast 3 5 → [3, 3, 6, 12, 24, 48, 93, 183, ... ]

add :: Int -> Int -> [Int] -> [Int]
add number limit storage
    | limit == length storage = drop 1 storage ++ [number]
    | otherwise = storage ++ [number]

sumLast' :: Int -> Int -> [Int] -> [Int]
sumLast' init n storage = sum storage : sumLast' init n (add (sum storage) n storage)

sumLast :: Int -> Int -> [Int]
sumLast init n = init : init : sumLast' init n [init, init]

-- Да се напише функция transformSum, която преобразува дърво с елементи цели числа в ново дърво със същата структура,
-- в което всеки елемент е заменен със сумата на елементите в поддървото с този корен в началното дърво.

data Tree a = EmptyTree | Tree a (Tree a) (Tree a) deriving Show

instance Foldable Tree where
        foldr _ nv EmptyTree = nv
        foldr op nv (Tree value left right) = foldr op (op value (foldr op nv left)) right 

subtreeSum :: Tree Int -> Int
subtreeSum EmptyTree = 0
subtreeSum (Tree value left right) = value + subtreeSum left + subtreeSum right

transformSum :: Tree Int -> Tree Int
transformSum EmptyTree = EmptyTree
transformSum (Tree value left right) = Tree (value + subtreeSum left + subtreeSum right) (transformSum left) (transformSum right)

example :: Tree Int
example = Tree 5 (Tree 4 (Tree 3 EmptyTree (Tree 1 EmptyTree EmptyTree)) EmptyTree) EmptyTree

-- Да се генерират всички подсписъци на даден такъв
--[1,2,3] -> [[1,2,3],[1,2],[1,3],[2,3],[1],[2],[3],[]]

subsequences :: [a] -> [[a]]
subsequences [] = []
subsequences xs = [] : [suffix | prefixes <- inits xs, suffix <- tails prefixes]
    where inits [] = []
          inits xs = xs : inits (init xs)
          tails [] = []
          tails xs = xs : tails (tail xs)

subsequences' :: [a] -> [[a]]
subsequences' [] = []
subsequences' xs = helper xs (length xs)
    where helper _ 0 = []
          helper [] _ = []
          helper xs n = chunk n xs ++ helper xs (n-1)
          chunk _ [] = []
          chunk 0 _ = []
          chunk n xs = take n xs : chunk n (tail xs)

-- Да се генерира поток sumsOfCubes от тези числа, които са сума от кубовете на две положителни цели числа
sumOfCubes = [x^3 + y^3 | x <- [1..], y <- [1..]]