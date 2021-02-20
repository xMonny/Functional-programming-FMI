import Prelude

data Tree a = Empty | Tree {root :: a, left :: Tree a, right :: Tree a} deriving (Show, Eq)
data Strategy = Preorder | Inorder | Postorder deriving (Show, Eq)

subtreeSum :: Tree Int -> Int
subtreeSum Empty = 0
subtreeSum (Tree root left right) = root + subtreeSum left + subtreeSum right

transferSum :: Tree Int -> Tree Int
transferSum Empty = Empty
transferSum (Tree root left right) = Tree newRoot newLeft newRight
    where newRoot = root + subtreeSum left + subtreeSum right
          newLeft = transferSum left
          newRight = transferSum right

order :: Tree a -> Strategy -> [a]
order Empty _ = []
order (Tree root left right) s
    | s == Preorder = [root] ++ order left s ++ order right s
    | s == Inorder = order left s ++ [root] ++ order right s
    | s == Postorder = order left s ++ order right s ++ [root]

isBinaryTree :: Tree Int -> Bool
isBinaryTree Empty = True
isBinaryTree (Tree value left right)
    | left == Empty || right == Empty = True
    | value > root left && value < root right = isBinaryTree left && isBinaryTree right
    | otherwise = False

addToBinary :: Int -> Tree Int -> Tree Int
addToBinary node Empty = Tree node Empty Empty
addToBinary node (Tree root left right)
    | node < root = Tree root (addToBinary node left) right
    | node > root = Tree root left (addToBinary node right)

height :: Tree a -> Int
height Empty = 0
height (Tree root left right)
    | height left > height right = 1 + height left
    | otherwise = 1 + height right

isBalanced :: Tree a -> Bool
isBalanced Empty = True
isBalanced (Tree root left right)
    | abs (height left - height right) > 1 = False
    | otherwise = isBalanced left && isBalanced right

isMemberInBinary :: (Ord a) => a -> Tree a -> Bool
isMemberInBinary _ Empty = False
isMemberInBinary e (Tree root left right)
    | e == root = True
    | e < root = isMemberInBinary e left
    | e > root = isMemberInBinary e right

valuesInBinary :: Tree a -> [a]
valuesInBinary Empty = []
valuesInBinary (Tree root left right) = root : (valuesInBinary left ++ valuesInBinary right)

sizeBinary :: Tree a -> Int
sizeBinary Empty = 0
sizeBinary (Tree root left right) = length (valuesInBinary (Tree root left right))

sortBinary :: Tree a -> [a]
sortBinary Empty = []
sortBinary tree = order tree Inorder

example :: Tree Int
example = Tree 4 (Tree 5 (Tree 6 Empty Empty) Empty) (Tree 11 Empty (Tree 10 Empty Empty))

binaryTree :: Tree Int
binaryTree = Tree 10 (Tree 8 (Tree 5 (Tree 3 Empty (Tree 4 Empty Empty)) (Tree 6 Empty (Tree 7 Empty Empty))) (Tree 9 Empty Empty))
                     (Tree 30 (Tree 25 Empty (Tree 28 Empty Empty)) (Tree 50 (Tree 41 Empty Empty) (Tree 100 Empty Empty)))

-- Път от корен до възел в двоично дърво кодираме с поредица от цифри 0 и 1, която започва с цифрата 1,
--а за всяка следваща цифра 0 означава завиване по левия клон, а 1 — по десния. 
--Да се реализира функция sameAsCode, която в двоично дърво от числа връща такова число x, което съвпада по 
--стойност с двоичното число, кодиращо пътя от корена до x, или 0, ако такова число няма. 
--Представянето на дървото е по ваш избор.

pow :: Int -> Int -> Int
pow _ 0 = 1
pow number degree = number * pow number (degree - 1)

convert :: Int -> Int -> Int -> Int -> Int
convert _ _ _ 0 = 0
convert from to pos number = remainder * pow from pos + convert from to (pos+1) (number `div` to)
    where remainder = number `rem` to

convertToDecimal :: Int -> Int
convertToDecimal = convert 2 10 0

putInList :: Int -> [Int]
putInList 0 = []
putInList number = putInList (number `div` 10) ++ [number `rem` 10]

makeSearch :: [Int] -> Int -> Tree Int -> Int
makeSearch _ _ Empty = 0
makeSearch [] _ _ = 0
makeSearch (x : storage) decimal (Tree root left right)
    | root == decimal = root
    | x == 0 = makeSearch storage decimal left
    | x == 1 = makeSearch storage decimal right

sameAsCode :: Int -> Tree Int -> Int
sameAsCode binary (Tree root left right) = makeSearch binaryList decimal tree
    where binaryList = putInList binary
          decimal = convertToDecimal binary
          tree = Tree root left right

--Да се напише функция maxSumPath, която приема за аргумент двоично дърво с числа във възлите и намира
-- максималната сума на числата по някой път от корен до листо.

maxSumPath :: (Num a, Ord a) => Tree a -> a
maxSumPath Empty = 0
maxSumPath (Tree root left right) = root + max (maxSumPath left) (maxSumPath right)

isLeaf :: (Eq a) => Tree a -> Bool
isLeaf Empty = False
isLeaf (Tree root left right) = left == Empty && right == Empty

--Да се напише функция prune, която по дадено дърво t връща ново дърво t', което представлява t,
--в което всички листа са премахнати.
prune :: (Eq a) => Tree a -> Tree a
prune Empty = Empty
prune (Tree root left right)
    | isLeaf left && isLeaf right = Tree root Empty Empty
    | not (isLeaf left) && not (isLeaf right) = Tree root (prune left) (prune right)
    | not (isLeaf left) = Tree root (prune left) Empty
    | not (isLeaf right) = Tree root Empty (prune right)

--Да се напише функция bloom, която по дадено дърво t връща ново дърво t', което представлява t,
--в което на всички листа са добавени по два наследника - нови листа. Стойността в тези нови листа да е същата,
--като в оригиналното листо, от което са излезли.

bloom :: (Eq a) => Tree a -> Tree a
bloom Empty = Empty
bloom (Tree root left right)
    | left == Empty && right == Empty = Tree root (Tree root Empty Empty) (Tree root Empty Empty)
    | otherwise = Tree root (bloom left) (bloom right)

--Да се напише функция treeМap, която map-ва дадена функция f на всички стойности в дадено дърво
--(тук не е задължително стойностите в дървото да са числа).

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ Empty = Empty
treeMap f (Tree root left right) = Tree (f root) (treeMap f left) (treeMap f right)

---------------------------------------------------
-- Разстояние между две точки с pattern matching
distance :: (Floating a) => (a, a) -> (a, a) -> a
distance p1 p2 = sqrt(dx^2 + dy^2)
    where dx = fst p2 - fst p1
          dy = snd p2 - snd p1

--Обръщане на списък
reverse' :: [a] -> [a]
reverse' = foldl (\rest x -> x:rest) []

reverse'' :: [a] -> [a]
reverse'' = foldr (\x rest -> rest ++ [x]) []

--zip
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x : xs) (y : ys) = (x,y) : zip' xs ys

zipWith' :: [a1] -> [a2] -> (a1 -> a2 -> a3) -> [a3]
zipWith' [] _ _ = []
zipWith' _ [] _ = []
zipWith' (x : xs) (y : ys) op = op x y : zipWith' xs ys op

--repeat element n times
repeat' :: a -> Int -> [a]
repeat' x n = foldr (\_ rest -> x:rest) [] [1..n]

repeat'' :: a -> Int -> [a]
repeat'' x n = map (\_ -> x) [1..n]

repeat''' :: a -> Int -> [a]
repeat''' x n = [x | _ <- [1..n]]

--number of dividers
dividers :: Int -> Int
dividers n = length [d | d <- [1..n], n `mod` d == 0]

--prime numbers
primes :: [Int] -> [Int]
primes xs = [x | x <- xs, dividers x == 2, x /= 2]

primes' :: [Int] -> [Int]
primes' xs = filter (/= 2) (filter (\x -> dividers x == 2) xs)

--pitagorean
pitagorean :: [(Int, Int, Int)]
pitagorean = [(a, b, c) | c <- [1..], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]

--unique elements from list
unique' :: (Eq a) => [a] -> [a]
unique' [] = []
unique' (x : xs) = x : unique' ([y | y <- xs, x /= y])

unique'' :: (Eq a) => [a] -> [a]
unique'' [] = []
unique'' (x : xs) = x : unique' (filter (/=x) xs)

---------------------------------------------------------

-- операции върху комплексни числа

compl :: (a, a) -> (a, a) -> (a -> a -> a) -> (a, a)
compl c1 c2 op = (changed_c1, changed_c2)
    where x1 = fst c1
          y1 = snd c1
          x2 = fst c2
          y2 = snd c2
          changed_c1 = op x1 x2
          changed_c2 = op y1 y2

complAdd :: (Num a) => (a, a) -> (a, a) -> (a, a)
complAdd c1 c2 = compl c1 c2 (+)

complSub :: (Num a) => (a, a) -> (a, a) -> (a, a)
complSub c1 c2 = compl c1 c2 (-)

complMul :: (Num a) => (a, a) -> (a, a) -> (a, a)
complMul c1 c2 = compl c1 c2 (*)

----------------------------------------------
--foldr или foldl
minimum' :: [Int] -> Int
minimum' [] = -1
minimum' (x : xs) = foldr min x xs --foldl same

maximum' :: [Int] -> Int
maximum' [] = -1
maximum' (x : xs) = foldr max x xs --foldl same

length' :: [a] -> Int
length' xs = sum (foldr (\_ rest -> 1:rest) [] xs)

length'' :: [a] -> Int
length'' xs = sum (foldl (\rest _ -> rest ++ [1]) [] xs)

all' :: (Eq a) => [a] -> a -> Bool
all' xs e = foldr (\i rest -> i == e && rest) True xs 
--foldl (\_ i -> i == e) True xs

any' :: (Eq a) => [a] -> a -> Bool
any' xs e = foldr (\i rest -> i == e || rest) False xs

-------------------------------------------------
-- sumLast init n - безкраен поток, който започва с init, а всеки следващ елемент е сума на последните n от потока
-- sumLast 3 5 → [3, 3, 6, 12, 24, 48, 93, 183, ... ]

sumLast' :: Int -> [Int] -> [Int]
sumLast' n storage
    | length storage >= n = current : sumLast' n (drop 1 storage ++ [current])
    | otherwise = current : sumLast' n (storage ++ [current])
    where current = sum storage

sumLast :: Int -> Int -> [Int]
sumLast init n = init : sumLast' n [init]

-- Да се генерират всички подсписъци на даден такъв
-- [1,2,3] -> [[1,2,3],[1,2],[2,3],[1],[2],[3]]
-- prefixes = [1,2,3], [1,2], [1]
-- suffixes of [1,2,3] = [1,2,3], [2,3], [3]
-- suffixes of [1,2] = [1,2], [2]
-- suffixes of [1] = [1]

sublist :: (Eq a) => [a] -> [[a]]
sublist [] = []
sublist xs = [] : [x | pref <- prefixes xs, x <- suffixes pref]
    where prefixes [] = []
          prefixes xs = xs : prefixes (init xs)
          suffixes [] = []
          suffixes xs = xs : suffixes (tail xs)

-- Да се генерира поток sumsOfCubes от тези числа, които са сума от кубовете на две положителни цели числа
sumOfCubes :: [Int]
sumOfCubes = [x^3 + y^3 | x <- [1..], y <- [1..]]

-- split - функция, която приема String и Char и разделя низа според разделителния знак
-- например: split "abc,def,ghi" ',' == ["abc", "def", "ghi"]
--           split "abc,def,ghi" ';' == ["abc,def,ghi"]
--           split "abc;def,ghi" ';' == ["abc", "def,ghi"]

split :: String -> Char -> [String]
split [] _ = []
split str c = untilFirstSeparator : split rest c
    where untilFirstSeparator = takeWhile (/= c) str
          rest = case dropWhile (/= c) str of
                    [] -> []
                    (_ : xs) -> xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

safeUncons :: [a] -> Maybe (a, [a])
safeUncons [] = Nothing
safeUncons (x : xs) = Just (x, xs)

findIndex :: (Eq a) => a -> [a] -> Maybe Int
findIndex _ [] = Nothing
findIndex s (x : xs)
    | s == x = Just 0
    | (Just idx) <- findIndex s xs = Just (1 + idx)
    | otherwise = Nothing

maybeToList' :: Maybe a -> [a]
maybeToList' Nothing = []
maybeToList' (Just x)  = [x]

mapMaybe' :: (a -> b) -> Maybe a -> Maybe b
mapMaybe' _ Nothing = Nothing
mapMaybe' f (Just x) = Just (f x)

data Days = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Enum, Show, Bounded, Ord, Eq)