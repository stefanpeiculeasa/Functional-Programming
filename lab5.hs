-- LAB5 map filter fold

-- EX 1

patrateimpare :: [Int] -> Int
patrateimpare l = foldr (+) 0 (map (^2) (filter odd l))

-- EX 2

all :: [Bool] -> Bool
all l = foldr (&&) True l

-- EX 3

allVerifies :: (Int -> Bool) -> [Int] -> Bool
allVerifies p l = foldr (&&) True (map p l)

-- EX 4

anyVerifies :: (Int -> Bool) -> [Int] -> Bool
anyVerifies p l = foldr (||) False (map p l)

-- EX 5

mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f l = foldr (\x acc -> f x : acc) [] l

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr p l = foldr (\x acc -> if p x then x : acc else acc) [] l

-- EX 6

listToInt :: [Int] -> Int
listToInt l = foldl (\acc x -> acc * 10 + x) 0 l

-- EX 7 a

rmChar :: Char -> String -> String
rmChar c s = filter (/= c) s

-- EX 7 b

rmCharsRec :: [Char] -> String -> String
rmCharsRec [] s = s
rmCharsRec (h:t) s = rmCharsRec t (rmChar h s)

-- EX 7 c

rmCharsFold :: String -> String -> String
rmCharsFold cs s = foldr rmChar s cs

-- EX 8

myReverse :: [Int] -> [Int]
myReverse l = foldr (\x acc -> acc ++ [x]) [] l

-- EX 9

myElem :: Int -> [Int] -> Bool
myElem y l = foldr (\x acc -> if x == y then True else acc) False l

-- EX 10

myUnzip :: [(a,b)] -> ([a],[b])
myUnzip l = foldr (\(x,y) (xs,ys) -> (x:xs, y:ys)) ([],[]) l

-- EX 11

union :: [Int] -> [Int] -> [Int]
union l1 l2 = foldr (\x acc -> if myElem x acc then acc else x : acc) l2 l1

-- EX 12

intersect :: [Int] -> [Int] -> [Int]
intersect l1 l2 = foldr (\x acc -> if myElem x l2 then x : acc else acc) [] l1

-- EX 13 

permutations :: [Int] -> [[Int]]