-- EX1

{-
[ x^2 |x <- [1..10], x `rem` 3 == 2 ] = [4,25,64]
[ (x,y) | x <- [1..5], y <- [x..(x+2)] ] = [(1,1),(1,2),(1,3),(2,2),(2,3),(2,4),(3,3),(3,4),(3,5),(4,4),(4,5),(4,6),(5,5),(5,6),(5,7)]
[ (x,y) | x <- [1..3], let k = x^2, y <- [1..k] ] = [(1,1),(2,1),(2,2),(2,3),(2,4),(3,1),(3,2),(3,3),(3,4),(3,5),(3,6),(3,7),(3,8),(3,9)]
[ x | x <- "Facultatea de Matematica si Informatica", elem x ['A'..'Z'] ] = FMI
[ [x..y] | x <- [1..5], y <- [1..5], x < y ] = [[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5],[2,3],[2,3,4],[2,3,4,5],[3,4],[3,4,5],[4,5]]
-}

-- EX2

factori :: Int -> [Int]
factori n = [x | x <- [1..n], n `mod` x == 0]

-- EX3

prim :: Int -> Bool
prim n = factori n == [1,n]

-- EX4

numerePrime :: Int -> [Int]
numerePrime n = [x | x <- [2..n], prim x]

-- EX5

myzip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
myzip3 [] _ _ = []
myzip3 _ [] _ = []
myzip3 _ _ [] = []
myzip3 (h1:t1) (h2:t2) (h3:t3) = (h1,h2,h3) : myzip3 t1 t2 t3

{-
map (\x -> 2 * x) [1..10] = [2,4,6,8,10,12,14,16,18,20]
map (1 `elem`) [[2,3], [1,2]] = [False,True]
map (`elem` [2,3]) [1,3,4,5] = [False,True,False,False]
-}

-- EX6

firstEl :: [(a,b)] -> [a]
firstEl l = map (\(x,y) -> x) l

-- EX7

sumList :: [[Int]] -> [Int]
sumList l = map (\x -> sum x) l

-- EX8

prel2 :: [Int] -> [Int]
prel2 l = map (\x -> if mod x 2 == 0 then x `div` 2 else 2 * x) l

-- EX9

ex9 :: Char -> [[Char]] -> [[Char]]
ex9 c l = filter (\x -> elem c x) l

-- EX10

ex10 :: [Int] -> [Int]
ex10 l = filter (\x -> mod x 2 /= 0) (map (\x -> x^2) l)

-- EX11 

ex11 :: [Int] -> [Int]
ex11 l = map (\(i,x) -> x) (filter (\(i,x) -> mod i 2 /= 0) (map (\(i,x) -> (i,x^2)) (zip [1..length l] l)))

-- EX12

aux :: String -> String
aux s = [c | c <- s,elem c "aeiouAEIOU"]

ex12 :: [String] -> [String]
ex12 l = map aux l

-- EX13

mymap :: (a -> b) -> [a] -> [b]
mymap _ [] = []
mymap f (h:t) = f h : mymap f t

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter p (h:t)
    | p h       = h : myfilter p t
    | otherwise = myfilter p t

-- EXTRA

step :: Char -> String -> [String]
step p c = [take i c ++ [p] ++ drop (i+1) c | i <- [0..length c - 1], c !! i == ' ']

next :: Char -> [String] -> [String]
next p cs = [x | s <- cs, x <- step p s]

winaux :: String -> Char
winaux l
    | (l !! 0) == (l !! 1) && (l !! 1) == (l !! 2) && (l !! 0) /= ' ' = l !! 0
    | (l !! 0) == (l !! 3) && (l !! 3) == (l !! 6) && (l !! 0) /= ' ' = l !! 3
    | (l !! 1) == (l !! 4) && (l !! 4) == (l !! 7) && (l !! 1) /= ' ' = l !! 1
    | (l !! 2) == (l !! 5) && (l !! 5) == (l !! 8) && (l !! 2) /= ' ' = l !! 2
    | (l !! 6) == (l !! 7) && (l !! 7) == (l !! 8) && (l !! 6) /= ' ' = l !! 6
    | (l !! 3) == (l !! 4) && (l !! 4) == (l !! 5) && (l !! 3) /= ' ' = l !! 3
    | (l !! 0) == (l !! 4) && (l !! 4) == (l !! 8) && (l !! 0) /= ' ' = l !! 0
    | (l !! 2) == (l !! 4) && (l !! 4) == (l !! 6) && (l !! 2) /= ' ' = l !! 2
    | otherwise = ' '

win :: Char -> [String] -> [String]
win p cs = filter (\x -> winaux x == p) (next p cs)