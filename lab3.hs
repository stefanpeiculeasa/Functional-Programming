import Data.Char

verifL :: [Int] -> Bool
verifL a = if mod (length a) 2 == 0
              then True
           else False

takefinal :: [b] -> Int -> [b]
takefinal a n = drop (length a - n) a

remove :: [a] -> Int -> [a]
remove a n = take n a ++ drop (n+1) a

myreplicate :: a -> Int -> [a]
myreplicate x n
    | n <= 0    = []
    | otherwise = x : myreplicate x (n-1)

sumImp :: [Int] -> Int
sumImp [] = 0
sumImp (h:t)
    | mod h 2 /= 0 = h + sumImp t
    | otherwise    = sumImp t

totalLen :: [String] -> Int
totalLen [] = 0
totalLen (h:t)
    | h !! 0 == 'A' = length h + totalLen t
    | otherwise     = totalLen t

nrVocale :: [String] -> Int
nrVocale [] = 0
nrVocale (h:t)
    | h == reverse h = length (filter (`elem` "aeiou") h) + nrVocale t
    | otherwise      = nrVocale t

insertDupaPar :: [Int] -> Int -> [Int]
insertDupaPar [] _ = []
insertDupaPar (h:t) x
    | mod h 2 == 0 = h : x : insertDupaPar t x
    | otherwise    = h : insertDupaPar t x

divizori :: Int -> [Int]
divizori n = [x | x <- [1..n], mod n x == 0]

listadiv :: [Int] -> [[Int]]
listadiv [] = []
listadiv (h:t) = divizori h : listadiv t

inIntervalComp :: [Int] -> Int -> Int -> [Int]
inIntervalComp l a b = [x | x <- l, x >= a, x <= b]

inIntervalRec :: [Int] -> Int -> Int -> [Int]
inIntervalRec [] _ _ = []
inIntervalRec (h:t) a b
    | h >= a && h <= b = h : inIntervalRec t a b
    | otherwise        = inIntervalRec t a b

pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (h:t)
    | h > 0     = 1 + pozitiveRec t
    | otherwise = pozitiveRec t

pozitiveComp :: [Int] -> Int
pozitiveComp l = length [x | x <- l, x > 0]

pozitiiImpareAux :: [Int] -> Int -> [Int]
pozitiiImpareAux [] _ = []
pozitiiImpareAux (h:t) index
    | mod h 2 /= 0 = index : pozitiiImpareAux t (index + 1)
    | otherwise        = pozitiiImpareAux t (index + 1)

pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec l = pozitiiImpareAux l 0

pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp l = [index | (index, value) <- zip [0..] l, mod value 2 /= 0]

multDigitsRec :: String -> Int
multDigitsRec [] = 1
multDigitsRec (h:t)
    | isDigit h = digitToInt h * multDigitsRec t
    | otherwise = multDigitsRec t

multDigitsCompAux :: String -> Int
multDigitsCompAux s = product [digitToInt c | c <- s, isDigit c]