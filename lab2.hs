import Data.List

myInt = 31415926535897932384626433832795028841971693993751058209749445923

double :: Integer -> Integer
double x = x+x

maxim :: Integer -> Integer -> Integer
maxim x y = if (x > y)
               then x
          else y

-- max3 x y z = let
--              u = maxim x y
--              in (maxim  u z)

max3 :: Integer -> Integer -> Integer -> Integer
max3 x y z = if (x >= y && x >= z)
                then x
             else if (y >= x && y >= z)
                then y
             else z

maxim4 :: Integer -> Integer -> Integer -> Integer -> Integer
maxim4 a b c d = let
                    u = maxim a b
                    v = maxim c d
                 in maxim u v

verificare_maxim4 :: (Integer -> Integer -> Integer -> Integer -> Integer) -> Integer -> Integer -> Integer -> Integer -> Bool
verificare_maxim4 f a b c d
    | f a b c d >= a = True
    | f a b c d >= b = True
    | f a b c d >= c = True
    | f a b c d >= d = True
    | otherwise      = False

-- EX 6

patrate :: Integer -> Integer -> Integer
patrate x y = x * x + y * y

par :: Integer -> String
par x = if (mod x 2 == 0)
           then "par"
         else "impar"

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

ex_d :: Integer -> Integer -> Bool
ex_d a b = if ( a > 2*b )
              then True
           else False
    
maxim_lista :: [Integer] -> Integer
maxim_lista [x] = x
maxim_lista (x:t) = maxim x (maxim_lista t)

-- EX 7

poly :: Double -> Double -> Double -> Double -> Double
poly a b c x = a*x^2 + b*x + c

-- EX 8

eeny :: Integer -> String
eeny x = if (mod x 2 == 0)
              then "eeny"
            else "meeny"

-- EX 9

fizzbuzz :: Integer -> String
-- fizzbuzz x = if (mod x 3 == 0 && mod x 5 == 0)
--                  then "FizzBuzz"
--               else if (mod x 3 == 0)
--                  then "Fizz"
--               else if (mod x 5 == 0)
--                  then "Buzz"
--               else ""

fizzbuzz x
    | mod x 3 == 0 && mod x 5 == 0 = "FizzBuzz"
    | mod x 3 == 0                 = "Fizz"
    | mod x 5 == 0                 = "Buzz"
    | otherwise                    = ""

-- Recursivitate

fibonacciCazuri :: Integer -> Integer
fibonacciCazuri n
    | n < 2     = n
    | otherwise = fibonacciCazuri (n - 1) + fibonacciCazuri (n - 2)
    
fibonacciEcuational :: Integer -> Integer
fibonacciEcuational 0 = 0
fibonacciEcuational 1 = 1
fibonacciEcuational n =
    fibonacciEcuational (n - 1) + fibonacciEcuational (n - 2)

-- EX 10
    
tribonacci :: Integer -> Integer
tribonacci 1 = 1
tribonacci 2 = 1
tribonacci 3 = 2
tribonacci n = tribonacci (n - 1) + tribonacci (n - 2) + tribonacci (n - 3)

-- EX 11

binomial :: Integer -> Integer -> Integer
binomial n 0 = 1
binomial 0 k = 0
binomial n k = binomial (n - 1) (k - 1) + binomial (n - 1) k
