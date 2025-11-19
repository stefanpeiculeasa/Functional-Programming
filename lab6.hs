data Fruct
  = Mar String Bool
  | Portocala String Int

-- EX 1
-- a)

ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Portocala "Tarocco" _) = True
ePortocalaDeSicilia (Portocala "Moro" _) = True
ePortocalaDeSicilia (Portocala "Sanguinello" _) = True
ePortocalaDeSicilia _ = False

--b)
nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia = foldr (\x acc -> case x of
                                Portocala "Tarocco" n -> n + acc
                                Portocala "Moro" n -> n + acc
                                Portocala "Sanguinello" n -> n + acc
                                _ -> acc) 0

--c)

nrMereViermi :: [Fruct] -> Int
nrMereViermi = foldr (\x acc -> case x of
                                Mar _ True -> 1 + acc
                                _ -> acc) 0

-- EX 2

type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show

--a)

vorbeste :: Animal -> String
vorbeste x = case x of
             Pisica _ -> "Meow!"
             Caine _ _ -> "Woof!"

--b)

rasa :: Animal -> Maybe String
rasa x = case x of
         Pisica _ -> Nothing
         Caine _ r -> Just r

-- EX 3

data Linie = L [Int]
   deriving Show
data Matrice = M [Linie]
   deriving Show

-- a)

verifica :: Matrice -> Int -> Bool
verifica (M l ) n = foldr (\(L el) acc -> sum el == n && acc) True l

-- b)

doarPozN :: Matrice -> Int -> Bool
doarPozN (M l) n = foldr (\(L el) acc -> (length el /= n || all (> 0) el) && acc) True l

--c)

corect :: Matrice -> Bool
corect (M l) = foldr (\(L el) acc -> length el == (case l of
                                              []      -> 0
                                              (L x:_) -> length x) && acc) True l