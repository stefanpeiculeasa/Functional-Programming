data Tree = Empty  -- arbore vid
  | Node Int Tree Tree Tree -- arbore cu valoare de tip Int in radacina
                            -- si 3 fii
  
extree :: Tree
extree = Node 4 (Node 5 Empty Empty Empty) 
                (Node 3 Empty Empty (Node 1 Empty Empty Empty)) Empty

class ArbInfo t where
  level :: t -> Int -- intoarce inaltimea arborelui; 
                    -- consideram ca un arbore vid are inaltimea 0
  sumval :: t -> Int -- intoarce suma valorilor din arbore
  nrFrunze :: t -> Int -- intoarce nr de frunze al arborelui

-- level extree
-- 3
-- sumval extree
-- 13
-- nrFrunze extree
-- 2

-- EX 1

instance ArbInfo Tree where
  level :: Tree -> Int
  level Empty = 0
  level (Node _ left mid right) =
    1 + maximum [level left, level mid, level right]

  sumval :: Tree -> Int
  sumval Empty = 0
  sumval (Node v left mid right) =
    v + sumval left + sumval mid + sumval right

  nrFrunze :: Tree -> Int
  nrFrunze Empty = 0
  nrFrunze (Node _ Empty Empty Empty) = 1
  nrFrunze (Node _ left mid right) =
    nrFrunze left + nrFrunze mid + nrFrunze right

class Scalar a where
  zero :: a 
  one :: a 
  adds :: a -> a -> a
  mult :: a -> a -> a
  negates :: a -> a
  recips :: a -> a

-- EX 2

instance Scalar Int where
  zero = 0
  one = 1
  adds = (+)
  mult = (*)
  negates = negate
  recips x = div 1 x 

class (Scalar a) => Vector v a where
  zerov :: v a
  onev :: v a
  addv :: v a -> v a -> v a -- adunare vector
  smult :: a -> v a -> v a  -- inmultire cu scalare
  negatev :: v a -> v a -- negare vector

data Vector2 a = V2 a a

instance (Scalar a) => Vector Vector2 a where
  zerov = V2 zero zero
  onev = V2 one one
  addv (V2 x1 y1) (V2 x2 y2) = V2 (adds x1 x2) (adds y1 y2)
  smult s (V2 x y) = V2 (mult s x) (mult s y)
  negatev (V2 x y) = V2 (negates x) (negates y)

data Vector3 a = V3 a a a

instance (Scalar a) => Vector Vector3 a where
  zerov = V3 zero zero zero
  onev = V3 one one one
  addv (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (adds x1 x2) (adds y1 y2) (adds z1 z2)
  smult s (V3 x y z) = V3 (mult s x) (mult s y) (mult s z)
  negatev (V3 x y z) = V3 (negates x) (negates y) (negates z)