class Collection c where
empty :: c key value
singleton :: key -> value -> c key value
insert
    :: Ord key
    => key -> value -> c key value -> c key value
lookup :: Ord key => key -> c key value -> Maybe value
delete :: Ord key => key -> c key value -> c key value
keys :: c key value -> [key]
values :: c key value -> [value]
toList :: c key value -> [(key, value)]
fromList :: Ord key => [(key,value)] -> c key value

-- 1

keys :: c key value -> [key]
keys = map fst . toList

values :: c key value -> [value]
values = map snd . toList

toList :: c key value -> [(key, value)]

fromList :: Ord key => [(key, value)] -> c key value
fromList = foldr (\(k, v) coll -> insert k v coll) empty

-- 2
newtype PairList k v
  = PairList { getPairList :: [(k, v)] }

instance Collection PairList where
  empty :: PairList k v
  empty = PairList []

  singleton :: k -> v -> PairList k v
  singleton k v = PairList [(k, v)]

  insert :: Ord k => k -> v -> PairList k v -> PairList k v
  insert k v (PairList xs) =
    let xs' = (k,v) : filter (\(k',_) -> k' /= k) xs
    in PairList xs'

  lookup :: Ord k => k -> PairList k v -> Maybe v
  lookup k (PairList xs) = Prelude.lookup k xs

  delete :: Ord k => k -> PairList k v -> PairList k v
  delete k (PairList xs) = PairList (filter (\(k',_) -> k' /= k) xs)

  toList :: PairList k v -> [(k, v)]
  toList = getPairList

-- 3
data SearchTree key value
  = Empty
  | BNode
      (SearchTree key value) -- elemente cu cheia mai mica
      key                    -- cheia elementului
      (Maybe value)          -- valoarea elementului
      (SearchTree key value) -- elemente cu cheia mai mare

insertNode :: Ord key => key -> value -> SearchTree key value -> SearchTree key value
insertNode k v Empty = BNode Empty k (Just v) Empty
insertNode k v (BNode left key val right)
  | k < key   = BNode (insertNode k v left) key val right
  | k > key   = BNode left key val (insertNode k v right)
  | otherwise = BNode left key (Just v) right

lookupNode :: Ord key => key -> SearchTree key value -> Maybe value
lookupNode _ Empty = Nothing
lookupNode k (BNode left key val right)
  | k < key   = lookupNode k left
  | k > key   = lookupNode k right
  | otherwise = val 

deleteNode :: Ord key => key -> SearchTree key value -> SearchTree key value
deleteNode _ Empty = Empty
deleteNode k (BNode left key val right)
  | k < key   = BNode (deleteNode k left) key val right
  | k > key   = BNode left key val (deleteNode k right)
  | otherwise = BNode left key Nothing right

toList :: SearchTree key value -> [(key, value)]
toList tree = case tree of
    Empty -> []
    BNode left k v right ->
        case v of
            Nothing    -> toList left ++ toList right
            Just value -> toList left ++ [(k, value)] ++ toList right

instance Collection SearchTree where
  empty :: SearchTree key value
  empty = Empty

  singleton :: key -> value -> SearchTree key value
  singleton k v = BNode Empty k (Just v) Empty

  insert :: Ord key => key -> value -> SearchTree key value -> SearchTree key value
  insert = insertNode

  lookup :: Ord key => key -> SearchTree key value -> Maybe value
  lookup = lookupNode

  delete :: Ord key => key -> SearchTree key value -> SearchTree key value
  delete = deleteNode

  toList :: SearchTree key value -> [(key, value)]
  toList = toListNode

-- 4
data Punct = Pt [Int]

data Arb = Vid | F Int | N Arb Arb
          deriving Show

class ToFromArb a where
 	    toArb :: a -> Arb
	    fromArb :: Arb -> a

data Punct = Pt [Int]

instance Show Punct where
    show (Pt xs) = "(" ++ inner xs ++ ")"
      where 
        inner []     = ""
        inner [y]    = show y
        inner (y:ys) = show y ++ ", " ++ inner ys

instance ToFromArb Punct where
    toArb (Pt xs) = listToArb xs
      where
        listToArb []     = Vid
        listToArb [y]    = F y
        listToArb (y:ys) = N (F y) (listToArb ys)

    fromArb arb = Pt (frontiera arb)
      where
        frontiera Vid       = []
        frontiera (F x)     = [x]
        frontiera (N l r)   = frontiera l ++ frontiera r

-- 6
data Geo a = Square a | Rectangle a a | Circle a
    deriving Show

class GeoOps g where
  perimeter :: (Floating a) => g a -> a
  area :: (Floating a) =>  g a -> a

instance GeoOps Geo where
    perimeter (Square s)       = 4 * s
    perimeter (Rectangle w h)  = 2 * (w + h)
    perimeter (Circle r)       = 2 * pi * r

    area (Square s)            = s * s
    area (Rectangle w h)       = w * h
    area (Circle r)            = pi * r * r

-- 7
instance (Floating a, Eq a) => Eq (Geo a) where
    g1 == g2 = perimeter g1 == perimeter g2