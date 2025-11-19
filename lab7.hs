data Expr = Const Int -- integer constant
          | Expr :+: Expr -- addition
          | Expr :*: Expr -- multiplication
           deriving Eq

data Operation = Add | Mult deriving (Eq, Show)

data Tree = Lf Int -- leaf
          | Node Operation Tree Tree -- branch
           deriving (Eq, Show)

instance Show Expr where
  show (Const x) = show x
  show (e1 :+: e2) = "(" ++ show e1 ++ " + "++ show e2 ++ ")"
  show (e1 :*: e2) = "(" ++ show e1 ++ " * "++ show e2 ++ ")"

-- 1
evalExp :: Expr -> Int
evalExp x = case x of
            Const n -> n
            e1 :+: e2 -> evalExp e1 + evalExp e2
            e1 :*: e2 -> evalExp e1 * evalExp e2

-- 2
evalArb :: Tree -> Int
evalArb x = case x of
            Lf n -> n
            Node Add t1 t2 -> evalArb t1 + evalArb t2
            Node Mult t1 t2 -> evalArb t1 * evalArb t2

-- 3
expToArb :: Expr -> Tree
expToArb x = case x of
                Const n -> Lf n
                e1 :+: e2 -> Node Add (expToArb e1) (expToArb e2)
                e1 :*: e2 -> Node Mult (expToArb e1) (expToArb e2)

data IntSearchTree value
  = Empty
  | BNode
      (IntSearchTree value)     -- elemente cu cheia mai mica
      Int                       -- cheia elementului
      (Maybe value)             -- valoarea elementului
      (IntSearchTree value)     -- elemente cu cheia mai mare
    deriving (Eq, Show)

-- 4

exampleTree :: IntSearchTree Int
exampleTree =
  BNode
    (BNode
      (BNode Empty 2 (Just 2) Empty)
      5 (Just 5)                    
      (BNode Empty 7 (Just 7) Empty)
    )
    10 (Just 10)                    
    (BNode
      Empty
      15 (Just 15)
      (BNode Empty 20 (Just 20) Empty)
    )

lookup' :: Int -> IntSearchTree value -> Maybe value
lookup' = \key tree -> case tree of
                Empty -> Nothing
                BNode left k v right ->
                  if key == k then v
                  else if key < k then lookup' key left
                  else lookup' key right

-- 5

keys ::  IntSearchTree value -> [Int]
keys = \tree -> case tree of
            Empty -> []
            BNode left k _ right -> keys left ++ [k] ++ keys right

-- 6

values :: IntSearchTree value -> [value]
values = \tree -> case tree of
            Empty -> []
            BNode left _ v right ->
              case v of
                Nothing -> values left ++ values right
                Just value -> values left ++ [value] ++ values right

-- 7

insert :: Int -> value -> IntSearchTree value -> IntSearchTree value
insert = \key value tree -> case tree of
                Empty -> BNode Empty key (Just value) Empty
                BNode left k v right ->
                  if key == k then BNode left k (Just value) right
                  else if key < k then BNode (insert key value left) k v right
                  else BNode left k v (insert key value right)

-- 8

delete :: Int -> IntSearchTree value -> IntSearchTree value
delete = \key tree -> case tree of
                Empty -> Empty
                BNode left k v right ->
                  if key == k then BNode left k Nothing right
                  else if key < k then BNode (delete key left) k v right
                  else BNode left k v (delete key right)

-- 9

toList :: IntSearchTree value -> [(Int, value)]
toList = \tree -> case tree of
            Empty -> []
            BNode left k v right ->
              case v of
                Nothing -> toList left ++ toList right
                Just value -> toList left ++ [(k, value)] ++ toList right

-- 10

fromList :: [(Int, value)] -> IntSearchTree value 
fromList = \lst -> foldr (\(k, v) acc -> insert k v acc) Empty lst

-- 11

printTree :: IntSearchTree value -> String
printTree = \tree -> case tree of
            Empty -> ""
            BNode left k _ right ->
              let leftStr = printTree left
                  rightStr = printTree right
                  in "(" ++ leftStr ++ ") " ++ show k ++ " (" ++ rightStr ++ ")"
