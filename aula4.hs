--Lazy evaluation / Infinite lists

--repeat (repeat the same value infinite times)
--cycle (repeat the same list of values infinite times)
--iterate (apply a function to a value infinite times)

type Person = (String,Int)
--type String = [Char]
type Pair a = (a,a)

-- data Maybe a = Nothing | Just a

data BST a = Empty | Node a (BST a) (BST a) deriving (Eq,Show)

sumTree :: Num a => BST a -> a
sumTree Empty  = 0
sumTree (Node v l r) = v + sumTree l + sumTree r

insert :: Ord a => a -> BST a -> BST a
insert x Empty = Node x Empty Empty
insert x t@(Node y l r) --t@ faz com que (Node y l r) seja substituido por t
    | x==y = t
    | x<y = Node y (insert x l) r
    | x>y = Node y l (insert x r)

build::Ord a => [a] -> BST a
build [] = Empty
build (x:xs) = insert x (build xs)

-- data Lista a = Vazia | Cons a (Lista a)

--procura:: (Eq k) => k -> Lista (k,v) -> Maybe v
--procura _ Vazia = Nothing
--procura x  (Lista (y,v) t)
--    | x==y = Just v
--    | otherwise = procura x t

--estaNaLista :: (Eq k) => k -> Lista(k,v) -> Bool
--estaNaLista x xs = case procura x xs of Just _ -> True
--                                        Nothing -> False


-- 4.5, 4.2, 4.3
data Arv a = Vazia | No a (Arv a) (Arv a) deriving Show

myTree = No 4 (No 2 Vazia Vazia) (No 8 Vazia Vazia)

myTree2 = No 4 (No 2 (No 3 Vazia Vazia) (No 5 Vazia Vazia)) (No 8 (No 1 Vazia Vazia) Vazia)

mapArv:: (a->b) -> Arv a -> Arv b
mapArv f Vazia = Vazia
mapArv f (No v l r) = No (f v) (mapArv f l) (mapArv f r)

listar:: Arv a -> [a]
listar Vazia = []
listar (No v l r ) = listar (l) ++ [v] ++ listar (r)

nivel:: Int -> Arv a -> [a]
nivel _ Vazia = []
nivel n t@(No v l r) = nivel2 n t 0

nivel2:: Int -> Arv a -> Int -> [a]
nivel2 _ Vazia _ = []
nivel2 n (No v l r) k
    | n==k = [v]
    | otherwise = (nivel2 n l (k+1)) ++ (nivel2 n r (k+1))


-- Infinite list worksheet 4.1, 4.2, 4.3, 4.4

calcPi :: Int -> Double
calcPi n =  sum (take n (zipWith (\x y -> x/y ) (cycle [4,-4]) [1,3..]))

calcPi2 :: Int -> Double
calcPi2 n = 3 + sum (take (n-1) (zipWith (/) (cycle [4,-4]) [x*(x+1)*(x+2) | x<-[2,4..]]))

