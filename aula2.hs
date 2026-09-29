myConcat :: [[a]] -> [a]
myConcat l = [ x | xs <- l, x <- xs]


myReplicate :: Integral b => b -> a -> [a]
myReplicate n x = [ x | _ <- [1..n]]


(@@) ::  Integral b => [a] -> b -> a
l @@ n = head [ x | (x,i) <- zip l [0..], i==n ]


myConcat2 :: [[a]] -> [a]
myConcat2 [] = []
myConcat2 (h:t) = h ++ myConcat2 t


myReplicate2 :: Integral b => b -> a -> [a]
myReplicate2 0 _ = [] 
myReplicate2 n x 
    | n<0 = error "argumento negativo"
    | otherwise = x:myReplicate2 (n-1) x


(@@@) ::  Integral b => [a] -> b -> a
(h:_) @@@ 0 = h
[] @@@ _ = error "index too large"
(h:t) @@@ n 
    | n<0 = error "indice negativo"
    |otherwise = t @@@ (n-1)


rev :: [a] -> [a]
rev [] = []
rev (h:t) = rev t ++ [h]

rev2 :: [a] -> [a]
rev2 l = rev2Aux l []

rev2Aux :: [a] -> [a] -> [a]
rev2Aux [] acc = acc
rev2Aux (h:t) acc = rev2Aux t (h:acc)


--Codex 3.3
intersperse :: a -> [a] -> [a]  -- introduce a value between elements of a list
intersperse _ [] = []
intersperse _ [a] = [a]
intersperse a (h:t) = (h:a:(intersperse a t) )

--Codex 3.4
nub :: Eq a => [a] -> [a]       -- remove repeated elements 
nub [] = []                             
nub (x:xs) = x:nub([ n | n <-xs, x/=n])      

--Codex 3.5
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
    | x<y = (x:y:ys)
    | otherwise = y:(insert x ys)

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)


--Codex 2.8
propDivs :: Integer -> [Integer]
propDivs n = [x | x <- [1..(n-1)], mod n x == 0]


--Codex 2.9
perfects :: Integer -> [Integer]
perfects n = [x | x <- [1..n], sum (propDivs x) == x ]


--Codex 2.11
isPrime :: Integer -> Bool
isPrime n = length [ x | x<- [1..n], mod n x == 0] == 2