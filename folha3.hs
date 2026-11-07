myand :: [Bool] -> Bool
myand [] = True
myand [a] = a
myand (x:xs) = x && myand xs

myor :: [Bool] -> Bool
myor [] = False
myor [a] = a
myor (x:xs) = x || myor xs

myconcat :: [[a]] -> [a] 
myconcat [] = []
myconcat (x:xs) = x ++ myconcat xs

myreplicate :: Int -> a -> [a]
myreplicate 0 _ = []
myreplicate n a = a:myreplicate (n-1) a

myindex :: [a] -> Int -> a
myindex [] _ = error ("empty list")
myindex (x:_) 0 = x
myindex (_:xs) n = myindex xs (n-1)

myelem :: Eq a => a -> [a] -> Bool
myelem _ [] = False
myelem v (x:xs) = v==x || myelem v xs

leastDiv :: Integer -> Integer
leastDiv n = leastDivAux 2 n

leastDivAux :: Integer -> Integer -> Integer
leastDivAux k n
    |k>n = n
    |mod n k == 0 = k
    |otherwise = leastDivAux (k+1) n

isPrimeFast :: Integer -> Bool
isPrimeFast n = leastDiv n == n


intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [a] = [a]
intersperse x (a:xs) = (a:x:intersperse x xs)

nub :: Eq a => [a] -> [a]
nub [] = []
nub (a:xs) = a:nub [x | x<-xs, x/=a]

insert :: Ord a => a ->[a] -> [a]
insert a [] = [a]
insert a (x:xs)
    | a<x = (a:x:xs)
    | otherwise = x:insert a xs

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x $  isort xs

merge :: Ord a => [a] -> [a] -> [a] 
merge [] l2 = l2
merge l1 [] = l1
merge (x:xs) (y:ys)
    | x<y = x:merge xs (y:ys)
    | otherwise = y:merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort [] = []
msort [a] = [a]
msort l = merge (msort $ take h l) (msort $ drop h l)
    where h = div (length l) 2

toBits :: Int -> [Int]
toBits n = reverse $ toBitsAux n

toBitsAux :: Int -> [Int]
toBitsAux 0 = [0]
toBitsAux 1 = [1]
toBitsAux n 
    | mod n 2 == 0 = 0:(toBitsAux $ div n 2)
    | otherwise = 1:(toBitsAux $ div n 2)

fromBits :: [Int] -> Int
fromBits l = fromBitsAux l 0

fromBitsAux :: [Int] -> Int -> Int
fromBitsAux []  acc = acc
fromBitsAux (x:xs) acc 
    | x==1 = fromBitsAux xs (2*acc+1)
    | otherwise = fromBitsAux xs (2*acc)

divisors :: Integer -> [Integer]
divisors n = filter (\k -> mod n k ==0) [1..n-1]

isPrimeFast' :: Integer -> Bool
isPrimeFast' 0 = False
isPrimeFast' 1 = False
isPrimeFast' n = all (\k -> mod n k /= 0) [2..s]
    where s = floor $ sqrt $ fromIntegral n


myappend :: [a] -> [a] -> [a]
myappend [] l = l
myappend l1 l2 = foldr ( (:) ) l2 l1
--myappend l1 l2 = foldr(\c acc -> c:acc) l2 l1

myconcat' :: [[a]] -> [a]
myconcat' [] = []
myconcat' l1 = foldr(\xs acc -> xs ++ acc) [] l1

myreverse :: [a] -> [a]
myreverse l = foldr(\c acc -> acc ++ [c]) [] l

myreverse' :: [a] -> [a]
myreverse' l = foldl(\acc c -> c:acc) [] l

myelem' :: Eq a => a -> [a] -> Bool
myelem' a l = foldr(\c acc -> c==a || acc  ) False l

fromBits' :: [Int] -> Int
fromBits' l = foldl(\acc c -> acc*2 + c) 0 l

group :: Eq a => [a] -> [[a]]
group [] = []
group (x:xs) = (takeWhile (==x) (x:xs)):(group $ (dropWhile (==x) xs))

intercalate :: a -> [a] -> [[a]]
intercalate x l = map(\i -> (take i l) ++ [x] ++ (drop i l)) [0..(length l)]

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = foldr (\c acc -> intercalate x c ++ acc) [] (permutations xs)
