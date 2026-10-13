myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f acc [] = acc
myFoldr f acc (x:xs) = f x (myFoldr f acc xs)

mapFilter :: (a->b) -> (a-> Bool) -> [a] -> [b]
mapFilter f p xs = map f (filter p xs)

mapFilter2 :: (a->b) -> (a-> Bool) -> [a] -> [b]
mapFilter2 f p xs = (map f .filter p) xs

dec2Int :: Integral a => [a] -> a
dec2Int xs = myFoldl (\acc x -> 10*acc + x) 0 xs


dec2Int2 :: Integral a => [a] -> a
dec2Int2 = myFoldl (\acc x -> 10*acc + x) 0

--dec2Int3 :: Integral a => [a] -> a
--dec2Int3 = myFoldl (\acc -> (+) 10*acc ) 0

dec2Int4 :: Integral a => [a] -> a
dec2Int4 = myFoldl (\acc -> (+) ((10*)acc) ) 0

dec2Int5 :: Integral a => [a] -> a
dec2Int5 = myFoldl (\acc -> ((+) . (10*))acc) 0

dec2Int6 :: Integral a => [a] -> a
dec2Int6 = myFoldl ((+) . (10*)) 0

myRev :: [a] -> [a]
myRev xs = myFoldr (\x acc -> acc ++ [x]) [] xs

myRev' :: [a] -> [a]
myRev' xs = myFoldl (\acc x -> x:acc ) [] xs

myRev'2 :: [a] -> [a]
myRev'2 = myFoldl (\acc x -> (:) x acc) []

myRev'3 :: [a] -> [a]
myRev'3 = myFoldl (\acc x -> flip (:) acc x) []

myRev'4 :: [a] -> [a]
myRev'4 = myFoldl (\acc -> flip (:) acc) []

myRev'5 :: [a] -> [a]
myRev'5 = myFoldl (flip (:)) []

--3.9
divisors :: Integer -> [Integer]
divisors n = filter (\x -> mod n x == 0) [1..n]

--3.10
isPrimeFast :: Integer -> Bool 
isPrimeFast 1 = False
isPrimeFast n = all (\x -> mod n x /= 0) [2..floor(sqrt (fromIntegral n))]