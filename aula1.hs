import Control.Monad.Cont (label)
areaTriangle :: Floating a => a -> a -> a -> a
areaTriangle a b c =  let s =(a+b+c)/2 in sqrt(s*(s-a)*(s-b)*(s-c))

areaTriangle2 :: Float -> Float -> Float -> Float
areaTriangle2 a b c = sqrt(s*(s-a)*(s-b)*(s-c))
    where s = (a+b+c)/2


halves :: [a] -> ([a], [a])
halves l = (take halfLen l, drop halfLen l)
    where halfLen = div (length l) 2

myLast :: [a] -> a
myLast l = head (reverse l)

myLast2 :: [a] -> a
myLast2 l = head (drop (length l -1) l)

myInit :: [a] -> [a]
myInit l = reverse (tail (reverse l))

myInit2 :: [a] -> [a]
myInit2 l = take ((length l) -1) l

five :: (Num a, Eq a) => a -> String
five n = if n==5 then "five" else "not five"

five2 :: (Eq a, Num a) => a -> String
five2 n
    |n == 5 = "five"
    |otherwise = "not five"

five3 :: (Eq a, Num a) => a -> String
five3 5 = "five"
five3 _ = "not five"

five4 :: (Eq a, Num a) => a -> String
five4 n = case n of 5 -> "five"
                    _ -> "not five"


short :: [a] -> Bool
short l = length l < 3


short2 :: [a] -> Bool
short2 [] = True
short2 [_] = True
short2 [_,_] = True
short2 _ = False

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

fib:: Int -> Int
fib 0 = 0
fib 1 = 1
fib n
    | n >= 0 = fib (n-1) + fib (n-2)
    | otherwise = error "numero negativo"


myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

ap :: [a] -> [a] -> [a]
ap [] l = l
ap (x:xs) l = (x):(ap xs l)

myrev :: [a] -> [a]
myrev [] = []
myrev (x:xs) = myrev xs ++ [x] 
