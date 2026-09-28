-- IN-3
{-
a) 3 - (-2)+ 1 => (3- (-2)) + 1
b) 4 / (-2)- 3 * 6 => (4 / (-2)) - (3*6)
c) (-) 2 3 * 6 => (2-3) * 6 
d) 100 ‘div‘ 4 ‘div‘ 3 => (100 div 4) div 3
e) 100 ‘div‘ div 4 3 => 100 div (4 div 3)
f) (+) 1 2 + 3 * 4 => (1 + 2) + (3 * 4)
g) (+)(5 ‘mod‘ 2 + 2)(mod 5 2) ((5 mod 2) +2) + (5 mod 2)
-}

--IN-6

half :: Fractional a => a -> a
half x = x/2

xor :: Bool -> Bool -> Bool
xor a b = (a && not b) || (not a && b)

cbrt :: Floating a => a -> a
cbrt a = a**(1/3)

heron :: Floating a => a -> a -> a -> a
heron a b c = sqrt (s*(s-a)*(s-b)*(s-c))
    where s = (a+b+c)/2

--IN-13

f :: (Ord a, Num a, Integral b) => a -> b
f x
    | x > 0 = 1
    | x < 0 = -1
    | x == 0 = 0

f1 :: (Ord a, Num a, Integral b) => a -> b
f1 0 = 0
f1 x = if x>0 then 1 else (-1)

--FT-3

mySwap :: (b, a) -> (a, b)
mySwap (a,b) = (b,a)

--FT-4

distance2 :: Floating a => (a, a) -> (a, a) -> a
distance2 (ax,ay) (bx,by) = sqrt((ax-bx)**2 + (ay-by)**2)

distanceInf :: (Num a, Ord a) => (a, a) -> (a, a) -> a
distanceInf (ax,ay) (bx,by) = max (abs (ax - bx)) (abs (ay - by)) 

--FT-10
g :: [a] -> (a,[a])
g (_:_:x:y) = (x,y)

g1 :: [a] -> (a,[a])
g1 l = (head (drop 2 l), tail (drop 2 l)) 

--FT-11

evaluateLength :: [a] -> String
evaluateLength l 
    | x>= 4 = "long"
    | x>= 2 = "medium"
    | x>= 0 = "short"
        where x = length l

evaluateLength2 :: [a] -> String
evaluateLength2 [] = "short"
evaluateLength2 [_] = "short"
evaluateLength2 [_,_] = "medium"
evaluateLength2 [_,_,_] = "medium"
evaluateLength2 _ = "long"