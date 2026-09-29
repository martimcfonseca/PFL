--Codex 1.2
leftHalf :: [a] -> [a]
leftHalf xs = take halfLen xs
    where halfLen = div (length xs) 2

rightHalf :: [a] -> [a]
rightHalf xs = drop  halfLen xs
    where halfLen = div (length xs) 2

--Codex 1.3(c)
myinit :: [a] -> [a]
myinit xs = take (length xs - 1) xs 

--Codex 1.3(d)
middle :: [a] -> a
middle xs = head (drop (div (length xs) 2) xs)

--Codex 1.4
checkTriangle :: Float -> Float -> Float -> Bool
checkTriangle a b c
    | a+b <= c = False
    | a+c <= b = False
    | b+c <= a = False
    | otherwise = True

--Codex 1.5
triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c = sqrt (s*(s-a)*(s-b)*(s-c))
    where s = (a+b+c)/2

--Codex 2.7
median :: Ord a => a -> a -> a -> a
median x y z
    |x >= y && x <= z = x 
    |x >= z && x <= y = x 
    |y >= x && y <= z = y
    |y >= z && y <= x = y
    | otherwise = z

median' :: (Ord a, Num a) => a -> a -> a -> a
median' x y z = x+y+z - (max (max x y) z) - (min (min x y) z)