-- CODEX Worksheet 4

-- 4.1

calcPi :: Int -> Double
calcPi n =  sum (take n (zipWith (\x y -> x/y ) (cycle [4,-4]) [1,3..]))

calcPi2 :: Int -> Double
calcPi2 0 = 0
calcPi2 n = 3 + sum (take (n-1) (zipWith (/) (cycle [4,-4]) [x*(x+1)*(x+2) | x<-[2,4..]]))

--4.2
primes :: [Integer]
primes = sieve [2..]

sieve :: [Integer] -> [Integer]
sieve (p:xs) = p : sieve [x | x <-xs, mod x p /=0]

countTwins :: Integer -> Integer -> Int
countTwins low high =  length $ filter (\(a,b) -> b-a == 2) $ zip (values) (tail values)
    where values = takeWhile (<high) $ dropWhile (<=low) primes