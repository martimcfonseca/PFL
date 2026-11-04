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
sieve [] = []
sieve (p:xs) = p : sieve [x | x <-xs, mod x p /=0]

countTwins :: Integer -> Integer -> Int
countTwins low high =  length $ filter (\(a,b) -> b-a == 2) $ zip (values) (tail values)
    where values = takeWhile (<=high) $ dropWhile (<low) primes


--4.3
hamAux :: Integer -> [Integer]
hamAux n = [2^i*3^j*5^k | i <- [0..n], j <- [0..n], k <- [0..n], i + j + k == n]

hamming :: [Integer]
hamming = concat [hamAux n  | n <- [0..]]


--4.4

merge :: [Integer] -> [Integer] -> [Integer]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x < y     = x : merge xs (y:ys)
  | x > y     = y : merge (x:xs) ys
  | otherwise = x : merge xs ys

hamming2 :: [Integer]
hamming2 = 1 : merge (map (2*) hamming) (merge (map(3*) hamming) (map (5*) hamming))


--4.5

user:: IO ()
user = do
  putStrLn "What is your name?"
  input <- getLine
  putStrLn $ "Hello, " ++ input ++ "! What is your age?"
  input <- getLine
  let num = (read input)::Int
  putStrLn $ "Next year you will be " ++ (show (num+1)) ++   " years old. Good-bye!"

--4.6

fizzBuzz :: Int -> String
fizzBuzz n
  | n `mod` 15==0 = "Fizz Buzz"
  | n `mod` 5==0 =  "Buzz"
  | n `mod` 3==0 =  "Fizz"
  | otherwise = show n

fizzBuzzList :: Int -> [String]
fizzBuzzList n = map fizzBuzz [1..n]

fizz :: IO ()
fizz = do
  putStrLn "Write a number:"
  input <- getLine
  let num = (read input) :: Int
  mapM_ putStrLn (fizzBuzzList num)