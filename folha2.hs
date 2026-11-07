propDivs :: Integer -> [Integer] 
propDivs n = [i | i<-[1..n-1], mod n i == 0]


perfects :: Integer -> [Integer] 
perfects n = [ i | i<- [1..n] , sum (propDivs i) == i]

pyths :: Integer -> [(Integer,Integer,Integer)]
pyths n = [(a,b,c) | a<- [1..n], b<- [1..n], c<- [1..n], a*a+b*b==c*c]

isPrime :: Integer -> Bool 
isPrime n = length [ i | i<- [1..n], mod n i == 0 ] == 2

myconcat :: [[a]] -> [a]
myconcat xss = [x | xs <-xss, x <-xs]

myreplicate :: Int -> a -> [a]
myreplicate n x = [x | _<-[1..n]]

binom :: Integer -> Integer -> Integer
binom n k = div (product [1..n])   $ (product [1..k]) * (product [1..n-k])

pascal :: Integer -> [[Integer]]
pascal n = [[binom i k | k <-[0..i]] |  i <-[0..n]]