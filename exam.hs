-- MT1 2022/2023

-- Grupo 1
-- The Protecting Friends for Life (PFL) Zoo houses many species of animals,
-- some of which were actually considered extinct in the past!
-- Consider the Species type synonym, which represents an animal species as a
-- tuple composed of the species’ name followed by its population in the zoo.

type Species = (String, Int)

-- The Zoo type synonym contains a set of species:

type Zoo = [Species]

-- 1.1
-- Implement isEndangered :: Species -> Bool, which receives a species and determines if it is endangered.
-- A species is considered endangered if there are less or equal to 100 individuals in the zoo.

isEndangered :: Species -> Bool
isEndangered (_,count) = count<= 100

-- 1.2
-- Implement updateSpecies :: Species -> Int -> Species, which,
-- given a species and an amount of newborn babies, returns a new instance of Species with the updated population.
updateSpecies :: Species -> Int -> Species
updateSpecies (name,c) born = (name,c+born)

-- 1.3
-- Must use recursion

-- Implement filterSpecies :: Zoo -> (Species -> Bool) -> Zoo, which,
-- given the list species of a zoo and a predicate (i.e. a function that performs a test on each species),
-- return the sublist of species that satisfy the predicate. The order of the species in the result must be the same as the input.
filterSpecies :: Zoo -> (Species -> Bool) -> Zoo
filterSpecies [] _ = []
filterSpecies (s:xs) f 
    | f s = (s:(filterSpecies xs f))
    | otherwise = filterSpecies xs f

-- 1.4
-- Must use a higher-order function, no recursion or list comprehension

-- Implement countAnimals :: Zoo -> Int, which, given the list species of a zoo,
-- counts the total population of the zoo.
countAnimals :: Zoo -> Int
countAnimals ss = sum $ map (\(n,c) -> c) ss

-- 1.5
-- Must use list comprehension

-- Implement substring :: (Integral a) => String -> a -> a -> String,
-- which returns the substring of a given string between an initial and final index
-- (the character on the final index should also be included in the result and both indices are within bounds).
-- Consider that the indices start at 0.
substring :: (Integral a) => String -> a -> a -> String
substring s i1 i2 = [c | (c,i)<- zip s [0..], i>=i1, i<=i2]

-- 1.6
-- Implement hasSubstr :: String -> String -> Bool,
-- which determines if the first string argument contains the second string argument
--     (i.e. the second argument is a substring of the first one).
hasSubstr :: String -> String -> Bool
hasSubstr hay need
    | length hay < length need = False
    | otherwise = any (==need) [substring  hay a b | a<-[0..length hay - 1], b<-[0..length hay - 1] ]

-- 1.7
-- Implement sortSpeciesWithSubstr :: Zoo -> String -> (Zoo,Zoo),
-- which divides the species of the zoo into a pair of sublists.
-- The first sublist stores all the species whose name contains the string argument,
-- while the second list has the remaining species.
-- The order of the species in each list of the resulting pair must match the input list.

sortSpeciesWithSubstr :: Zoo -> String -> (Zoo,Zoo)
sortSpeciesWithSubstr ss str = ([s | s@(name1,_)<-ss , hasSubstr name1 str ],[s | s@(name2,_)<-ss , not $ hasSubstr name2 str ])

-- Grupo 2
-- Consider a dendogram as a binary tree where each path leads to a string.
-- Each non-leaf node of the dendrogram specifies the horizontal distance from the father node
-- to each of the two child nodes. A father node is always at an equal horizontal distance
-- from both its children.
data Dendogram = Leaf String | Node Dendogram Int Dendogram

myDendo :: Dendogram
myDendo = Node (Node (Leaf "dog") 3 (Leaf "cat")) 5 (Leaf "octopus")

-- 2.1
-- Implement dendoWidth :: Dendogram -> Int, which returns the width of a dendogram (i.e. the horizontal distance between the leftmost and rightmost leaf nodes).
dendoWidth :: Dendogram -> Int
dendoWidth t = (dendoWidthLeft t) + (dendoWidthRight t)

dendoWidthLeft :: Dendogram -> Int
dendoWidthLeft (Leaf _) = 0
dendoWidthLeft (Node left d _) = d + (dendoWidthLeft left)

dendoWidthRight :: Dendogram -> Int
dendoWidthRight (Leaf _) = 0
dendoWidthRight (Node _ d right) = d + (dendoWidthRight right)


-- 2.2
-- Implement dendoInBounds :: Dendogram -> Int -> [String],
-- which returns the list of strings whose leaf nodes are up to a certain horizontal distance
-- from the root of the dendogram. Any order of the output list will be accepted.
dendoInBounds :: Dendogram -> Int -> [String]
dendoInBounds t d = dendoInBoundsAux t 0 (-d) d

dendoInBoundsAux :: Dendogram -> Int -> Int -> Int -> [String]
dendoInBoundsAux (Leaf s) d l r
    | d>=l && d<=r = [s]
    | otherwise = []
dendoInBoundsAux (Node left x right) d l r = (dendoInBoundsAux left (d-x) l r) ++ (dendoInBoundsAux right (d+x) l r) 

-- Grupo 3
-- The rabbit population of the zoo increases every year! In year 0, there were 2 rabbits,
-- while in year 1, there were 3 rabbits. In the following years, the number of rabbits corresponds
-- to the sum of the rabbit population of the two previous years.

-- 3.1
-- Implement rabbits :: (Integral a) => [a],
-- which returns an infinite list with the rabbit population of each year (starting at year 0).

rabbits :: (Integral a) => [a]
rabbits = 2:3:(zipWith (+) rabbits (tail rabbits))

-- 3.2
-- Implement rabbitYears :: (Integral a) => a -> Int, which returns
-- the number of years needed for the rabbit population to be greater or equal to the input integral value.
-- Note: The output type of rabbitNumYears is Int just to make life easier for the students
-- if they use length, which returns an Int (and not any instance of Integral).

rabbitYears :: (Integral a) => a -> Int
rabbitYears a = length $ takeWhile (<a) rabbits

-- MT1 recurso 2024/25

-- º 1.1
{-
You are not allowed to import any libraries unless stated otherwise. As such, do not call the "import" function.

Consider the following types to represent a fado (traditional Portuguese music)
concert from the Porto Folclórico de Fado (PFL), known for its quite modern concerts.

A concert is made up of a sequence of performances.
A performance is a structure with the name of the singers and a list of pairs of instruments and their respective player.
The same instrument may appear more than once in the same performance.
The lists of singers and instrument-players can be ordered in any way possible.
Singers and instrument players may be the same between different performances.
Assume that none of the singers shares a name with an instrument player.

To help you test your functions, consider the concert presented below.
-}
type Performance = ([String], [(String, String)])
type Concert = [Performance]

myConcert :: Concert
myConcert = [
  (["Mariza"], [("guitar", "Mario")]),
  (["Carminho"], [("piano", "Paulo"), ("drums", "Rui"), ("saxophone", "Henrique")]),
  (["Carminho"], []),
  (["Carminho", "Amalia"], []),
  (["Amalia", "Mariza", "Camane"], [("guitar", "Filipe"), ("guitar", "Mario")]),
  (["Camane", "Amalia", "Mariza"], [("saxophone", "Luis"), ("guitar", "Mario")]),
  (["Amalia"], [])
  ]

-- ª
{-
Implement isSoloACappella :: Performance -> Bool, which, given a performance, determines if only one singer participated AND no musical instruments were played.

Examples:
ghci> isSoloACappella (["Carminho"], [])
True
ghci> isSoloACappella (["Carminho", "Amalia"], [])
False
-}
isSoloACappella :: Performance -> Bool
isSoloACappella (singer, instruments) = (length singer == 1) && (length instruments == 0)

-- º 1.2
-- ª
{-
Implement containsInstrument :: Performance -> String -> Bool, which,
given a performance and an instrument name,
determines if at least one person played that instrument in the performance.

Examples:
ghci> containsInstrument (["Amalia"], [("piano", "Paulo"), ("drums", "Rui"), ("saxophone", "Henrique")]) "drums"
True
ghci> containsInstrument (["Amalia"], [("piano", "Paulo"), ("drums", "Rui"), ("saxophone", "Henrique")]) "guitar"
False
-}
containsInstrument :: Performance -> String -> Bool
containsInstrument (_,ins) str = containsInstrumentAux ins str

containsInstrumentAux :: [(String,String)] -> String -> Bool
containsInstrumentAux [] _ = False
containsInstrumentAux ((ins,_):xs) val = (ins == val) ||  (containsInstrumentAux xs val)