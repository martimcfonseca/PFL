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

-- º 1.3
-- ª
{-
Implement countInstrument :: Concert -> String -> Int, which, given a concert and an instrument name, returns the number of performances where at least one person played that instrument.

Examples:
ghci> countInstrument myConcert "guitar"
3
ghci> countInstrument myConcert "drums"
1

Constraint: in this exercise, you must use higher-order functions. You are not allowed to define new recursive functions or use list comprehensions. Solutions that do not follow the constraints will receive at most 25% of the question score.
-}
countInstrument :: Concert -> String -> Int
countInstrument c name = length $ filter (\perf -> containsInstrument perf name) c

-- º 1.4
-- ª
{-
Implement concertSummary :: Concert -> [([String],[String])], which, given a concert, returns a list with a summary of each performance (in the same order as in the concert).

Each performance is summarized with a pair containing:
- the names of the first two singers (or only one singer, if only one exists).
- the names of the instrument players in odd positions in the list of instrument-player pairs (with position indices starting at 1).

Examples:
ghci> concertSummary $ take 2 myConcert 
[(["Mariza"],["Mario"]),(["Carminho"],["Paulo","Henrique"])]
ghci> concertSummary myConcert 
[(["Mariza"],["Mario"]),(["Carminho"],["Paulo","Henrique"]),(["Carminho"],[]),(["Carminho","Amalia"],[]),(["Amalia","Mariza"],["Filipe"]),(["Camane","Amalia"],["Luis"]),(["Amalia"],[])]

Constraint: in this exercise, you must use list comprehensions. You are not allowed to define new recursive functions or use higher-order functions (map, filter, foldl ...). Solutions that do not follow the constraints will receive at most 25% of the question score.
-}
concertSummary :: Concert -> [([String],[String])]
concertSummary c = [(take 2 sing,[name | (name,i) <- zip [p | (_,p) <-ins] [1..], (mod i 2 == 1) ]) | (sing,ins) <- c ]

-- º 1.5
-- ª
{-
Implement myIntersect :: (Eq a) => [a] -> [a] -> [a], which, given two lists l1 and l2, returns the elements of l1 that are also present in l2.
The resulting list must maintain the order of the elements of the first input list.
Note that the result might contain duplicates if there are duplicates in l1.

Examples:
ghci> myIntersect "abcd" "da"
"ad"
ghci> myIntersect "abba" "dab"
"abba"
-}
myIntersect :: (Eq a) => [a] -> [a] -> [a]
myIntersect l1 l2 = filter (\a -> elem a l2) l1

-- º 1.6
-- ª
{-
Implement sortUniques :: Ord a => (a -> a -> Bool) -> [a] -> [a], which given a list l and a comparison function f (which indicates if an element e1 is smaller than e2), returns a sorted list of l that does not contain duplicates using the comparison criterion of f.

Example:
ghci> sortUniques (<) [3,2,4,3,1,2]
[1,2,3,4]
-}
sortUniques :: Ord a => (a -> a -> Bool) -> [a] -> [a]
sortUniques _ [] = []
sortUniques _ [a] = [a]
sortUniques f (x:xs) = removeConsecutives $ (sortUniques  f left) ++ [x] ++ (sortUniques f right)
    where   left = filter (\y -> f y x) xs
            right = filter(\y -> not $ f y x) xs
    
removeConsecutives :: Ord a => [a] -> [a]
removeConsecutives [] = []
removeConsecutives [a] = [a]
removeConsecutives (a:b:xs) 
    | a == b = b:removeConsecutives (xs)
    | otherwise = a:removeConsecutives (b:xs)

-- º 1.7
-- ª
{-
Implement tiredSingers :: Concert -> [String], which, given a concert, returns a list of the tired singers' names, ordered alphabetically.
A singer is tired if they have at least three consecutive performances sometime during the concert.

Suggestion: use myIntersect and sortUniques to help you out.

In the example below, Mariza is NOT a tired singer since her three performances were not consecutive.

Example:
ghci> tiredSingers myConcert 
["Amalia","Carminho"]
-}

tiredSingers :: Concert -> [String]
tiredSingers c = sortUniques  (<) $ tiredSingersAux c

tiredSingersAux :: Concert -> [String]
tiredSingersAux [] = []
tiredSingersAux [_] = []
tiredSingersAux [_,_] = []
tiredSingersAux ((n1,_):b@(n2,_):c@(n3,_):xs)
    | myIntersect n1 (myIntersect n2 n3) == [] = tiredSingersAux (b:c:xs)
    | otherwise = myIntersect n1 (myIntersect n2 n3) ++ tiredSingersAux (b:c:xs) 

-- º 1.8
-- ª
{-
Implement printInstrumentPlayers :: Performance -> IO (), which, given a performance, prints in the console a sequence of lines about what instrument each person played, in the same order as in the instrument-player list of the performance.
Print exactly one newline per performance. The words of each line must be separated by exactly one space.

Example:
ghci> printInstrumentPlayers (myConcert !! 1)
Paulo played the piano
Rui played the drums
Henrique played the saxophone
-}
printInstrumentPlayers :: Performance -> IO ()
printInstrumentPlayers (_,ips) = printInstrumentPlayersAux ips

printInstrumentPlayersAux :: [(String,String)] -> IO ()
printInstrumentPlayersAux [] = return ()
printInstrumentPlayersAux ((i,p):ps) = do
    putStrLn $ p ++ " played the " ++ i
    printInstrumentPlayersAux ps


-- º 2.1
{-
Consider the following type to represent a map as a Binary Search Tree (BST), which stores a set of key-value pairs. Non-leaf nodes of this tree contain (in order):
- a key-value pair (k,v);
- a left subtree, where, for all the key-value pairs (k',v'), k' < k.
- a right subtree, where, for all the key-value pairs (k',v'), k' > k.

Consider that the tree does not contain duplicate pairs (i.e. two or more pairs with the same key k). The trees can, however, contain several pairs with the same value v.
-}

data TreeMap a b = Empty | Node (a,b) (TreeMap a b) (TreeMap a b) deriving (Eq, Ord, Read, Show)

{-
Example of a TreeMap:
-}
myTree:: TreeMap Int String
myTree = Node (5,"five") (Node (3,"three") (Node (2,"two") Empty Empty) Empty) (Node (7,"seven") Empty Empty)
-- ª
{-
Implement minTree :: (Ord a) => (TreeMap a b) -> Maybe b, which returns a Maybe structure with the value v of the node (k,v) with the smallest key k in the input tree.
If the tree is empty, then a Nothing must be returned.

Example:
ghci> minTree myTree
Just "two"
-}
minTree :: (Ord a) => (TreeMap a b) -> Maybe b
minTree Empty = Nothing
minTree (Node (_,v) Empty _) = Just v
minTree (Node _ l _) = minTree l

-- º 2.2
-- ª
{-
Implement ceilingKey :: (Ord a) => (TreeMap a b) -> a -> Maybe b, which, given a tree and a key k, returns a Maybe structure with the value v of the smallest key k' of the tree where k' >= k.
If the input key is greater than all of the tree's keys, then return a Nothing.

Solutions that only look for exact matches of the key will get up to 20% of the question's grade.

Examples:
ghci> ceilingKey myTree 3
Just "three"
ghci> ceilingKey myTree 1
Just "two"
ghci> ceilingKey myTree 6
Just "seven"
ghci> ceilingKey myTree 8
Nothing
-}
ceilingKey :: (Ord a) => (TreeMap a b) -> a -> Maybe b
ceilingKey Empty _ = Nothing
ceilingKey (Node (k',v) l r) k
    | k' < k = ceilingKey r k
    | k' > k = case ceilingKey l k of
            Just v1 -> Just v1
            Nothing -> Just v
    | otherwise = Just v