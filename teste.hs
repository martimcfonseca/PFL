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

isSoloACappella :: Performance -> Bool
isSoloACappella ([_], []) = True
isSoloACappella _ = False

containsInstrument :: Performance -> String -> Bool
containsInstrument (_, ips) inst = aux ips inst
    where   aux [] _ = False
            aux ((i,_):t) inst = i == inst || aux t inst


countInstrument :: Concert -> String -> Int
countInstrument c inst = length $ filter (\p -> containsInstrument p inst) c

countInstrument' :: Concert -> String -> Int
countInstrument' c inst = sum $ map (\p -> if containsInstrument p inst then 1 else 0) c

countInstrument'' :: Concert -> String -> Int
countInstrument'' c inst = foldr(\p acc -> (if containsInstrument p inst then 1 else 0) + acc) 0 c

concertSummary :: Concert -> [([String], [String])]
concertSummary c = [(take 2 singers, [player | ((_,player),i) <- zip instruments [0 ..], mod i 2 == 0]) | (singers, instruments) <- c]


myIntersect :: (Eq a) => [a] -> [a] -> [a]
myIntersect xs ys = [x | x <- xs, elem x ys]

sortUniques :: Ord a => (a -> a -> Bool) -> [a] -> [a]
sortUniques _ [] = []
sortUniques _ [x] = [x]
sortUniques cmp l = merge (sortUniques cmp l1) (sortUniques cmp l2) cmp
    where   (l1,l2) = splitAt (div (length l) 2) l
            merge [] l _ = l
            merge l [] _  = l
            merge (x:xs) (y:ys) cmp
                | cmp x y = x:merge xs (y:ys) cmp
                | cmp y x = y:merge (x:xs) ys cmp
                | otherwise = x:merge xs ys cmp

tiredSingers :: Concert -> [String]
tiredSingers [] = []
tiredSingers [_] = []
tiredSingers [_,_] = []
tiredSingers c = sortUniques (<)  $ concat [ myIntersect (myIntersect ss ss1) ss2 | ((ss,_),(ss1,_),(ss2,_)) <- zip3 c (drop 1 c) (drop 2 c)]

printInstrumentPlayers :: Performance -> IO ()
printInstrumentPlayers (_,ips) = aux ips
    where   aux [] = return ()
            aux ((i,p):ps) = do
                putStrLn $ p ++ " played the " ++ i
                aux ps



data TreeMap a b = Empty | Node (a,b) (TreeMap a b) (TreeMap a b) deriving (Eq, Ord, Read, Show)

minTree :: (Ord a) => (TreeMap a b) -> Maybe b
minTree Empty = Nothing
minTree (Node (_,v) Empty _) = Just v
minTree (Node _ l _) = minTree l

ceilingKey :: (Ord a) => (TreeMap a b) -> a -> Maybe b
ceilingKey Empty _ = Nothing
ceilingKey (Node (k,v) l r) search
    | k < search = ceilingKey r search
    | k > search = case ceilingKey l search of
        Just v1 -> Just v1
        Nothing -> Just v
    | otherwise = Just v