{-
PFL MT1 - Part2 2024/2025

Solution code by: Gonçalo Leão
-}
-- º 1.1
{-
You are not allowed to import any libraries unless stated otherwise. As such, do not call the "import" function.

Consider the following types to represent a football league.
-}
type Match = ((String,String), (Int,Int))
type MatchDay = [Match]
type League = [MatchDay]

myLeague :: League
myLeague = [
  [(("Porto","Sporting"),(2,2)),(("Benfica","Vitoria SC"),(4,0))],
  [(("Porto","Benfica"),(5,0)),(("Vitoria SC","Sporting"),(3,2))],
  [(("Vitoria SC","Porto"),(1,2)),(("Sporting","Benfica"),(2,1))]
  ]

{-
A football league is made up of a sequence of match days. In each match day, a set of matches occurs.
The match is a structure with the name of the home and away team, which play against each other, alongside the number of goals of each team. For instance, in the match where Porto was the home team and scored 5 goals and Benfica was the away team and scored 0 goals, the respective value of Match is: (("Porto","Benfica"),(5,0)).

Assume that all teams play exactly one match in each matchday.

To help you test your functions, consider the league presented above.
-}

-- For the automatic corrector
pfl_liga3 :: League -- https://www.zerozero.pt/edicao/liga-3-2-fase-manutencao-serie-2-2023-24/183123/calendario
pfl_liga3 = [
  [(("Sporting B","Pero Pinheiro"),(1,0)),(("Oliv Hospital","Caldas"),(3,1)),(("1º Dezembro","Amora"),(1,0))], -- J2
  [(("1º Dezembro","Sporting B"),(1,4)),(("Amora","Oliv Hospital"),(2,0)),(("Caldas","Pero Pinheiro"),(4,1))], -- J3
  [(("Pero Pinheiro","Oliv Hospital"),(3,2)),(("Amora","Sporting B"),(1,3)),(("Caldas","1º Dezembro"),(2,1))], -- J1
  [(("Oliv Hospital","1º Dezembro"),(1,0)),(("Sporting B","Caldas"),(3,0)),(("Pero Pinheiro","Amora"),(0,1))], -- J4
  [(("Caldas","Amora"),(1,2)),(("Oliv Hospital","Sporting B"),(0,2)),(("Pero Pinheiro","1º Dezembro"),(1,3))] -- J5
  ]
  
pfl_liga3_alt :: League -- Outra liga, com empates: https://www.zerozero.pt/edicao/liga-3-2-fase-manutencao-serie-1-2023-24/183122/calendario
pfl_liga3_alt = [
  [(("Fafe","Canelas"),(2,1)),(("Anadia","Vianense"),(2,0)),(("Sanjoanense","Trofense"),(0,3))], -- J1
  [(("Trofense","Fafe"),(1,3)),(("Canelas","Anadia"),(2,0)),(("Vianense","Sanjoanense"),(1,0))], -- J2
  [(("Vianense","Trofense"),(1,1)),(("Sanjoanense","Canelas"),(1,1)),(("Anadia","Fafe"),(0,4))], -- J3
  [(("Trofense","Anadia"),(0,0)),(("Fafe","Sanjoanense"),(0,2)),(("Canelas","Vianense"),(1,3))], -- J4
  [(("Anadia","Sanjoanense"),(1,2)),(("Fafe","Vianense"),(2,0)),(("Canelas","Trofense"),(1,1))] -- J5
  ]


{-
Implement winner :: Match -> String, which, given match, returns the name of the match's winner or "draw" in case both teams scored the same number of goals.

Examples:
ghci> winner (("Porto","Benfica"),(5,0))
"Porto"
ghci> winner (("Porto","Sporting"),(2,2))
"draw"
-}
winner :: Match -> String
winner ((h,a),(sh,sa)) 
    | sh < sa = a
    | sh > sa = h
    | otherwise = "draw"

-- º 1.2
{-
Implement matchDayScore :: String -> MatchDay -> Int, which, given team and a matchday, returns the score obtained by that team on their match of that matchday. Winning a match gives a team 3 points, drawing gives 1 point and losing gives 0 points. If the input team does not exist in the league, then return 0.

Examples:
ghci> matchDayScore "Porto" (myLeague !! 0)
1
ghci> matchDayScore "Benfica" (myLeague !! 0)
3
ghci> matchDayScore "Vitoria SC" (myLeague !! 0)
0
-}
matchDayScore :: String -> MatchDay -> Int
matchDayScore name matches 
    |length res ==0 = 0
    |otherwise = head res
    where res =[ matchScore name m | m@((h,a),_) <- matches, name==h || name == a ]


matchScore :: String -> Match -> Int
matchScore name match 
    |winner match == name = 3
    |winner match == "draw" = 1
    |otherwise = 0


{-
To help you out in the next question, you can use the leagueScore and sortByCond functions, presented below, which you can also use later on this exam if needed. DO NOT COPY the code of these three functions to any of the answer boxes of this exam. Just call the functions and assume they will run using the code below.
-}
leagueScore :: String -> League -> Int
leagueScore t = foldr (\d acc -> matchDayScore t d + acc) 0

sortByCond :: Ord a => [a] -> (a -> a -> Bool) -> [a]
sortByCond [] _ = []
sortByCond [x] _ = [x]
sortByCond l cmp = merge (sortByCond l1 cmp) (sortByCond l2 cmp) cmp
  where (l1 ,l2) = splitAt (div (length l) 2) l

merge :: Ord a => [a] -> [a] -> (a -> a -> Bool) -> [a]
merge [] l _ = l
merge l [] _ = l
merge (x:xs) (y:ys) cmp
  | cmp x y = x:(merge xs (y:ys) cmp)
  | otherwise = y:(merge (x:xs) ys cmp)

-- ª
{-
Implement ranking:: League -> [(String,Int)], which returns a list of pairs (team,score) with the score obtained by each team during the league. The league score is the sum of the points obtained by the team in each matchday. The output list must be ordered by the scores, in decreasing order. In case of a tie in the scores, return the team name that comes first alphabetically.

Examples:
ghci> ranking myLeague 
[("Porto",7),("Sporting",4),("Benfica",3),("Vitoria SC",3)]
ghci> ranking (tail myLeague)
[("Porto",6),("Sporting",3),("Vitoria SC",3),("Benfica",0)]
ghci> ranking [] 
[]
-}

ranking :: League -> [(String,Int)]
ranking [] = []
ranking l@(d:_) = sortByCond [(name, leagueScore name l) | name <- concat [[s1,s2] | ((s1,s2),_) <- d]] (\(n1, s1) (n2, s2) -> s1 > s2 || (s1 == s2 && n1<n2))

-- º 1.4
-- ª
{-
Implement numMatchDaysWithDraws :: League -> Int, which returns the amount of matchdays in the league where at least one match ended in a draw.

Constraint: in this exercise, you must use higher-order functions. You are not allowed to define new recursive functions or use list comprehensions.

Example:
ghci> numMatchDaysWithDraws myLeague 
1
-}
numMatchDaysWithDraws :: League -> Int
numMatchDaysWithDraws l =  length $ filter (\mday -> any (=="draw") (map winner mday) ) l

-- º 1.5
-- ª
{-
Implement bigWins :: League -> [(Int,[String])], which returns a list of pairs (i,ws), where i is the index of the matchday (with indices starting at 1)
and ws are the names of teams from that matchday that won their match with a goal difference of at least 3 goals. Any order of the winners ws of each matchday will be accepted.

Constraint: in this exercise, you must use list comprehensions. You are not allowed to define new recursive functions or use higher-order functions (map, filter, foldl ...).

Examples:
ghci> bigWins myLeague
[(1,["Benfica"]),(2,["Porto"]),(3,[])]
-}

bigWins :: League -> [(Int,[String])]
bigWins l = [ (i,[ winner m| m@(_,(s1,s2)) <- (l !! (i-1)), s1-s2>=3 || s2-s1>=3]) | i<-[1..(length l)] ]

-- º 1.6
-- ª
{-
Implement winningStreaks :: League -> [(String,Int,Int)], which returns a list of winning streaks, described by a triple (t,s,e), where:
- t is the name of the team involved in the winning streak;
- s and e are the indices of the streak's first and last matchday (with indices starting at 1).
A winning streak is a sequence of wins in 2 or more consecutive matchdays.

Note that the same team may have more than one winning streak and thus appear multiple times in the output. Any order of the output list will be accepted.

Examples:
ghci> winningStreaks myLeague 
[("Porto",2,3)]
ghci> winningStreaks [[(("RM","Barca"),(4,0))],[(("Barca","RM"),(1,2))],[(("RM","Barca"),(2,2))],[(("RM","Barca"),(0,4))],[(("Barca","RM"),(3,0))],[(("Barca","RM"),(3,1))]]
[("RM",1,2),("Barca",4,6)]
-}
myLeague2 :: League
myLeague2 = [[(("RM","Barca"),(4,0))],[(("Barca","RM"),(1,2))],[(("RM","Barca"),(2,2))],[(("RM","Barca"),(0,4))],[(("Barca","RM"),(3,0))],[(("Barca","RM"),(3,1))]]

winningStreaks :: League -> [(String,Int,Int)]
winningStreaks l = filter (\(_,i,j) -> j>i) $ makeWinStreaks  [] $ winningStreaksAux l 1 []

winningStreaksAux :: League -> Int -> [(String,Int,Int)] -> [(String,Int,Int)]
winningStreaksAux [] _ acc = acc
winningStreaksAux (m_day:m_days) i acc = sortByCond (acc ++ winningStreaksAux m_days (i+1) [(w,i,i) | m <- m_day, let w = winner m, w/="draw"]) (\(n1,i1,_) (n2,i2,_) -> n1 < n2 || (n1 == n2 && i1<i2) )

makeWinStreaks :: [(String,Int,Int)] -> [(String,Int,Int)] -> [(String,Int,Int)]
makeWinStreaks _ [] = []
makeWinStreaks acc (x:[]) = acc++[x]
makeWinStreaks acc (x1@(w1,i1,_):x2@(w2,_,f2):xs)
    | w1 /= w2 = makeWinStreaks (acc ++ [x1]) (x2:xs)
    | otherwise = let x3 = (w1,i1,f2) in makeWinStreaks acc (x3:xs)


-- º 2.1
{-
Consider the following types from your Haskell practical assignment, to represent roadmaps, where roads of a given distance connect cities. To help you out during testing, two roadmaps are provided.
-}
type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

gTest1 :: RoadMap
gTest1 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest2 :: RoadMap -- unconnected graph
gTest2 = [("0","1",4),("2","3",2)]

-- ª
{-
Implement adjacent :: RoadMap -> City ->[(City,Distance)], which returns the cities adjacent to a particular city (i.e. cities with a direct edge between them) and the respective distances to them. Any order of the output list will be accepted. Note that a city is NOT adjacent to itself.
Examples:
ghci> adjacent gTest1 "0"
[("1",10),("2",15),("3",20)]
ghci> adjacent gTest2 "0"
[("1",4)]
-}
adjacent :: RoadMap -> City ->[(City,Distance)]
adjacent r_map city = [(if c1==city then c2 else c1,d) | (c1,c2,d) <- r_map, city==c1 || city==c2]

-- º 2.2
-- ª
{-
Implement areConnected :: RoadMap -> City -> City -> Bool, which determines if there is a path, composed of one or more roads, connecting both input cities of the input roadmap. Consider that a city is connected to itself.

Examples:
ghci> areConnected gTest1 "0" "3"
True
ghci> areConnected gTest2 "0" "3"
False
-}
areConnected :: RoadMap -> City -> City -> Bool
areConnected rmap x y = areConnectedAux rmap [x] y

areConnectedAux :: RoadMap -> [City] -> City -> Bool
areConnectedAux _ [] _ = False
areConnectedAux rmap (c:cs) x
    | c == x = True
    | otherwise = or [areConnectedAux rmap (a:c:cs) x | (a,_) <- adjacent rmap c, not (elem a (c:cs))]

-- º 3.1
{-
Consider the following type to represent a KD-tree, which stores a set of 2D points. Non-leaf nodes of this tree contain:
- the coordinates of a point (x,y);
- the indication of whether the node is an X or Y node;
- a left subtree, where, for all of its points (x',y'), x' < x if this is a X node, or y' < y if this is a Y node;
- a right subtree, where, for all of its points (x',y'), x' >= x if this is a X node, or y' >= y if this is a Y node.

The root of any non-empty KdTree is always an X node and each level of depth in tree alternates between both kinds of nodes. Thus, the direct non-empty children of the root are Y nodes and the non-empty grandchildren are X nodes (if they exist).

A KD-tree thus behaves like a binary search tree that alternates the criterion used to divide the search space. Consider that the tree does not contain duplicate points (i.e. two or more points with the exact coordinates). The trees can, however, contain several points with the same x coordinate (but different y coordinates), or vice-versa. To help you out during testing, two trees are provided.
-}
data KdTree = Empty | Node Char (Int,Int) KdTree KdTree deriving (Eq,Show)

tree1 :: KdTree
tree1 = Node 'x' (3,3) (Node 'y' (2,2) Empty Empty) (Node 'y' (4,4) Empty Empty)

tree2 :: KdTree
tree2 = Node 'x' (3,3) (Node 'y' (2,2) (Node 'x' (1,1) Empty Empty) Empty) (Node 'y' (4,4) (Node 'x' (3,2) Empty Empty) Empty)

-- ª
{-
Implement insert :: (Int,Int) -> KdTree -> KdTree, which given a point p and a tree t,
returns a new tree with the result of adding p to t.
Point p should be added on the appropriate leaf node (Empty),
according to the rules of the kd-tree stated before. If point p is already in the tree, then return t.

Examples:
ghci> insert (3,2) tree1
Node 'x' (3,3) (Node 'y' (2,2) Empty Empty) (Node 'y' (4,4) (Node 'x' (3,2) Empty Empty) Empty)
ghci> insert (1,1) (insert (3,2) tree1)
Node 'x' (3,3) (Node 'y' (2,2) (Node 'x' (1,1) Empty Empty) Empty) (Node 'y' (4,4) (Node 'x' (3,2) Empty Empty) Empty)
-}
insert :: (Int,Int) -> KdTree -> KdTree
insert p t = insertAux 'x' p t

insertAux :: Char -> (Int,Int) -> KdTree ->KdTree
insertAux c p Empty = Node c p Empty Empty
insertAux 'x' (x,y) t@(Node c (node_x,node_y) left right)
  | x<node_x = (Node 'x' (node_x,node_y) (insertAux 'y' (x,y) left) right)
  | x==node_x && y==node_y = t
  | otherwise = (Node 'x' (node_x,node_y)  left (insertAux 'y' (x,y) right))
insertAux 'y' (x,y) t@(Node c (node_x,node_y) left right)
  | y<node_y = (Node 'y' (node_x,node_y) (insertAux 'x' (x,y) left) right)
  | x==node_x && y==node_y = t
  | otherwise = (Node 'y' (node_x,node_y)  left (insertAux 'x' (x,y) right))


{-
Implement putTreeStr :: KdTree -> IO (), which prints the contents of the tree in several lines, in a user-friendly manner. The output format must be exactly as detailed in the examples (including regarding the ' ' and newlines).

Each tree is printed as follows:
- a line for the root's point;
- a line with the condition that all points of the left child must follow;
- the left child (if it exists);
- a line with the condition that all points of the right child must follow;
- the right child (if it exists).

At each level of depth in tree, all of the prints must be prefixed by exactly two additional spaces: "  ".
Note that nothing should be printed for empty trees.

Examples:
ghci> putTreeStr Empty
ghci> putTreeStr tree1
(3,3)
x<3
  (2,2)
  y<2
  y>=2
x>=3
  (4,4)
  y<4
  y>=4
ghci> putTreeStr tree2
(3,3)
x<3
  (2,2)
  y<2
    (1,1)
    x<1
    x>=1
  y>=2
x>=3
  (4,4)
  y<4
    (3,2)
    x<3
    x>=3
  y>=4
-}
putTreeStr :: KdTree -> IO ()
putTreeStr t = putTreeStrAux t ""

putTreeStrAux :: KdTree -> String -> IO ()
putTreeStrAux Empty _ = return ()
putTreeStrAux  (Node c (x,y) l r) prefix= do
    putStrLn $ prefix ++ show (x,y)
    putStrLn $ prefix ++ [c] ++ "<" ++ show var
    putTreeStrAux l (prefix ++ "  ")
    putStrLn $ prefix ++ [c] ++ ">=" ++ show var
    putTreeStrAux r (prefix ++ "  ")
    where var = if c =='x' then x else y