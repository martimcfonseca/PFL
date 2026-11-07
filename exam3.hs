-- MT1 2025/2026

-- º 1.1
{-
Do not import any modules in any of the questions.
If you are allowed to use functions from certain modules in certain questions, assume that the import statements are already written. DO NOT write them in the answers to the questions.

Let us represent a set of connection between cities with distances using the types below:
-}
type Node = String   -- some city
type Dist = Int      -- some distance
type Edges = [(Node,Node,Dist)] -- directed connections

{-
For example, the following edges represent some connections in Portugal (the distances are in kilometers).
-}
portugal :: Edges
portugal = [ ("Porto", "Aveiro", 76)
           , ("Aveiro", "Coimbra", 63)
           , ("Aveiro", "Leiria", 117)
           , ("Coimbra", "Leiria", 76)
           , ("Leiria", "Santarem", 83)
           , ("Santarem", "Lisboa", 82)
           ]

-- ª
{-
Define a function «inverse :: Edges -> Edges» that reverses the edges maintaining the distances. Keep the edge order of the input list.

Example:
ghci> inverse [("A","B",10), ("C","D",15)]
[("B","A",10), ("D","C",15)]

Restriction: For this exercise you should use list comprehensions. Do not define recursive functions or use higher-order functions.
-}

inverse :: Edges -> Edges
inverse edges = [(b,a,d) | (a,b,d) <- edges]

-- º 1.2
-- ª
{-
Define a function «newEdges :: Edges -> Edges», that composes pairs of edges:

* if there is an edge (a,b) with distance d and another (b,c) with distance d', then there will be new edge (a,c) with distance d+d';
* exception: don't add edges (a,a) from a node to itself.

The result list should contain only the new edges.

Examples:
ghci> newEdges [("A","B",10), ("B","C",15), ("C","D",20)]
[("A","C",25), ("B","D",35)]
ghci> newEdges [("A","B",10), ("B","A",15)]
[]

Restriction: For this exercise you should use list comprehensions. Do not define recursive functions or use higher-order functions.
-}

newEdges :: Edges -> Edges
newEdges edges = [ (a1,b2,d1+d2)| ((a1,b1,d1),(a2,b2,d2)) <- zip edges (drop 1 edges), b1 == a2, a1 /= b2]

-- º 1.3
-- ª
{-
Define a function «pathDistance :: Edges -> [Node] -> Maybe Dist» that takes a list of edges and a list of nodes and computes the total distance of a path made up of the nodes in sequence (or Nothing if any of the connections in the path does not occur in the edges).

Examples:
ghci> pathDistance [("A","B",10),("A","D",20),("B","C",15)] ["A","B","C"]
Just 25
ghci> pathDistance [("A","B",10),("A","D",20),("B","C",15)] ["A","C"]
Nothing
-}

pathDistance :: Edges -> [Node] -> Maybe Dist
pathDistance _ [] = Just 0
pathDistance edges (start:path) = aux start 0 path
  where
    aux _ acc [] = Just acc
    aux v acc (u:p) =
      case [d | (x,y,d)<-edges, x==v && y==u] of
        (d:_) -> aux u (acc+d) p
        [] -> Nothing

-- º 1.4
-- ª
{-
Define a recursive function «shortest :: Edges -> Edges» that removes duplicated connections from a list of edges, keeping only the connection with minimum distance.

Example:
ghci> shortest [("A","B",10), ("B","C",10), ("A","B",7), ("B","C",15)]
[("A","B",7), ("B","C",10)]
-}

shortest :: Edges -> Edges
shortest [] = []
shortest edges@((x,y,_):e) = (x,y,(minDistance edges (x,y))):(shortest $ edgesRemove e (x,y)) 

minDistance :: Edges -> (Node,Node) -> Dist
minDistance edges (a,b) = minimum [d | (x,y,d) <- edges, a==x, y==b]

edgesRemove :: Edges -> (Node,Node) -> Edges
edgesRemove edges (a,b) = [(x,y,d) | (x,y,d) <- edges, a/=x, y/=b]


-- º 2.1
{-
Consider the «Prop» data type for propositions presented in the Lectures.
-}
type VarName = Char

data Prop = Const Bool
          | Variable VarName
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          deriving Show
-- ª
{-
Define a recursive function «rename :: [(VarName,VarName)] -> Prop -> Prop» that renames all variables consistently using the list of pairs given as first argument.
If a variable does not occur in the list, then leave it unchanged.
Suggestion: use the lookup function from the Prelude, whose signature is «lookup :: Eq a => a -> [(a, b)] -> Maybe b».

Examples:
ghci> rename [('a','x'),('b','y')] (And (Variable 'a') (Variable 'b'))
And (Variable 'x') (Variable 'y')
ghci> rename [('a','b'),('b','a')] (And (Variable 'a') (Variable 'b'))
And (Variable 'b') (Variable 'a')
ghci> rename [('a','x')] (And (Variable 'a') (Variable 'b'))
And (Variable 'x') (Variable 'b')
-}

rename :: [(VarName,VarName)] -> Prop -> Prop
rename _ (Const b) = Const b
rename ren (Variable name) = case lookup name ren of
                            Just x -> Variable x
                            Nothing -> Variable name 
rename ren (Not p) = Not (rename ren p)
rename ren (And p1 p2) = And (rename ren p1) (rename ren p2)
rename ren (Imply p1 p2) = Imply (rename ren p1) (rename ren p2)


-- º 2.2
-- ª
{-
Define an auxiliary recursive function nubBy :: (a -> a -> Bool) -> [a] -> [a], that removes duplicate values from a list using the supplied argument function to check for equality.
In particular, the usual nub function is the same as nubBy (==).
For each value that appears multiple times in the input list, keep the first occurrence in the output list.

Examples:
ghci> nubBy (\(a,_) (c,_) -> a == c) [(3,3),(1,2),(3,1)]
[(3,3),(1,2)]
ghci> nubBy (==) "AbaaBCc"
"AbaBCc"
-}

nubBy :: (a -> a -> Bool) -> [a] -> [a]
nubBy _ [] = []
nubBy eq (x:xs) = x : nubBy eq [y | y <-xs, not (eq x y)]

-- º 2.3
-- ª
{-
Using the rename function and nubBy, define a function normalize :: Prop -> Prop, that changes all variables in a proposition to 'a', 'b', 'c', etc. consistently.
The variables' alphabetical order should match the order in which they first appear in the proposition if read as a String (from left to right).
Suggestion: define an auxiliary function vars to list all variables in a proposition (as was done in the lectures).

Examples:
ghci> vars (And (Variable 'a') (Variable 'b'))
"ab"
ghci> vars (Imply (Variable 'x') (Variable 'x'))
"xx"
ghci> normalize (And (Variable 'p') (Variable 'r'))
And (Variable 'a') (Variable 'b')
ghci> normalize (Imply (Variable 'x') (Variable 'x'))
Imply (Variable 'a') (Variable 'a')
-}

normalize :: Prop -> Prop
normalize p = rename v p
    where v = zip (nubBy (==)  (vars p)) ['a'..]

vars :: Prop -> [VarName]
vars (Const _) = []
vars (Variable x) = [x]
vars (Not p) = vars p
vars (And p1 p2) = (vars p1) ++ (vars p2)
vars (Imply p1 p2) = (vars p1) ++ (vars p2)


