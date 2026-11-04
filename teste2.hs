type Node = String
type Dist = Int
type Edges = [(Node, Node, Dist)]

inverse :: Edges -> Edges
inverse edges = [(y,x,d) | (x,y,d) <- edges]

newEdges :: Edges -> Edges
newEdges edges = [(x,z,d+d') | (x,y,d) <- edges, (y',z,d') <- edges, y == y' && x /= z]

pathDistance :: Edges -> [Node] -> Maybe Dist
pathDistance _ [] = Just 0
pathDistance edges (start:path) = aux start 0 path
    where   aux _ acc [] = Just acc
            aux v acc (u:path) = case [ d | (x,y,d) <- edges, x== v && y == u] of
                    (d:_) -> aux u (acc + d) path
                    [] -> Nothing


shortest :: Edges -> Edges
shortest [] = []
shortest ((x,y,d):edges) = (x,y,d') : edges'
    where   d' = minimum (d : [d' | (a,b,d') <- edges, a==x, b==y])
            edges' = [(a,b,d) | (a,b,d) <- edges, a/= x, b/= y]


type VarName = Char

data Prop = Const Bool | Variable VarName | Not Prop | And Prop Prop | Imply Prop Prop deriving Show



rename :: [(VarName, VarName)] -> Prop -> Prop
rename _ (Const b) = Const b
rename ren (Variable x) = case lookup x ren of
    Just y -> Variable y
    Nothing -> Variable x
rename ren (Not p) = Not (rename ren p)
rename ren (And p q ) = And (rename ren p) (rename ren q)
rename ren (Imply p q) = Imply (rename ren p) (rename ren q)


nubBy :: (a -> a -> Bool) -> [a] -> [a]
nubBy _ [] = []
nubBy eq (x:xs) = x : nubBy eq [x' | x' <- xs, not (eq x x')]

normalize :: Prop -> Prop
normalize p = rename (zip (findUniqueNames p ) ['a'..]) p
    where   findUniqueNames p = nubBy (==) (vars p)

vars :: prop -> [VarName]
vars (Const _) = []
vars (Variable x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
