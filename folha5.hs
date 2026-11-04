--5.1
data Shape = Circle Float           -- radius
    | Rectangle Float Float         -- width, height
    | Triangle Float Float Float    -- 3 sides

--5.1 a)

area :: Shape -> Float
area (Circle r) = pi * r ** 2
area (Rectangle w h) = w*h
area (Triangle a b c) = sqrt(s*(s-a)*(s-b)*(s-c))
    where s = (a+b+c)/2

explain :: Shape -> String
explain (Circle r) = "A circle with radius " ++ show r
explain (Rectangle w h) = "A rectangle with " ++ show w ++ " by " ++ show h
explain (Triangle a b c ) = "A triangle with sides " ++ show a ++ ", " ++ show b ++ " and " ++ show c


--5.2 

data Suit = Clubs | Spades | Hearts | Diamonds deriving (Eq, Ord, Show, Enum)
data Face = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | J | Q | K | A deriving (Eq, Ord, Show, Enum)
data Card = Card Face Suit deriving (Eq, Show)

allCards :: [Card]
allCards = [Card f s | f <- [Two .. A], s <- [Clubs .. Diamonds]]

--5.3
cmp1 :: Card -> Card -> Ordering
cmp1 (Card f1 s1) (Card f2 s2) 
    | s1<s2 = LT
    | s1>s2 = GT 
    | f1<f2 = LT
    | f1>f2 = GT
    | otherwise = EQ

cmp2 :: Card -> Card -> Ordering
cmp2 (Card f1 s1) (Card f2 s2) 
    | f1<f2 = LT
    | f1>f2 = GT 
    | s1<s2 = LT
    | s1>s2 = GT
    | otherwise = EQ

--5.4 
data Set a = Empty                    -- empty set
           | Node a (Set a) (Set a)   -- value, left and right subtrees

insert :: Ord a => a -> Set a -> Set a -- insert a value into the tree
insert n Empty = Node n Empty Empty
insert n (Node k l r) 
    | n == k = Node k l r
    | n < k = Node k (insert n l) r
    | otherwise = Node k l (insert n r)

size :: Set a -> Int
size Empty = 0
size (Node _ l r) = 1 + size l + size r

height :: Set a -> Int
height Empty = 0
height (Node _ l r) = 1 + max (height l) (height r)

--5.7
type Name = Char -- ’x’, ’y’, ’z’, etc.
type Env = [(Name, Bool)]
data Prop = Const Bool
    | Var Name
    | Not Prop
    | And Prop Prop
    | Or Prop Prop

vars :: Prop -> [Name]
vars (Const _) = []
vars (Var v) = [v]
vars (Not p) = vars p
vars (And p1 p2) = vars p1 ++ vars p2
vars (Or p1 p2) = vars p1 ++ vars p2

--5.8

booleans :: Int -> [[Bool]]
booleans 0 = [[]]
booleans n = [ x : xs | x <- [False,True], xs <- booleans (n-1)]

booleans' :: Int -> [[Bool]]
booleans' n
    | n>0 = (booleansAcc (n-1) [False]) ++ (booleansAcc (n-1) [True])
    |otherwise = [[]]

booleansAcc :: Int -> [Bool] -> [[Bool]]
booleansAcc 0 acc = [acc]
booleansAcc 1 acc = [acc++[False],acc++[True]]
booleansAcc n acc = (booleansAcc (n-1) (acc ++ [False])) ++ (booleansAcc (n-1) (acc ++ [True]))

--5.9
environments :: [Name] -> [Env]
environments l = map (zip l) (booleans (length l))

--5.10
eval :: Env -> Prop -> Bool
eval env (Const b) = b
eval env (Var x) =
    case lookup x env of
    Just b -> b
    Nothing -> error "undefined variable"
eval env (Not p) = not (eval env p)
eval env (And p q) = eval env p && eval env q
eval env (Or p q) = eval env p || eval env q

nub :: Eq a => [a] -> [a]
nub [] = []                             
nub (x:xs) = x:nub([ n | n <-xs, x/=n])   

table :: Prop -> [(Env,Bool)]
table p = [ (env,(eval env p)) | env <- (environments ( nub $ vars p))]

--5.11
satisfies:: Prop -> [Env]
satisfies p = [ env | (env,bool) <- (table p), bool == True ]