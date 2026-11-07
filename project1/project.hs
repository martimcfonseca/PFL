module Main where

import Parsing
import Data.List (nub)

type Name = Char -- ’x’, ’y’, ’z’, etc.
type Env = [(Name, Bool)]
data Prop = Const Bool
    | Var Name
    | Not Prop
    | And Prop Prop
    | Or Prop Prop

eval :: Env -> Prop -> Bool
eval env (Const b) = b
eval env (Var x) =
    case lookup x env of
    Just b -> b
    Nothing -> error "undefined variable"
eval env (Not p) = not (eval env p)
eval env (And p q) = eval env p && eval env q
eval env (Or p q) = eval env p || eval env q

vars :: Prop -> [Name]
vars (Const _) = []
vars (Var v) = [v]
vars (Not p) = vars p
vars (And p1 p2) = vars p1 ++ vars p2
vars (Or p1 p2) = vars p1 ++ vars p2

booleans :: Int -> [[Bool]]
booleans 0 = [[]]
booleans n = [ x : xs | x <- [False,True], xs <- booleans (n-1)]

environments :: [Name] -> [Env]
environments l = map (zip l) (booleans (length l))
 

table :: Prop -> [(Env,Bool)]
table p = [ (env,(eval env p)) | env <- (environments ( nub $ vars p))]

satisfies:: Prop -> [Env]
satisfies p = [ env | (env,bool) <- (table p), bool == True ]

classify :: Prop -> String
classify p 
    | ls == 0 = "CONTRADICTION"
    | lt == ls = "TAUTOLOGY"
    | otherwise = "CONTINGENT"
        where   lt = length $ table p
                ls = length $ satisfies p

-----------------------------------------------------------

-- | a parser for propositions
-- Grammar rules:
--
--prop ::= term propCont

--propCont ::= | term propCont | ε

--term ::= factor termCont

--termCont ::= & factor termCont | ε

--factor ::= ! factor | name | constant | ( prop )

--constant ::= T | F

prop :: Parser Prop
prop = do
    t <- term
    propCont t

propCont :: Prop -> Parser Prop
propCont acc= do
    char '|'
    t <- term
    propCont (Or acc t)
    <|> return acc

term :: Parser Prop
term = do
    f <- factor
    termCont f

termCont :: Prop -> Parser Prop
termCont acc = do
    char '&'
    f <- factor
    termCont (And acc f)
    <|> return acc

factor :: Parser Prop
factor = do { char '!';
        f <- factor;
        return (Not f);
        }
    <|> do {
        n <- name;
        return (Var n);
    }
    <|> do {
        c <- constant;
        return (Const c);
    }
    <|> do{ char '(';
        p <- prop;
        char ')';
        return p;
    }

name :: Parser Char
name = do
    x <- satisfy (\c -> c>= 'a' && c<='z')
    return x

constant :: Parser Bool
constant = do
    x <- satisfy (\c -> c == 'T' || c == 'F')
    return $ if x=='T' then True else False

---------------------------------------------------------


main :: IO ()
main
  = do txt <- getContents
       calculator (lines txt)

-- | read-eval-print loop
calculator :: [String] -> IO ()
calculator []  = return ()
calculator (l:ls) = do putStrLn (evaluate l)
                       calculator ls  

-- | evaluate a single expression
evaluate :: String -> String
evaluate txt
  = case parse prop txt of
      [ (tree, "") ] ->  classify tree
      _ -> "parse error; try again" 