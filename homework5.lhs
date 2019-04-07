\begin{code}
--Jacob Collins (jacollins)
import Data.List

--5.1 Trees

data Tree a = E

            | T a (Tree a) (Tree a)

            deriving (Eq, Show)

--similar to the level oriented solution found in
-- breadth-first numbering: lessons from a small exercise in algorithm design
-- what differentiates my code from this solution is that I seperately calculate a list
-- of available indices as oppose to using lazy evaluation
-- This is done by calculating the amount of nodes in each level (levelList)
-- and then adding these with scan1 and adding one to find available indices and using 1: init to make
-- a comrehensive list of available indices for each level and using this as an argument
bfnum :: Tree a -> Tree Int
bfnum tree =  snd . bfnumTree $ ( (1 : (init . map (+1) . scanl1 (+) $ map length $ (levelList tree))), tree)
    where 
    bfnumTree (indices, E) = (indices, E)
    bfnumTree (x : xs, T mid left right) = (x + 1: indices'', T x leftSide rightSide)
        where 
            (indices',  leftSide) = bfnumTree (xs,  left)
            (indices'', rightSide) = bfnumTree (indices', right)


levelList :: Tree a -> [[a]]
levelList E = []
levelList (T m left right) = [m] : merge (levelList left) (levelList right)

merge :: [[a]] -> [[a]] -> [[a]]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = (x ++ y) : merge xs ys

test3 = bfnum (T 'a' (T 'b' E (T 'c' E E)) (T 'd' E E)) == (T 1 (T 2 E (T 4 E E)) (T 3 E E))


--5.2 Expression Trees

type Identifier = String

data Expr = Num Integer

          | Var Identifier

          | Let {var :: Identifier, value :: Expr, body :: Expr}

          | Add Expr Expr

          | Sub Expr Expr

          | Mul Expr Expr

          | Div Expr Expr

          deriving (Eq)

instance Show Expr where
    show (Num x) =  show x
    show (Var x) = x
    show (Let x y z) = "let " ++ x ++ " = " ++  show y ++ " in " ++ show z ++ " end"
    show (Sub x y) =  show x ++ " - " ++ show y 
    show (Add x y) =  show x ++ " + " ++ show y 
    show (Mul x y) =  show x ++ " * " ++ show y 
    show (Div x y) =  show x ++ " / " ++ show y 

type Env = Identifier -> Integer

emptyEnv :: Env

emptyEnv = \s -> error ("unbound: " ++ s)

extendEnv :: Env -> Identifier -> Integer -> Env

extendEnv oldEnv s n s' = if s' == s then n else oldEnv s'

evalInEnv :: Env -> Expr -> Integer

evalInEnv emptyEnv x = case x of
    Num x -> x
    Var x -> emptyEnv x
    Add y z -> evalInEnv emptyEnv y + evalInEnv emptyEnv z
    Sub y z -> evalInEnv emptyEnv y - evalInEnv emptyEnv z
    Mul y z -> evalInEnv emptyEnv y * evalInEnv emptyEnv z
    Div y z -> evalInEnv emptyEnv y `div` evalInEnv emptyEnv z
    Let y z i -> let q = extendEnv emptyEnv y h
                     h = (evalInEnv emptyEnv z)
                 in evalInEnv q i
     
--5.3 Infinite Lists

diag :: [[a]] -> [a]
diag = concat . map  (reverse) . getDiagonals [] 
    

getDiagonals :: [[a]] -> [[a]] -> [[a]]
getDiagonals y (x:xs) 
    | null(x:xs) =  []
    | otherwise = map head y : (getDiagonals (x:(map tail y)) xs)
    
         

test = take 20 (diag qlist2) == ["1","2", "1/2","3", "1", "1/3","4", "3/2", "2/3", "1/4","5", "2", "1", "1/2", "1/5","6", "5/2", "4/3", "3/4", "2/5"]

test2 = evalInEnv emptyEnv (Let "x" (Num 3) (Add (Var "x") (Num 5))) == 8

-- The standard table of all positive rationals, in three forms: 
-- (1) as floats S
rlist = [ [i/j | i<-[1..]] | j <- [1..] ] 
-- (2) as strings, not reducd 
qlist1 = [ [show i ++ "/" ++ show j | i<-[1..]] | j <- [1..] ] 
-- (3) as strings, in reduced form 
qlist2 = [ [fracString i j | i <- [1..]] | j <- [1..] ]
-- take a numerator and denominator, reduce, and return as string 
fracString num den = if denominator == 1 
                then show numerator 
                else show numerator ++ "/" ++ show denominator
    where c = gcd num den 
          numerator = num `div` c 
          denominator = den `div` c
-- Take an n-by-n block from the top of a big list of lists 
block n x = map (take n) (take n x)

\end{code}