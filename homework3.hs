--No Other Imports Are Allowed

import Data.List

--Jacob Collins

-- jacollins

--3.1 Lists And Trees (10pts)

data Tree a = LeafT a | NodeT (Tree a) (Tree a) deriving (Eq, Show)



balance :: [a] -> Tree a
balance [x] = LeafT x
balance xs = NodeT (balance left) (balance right)
                 where 
                 (left,right) = splitAt (length xs `div` 2) xs
                 



--3.2 Simple Functions On Numbers (10pts)

goldbach :: Int -> [(Int,Int)]
goldbach 0 = []
goldbach num = sort(filt(x:xs))
    where
    (x:xs) = reverse([(x , y) | x <- primeList num, y <- primeList num, x + y == num])
    factors n = [x | x <- [1..n], n `mod` x == 0]
    prime n = factors n == [1,n]
    primeList n = [x | x <- [2..n-2], prime x ]

filt :: [(Int,Int)] -> [(Int, Int)]
filt [] = []
filt (x:xs) = if (switch (x)) `elem` xs || x `elem` xs then filt xs else [x] ++ filt xs
        where
            switch (x,y) =   (y,x)

--3.3 Higher-Order Functions (10pts)

church :: Int -> (c -> c) -> c -> c

church num f = foldr (.) myIdentity $ replicate num f

myIdentity :: a -> a
myIdentity a = a


--3.4 Recursive Functions Over Lists (10pts)

type Set = [Int]

powerset :: [Int] -> [[Int]]
powerset [] = [[]]
powerset (xs) = subsequences(xs)



--3.5 Lists And Strings (10pts)

example :: [[(Double, Double)]]

example = [[(100.0,100.0),(100.0,200.0),(200.0,100.0)],

  [(150.0,150.0),(150.0,200.0),(200.0,200.0),(200.0,150.0)]]



makeCommand :: [[(Double, Double)]] -> String
makeCommand [] = " "
makeCommand [x: []] = " "
makeCommand x = headCommand x ++ concat(map midTextList x) ++ "showpage\n%%EOF"

headCommand :: [[(Double, Double)]] -> String
headCommand x = "\n%!PS-Adobe-3.0 EPSF-3.0\n%%BoundingBox: " ++ show(getLowX x) ++ " " ++ show(getLowY x)++ " "  ++ show(getHighX x) ++ " "  ++ show(getHighY x) ++ "\n\n"
        where
            --returns least x of the list of tuples
            getLowX list =  fst (minimum (map minimum(list)))
            --returns least y of the list of tuples
            getLowY list = fst(minimum(map minimum(map (map swap) list)))
            -- returns greatest x of the list of tuples
            getHighX list = fst (maximum (map maximum(list)))
            --returns greatest y of the list of tuples
            getHighY list = fst(maximum(map maximum(map (map swap) list)))



midTextList :: [(Double, Double)] -> String
midTextList [] = "\n"
midTextList (x:xs) = firstLine x ++ otherLines xs

firstLine :: (Double,Double) -> String
firstLine x = show(fst(x)) ++ " " ++ show(snd(x)) ++ " moveto \n"

otherLines :: [(Double,Double)] -> String
otherLines [] =  "closepath\nstroke\n\n" 
otherLines (x:xs) = show(fst(x)) ++ " " ++ show(snd(x))  ++ " lineto\n" ++ otherLines xs

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)         


--3.6 Trees (25pts)

data T = Leaf | Node T T deriving (Eq, Show)



data P = GoLeft P | GoRight P | This deriving (Eq, Show)
 


allpaths :: T -> [P]

allpaths Leaf = [This]
allpaths (Node left right) = concat([This]:  [map(GoLeft) (allpaths left)] ++ [map(GoRight) (allpaths right)]) 


--3.7 Logic (25pts)

type Expr = [[Int]] 


eval :: (Int -> Bool) -> Expr -> Bool
eval p x =    and(map or ((map.map) (checking) x))
            where
            checking x
                | x < 0 = not(p (-x)) 
                | otherwise = (p x)
    



satisfiable :: Expr -> Bool

satisfiable x = and(map or(keepGoing y x))
    where 
    y = ((map.map) newF (map truths (map length x)))

keepGoing :: [[Int->Bool]] -> Expr -> [[Bool]]
keepGoing [] y = []
keepGoing x [] = []
keepGoing (z:zs) (x:xs) = [keepGoingDeeper z (x:xs)] ++ keepGoing zs xs
                
keepGoingDeeper :: [Int -> Bool] -> Expr -> [Bool]
keepGoingDeeper [] ex = [False]
keepGoingDeeper (x:xs) ex = [eval x ex] ++ keepGoingDeeper xs ex

truths :: Int -> [[Bool]]
truths 0 = [[]]
truths n = 
    do
    b <- [True,False]
    map (b :) (truths (n - 1))



newF :: [Bool] -> (Int -> Bool)
newF [] _ = False 
newF x p 
    | (length(x)) > p = (!!) x p
    | otherwise = False


