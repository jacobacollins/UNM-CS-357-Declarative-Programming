module Homework4 where


--Jacob Collins
-- 101 693 075 
--jacollins
--No other imports allowed

import qualified Data.List as L



--4.1 Genome Lists (40pts)
insertions :: String -> [String]
insertions xs = concatMap (insertN xs) "AGCT"
        where 
        insertN [] _ = []
        insertN [x] genome = [genome:x:[], x:genome:[]]
        insertN (x:xs) genome = [genome:x:xs] ++ (map (x:) (insertN xs genome))


deletions :: String -> [String]
deletions x = makeArray (length(x)) x
    where
    makeArray x y = if (x - 1) >= 0 then  makeArray (x-1) y ++ [deleteN (x - 1) y] else []
    deleteN _ []     = []
    deleteN i (a:as)
        | i == 0    = as
        | otherwise = a : deleteN (i-1) as

substitutions :: String -> [String]
substitutions xs = concatMap (substituteN xs) "AGCT"
        where 
        substituteN [] _ = []
        substituteN [x] genome = [[genome]]
        substituteN (x:xs) genome = [genome:xs] ++ (map (x:) (substituteN xs genome))

        
transpositions :: String -> [String]
transpositions []  = []
transpositions [x] = [[x]]
transpositions (x:xs:[]) = [xs:x:[]]
transpositions (x:xs:xss) = [xs:x:xss] ++ (map (x:) (transpositions (xs:xss)))



--4.2 Sorting (20pts)

insert :: Ord a => a -> [a] -> [a]
insert y [] = [y]
insert y (x:xs)
    | y <= x = y:x:xs
    | otherwise = x: insert y xs


isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)




fileisort :: String -> String -> IO ()
fileisort f1 f2 = do
    contents <- readFile f1
    writeFile f2 (unlines. isort. lines $ contents)
   



--4.3 Game Trees (40pts)

data Field = B | R | G
                deriving(Eq,Show)

instance Ord Field where
    --RBG Ord definition
    compare a b
        | a == R  && b /= R = LT
        | a == B && b == R = GT
        | a == B && b == G = LT
        | a == G && b /= G = GT
        | otherwise = EQ

 
             

type Board = [Field]



strategyForRed :: Board -> Int
strategyForRed x =  returnListIndex x (bestmove x R) 0



strategyForGreen :: Board -> Int

strategyForGreen x = returnListIndex x (bestmove x G) 0


returnListIndex :: Board ->  Board -> Int -> Int
returnListIndex [] (y:ys) z = 0
returnListIndex (x:xs) [] z = 0
returnListIndex (x:xs) (y:ys) z
        | x == y = returnListIndex xs ys (z + 1)
        | otherwise = z

        
--run this to test every board!!!
--assumes R goes first
-- tested all 2530 boards
--R wins where it can, G blocks where it can, but is still a full list of ALL possible boards
testAllBoards :: [Board]
testAllBoards = playBoards list

--list of EVERY possible board (no duplicates, aren't more green pieces to red, and there aren't any existing wins, and there are blank spaces)
list :: [Board]
list = [x | x <- y,  won x == False, full x == False, checkR x >= checkG x]
     where 
        y = map change [x| x <- mapM (const "RGB") [1..9], head x < head (tail x)]
 

-- function that plays board to end as means of extensive testing
--take about a minute and a half to play from a blank board
playBoard :: Board -> Field ->  Board
playBoard board firstPlayer 
    | board == [] = []
    | full board == True = board
    | won board == True = board
    | otherwise = playBoard(bestmove board firstPlayer) (next(firstPlayer))


--4.4 (Optional) Drawing Game Trees and Strategies (30pts EC)

drawStrategy :: Bool -> String -> IO ()

drawStrategy = undefined






----Helper functions 

--changes string list to valid board
change:: String -> Board
change [] = []
change (x:xs) 
        | x == 'B' = [B] ++ change xs
        | x == 'G' = [G] ++ change xs
        | otherwise  = [R] ++ change xs

--checks amount of R pieces
checkR :: Board -> Int
checkR [] = 0
checkR (x:xs) 
   | x == R = 1 + checkR xs
   | otherwise = checkR xs

--checks amount of G pieces
checkG :: Board -> Int
checkG [] = 0
checkG (x:xs) 
   | x == G = 1 + checkG xs
   | otherwise = checkG xs


--plays list of boards with playBoard
playBoards :: [Board] -> [Board]
playBoards [] = []
playBoards (x:xs)   
    | checkG x < checkR x = [playBoard x G] ++ playBoards xs
    | otherwise = [playBoard x R] ++ playBoards xs


   -- Helper Functions (modified book code...you said we could do this #neverforget)

next :: Field -> Field
next R = G
next G = R
next B = B

empty :: Board
empty = replicate 9 B

full :: Board -> Bool
full = all (/= B)

turn :: Board -> Field
turn g = if rs <= gs then R else G
    where
        rs = length(filter (== R) g)
        gs = length(filter (== G) g)

wins :: Field -> Board -> Bool
wins  p g = any line (rows ++ cols ++ dias)
        where
            line = all (==p)
            rows = chunks 3 g
            cols = L.transpose (chunks 3 g)
            dias = [diag (chunks 3 g), diag (map reverse((chunks 3 g)))]

won :: Board -> Bool
won g = wins G g || wins R g

chunks :: Int -> Board -> [Board]
chunks _ [] = []
chunks n xs =
    let (ys, zs) = splitAt n xs
    in  ys : chunks n zs

diag :: [Board] -> [Field]
diag g = [g !! n !! n | n <- [0..3-1]]

valid :: Board -> Int -> Bool
valid g i = i >=0 && i < 9 && g !! i ==B

move:: Board -> Int -> Field -> [[Board]]
move b i p = if valid b i then [chop 3 (xs ++ [p] ++ ys)] else []
    where
        (xs, B: ys) = splitAt i b

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs: chop n (drop n xs)

moves:: Board -> Field -> [Board]
moves g p 
    | won g = []
    | full g = []
    | otherwise = concat (filter (/= [])((map.map) (concat) [move g i p | i <- [0..(8)]]))

data Tree a = Node a [Tree a] deriving Show

gametree :: Board -> Field -> Tree Board
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

minimax :: Tree Board -> Tree (Board, Field)
minimax (Node g [])
    | wins G g = Node (g,G) []
    | wins R g = Node (g,R) []
    | otherwise = Node (g, B) []
minimax (Node g ts)
    | turn g == R = Node (g, minimum ps) ts'
    | turn g == G = Node (g, maximum ps) ts'
                    where
                        ts' =map minimax ts
                        ps = [p | Node (_,p) _ <- ts']



bestmove :: Board -> Field -> Board

bestmove g p 
        | won g == True = error "game is already won"
        | full g == True = error "Board is full"
        | [g' | Node(g',p') _ <- ts, p' == best] == [] = error "no possible best moves from this point"
        |otherwise = head[g' | Node(g',p') _ <- ts, p' == best]
               where
                  tree = prune 9 (gametree g p)
                  Node (_ , best) ts = minimax tree 

