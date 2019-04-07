--Jacob Collins
-- 2/9/2018
--Homework 1

--Checks for even numbers
test :: Int -> Int -> Bool
test x y = if x `mod` 2 == 1 && y `mod` 2 == 1 then True else False

--Duplicates chars
stutter :: [Char] -> [Char]
stutter [] = []
stutter (x:xs) = x: x: stutter(xs)

--Compresses duplicate chars
compress :: [Char] -> [Char]
compress[] = []
compress (x:xs) = if x == head(xs) then x: compress(drop 1(xs)) else compress(xs)

--adds corresponding elements of a list
zipSum :: [Int] -> [Int] -> [Int]
zipSum[] _ = []
zipSum _ [] = []
zipSum(x:xs) (y:ys) = (x + y): zipSum xs ys

--Returns union of two Int lists
setUnion :: [Integer] -> [Integer] -> [Integer]
setUnion[] [] = [] 
setUnion(x:xs) (y:ys) 
    |x < y  = x : y: setUnion xs ys
    |y < x = y: x: setUnion xs ys
    |x == y = x: y: setUnion xs ys

--Returns intersection of two int lists
setIntersection :: [Integer] -> [Integer] -> [Integer]
setIntersection [] _ = []
setIntersection _ [] = []
setIntersection(x:xs) ys
    | x `elem` ys = x: setIntersection xs ys
    |otherwise = setIntersection xs ys
    
--Returns set difference of two int lists
setDifference :: [Integer] -> [Integer] -> [Integer]
setDifference [] _ = []
setDifference _ [] = []
setDifference (x:xs) (y:ys) 
       | (not)(x `elem` (y:ys)) = x:  setDifference xs (y:ys)
       | otherwise =  setDifference xs (ys)
   

--Checks if two int lists are equal
setEqual :: [Integer] -> [Integer] -> Bool
setEqual(x:xs) (y:ys)
    | x == y = setEqual xs ys
    |otherwise = False
setEqual [] [] = True
setEqual [] _ = False
setEqual _ [] = False


--provides digital root of a number
dr :: Integer -> Int
dr x = if x > 10 then fromIntegral(dr (x `div` 10 + x `mod` 10)) else fromIntegral(x)