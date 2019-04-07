{-

Jacob Collins

101693075

-}



--No other imports are allowed

import Data.List



--2.1

collatz :: [Int] ->  Int
collatz [] = 0
collatz x = clength(map compute x)
   


--2.2

haskellFileNames :: [String] -> [String]

haskellFileNames [] = []
haskellFileNames (x:xs)
    | isSuffixOf ".hs" x = x: haskellFileNames xs
    | isSuffixOf ".lhs" x = x: haskellFileNames xs
    | otherwise = haskellFileNames xs  
    
--2.3

select :: (t -> Bool) -> [t] -> [a] -> [a]

select _ [] _ = []
select _ _ [] = []
select p (x:xs) (y:ys) = if p x then y: select p xs ys else select p xs ys

--2.4
prefixSum :: [Int] -> [Int]
prefixSum [] = []
prefixSum (x:xs) =  prefixSum (init(x:xs))  ++ [sum  $ reverse(x:xs)]

--2.5

numbers :: [Int] -> Int
numbers [] = 0
numbers x = foldl (\x y -> 10*x+y) 0 x



--2.6

type Numeral = (Int, [Int])



example = (10, [1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0])



--2.6 1

makeLongInt :: Integer -> Int -> Numeral
makeLongInt x 0 = (0, [])
makeLongInt x y = (y,doMath x y)
    where doMath x y 
            | fromInteger(x) `div` y == 0 = fromInteger(x) `mod` y :[]
            | otherwise =   doMath z y ++ [(fromInteger(x) `mod` y)]
             where z = toInteger(fromInteger(x) `div` y)

--2.6 2

evaluateLongInt :: Numeral -> Integer
evaluateLongInt (0, _) = 0
evaluateLongInt (_, []) = 0
evaluateLongInt (x , (y:ys)) = toInteger(foldl (\x y -> 10*x+y) 0 (doMath q $ x))
    where
     q = foldl (\x y -> 10*x+y) 0 (y:ys)
     doMath x y 
        | x `div` y == 0 = x `mod` y :[]
        | otherwise =   doMath z y ++ [x `mod` y]
        where z = x `div` y

 
--2.6 3

changeRadixLongInt ::  Numeral -> Int -> Numeral
changeRadixLongInt (n, []) radix = (radix, [])
changeRadixLongInt (n,l) radix = (radix, unfoldNumeral convertNumber (n,l))
  where  convertNumber(_,[]) = (Nothing)
         convertNumber x = Just (changeBaseMath x radix)
         unfoldNumeral n = reverse . unfoldr (fmap switch . n)
            where switch (x,y) =   (y,x)

--2.6 4

addLongInts :: Numeral -> Numeral -> Numeral
addLongInts (x1, y1) (x2, y2)

    | length y1 > length y2 && x1 > x2 = doMath (x1, y1)(x2, checkLength (length(y1)) (changeBase y2 x2 x1))
    | length y1 > length y2 && x2 > x1 = doMath (x1, y1)(x2, checkLength (length(y1))  (changeBase y1 x1 x2))
    | length y2 > length y1  &&  x1 > x2 = doMath (x1, checkLength (length(y2)) (changeBase y2 x2 x1)) (x2, y2)
    | length y2 > length y1 && x2 > x1 = doMath (x1, checkLength (length(y2)) (changeBase y1 x1 x2)) (x2, y2)
    | otherwise = doMath (x1, y1) (x2, y2)
    where 
        doMath (x1, y1) (x2, y2)
            | x1 == x2 = (x2, addDigits 0 x2 y1 y2)
            | x1 > x2 = (x1, addDigits 0 x1 y1 y2) 
            | x2 > x1 = (x2, addDigits 0 x2 y2  y1)
        
        
 --2.6 5
   
mulLongInts :: Numeral -> Numeral -> Numeral

mulLongInts (x1, y1) (x2, y2)

    | length y1 > length y2 && x1 > x2 = doMath (x1, y1)(x2, checkLength (length(y1)) (changeBase y2 x2 x1))
    | length y1 > length y2 && x2 > x1 =  doMath (x1, y1)( x2, checkLength (length(y1)) (changeBase y1 x1 x2)) 
    | length y2 > length y1  &&  x1 > x2 = doMath (x1, checkLength (length(y2)) (changeBase y2 x2 x1)) (x2, y2)
    | length y2 > length y1 && x2 > x1 = doMath (x1, checkLength (length(y2)) (changeBase y1 x1 x2)) (x2, y2)
    | otherwise = doMath (x1, y1) (x2, y2)
    where 
        doMath (x1, y1) (x2, y2) = (x1,y1)
            -- | x1 == x2 = (x2, multiplyDigits 0 x2 y1 y2)
            -- | x1 > x2 = (x1, multiplyDigits 0 x1 y1 y2) 
            -- | x2 > x1 = (x2, multiplyDigits 0 x2 y2  y1)
        








-- helper functions
compute :: Int -> [Int]
compute 1 =  1: []
compute n 
    | n `mod` 2 == 0 = n: compute (n `div` 2)
    | n `mod` 2 == 1 = n : compute (3*n + 1)


clength :: [[Int]] -> Int
clength [] = 0
clength xss =head $ snd $ maximum $ [(length xs, xs) | xs <- xss]


changeBaseMath ::  Numeral -> Int -> (Numeral,Int)
changeBaseMath (r,(x:xs)) y  =   
    let  div carry d = switch ((carry*r+d) `divMod` y)
         (carry, keep) = mapAccumR div 0 (reverse (x:xs))
    in   (removeZeros(r,reverse keep), carry)
        where 
            removeZeros (r,d) = (r, dropWhile (==0) d)
            switch (x,y) = (y,x)

changeBase :: [Int] -> Int -> Int -> [Int]
changeBase n r1 r2 = snd $ changeRadixLongInt (r1,n) r2



addDigits :: Int -> Int -> [Int] -> [Int] -> [Int]
addDigits _ _ [] a = a
addDigits _ _ a [] = a 
addDigits c r xs ys = addDigits (rest `div` r) r (init(xs)) (init(ys)) ++ [(rest `mod` r)]
    where 
    rest = last(xs) + last(ys) + c

checkLength :: Int -> [Int] -> [Int]
checkLength size list 
        | length(list) < size  =  checkLength size ([0] ++ list)
        | otherwise = list

multiplyDigits ::  [Int] -> [Int] -> [Int]
multiplyDigits = undefined
    
    where
    doMath x y
        | x >= 1        = y + doMath (x-1) y
        | otherwise     = 0