module Golf where

import Data.List
{-
 skips :: [a] -> [[a]]
 The output of skips is a list of lists. The first list in the output should be the same as the input list.
 The second list in the output should contain every second element from the input list. . . 
 and the nth list in the output should contain every nth element from the input list.
-}
skips :: [a] -> [[a]]	
skips lst = map  everyNth  [(y,lst) | y <- [1..(length lst)]]

everyNth (n,elems) = reverse $ everyNthHelp n elems []
 where
  everyNthHelp _ [] ans =  ans
  everyNthHelp 1 (x:xs) ans =  everyNthHelp n xs (x:ans)
  everyNthHelp k (x:xs) ans =  everyNthHelp (k-1) xs ans

--Can we do something with inits and subsequences functions in Data.List ?  

--sk lst = foldr (\x,acc -> x !! (length x - 1)) [] (replicate (length lst) lst)

localMaxima :: [Integer] -> [Integer]
localMaxima xs = foldl help [] xs

help [] y = [y]
help (x:xs) y | (x < y) = (y:xs)
help l@(x:xs) y | (x > y) = l

lMaxima xs = lMaximaHelp xs []
 where
  lMaximaHelp (x:y:z:xs) ans | (x < y) && (y > z) = lMaximaHelp (z:xs) (y:ans)
  lMaximaHelp (x:y:z:xs) ans = lMaximaHelp (z:xs) ans
  lMaximaHelp _ ans = ans

lMax :: [Integer] -> [Integer]
lMax (x:y:z:xs) | (x < y ) && (y > z) = y : lMax (z:xs)
lMax (x:y:z:xs) = lMax (z:xs)
lMax _ = []

--lma (x:y:xs) = foldl (\acc,z -> if ((x < y ) && (y > z)) then y:acc else acc ) [] xs

lma1 xs = [ y | x <- xs , y <- tail xs , z <- tail $ tail xs , (x < y) && (y > z) ]

t xs = map (\l@(l1,l2,l3) -> l2) (filter (\p@(p1 , p2 , p3) -> (p1 < p2) && (p2 > p3))  (zip3 xs (tail xs) (tail $ tail $ xs)))
 