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

localMaxima :: [Integer] -> [Integer]
localMaxima xs = foldl help [] xs

help [] y = [y]
help (x:xs) y | (x < y) = (y:xs)
help l@(x:xs) y | (x > y) = l

lMaxima xs = lMaximaHelp xs []
 where
  lMaximaHelp (x:y:z:xs) ans | (x < y) && (y > z) = lMaximaHelp (z:xs) (y:ans)
  lMaximaHelp _ ans = ans
 