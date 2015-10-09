--file cs194-hw1.hs
toDigits :: Integer -> [Integer]
toDigits num = toDigitsHelp num []
			    where
			    	toDigitsHelp intNum acc
			    		| intNum <= 0 			  = acc
			    		| mod intNum 10 == intNum = intNum : acc
			    		| otherwise 			  = toDigitsHelp (div intNum 10) ((mod intNum 10) : acc)

toDigitsRev :: Integer -> [Integer]
toDigitsRev  = reverse toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = dblHelp (reverse xs) []
					  where
					  	dblHelp [] ans = ans
					  	dblHelp (y : []) ans = dblHelp [] (y : ans)
					  	dblHelp (y : z : ys) ans = dblHelp ys ((z*2) : y : ans)


sumDigits :: [Integer] -> Integer
sumDigits xs = 	sum [ sum y | y <- [toDigits x | x <- xs]]	
{-
	The above function can be expressed as 
	"sum (concatMap toDigits xs)"
	using the higher order functions of Haskell
-}

validate :: Integer -> Bool
validate cardNum = (mod (sumDigits (doubleEveryOther (toDigits cardNum))) 10) == 0

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

hanoi 0 pegOne pegTwo pegThree = []
hanoi 1 pegOne pegTwo pegThree = [(pegOne,pegTwo)]
hanoi n pegOne pegTwo pegThree = (hanoi (n-1) pegOne pegThree pegTwo) 
								 ++ (hanoi 1 pegOne pegTwo pegThree)
								 ++ (hanoi (n-1) pegThree pegTwo pegOne)


