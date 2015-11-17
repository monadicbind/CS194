--Fib.hs
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

foldl' f z [] = z
foldl' f z (x:xs) = let z' = z `f` x
                    in seq z' $ foldl' f z' xs

fibs1 :: [Integer]
fibs1 = map fib [0..]

--Have to work on another implementation of Fibonacci in haskell.
fibs2 :: [Integer]
fibs2 = foldr (\x acc -> (sum $ take 2 acc) : acc) [1,0] [2..20]

fibs3 = 0 : 1 : zipWith (+) fibs3 (tail fibs3)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x y) = x : streamToList y

instance Show a => Show (Stream a) where
 show k = show (take 30 $ streamToList k)

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x) 

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x y) = Cons (f $ x) (streamMap f y)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamMap f $ streamFromSeed f x)

nats :: Stream Integer
nats = streamFromSeed (\x -> x + 1) 0


intersperseStreams :: Stream Integer -> Stream Integer -> Stream Integer
intersperseStreams n@(Cons x y) k =  Cons x (intersperseStreams k y)

ruler :: Stream Integer
ruler = foldr intersperseStreams (streamRepeat 0) (map streamRepeat [0..])

--0 1 0 2 0 1 0 3 0 1   0  2  0  1  0  4 0  1  0  2  0   1  0	 3  0  1  0  2  0  1  0  5  0  1  0  2  0  1  0  3  0  1  0  2  0  1  0  4
--1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48
--121 3 121 4 121 3 121 5 121 3 121 4 121 3 121 
-- 0:0+1:0:1+1:0:0+1:0: 1+1+1:0:0+1:0:0+1+1:0:0+1:0:1+1+1+1:0


--te = Cons 0 (Cons 1 )

te (Cons x1 (Cons x2 y)) k@(Cons a1 (Cons a2 b)) = let az = Cons x1 (Cons (x1+a1) (Cons x2 (Cons (x1+a1+a2) (intersperseStreams az b))))
												   in intersperseStreams az b