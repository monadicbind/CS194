--Intermediate Exercises in Haskell
-- URL http://blog.tmorris.net/posts/20-intermediate-haskell-exercises/
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

class Fluffy f where
 furry :: (a -> b) -> f a -> f b

--Exercise 1
instance Fluffy [] where
 furry =  map

--Exercise 2
instance Fluffy Maybe where
 furry _ Nothing = Nothing
 furry func (Just a) = Just (func a)

--Exercise 3
instance Fluffy ((->) t)  where
 furry func funcThatTakesAt = func . funcThatTakesAt

{-Example Unit Test
  Havent read IO in CS 194 so will execute Unit Tests Manually for now.

  let test = furry (\x -> take 5 (repeat x)) (\y -> "sathish")
  here f -> a :: (t -> [Char]) == :t (\y -> "sathish")
       a -> b :: ([Char] -> [[Char]]) = :t (\x -> take 5 (repeat x)) 
       f -> b :: (t -> [[Char]]) = :t test
-} 

newtype EitherLeft b a = EitherLeft (Either a b) deriving (Show)
newtype EitherRight a b = EitherRight (Either a b) deriving (Show)

{-
Understanding Either
data Either a b = Left a | Right b
-}

instance Fluffy (EitherLeft t) where
 --furry :: (a -> b) -> EitherLeft t a -> EitherLeft t b
 furry func (EitherLeft (Left a)) = EitherLeft (Left (func a))
 furry _ (EitherLeft (Right t)) = EitherLeft (Right t)

instance Fluffy (EitherRight t) where
--furry :: (a -> b) -> EitherRight t a -> EitherRight t b	
 furry func (EitherRight (Right a)) = EitherRight (Right (func a))
 furry func (EitherRight (Left t)) = EitherRight (Left t)

class Misty m where
 banana :: (a -> m b) -> m a -> m b
 unicorn :: a -> m a

 --Exercise 6
 -- ( use banana and / or unicorn )
 furry' :: (a -> b) -> m a -> m b
 furry' func = banana (\a -> unicorn (func a))

instance Misty [] where
 unicorn = (: [])
 banana = concatMap

instance Misty Maybe where
 --unicorn :: a -> Maybe a	
 unicorn f1 = Just f1
 --unicorn f1 (Nothing) = Nothing
 banana _ Nothing = Nothing
 banana f (Just x) = f x

instance Misty ((->) t) where
 --unicorn :: a -> m -> a	
 unicorn f1 f2 = f1
 -- banana :: (a -> m -> b) -> (m -> a) -> (m -> b)
 --banana func funcThatTakesmAndGivesa = (\m -> (func (funcThatTakesmAndGivesa m) m))
 banana f1 f2 f3 = f1 (f2 $ f3) f3
 --banana1 :: (a -> m -> b) -> (m -> a) -> m ->b
 --banana1 f1 f2 f3 = f1 $ f2 . f3 $ f3

instance Misty (EitherLeft t) where
 --unicorn :: a -> EitherLeft t a	
 unicorn a = EitherLeft (Left a)
 --banana :: (a -> EitherLeft t b) -> (EitherLeft t a) -> (EitherLeft t b)
 banana func (EitherLeft (Left a)) = func a
 banana func (EitherLeft (Right t)) = EitherLeft (Right t)

jellybean :: Misty m => m (m a) -> m a
--jb :: Misty m => m -> (m -> a) -> (m -> a)
jellybean = banana (\b -> b)