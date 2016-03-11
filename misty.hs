--misty.hs
--Intermediate Exercises in Haskell
-- URL http://blog.tmorris.net/posts/20-intermediate-haskell-exercises/
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

class Fluffy f where
 furry :: (a -> b) -> f a -> f b


class Misty m where
 banana :: (a -> m b) -> m a -> m b
 unicorn :: a -> m a

 --Exercise 6
 -- ( use banana and / or unicorn )
 furry' :: (a -> b) -> m a -> m b
 furry' func = banana $ unicorn . func

instance Misty [] where
 unicorn = (: [])
 banana = concatMap 

instance Misty ((->)t) where
 unicorn x = (\_ -> x) 
 banana f1 f2 = \x -> f1 (f2 $ x) x

apple :: (Misty m) => m a -> m (a -> b) -> m b
apple mx = banana . (flip furry') $ mx

moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy [] _ = unicorn []
moppy (x:xs) f = (flip banana) (f x) (\b -> moppy xs f)

sausage :: (Misty m) => [m a] -> m [a]
sausage = (flip moppy) $ id

--Can we redo the banana2 , banana3 and banana4 without parameters ?
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2= (flip apple .) . furry'

--banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 f x1 x2 x3 = apple x3 (banana2 f x1 x2)

banana4 f x1 x2 x3 x4 = apple x4 (banana3 f x1 x2 x3)

newtype State s a = State { state :: (s -> (s,a))}

instance Fluffy (State s) where
	--furry :: (a -> b) -> (State s a) -> (State s b)
	furry f (State st) = State (\s1 -> (s1 , f (snd (st s1))))
	--furry f st = State ( \s1 -> (s1 , f ( snd ((state st) s1))))

instance Misty (State s) where
	unicorn = \a1 -> State (\s1 -> (s1 , a1))
	banana :: (a -> State s b) -> State s a -> State s b
	banana f (State sta) = State (\s1 -> (s1 , snd ((state (f (snd (sta s1)))) (fst (sta s1)))))

