--JoinList.hs
{-# LANGUAGE GeneralizedNewtypeDeriving
           , ScopedTypeVariables ,
           FlexibleInstances , InstanceSigs
   #-}

module JoinList where

import Data.Monoid

import Sized
import Buffer
import Scrabble

newtype Sum1 a = Sum1 a deriving (Eq, Ord, Num, Show)

instance Num a => Monoid (Sum1 a) where
 mempty = Sum1 0
 mappend = (+)

getSum1 :: Sum1 a -> a
getSum1 (Sum1 a) = a

data JoinList  m a = Empty
                    | Single m a
                    | Append m (JoinList m a) (JoinList m a)
 deriving (Show,Eq)                                 

{-Exercise 1 : We first consider how to write some simple operations
on these JoinLists. Perhaps the most important operation we will
consider is how to append two JoinLists. 

Previously, we said that the point of JoinLists is to represent append operations as data, 
but what about the annotations? 
Write an append function for JoinLists that yields a new JoinList whose monoidal 
annotation is derived from those of the two arguments.

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a

You may find it helpful to implement a helper function

tag :: Monoid m => JoinList m a -> m

which gets the annotation at the root of a JoinList.
-}

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty Empty = Empty
(+++) x y = Append ((tag x) <> (tag y)) x y

--tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m a) = m
tag (Append m x y) = m

{-
 Exercise 2 : The first annotation to try out is one for fast indexing
into a JoinList. The idea is to cache the size (number of data elements)
of each subtree. This can then be used at each step to determine
if the desired index is in the left or the right branch.

We have provided the Sized module that defines the Size type,
which is simply a newtype wrapper around an Int. In order to make
Sizes more accessible, we have also defined the Sized type class
which provides a method for obtaining a Size from a value.
Use the Sized type class to write the following functions.

1. Implement the function
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a

indexJ finds the JoinList element at the specified index. If the
index is out of bounds, the function returns Nothing. By an index
in a JoinList we mean the index in the list that it represents.
-}

--Sathish : I dont think I follow this particular question. 
--What are we indexing with ?

--Look at  - http://apfelmus.nfshost.com/articles/monoid-fingertree.html

{-

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)
(indexJ i jl) == (jlToList jl !!? i)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

The jlToList is the method which when observed , will show
that the Single node is going to be the first one in the list
and hence index 0.

-}

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ n _ | n < 0 = Nothing
indexJ n (Single m a) = if (n == 0) then Just a else Nothing
indexJ n t@(Append m a b ) = if (n < (getSize (size (tag a)))) then indexJ n a else indexJ (n - (getSize (size (tag a)))) b

{-
Implement the function

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a

The dropJ function drops the first n elements from a JoinList.
This is analogous to the standard drop function on lists. 
Formally, dropJ should behave in such a way that

jlToList (dropJ n jl) == drop n (jlToList jl)

-}	
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ 0 t = t
dropJ n (Single m a) | n > 0= Empty
dropJ n (Append m a b) = if (n < (getSize (size m))) 
	                      then if (n >= (getSize (size (tag a)))) 
	                      	    then (dropJ (n - (getSize (size (tag a)))) b)
	                      	    else Append ((tag (dropJ n a)) <> (tag b)) (dropJ n a) b
	                      else Empty	
{-
Testing functions:

let n1 = Single (Size 1) "a"
let n2 = Single (Size 1) "b"
let n3 = Single (Size 1) "c"
let n4 = Single (Size 1) "d"
let n5 = Single (Size 1) "e"
let n1n2 = Append (Size 2) n1 n2
let n3n4 = Append (Size 2) n3 n4
let n3n4n5 = Append (Size 3) n3n4 n5
let n1n2n3n4n5 = Append (Size 5) n1n2 n3n4n5

dropJ 0 n1n2n3n4n5
dropJ 1 n1n2n3n4n5
dropJ 2 n1n2n3n4n5
dropJ 3 n1n2n3n4n5
dropJ 4 n1n2n3n4n5
dropJ 5 n1n2n3n4n5
-}	             

{-
Finally, implement the function
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a

The takeJ function returns the first n elements of a JoinList,
dropping all other elements. Again, this function works similarly
to the standard library take function; that is, it should be the case
that

 jlToList (takeJ n jl) == take n (jlToList jl).

Ensure that your function definitions use the size function from
the Sized type class to make smart decisions about how to descend
into the JoinList tree
-}         

--Can we try to balance these trees ?
--Think about the case where , we index == the size of the sub tree , do we need to recurse ?
-- is it not true that take 1 [1,2,3] == drop 2 [3,2,1] ?? Can we not use this law to write takeJ
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 0 _ = Empty
takeJ n t@(Single m a) | n > 0 = t
takeJ n t@(Append m a b) = if ( n >= (getSize (size m))) 
	                        then t 
	                        else if (n > (getSize (size (tag a))))
	                        	  then (Append ((tag a) <> (tag (takeJ (n - (getSize (size (tag a)))) b))) a (takeJ (n - (getSize (size (tag a)))) b))
	                        	  else 	takeJ n a

scoreString :: String -> Score
scoreString xs = foldl (\acc a -> acc <> (score a)) mempty xs

scoreLine :: String -> JoinList Score String
scoreLine xs = Single (scoreString xs) xs
 




