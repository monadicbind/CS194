{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, InstanceSigs,FlexibleContexts #-}
module Scrabble where
 
import Data.Monoid
import Data.Char
import JoinList
import Buffer
import Sized

newtype Score =  Score Int
  deriving (Eq, Ord, Show,Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Monoid Score where
	mempty = Score 0
	mappend = (+)

score :: Char -> Score
score x | (elem (toUpper x) "ANEILORSTU") = Score 1 
score x | (elem (toUpper x) "BCMP") = Score 3
score x | (elem (toUpper x) "DG")  = Score 2
score x | (elem (toUpper x) "FHVWY") = Score 4
score x | (elem (toUpper x) "K")  = Score 5
score x | (elem (toUpper x) "JX")  = Score 8
score x | (elem (toUpper x) "QZ")  = Score 10
score _ = Score 0

scoreString :: String -> Score
scoreString xs = foldl (\acc a -> acc <> (score a)) mempty xs

scoreLine :: String -> JoinList Score String
scoreLine xs = Single (scoreString xs) xs

instance Buffer (JoinList (Score,Size) String) where
 toString :: JoinList (Score,Size)	String -> String
 toString Empty = ""
 toString (Single m str) = str
 toString (Append m jla jlb) = (toString jla) ++ (toString jlb)

 fromString :: String -> (JoinList (Score,Size) String)
 fromString str = foldr (\line acc -> (Single (scoreString line , Size 1) line )+++ acc ) Empty (lines str)

 -- | Extract the nth line (0-indexed) from a buffer.  Return Nothing
 -- for out-of-bounds indices.
 line :: Int -> (JoinList (Score,Size) String) -> Maybe String
 line index jl = indexJ index jl

 

