{-# LANGUAGE FlexibleInstances, TypeSynonymInstances,FlexibleInstances , InstanceSigs #-}
module JoinListBuffer where

import Data.Monoid

import Buffer
import JoinList
import Sized
import Scrabble

instance Buffer (JoinList (Score,Size) String) where
 toString :: JoinList (Score,Size)	String -> String
 toString Empty = ""
 toString (Single m str) = str
 toString (Append m jla jlb) = (toString jla) ++ (toString jlb)

 fromString :: String -> (JoinList (Score,Size) String)
 fromString str = foldr (\line acc -> (Single (scoreString line , Size 1) line ) +++ acc ) Empty (lines str)

 -- | Extract the nth line (0-indexed) from a buffer.  Return Nothing
 -- for out-of-bounds indices.
 line :: Int -> (JoinList (Score,Size) String) -> Maybe String
 line index jl = indexJ index jl
 
 -- | @replaceLine n ln buf@ returns a modified version of @buf@,
 --   with the @n@th line replaced by @ln@.  If the index is
 --   out-of-bounds, the buffer should be returned unmodified.
 replaceLine :: Int -> String -> (JoinList (Score,Size) String) -> (JoinList (Score,Size) String)
 replaceLine n modStr jl = if (n < 0 || n > (numLines jl))
 	                        then jl
 	                        else (takeJ (n) jl) +++ (fromString modStr) +++ (dropJ (n+1) jl)
 -- | Compute the number of lines in the buffer.
 numLines :: (JoinList (Score,Size) String) -> Int
 numLines jl = getSize (size (snd (tag jl)))

 -- | Compute the value of the buffer, i.e. the amount someone would
 --   be paid for publishing the contents of the buffer.
 value :: (JoinList (Score,Size) String) -> Int
 value jl = getScore (fst (tag jl))	