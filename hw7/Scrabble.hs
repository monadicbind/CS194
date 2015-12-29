{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, InstanceSigs,FlexibleContexts #-}
module Scrabble where
 
import Data.Monoid
import Data.Char
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



