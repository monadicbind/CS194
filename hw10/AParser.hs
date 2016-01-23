{- CIS 194 HW 10
   due Monday, 1 April
-}
{-# LANGUAGE InstanceSigs #-}
module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

--Exercise 1
instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f parserA = Parser (\str -> case (runParser parserA str) of
                                    Just (a,restOfStr) -> Just (f a , restOfStr) 
                                    otherwise -> Nothing
                          )

--Exercise 2
instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser (\str -> Just (x, str))

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  parser1 <*> parser2 = Parser (\str -> case (runParser parser1 str) of
                                          Just (fa2b , restOfStr) -> case (runParser parser2 restOfStr) of
                                                                        Just (a , restStr) -> Just (fa2b a , restStr)
                                                                        otherwise          -> Nothing
                                          otherwise -> Nothing
                                )

--Exercise 3
abParser :: Parser (Char,Char)
--abParser = Parser (\('a':'b':xs) -> Just (('a','b'),xs))
{--
char a = Parser Char
char b = Parser Char
 (Char -> Char -> (Char,Char)) -> Parser Char -> Parser Char -> Parser (Char,Char)
 f :: (Char -> Char -> (Char,Char))
 a :: Parser Char
 b :: Parser Char
 c :: Parser (Char,Char)
 (Applicative a , Applicative b) => c = f <$> a <*> b , provided 
--}
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ =  (\_ -> ()) <$> abParser
--Is there a function that returns a Unit in haskell ?? I want to replace \_ -> () with that function

intPair :: Parser [Integer]
{--
The idea here is to write the function (Integer -> Char -> Integer -> [Integer])
(Integer -> Char -> Integer -> [Integer]) -> Parser Integer 
                                          -> Parser Char 
                                          -> Parser Integer 
                                          -> Parser [Integer]
--}

--Please think about a nicer function , Maybe ??
intPairHelper :: Integer -> Char -> Integer -> [Integer]
intPairHelper x ' ' y = x : y : []
--Do we really need this function ??
-- I am sure that if the Parser combination runs , the Char value will be ' ' else you 
-- get back a Nothing.

intPair = (\x y z -> [x,z]) <$> posInt <*> char ' ' <*> posInt

--Exercise 4
{--
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
--}

instance Alternative Parser where
  empty :: Parser a
  --empty represents a Parser that always fails
  empty = Parser (\str -> Nothing)

  (<|>) :: Parser a -> Parser a -> Parser a
  {--
    p1 <|> p2 represents the parser which first tries running p1. 
    If p1 succeeds then p2 is ignored and the result of p1 is returned.
    Otherwise, if p1 fails, then p2 is tried instead.
  --}
  {--
  parser1 <|> parser2 = Parser (\str -> case (runParser parser1 str) of
                                          Nothing -> case (runParser parser2 str) of
                                                        Nothing -> empty
                                                        justVal -> justVal
                                          justVal -> justVal

                                )
  --}
  --Taking into consideration that Maybe is an instance of Alternative 
  --The hint provided by the Assignment
  parser1 <|> parser2 = Parser (\str -> ((runParser parser1 str) <|> (runParser parser2 str)))
  --This could probably be written much more simply and efficiently without going
  -- into deeper pattern matching ?? 

  --Exercise 5
intOrUpperCase :: Parser ()
intOrUpperCase = ((\_ -> ()) <$> posInt) <|> ((\_ -> ()) <$> (satisfy isUpper))


