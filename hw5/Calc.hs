{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Calc where

import ExprT
import Parser
import StackVM
import qualified Data.Map as M

eval :: ExprT -> Integer
eval expr = case expr of
 ExprT.Lit intVal -> intVal
 ExprT.Add expr1 expr2 -> (eval expr1) + (eval expr2)
 ExprT.Mul expr1 expr2 -> (eval expr1) * (eval expr2)

evalStr :: String -> Maybe Integer
evalStr inpStr = case (parseExp ExprT.Lit ExprT.Add ExprT.Mul inpStr) of
 Just expVal -> Just (eval expVal)
 otherwise -> Nothing

class Expr a where
 lit :: Integer -> a
 add :: a -> a -> a
 mul :: a -> a -> a

instance Expr ExprT where
 lit x = ExprT.Lit x
 add x y = ExprT.Add x y
 mul x y = ExprT.Mul x y

reify :: ExprT -> ExprT
reify = id 

instance Expr Integer where
 lit x = x
 add x y = x+y
 mul x y = x*y

instance Expr Bool where
 lit x = if (x <= 0) then False else True
 add x y = x || y
 mul x y = x && y

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
 lit x = MinMax x
 add (MinMax x) (MinMax y) = MinMax (max x y)
 mul (MinMax x) (MinMax y) = MinMax (min x y)

instance Expr Mod7 where
 lit x =  Mod7 (mod x 7)
 add (Mod7 x) (Mod7 y) = lit (x+y)
 mul (Mod7 x) (Mod7 y) = lit (x*y)

instance Expr Program where
 lit x =  [PushI x]
 add x y = x ++ y ++ [StackVM.Add]
 mul x y = x ++ y ++ [StackVM.Mul]

class HasVars a where
 var :: String -> a

data VarExprT = Var String 
 		   | Lit Integer
           | Add VarExprT VarExprT
           | Mul VarExprT VarExprT
 deriving (Show, Eq)

instance Expr VarExprT where
 lit x = Calc.Lit x
 add x y = Calc.Add x y
 mul x y = Calc.Mul x y

instance HasVars VarExprT where
 var str = Calc.Var str            

{-
The below solution for Ex:6 is very interesting and not that intuitive
to start with , but think this way , instance of Expr should return a
function that takes in a Map and spits out a Maybe. Where you get the Map
is consired out of context for the instance that we are trying to build.
-}
instance HasVars (M.Map String Integer -> Maybe Integer) where
 var str =  \m -> M.lookup str m

instance Expr  (M.Map String Integer -> Maybe Integer) where
 lit x = \_ -> Just x
 add x y = \m -> case (x m , y m) of
 					(Just a , Just b) -> Just (a + b)
 					_ -> Nothing
 mul x y = \m -> case (x m , y m) of
 					(Just a , Just b) -> Just (a * b)
 					_ -> Nothing 						

withVars :: [(String, Integer)]
          -> (M.Map String Integer -> Maybe Integer)
          -> Maybe Integer
withVars vs exp = exp $ M.fromList vs          
