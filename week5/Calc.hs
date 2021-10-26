-- Hunter Harris
-- Haskell Homework 5: Calc
-- 23 October 2021

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Calc where

import ExprT
import Parser
import StackVM
import Data.Maybe

-- Exercise 1: Version 1 ------------------------------------------------------

eval :: ExprT -> Integer
eval (ExprT.Lit n) = n
eval (ExprT.Add l r) = (eval l) + (eval r)
eval (ExprT.Mul l r) = (eval l) * (eval r)

-- Exercise 2: evalSring ------------------------------------------------------

evalStr :: String -> Maybe Integer
evalStr expression =
  case (parseExp ExprT.Lit ExprT.Add ExprT.Mul expression) of
       Nothing -> Nothing
       Just n  -> Just (eval n)

-- Exercise 3: Expr -----------------------------------------------------------

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul

-- used to constrain a Expr to a ExprT
reify :: ExprT -> ExprT
reify = id

-- Exercise 4: Custom Calculators ---------------------------------------------

-- Integer:
-- Works like the original calculator
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

-- Bool:
-- False is <= 0, True is > 0
-- Add is OR, mul is AND
instance Expr Bool where
  lit n = n > 0
  add = (||)
  mul = (&&)

-- MinMax:
-- Add is MAX, mul is MIN
newtype MinMax = MinMax Integer deriving (Eq, Ord, Show)
instance Expr MinMax where
  lit n = MinMax n
  add l r = max l r
  mul l r = min l r

-- Mod7:
-- All values should be in range 0..7
-- NOTE: In the previous problem we only needed to compare values using Ord 
-- methods, which can be derived. However, in this one we are doing actual 
-- arithmatic so we must create an instance of Num for our type Mod7, since
-- Num funcitons cannot be implicitly derived. As a bonus for the extra 
-- effort we can use the fromInteger function directly in our Expr instance.
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Num Mod7 where
  (+) (Mod7 l) (Mod7 r) = Mod7 ((l + r) `mod` 7)
  (*) (Mod7 l) (Mod7 r) = Mod7 ((l * r) `mod` 7)
  (-) (Mod7 l) (Mod7 r) = Mod7 ((l - r) `mod` 7)
  abs (Mod7 n) = (Mod7 (abs n))
  signum (Mod7 n)
    | n < 0     = (-1)
    | n > 0     = 1
    | otherwise = 0
  fromInteger n = Mod7 (n `mod` 7)

instance Expr Mod7 where
  lit n = fromInteger n
  add l r = l + r
  mul l r = l + r

-- All that instance defining stuff is fun but if we wanted to be quick we
-- could just put the `mod` functionality into the instance for Expr
newtype Mod7' = Mod7' Integer deriving (Eq, Show)

instance Expr Mod7' where
  lit n = Mod7' (n `mod` 7)
  add (Mod7' l) (Mod7' r) = Mod7' ((l + r) `mod` 7)
  mul (Mod7' l) (Mod7' r) = Mod7' ((l * r) `mod` 7)

-- Exercise 5: StackExp -------------------------------------------------------

-- Create an instance of Expr for Program
instance Expr StackVM.Program where
  lit n = [StackVM.PushI n]
  add l r = l ++ r ++ [StackVM.Add]
  mul l r = l ++ r ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

compileToStack :: String -> Either String StackVal
compileToStack = stackVM . fromMaybe [] . compile

