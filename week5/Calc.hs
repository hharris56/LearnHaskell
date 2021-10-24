-- Hunter Harris
-- Haskell Homework 5: Calc
-- 23 October 2021

{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser

-- Exercise 1: Version 1 ------------------------------------------------------

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add l r) = (eval l) + (eval r)
eval (Mul l r) = (eval l) * (eval r)

-- Exercise 2: evalSring ------------------------------------------------------

evalStr :: String -> Maybe Integer
evalStr expression =
  case (parseExp Lit Add Mul expression) of
       Nothing -> Nothing
       Just n  -> Just (eval n)

-- Exercise 3: Expr -----------------------------------------------------------

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit n = Lit n
  add l r = (Add l r)
  mul l r = (Mul l r)

-- used to constrain a Expr to a ExprT
reify :: ExprT -> ExprT
reify = id

-- Exercise 4: Custom Calculators ---------------------------------------------

-- Integer:
-- Works like the original calculator
instance Expr Integer where
  lit n = n
  add l r = l + r
  mul l r = l * r

-- Bool:
-- False is <= 0, True is > 0
-- Add is OR, mul is AND
instance Expr Bool where
  lit n = n > 0
  add l r = l || r
  mul l r = l && r

-- MinMax:
-- Add is MAX, mul is MIN
newtype MinMax = MinMax Integer deriving (Eq, Ord, Show)
instance Expr MinMax where
  lit n = MinMax n
  add l r = max l r
  mul l r = min l r

-- Mod7:
-- All values should be in range 0..7
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Num Mod7 where
  (+) (Mod7 l) (Mod7 r) = Mod7 ((l + r) `mod` 7)
  (*) (Mod7 l) (Mod7 r) = Mod7 ((l * r) `mod` 7)

instance Expr Mod7 where
  lit n = Mod7 (n `mod` 7)
  add l r = l + r
  mul l r = l + r
