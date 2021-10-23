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
  lit :: a -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit (Lit n) = Lit n
  add (Add l r) = (Add l r)
  mul (Mul l r) = (Mul l r)
