-- Hunter Harris
-- Haskell HW #4
-- 18 October 2021

module Homework4 where

-- Exercise 1: Wholemeal Programming ------------------------------------------

-- Function 1
fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (*) 1 . map (\x -> x - 2) . filter even

-- Function 2
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
  | even n    = n:chain (n `div` 2)
  | otherwise = n:chain (3 * n + 1)

chain' :: Integer -> Integer
chain' n | even n = n `div` 2 | otherwise = 3 * n + 1

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate chain'

-- Exercise 2: Folding with Trees ---------------------------------------------

-- Given a list of elements, construct a BALANCED binary tree.
-- A balanced tree when
--   it has < 2 difference in height between its left and right trees
--   and all of its subtrees are also balanced
-- Remember, height is the length of a path from root to the deepest node
-- ex            Height
--         A       0
--       /   \     
--      B     C    1
--    /   \
--   D     E       2
-- The above tree is balanced bc the depth from A is 2 and 1 respectively,
-- less than a 2 height difference AND the subtrees B and C also meet the
-- criteria for being balanced.
-- Use the following definition for a tree

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
        deriving (Show, Eq)

-- Note: 
-- Leaf represents and empty node (it basically means null)
-- Each Node holds 
-- * an Integer representing its height
-- * an element of type 'a' (type variable)
-- * and 2 more Trees of the same type ('a')







