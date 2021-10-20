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
-- Remember - height is the length of a path to the deepest node
-- ex    Value           Heights
--         A                2
--       /   \            /   \ 
--      B     C          1     0
--    /   \            /   \
--   D     E          0     0
-- The above tree is balanced bc the difference in distance to the deepest 
-- node in each sub tree is less than one. This is true for the sub tree B
-- as well. Remember - a leaf will always have height of 0 since it is the
-- deepest node within its own context.
--
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

-- write a function that generates a balanced binary tree from a list of
-- values using foldr

-- get height the height value of a tree
treeHeight :: Tree a -> Integer
treeHeight Leaf = -1
treeHeight (Node h _ _ _) = h

-- insert a node into the tree and keep tree balanced
addNode :: a -> Tree a -> Tree a
addNode x Leaf = Node 0 Leaf x Leaf
addNode x (Node h l v r)
  | treeHeight l > treeHeight r = Node h l v (addNode x r )
  | treeHeight l < treeHeight r = Node h leftTree v r
  | otherwise                   = Node newHeight leftTree v r
  where
    leftTree = (addNode x l)
    newHeight = treeHeight leftTree + 1

-- fold over a list of elements to create a balanced binary tree
foldTree :: [a] -> Tree a
foldTree = foldr addNode Leaf 

-- How it works
-- treeHeight gets the height of a tree using the height value. We want
-- to prioritize selecting TRUE leaf nodes (non populated ones) so we set
-- the value for a Leaf to -1 which will register as less than a Node with
-- no child nodes (that would be 0).
-- addNode adds a new node to the tree by the following logic:
-- It compares the height of the left and right sub trees to know which
-- way to traverse. If the sub trees are even, we need to calculate the new
-- height of the subtree we decide to add to, as the height could change for
-- our current node.
-- foldTree wraps this all together to create a pipeline for folding this
-- funciton over a list of elements. addNode's first argument (the element)
-- will be supplied by the next element of the array and the second argument
-- will be supplied by the result of the previous iteration (the balanced tree)



