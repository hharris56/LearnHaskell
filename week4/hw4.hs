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

-- Exercise 3: More Folds! ----------------------------------------------------

-- 1. Implement xor
-- returns True if and only iff there are an odd number of True values
-- contained in the input list. Number of false values does not matter, the 
-- solution must implement foldr

-- for fun, without foldr
xor' :: [Bool] -> Bool
xor' = even . length . filter (==True)

-- using foldr
xor :: [Bool] -> Bool
xor = even . foldr (\True n -> 1 + n) (0 :: Integer)

-- Note: we have to include the (0 :: Integer) here to tell the compiler
-- we expect and Integer type from our foldr call, otherwise we will get a
-- warning that calling 'even' constrains the type to an integral (which we
-- already know to be true)

-- 2. Implement map
-- has the same behavior as map fucntion, but implemented using foldr
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> (f x) : xs) []

{- 3. Implement foldl (Optional)
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f base = reverse . (foldr f base) . reverse
-}

-- Exercise 4: Finding Primes -------------------------------------------------

-- For a given number n, generate all odd prime numbers up to 2n + 2 using
-- the sieveSundaram formula. You may also use the definition for a cartesian
-- product below
-- 
-- Algorith:
-- find and mark all numbers with following form
-- i + j + 2ij <= n
-- where
-- i, j are Natural Numbers
--     and
-- 1 <= i <= j
-- For any remaining numbers apply 2n + 1
-- That will be the list of odd primes

-- compute the cartesian product (get every possible combination)
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

-- formula for converting to a marked value
mValue :: (Integer, Integer) -> Integer
mValue (i, j)= i + j + (2*i*j)

-- formula for converting to a prime number
pValue :: Integer -> Integer
pValue n = 2*n + 1

-- for a given combination of i and j, do they qualify as a marked number 
isMarked :: Integer -> (Integer, Integer) -> Bool
isMarked n (i, j) = (i + j + (2*i*j)) <= n

-- generate all combos for the sieve
generateCombos :: Integer -> [(Integer, Integer)]
generateCombos n = filter (\(i,j) -> j >= i) .
                   filter (isMarked n) $ cartProd [1..n] [1..n]

-- generate list of marked values up to n
markedValues :: Integer -> [Integer]
markedValues = map mValue . generateCombos 

-- generate all odd prime numbers up to 2n + 2
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map pValue $ filter (`notElem` markedValues n) [1..n]

-- How it works
-- To solve the problem we developed a series of steps, then created a
-- pipeline to execute the steps in succession. The steps are as follows
-- 1. generate solution list
--            -> [1..n] <-
-- 2. generate list of marked numbers 1..n
--            markedValues
-- 3. filter marked values out of the solution list
--            filter (`notElem markedValues n) [1..n]
-- 4. map 2n + 1 onto the solution list
--            map pValue $ ...


