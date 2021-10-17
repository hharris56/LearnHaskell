-- Hunter Harris
-- Learning Haskell Lecture 4
-- 10 October 2021

module Lecture4 where

-- Anonymous Functions --------------------------------------------------------

-- Define an anonymous function using the "\" character.
-- ex: (\x -> x*2) 5 == 10
--
-- Above we have defined a lambda (or anonymous) function that takes one 
-- argument x and returns x * 2. Passing 5 to this function gives us 10 as a
-- result.
--
-- This functions can be useful when we dont want to name a function only used
-- once, ie for a filter or mapping call.
-- ex:

greaterThan100 :: [Integer] -> [Integer]
greaterThan100 xs = filter (\x -> x > 100) xs

-- Lambda functions are not limited to one argument either
-- ex: (\x y -> x + y)
--
-- Above is a lambda function to handle addition between 2 numbers

-- Operator Sections ----------------------------------------------------------

-- In the particular case of greaterThan100 there is an even better way to
-- write it, using an Operator Section. Operator Sections are equivalent to
-- a function with one argument and act as follows.
--
-- Consider ? to be an operator.
-- (?y) == \x -> x ? y
-- (y?) == \x -> y ? x

-- So in our case we can write greaterThan100 as

greaterThan100' :: [Integer] -> [Integer]
greaterThan100' xs = filter (>100) xs

-- Function Composition -------------------------------------------------------

-- Is a funciton of type (b -> c) -> (a -> b) -> (a -> c) possible?
-- 
-- foo f g = ...
--
-- In place of ... we will need to write a function of type a -> x
-- To make a function lets used a lambda expression
--
-- foo f g = \x -> ...
-- 
-- Variable x will be of type a, so now in place of ... we need to write an 
-- expression of type c. We can accomplish this using foo's arguments as so:

foo :: (b -> c) -> (a -> b) -> (a -> c)
foo f g = \x -> f (g x)

-- So what was the point of this exercise? Well the function foo is actually
-- written as "." and represents function composition. If f and g are functions
-- then f . g returns a function that does g first then f.
--
-- As an example look how the following function can be rewritten
myTest :: [Integer] -> Bool
myTest xs = even (length (greaterThan100 xs))

myTest' :: [Integer] -> Bool
myTest' = even . length . greaterThan100

-- myTest' can now be seen as a pipeline that first calls greaterThan100, then
-- gets the length, then calls even on that length. Function composition
-- makes this much clearer from a glance at the code

-- Currying and Partial Application -------------------------------------------










