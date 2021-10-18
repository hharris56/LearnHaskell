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

-- NEWS FLASH !! ALL FUNCTIONS IN HASKELL TAKE A SINGLE ARGUEMNT
-- take this for example
f1 :: Int -> Int -> Int
f1 x y = 2*x + y

-- f takes one argument, and returns a function of type Int -> Int. Meaning
-- f can be rewritten like
f1' :: Int -> (Int -> Int)
f1' x y = 2*x + y

-- So if a funciton is type W -> X -> Y -> Z it can be viewed as
-- W -> (X -> (Y -> Z))... we can always add or remove parantheses from the
-- rightmost 'top-level' arrow in a type. Similarly, function application is
-- also like this but left associative ie "f 3 2" is shorthand for "(f 3) 2"
--
-- In a nutshell multiple arguments are really just syntax sugar for lambda
-- functions. Take a look at the following
-- f x y z = ...
--    is shorthand for
-- f = \x -> (\y -> (\z -> ...))
--
-- \x y z -> ...
--    is shorhand for
-- \x -> (\y -> (\z -> ...))
-- 
-- This idea of treating multi argument functions as single argument
-- functions that return other functions is called 'currying'.
--
-- If we want a true multi argument function you would have to represent
-- them as a typle. ex
f1'' :: (Int, Int) -> Int
f1'' (x,y) = 2*x + y

-- Though we are passing two values (x and y) we treat the tuple as a 
-- single argument though so it could still be seen as such.
--  
-- curry / uncurry are standard library functions that may be of use. In
-- particular uncurry can be used to apply functions to a pair like so
-- uncurry (+) (2,3)

-- Partial Application --------------------------------------------------------

-- The act of applying some of a functions arguments to recieve a function 
-- of the remaining arguments is called "partial application". Due to Haskell's
-- curried nature, partial application is really easy... as long as it is to 
-- the first argument(s). This limitation encourages you to consider the 
-- ordering of arguments when defining a function to make partial application
-- easier in the future. Argument that are often the same value should come
-- first, while ones that have the greatest variation should be last.
-- Note: Infix operators allow partial application to either of their
-- arguments using an operator section (see above section)

-- Wholemeal Programming ------------------------------------------------------

-- Lets try using what we've learned to achieve a wholemeal style of 
-- programming. Consider the following function

foobar :: [Integer] -> Integer
foobar []     = 0
foobar (x:xs)
  | x > 3     = (7*x + 2) + foobar xs
  | otherwise = foobar xs

-- It works but its not good Haskell style. The problem is that its
-- 1. doing too much at once
-- 2. working at too low a level
--
-- Rather than think about each individual element, lets look at this
-- problem as incremental transformations to the whole input.

foobar' :: [Integer] -> Integer
foobar' = sum . map (\x -> 7*x + 2) . filter (>3)

-- So, what is going on here?
-- This defines foobar as a "pipeline" of three functions.
-- First, we filter our list for all elements greater than 3
-- Next we map our arithmatic expression onto the elements of the list
-- Finally we sum up the results

