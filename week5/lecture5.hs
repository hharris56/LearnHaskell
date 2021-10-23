-- Hunter Harris
-- Haskell lecture 5
-- 23 October 2021

{-# LANGUAGE FlexibleInstances #-}

module Lecture5 where

-- Type Classes ---------------------------------------------------------------

-- Num, Eq, Ord, and Show are examples of type classes. New types can create
-- an instance of a type class if they define the required functions. 
-- For example:
--
-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
--
-- If a type wants to create an instance of Eq it must define the == and /=
-- functions. Lets practice by defining our own type and declaring an 
-- instance of Eq for it.

data Foo = F Int | G Char

instance Eq Foo where
  (F i1) == (F i2) = i1 == i2
  (G c1) == (G c2) = c1 == c2
  _ == _ = False

  foo1 /= foo2 = not (foo1 == foo2)

-- In this case we had to define ==, then /= in terms of ==. The Eq class
-- is actually declared with both == and /= in reference to one another:
--
-- class Eq a where
--   (==), (/=) :: a -> a -> Bool
--   x == y = not (x /= y)
--   x /= y = not (x == y)
--
-- This means that when declaring an instance of Eq we can define whichever
-- is more convenient (== or /=), and the Eq will derive the other in terms
-- of the first. PS.Be careful, without either definition we get infinite
-- recursion!
--
-- Eq (along with a few other type classes) is special and can be
-- automatically generated like so

data Foo' = F' Int | G' Char
          deriving (Eq, Ord, Show)

-- Type classes are very similar to Java interfaces. They define a set of
-- types / classes that implement a specified list of operations. However,
-- there are some key differences
-- 1. When a java class is defined, any interfaces it implements must be
--    declared. On the contrary, type classes are declared separately from
--    type definitions, and could even be put in a differnt module.
--    **HINT: This is a bit confusing, but look above where we defined Foo,
--      and then below that we defined an instance of Eq for Foo. These are
--      separate pieces of code, and could be separated into different
--      modules.
-- 2. Types specified for type class methods are more general than signatures
--    found in java interface methods... Honestly refer to the lecture for 
--    this one.

-- Other Standard Type Classes
-- Ord: (< , <=)
--    For types whos elements can be totally ordered
-- Num: (+, -, *)
--    For numeric types. Important Note: Integer literals are type class
--    polymorphic. That means that '5' could be used as an Int, Integer,
--    Double, or any other type that is an instance of Num (including ones
--    that YOU define!!)
-- Show:
--    Used to convert values into Strings
-- Read: 
--    Dual of Show
-- Integral:
--    Whole number types like Int and Integer

-- Lets put it all together and make our own type class

class Listable a where
  -- toList :: Listable a => a -> [Int]
  toList :: a -> [Int]

-- we can make a singleton list for Ints
instance Listable Int where
  -- toList :: Int -> [Int]
  toList x = [x]

-- same with Bools but we will assign 0 and 1
instance Listable Bool where
  -- toList :: Bool -> [Int]
  toList True = [1]
  toList False = [0]

-- Int lists dont need any conversion
instance Listable [Int] where
  toList = id

-- finally, lets handle some tree flattening
data Tree a = Empty | Node a (Tree a) (Tree a)

instance Listable (Tree Int) where
  toList Empty        = []
  toList (Node x l r) = toList l ++ [x] ++ toList r

-- Type class constraints will transfer to functions defined in terms of them.
-- For example, see the type of sumL below
sumL :: Listable a => a -> Int
sumL x = sum (toList x)

-- What about this one?
foo :: (Listable a, Ord a) => a -> a -> Bool
foo x y = sum (toList x) == sum (toList y) || x < y

-- Foo will only work for types which are instances of BOTH Listable and Ord,
-- since it uses both toList and comparison arguments

-- Finally lets look at a more complex example

instance (Listable a, Listable b) => Listable (a,b) where
  toList (x,y) = toList x ++ toList y

-- Notice how we can put type class contraints on an instance similar to
-- a function type. This says that a pair type (a,b) is an instance of 
-- Listable as long as both a and b are instances of listable as well. Then
-- we get to define toList for this instance of Listable, which is just
-- adding together the 'regular' toList calls for types a and b.
