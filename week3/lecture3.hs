-- Hunter Harris
-- Haskell Lecture Week 3
-- 5 October 2021


-- !! 'Cons' here is just a constructor name
data IntList = Empty | Cons Int IntList
             deriving Show

-- Explicitly defined recursion examples

absAll :: IntList -> IntList
absAll Empty       = Empty
absAll (Cons x xs) = Cons (abs x) (absAll xs)

squareAll :: IntList -> IntList
squareAll Empty       = Empty
squareAll (Cons x xs) = Cons (x*x) (squareAll xs)

-- Lots of repeated logic - no good! Instead, lets write a function 
-- that takes another function of type Int -> Int as an argument

mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList _ Empty       = Empty
mapIntList f (Cons x xs) = Cons (f x) (mapIntList f xs)

{- Congradulations! This is the basis of the 'map' library function

addOne x = x + 1
absOne x = abs x
square x = x * x

-- Now we can use functions like the ones defined above to map over
-- an IntList like so

exampleList = Cons -5 (Cons 3 (Cons 21 (Cons 8 (Cons -11 Empty))))

mapIntList addOne exampleList
mapIntList absOne exampleList
mapIntList square exampleList

-}

-- Lets take a closer look at how polymorphism in haskell can allow
-- us to generalize not only the functions, but data types as well

data List t = E | C t (List t)

-- We will use E and C since 'Empty' and 'Cons' are taken by IntList
-- 't' is a type variable, you can tell b/c it starts with lowercase

lst1 :: List Int
lst1 = C 3 (C 4 (C 5 E))

lst2 :: List Char
lst2 = C 'x' (C 'y' (C 'z' E))

lst3 :: List Bool
lst3 = C True (C False E)

-- Now, lets generalize our mapIntList function. Our first thought
-- would be to make the function variable type (t -> t)... however,
-- we want to be able to map to another type, meaning we should do
-- something more like this

mapList :: (a -> b) -> List a -> List b
mapList _ E        = E
mapList f (C x xs) = C (f x) (mapList f xs)

-- this solution lets us go from any type 'a' to any type 'b'
-- within our map function 'f'

-- appease the compiler
main :: IO ()
main = putStrLn "hello"
