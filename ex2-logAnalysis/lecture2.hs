-- {-# LANGUAGE InstanceSigs #-}

-- Hunter Harris
-- Lecture 2 practice
-- 8 27 2021

import Data.List

-- Enumerations

data Brand = BMW
 | Toyota
 | VW
 | Honda
 | Ford
 deriving Show

listO'Brands :: [Brand]
listO'Brands = [Toyota, VW, Honda, Ford] 

-- Is a Brand german?
isGerman :: Brand -> Bool
isGerman VW = True
isGerman BMW = True
isGerman _ = False

data FailableDouble = Failure
 | OK Double
 deriving Show

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d) = d

-- say person twice to define the constructor
data Person = Person String Int Brand
 deriving Show

hunter :: Person
hunter = Person "Hunter" 22 VW
haleem :: Person
haleem = Person "Haleem" 23 BMW
edgar :: Person
edgar = Person "Edgar" 91 Honda

-- given a person return the age
getAge :: Person -> Int
getAge (Person _ a _) = a

-- a type can have different constructors
data NewType = Type1 String Int Bool
             | Type2 Int Bool
             | Type3 Bool
             | Type4 
  deriving Show

-- and each type can be treated differently with pattern matching
eval :: NewType -> Bool
eval (Type1 a b c) = (a > "richard" && b >= 20) || c
eval (Type2 b c)   = b `div` 10 > 2 || c
eval (Type3 c)     = c
eval Type4         = False

-- you can apply pattern requirements
-- x@pattern form lets you reference the pattern directly
-- ^^ kinda like naming a local variable to use
-- prints various message depending on Person
foo :: Person -> String
foo (Person n 80 b) = "80yo " ++ n ++ " caught driving a " ++ show b
-- foo (Person _ _ (isGerman b)) = "Das Komforteering"
foo pat@(Person n _ _) = "The name of pattern (" ++ show pat ++ ") is " ++ n

-- datatypes can be recursive
data Tree = Leaf Char
          | Node Tree Int Tree
 deriving Show

tree :: Tree
tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))

main :: IO ()
main =  putStrLn ( intercalate "\n" (map foo roster) )
 where
  roster = [(Person "Eli" 23 Toyota), 
            (Person "Harold" 80 Ford),
            hunter,
            haleem,
            edgar]
