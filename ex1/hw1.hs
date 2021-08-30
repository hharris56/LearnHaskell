{-# OPTIONS_GHC -Wall #-}

-- Hunter Harris
-- Haskell HW #1
-- 8 27 2021

import Data.List

-- EXERCISE 1 - VALIDATING CREDIT CARD NUMBERS

-- converts large number to list of digits
toDigits :: Int -> [Int]
toDigits num
 | num <= 0  = []
 | otherwise = num `mod` 10 : toDigits (num `div` 10)

-- double every second digit in a given array
doubleEveryOther :: [Int] -> [Int]
doubleEveryOther []           = []
doubleEveryOther (x:[])       = [x]
doubleEveryOther (x:(y:zs))   = 2*x : (y : (doubleEveryOther zs))

-- sum the digits of a given number
sumDigits :: Int -> Int
sumDigits num = (num `mod` 10) + (num `div` 10)

-- get checkSum value for a given array
checkSum :: [Int] -> Int
checkSum [] = 0
checkSum (x:xs) = (sumDigits x) + (checkSum xs)

-- validate checksum
checkValid :: [Int] -> Bool
checkValid arr = res `mod` 10 == 0
 where
  res = checkSum (doubleEveryOther (reverse arr)) 

main :: IO ()
main =
 let num = toDigits 4833160142311627
 in
  if checkValid num
   then putStrLn "True"
   else putStrLn "False"


-- EXERCISE 2 - TOWERS OF HANOI

--type Peg = String
--type Move = (Peg, Peg)

-- Given the # of discs and 3 peg names, return a list of peg moves
--hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

-- TODO: Dont know how to do this one... how to know when done?
-- POSSIBLE: define helpers to determine # of discs on each peg


