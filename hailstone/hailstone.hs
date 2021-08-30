-- Hunter Harris
-- 8 23 2021
-- Hailstone algorthim (3x + 1)

import Data.List
import Text.Printf

-- given a seed number, calculate hailstone number
hailstone :: Integer -> Integer
hailstone seed = aux seed 1
 where
  aux n i
   | n == 1         = i
   | n `mod` 2 == 0 = aux ( n `div` 2 ) ( i + 1 )
   | otherwise      = aux ( n * 3 + 1 ) ( i + 1 )

-- given a seed number, return array containing hailstone path
hailstoneArray :: [Integer] -> [Integer]
hailstoneArray stoneArr
 | head stoneArr <= 1 
 = reverse stoneArr
 | (head stoneArr) `mod` 2 == 0 
 = hailstoneArray ( (head stoneArr) `div` 2 : stoneArr )
 | otherwise                    
 = hailstoneArray ( (head stoneArr) * 3 + 1 : stoneArr )

-- given an array of integers, return string with ' ' inbetween
makeString :: [Integer] -> String
makeString []     = ""
makeString (x:xs) = (show x) ++ " " ++ makeString xs

-- generate output for display
genOutput :: [Integer] -> String
genOutput hailstoneArr = msg ++ arr
 where
  arr = makeString hailstoneArr
  msg = printf "Hailstone #: %d\n" (length hailstoneArr)

main :: IO ()
main = do
 putStrLn "Enter seed number"
 seed <- getLine
 let seedAsNumber = read seed::Integer
 --putStrLn ( show (hailstone seedAsNumber) )
 putStrLn ( genOutput ( hailstoneArray [seedAsNumber] ) )


