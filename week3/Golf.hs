-- Hunter Harris
-- Learn Haskell HW3 Golf
-- 6 October 2021



module Golf where

-- import Safe.Partial

-------------------------------------------------------------------------------

-- Task #1 - Hopscotch
-- For a given list, return a list of lists starting with the
-- original, then one with every second element, then every
-- third, up to every nth where n is # of letters in the word
--
-- ex: skips "ABCD" == ["ABCD", "BD", "C", "D"]

skips :: [a] -> [[a]]
skips xs = [[e | (e, i) <- zip xs [1..], i `mod` n == 0] | n <- [1..length xs]]

-- How it works
-- Lets start by breaking down this list comprehension into 2 pieces
-- Part1: [ *Part2* | n <- [1..length xs] ]
--  This inital list comprehension simply creates another list with
--  the same length as our input containing numbers 1 to *list length*
-- Part2: [e | (e, i) <- zip xs [1..], i `mod` n == 0]
--  Again, lets break this down a bit
--  Part2.A: (e, i) <- zip xs [1..]
--   We use zip to add index information to each element.
--   Essentially, we are going through and turning each element into
--   a tuple containing the element and index, hence (e, i)
--  Part2.B: i `mod` n == 0
--   Remember that index we just assigned? And that list of numbers
--   1 to *list length* we created in Part1? Now lets see if our
--   current index i is a multiple of n... if so, it passes!
-- Summary:
--  Put it all together and we have this logic,
--  For each number n, 1 to *list length*, zip index info into our
--  input list and filter for only the elements who are at index
--  multiples of n

-------------------------------------------------------------------------------

-- Task #2 - Local Maxima
-- For a given Int array, return an array of Ints that were greater
-- than the values directly before and after it
--
-- ex: localMaxima [2, 9, 5, 6, 1,] == [9, 6]

localMaxima :: [Integer] -> [Integer]
localMaxima []           = []
localMaxima (xl:x:xr:xs)
  | (x > xl) && (x > xr) = x : (localMaxima $ x:xr:xs)
  | otherwise            = localMaxima $ x:xr:xs
localMaxima (_:xs)       = localMaxima xs

-- How it works
-- In order to check if a value is a local maxima we must compare it
-- to its two neighboring values. We use pattern matching to establish
-- AT LEAST 4 values in a list (left, target, right, rest of list) to
-- immediately rule out anything with no neighbors. After that we simply
-- compare if that target value is greater than its two neighboring
-- values. If so, add it to the list and continue, otherwise simply
-- move on. The next call should start with the current target value

-- Attempt 2

-- localMaximaF :: [Integer] -> [Integer]
-- localMaximaF l = [x | (xl:x:xr:xs) <- l, (x > xl) && (x > xr)]


-- [ [e | (e, i) <- zip xs [1..], i `mod` n == 0] | n <- [1..length xs] ]




-------------------------------------------------------------------------------

