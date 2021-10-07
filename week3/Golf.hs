-- Hunter Harris
-- Learn Haskell HW3 Golf
-- 6 October 2021

module Golf where



-- Task #1 - Hopscotch
-- For a given list, return a list of lists starting with the
-- original, then one with every second element, then every
-- third, up to every nth where n is # of letters in the word
--
-- ex: skips "ABCD" == ["ABCD", "BD", "C", "D"]

skips :: [a] -> [[a]]
skips [] = []
skips xs = sol
  where
    sol = [ [e | (e, i) <- zip xs [1..], i `mod` n == 0] | n <- [1..length xs] ]

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
