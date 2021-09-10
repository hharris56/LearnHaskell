-- Hunter Harris
-- List Exercises
-- 9 8 2021

module ListEx where


-- EXERCISE 1 : inList

-- given a comparison funciton, an element, and a list of elements
-- return boolean of if element is present in the list
inList :: (Eq a) => a -> [a] -> Bool
inList _ [] = False
inList e (x:xs) = ( e == x ) || ( inList e xs )
{- Why this works ^^
-- Use an 'or' operator to chain the result of this iteration
-- with the result of all subsequent iterations... if there is
-- at least 1 true the result will be true
inList e (x:xs) = if ( e == x )
                    then True
                    else inList e xs
-- my original solution ^^
-- technically faster bc once a match is found the function 
-- does not continue but this gain is marginal and the other 
-- solution is cleaner -}


-- EXERCISE 2 : nub

-- given a list of elements, return a list with all duplicates removed
nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x:xs)
  | inList x xs = nub xs
  | otherwise   = x : nub xs


-- EXERCISE 3 : isAsc

-- given a list of numbers, return boolean of if list is in ascending order
isAsc :: [Int] -> Bool
isAsc [] = True
isAsc [_] = True
isAsc (x:xs) = x <= head xs && isAsc xs
{- Another Option
isAsc (x:y:xs) = (x <= y) && isAsc (y:xs)
-- this method shows we can extend the pattern matching to ensure
-- further than just (x:xs) ... this can be useful -}


-- EXERCISE 4 : hasPath

-- Given a list and an element, return the list with the element removed
removeElem :: (Eq a) => [a] -> a -> [a]
removeElem [] _ = []
removeElem (x:xs) e
  | x == e    = removeElem xs e
  | otherwise = x : removeElem xs e

-- given a directed graph in the form of a list of Int tuples,
-- return a boolean of it a path exists from the first to the second node
hasPath :: [(Int, Int)] -> Int -> Int -> Bool
hasPath list start target = aux list start target
  where
    aux [] s t = s == t
    aux ((n1, n2):xs) s t
      | s == n1 && t == n2 = True
      | s == n1   = hasPath (removeElem list (n1, n2)) n2 t ||
                    aux xs s t
      | otherwise = aux xs s t

hasPath2 :: [(Int, Int)] -> Int -> Int -> Bool
hasPath2 [] s t = s == t
hasPath2 list start target
  | start == target = True
  | otherwise = 
    let list' = [ (n, m) | (n, m) <- list, n /= start] in
        or [ hasPath list' m y | (n, m) <- list, n == start]
{- So admittedly, this second solution is a bit confusing but I think
-- I understand what is happening. Using list comprehension we generate
-- 2 new lists. This first one is called list' and this consists of all
-- remaining nodes that do not match with start (ie: all nodes that have not
-- been visited yet). The second list is unnamed but it is a list of boolean
-- values retrieved from calling hasPath on our new list (list') with the
-- visted nodes destination as the new starting value. We then perform an
-- 'or' operation on the entire list, and if any of the results are true,
-- the entire expression will result in true. The logic is mainly the same
-- as my previous solution, but writen in a more comprehensive manner -}
