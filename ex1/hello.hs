-- Hunter Harris
-- 8 23 2021

sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)

hailstone :: Integer -> Integer
hailstone n
 | n == 1         = n
 | n `mod` 2 == 0 = hailstone n `div` 2
 | otherwise      = hailstone 3 * n + 1

main :: IO ()
main = putStrLn ( 
 show ( hailstone 8 )
 )
