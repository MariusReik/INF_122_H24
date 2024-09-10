module Week36 where

-- Check if a list is a palindrome
palindrome :: (Eq a) => [a] -> Bool
palindrome xs = xs == reverse xs

-- Return the third element of a list
third1, third2, third3 :: [a] -> a

third1 xs = head (tail (tail xs))  -- Access the third element by skipping first two elements

third2 xs = xs !! 2  -- Access the third element using index

third3 (_:_:x:_) = x  -- Pattern match to get the third element

-- Compute the sum of squares from 1 to x
squares1, squares2 :: Int -> Int

squares1 x = sum [n^2 | n <- [1..x]]  -- Using list comprehension

squares2 x = if x > 0 then x*x + squares2 (x-1) else 0  -- Using recursion

-- Generate Pythagorean triples with elements not exceeding n
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- m,
                       y <- m,
                       z <- m,
                       x^2 + y^2 == z^2 ]
          where m = [1..n]

-- Double a digit and subtract 9 if greater than 9
luhnDouble :: Int -> Int
luhnDouble n | n > 4 = 2 * n - 9
             | otherwise = 2 * n

-- Check if a 4-digit bank number is valid
luhnFixed :: Int -> Int -> Int -> Int -> Bool
luhnFixed a b c d = (sumDigits `mod` 10) == 0
  where sumDigits = luhnDouble a + b + luhnDouble c + d

-- Validate a list of digits using the Luhn algorithm
luhn :: [Int] -> Bool
luhn xs = sum [if even i then n else luhnDouble n | (n,i) <- zip (reverse xs) [0..]] `mod` 10 == 0
