module Week35 where

double :: [Int] -> [Int]
double = map (*2)

sm1, sm2 :: [Int] -> Int

sm1 = foldr (+) 0

sm2 xs = sum xs

adds :: [(Int,Int)] -> [Int]
adds =  map (uncurry (+))

q1 :: Int -> Int
q1 = (+) 2

q2 :: Int -> Int
q2 = (+ 2)

q3 :: Int -> Int
q3 = (2 +)

q4 :: ([String], Char)
q4 = (["foo", "bar"], 'a')

-- q5 is invalid
-- q5 :: ? 
-- q5 = (["foo", 'a'], "bar")

q6 :: [(Bool, [[Char]])]
q6 = [(True, []), (False, [['a']])]

q7 :: Int -> [b] -> b
q7 = \x y -> y !! x

q8 :: [Int -> [a] -> [a]]
q8 = [take, drop, \x y -> [y !! x]]
