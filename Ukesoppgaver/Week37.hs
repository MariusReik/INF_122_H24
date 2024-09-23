module Week37 where

import Prelude hiding (and, concat, replicate, (!!), elem)
import Data.Char

euclid :: Int -> Int -> Int
euclid a b
      | b == 0     = abs a
      | otherwise  = euclid b (a `mod` b)

and :: [Bool] -> Bool
and = foldr (&&) True

concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat xss


replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n-1) x


(!!) :: [a] -> Int -> a
(x:_)  !! 0 = x              
(_:xs) !! n = xs !! (n - 1)  


elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (y:ys) = x == y || elem x ys

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys  
merge xs [] = xs  
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)  
  | otherwise = y : merge (x:xs) ys  



half :: [a] -> ([a],[a])
half [] = ([],[])
half xs = splitAt (length xs `div` 2 + length xs `mod` 2) xs


msort :: (Ord a) => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
  where
    (left, right) = half xs


tokenise :: String -> [String]
tokenise [] = []
tokenise (c:cs)
  | c `elem` "+-*/()" = [c] : tokenise cs
  | isDigit c         = let (num, rest) = span isDigit (c:cs) in num : tokenise rest
  | isAlpha c         = let (alpha, rest) = span isAlpha (c:cs) in alpha : tokenise rest
  | otherwise         = tokenise cs


