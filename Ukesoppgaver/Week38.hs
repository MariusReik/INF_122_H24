module Week38 where

remg :: [a] -> (a -> Bool) -> [a]
remg lst p = go lst
    where
        go [] = []
        go (x:xs)
            | p x       = xs
            | otherwise = x : go xs

mapF :: (a -> b) -> [a] -> [b]
mapF f = foldr (\x acc -> f x : acc) []


counterExample :: [Bool]
counterExample = repeat False


-- 7.1
mapFilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapFilter f p = map f . filter p


-- 7.4
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10 * x + y) 0

-- 7.9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g = zipWith (curry (\(i, x) -> (if even i then f else g) x)) [0..]

-- 7.10
luhnDouble :: Int -> Int
luhnDouble d    | d >= 5        = 2 * d - 9
                | otherwise     = 2 * d

luhn :: [Int] -> Bool
luhn ns = (sum (altMap id luhnDouble (reverse ns)) `mod` 10) == 0