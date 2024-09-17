module Week34 where

isEmpty :: [Int] -> Bool
isEmpty [] = True
isEmpty (head:tail) = False

safeHead :: [Int] -> [Int]
safeHead [] = []
safeHead list = [head list]

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
    let     lower = quickSort [y | y <- xs, y <= x] 
            higher = quickSort [y | y <- xs, y >  x] 
    in lower ++ [x] ++ higher