module Week39 where

import qualified Data.Set as S
import Data.List

data Set a = Empty | Singleton a | Union (Set a) (Set a)
    deriving (Eq, Show)


fromList :: [a] -> Set a
fromList []     = Empty
fromList [x]    = Singleton x
fromList (x:xs) = Union (Singleton x) (fromList xs)

isIn :: Eq a => a -> Set a -> Bool
isIn _ Empty = False
isIn x (Singleton y) = x == y
isIn x (Union set1 set2) = isIn x set1 || isIn x set2

subset :: Eq a => Set a -> Set a -> Bool
subset Empty _ = True
subset (Singleton x) set2 = isIn x set2
subset (Union set1 set2) set3 = subset set1 set3 && subset set2 set3

setEq :: Eq a => Set a -> Set a -> Bool
setEq set1 set2 = subset set1 set2 && subset set2 set1


data Nat = Zero | Succ Nat
    deriving (Show, Eq)


foldNat :: a -> (a -> a) -> Nat -> a
foldNat z _ Zero     = z
foldNat z f (Succ n) = f (foldNat z f n)

foldSet :: b -> (a -> b) -> (b -> b -> b) -> Set a -> b
foldSet z _ _ Empty = z
foldSet z f _ (Singleton x) = f x
foldSet z f g (Union set1 set2) = g (foldSet z f g set1) (foldSet z f g set2)

isInF :: Eq a => a -> Set a -> Bool
isInF x = foldSet False (== x) (||)

subsetF :: Eq a => Set a -> Set a -> Bool
subsetF set1 set2 = foldSet True (`isInF` set2) (&&) set1
