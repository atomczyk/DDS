module AuxiliaryFunctions where

import Language
import Data.Tree
import Sort
import Data.List
import Text.PrettyPrint
import Data.Set hiding (filter, map, null)


-- | Creates list of pairs.
pairs :: Eq a => [a] -> [(a,a)]
pairs l = filterpairs ([(x,y) | x <- l, y <- l, x /= y])

-- | Filters out repeated pairs.
filterpairs :: Eq a => [(a, a)] -> [(a, a)]
filterpairs [] = []
filterpairs ((x,y):xs) = filter (/= (y,x)) ((filterpairs xs) ++ [(x,y)])


-- | Unpais the tuples.
unpair :: [(a, a)] -> [a]
unpair [] = []
unpair (x:xs) = fst x:snd x:(unpair xs)

-- | Removes duplicates.
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs)   | x `elem` xs   = rmdups xs
                | otherwise     = x : rmdups xs

-- | Check if a given lists is comprised  of duplicates.
hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates xs = length (nub xs) /= length xs

-- | Swaping two elements in a pair.
swap :: (a, b) -> (b, a)
swap (x,y) = (y,x)
