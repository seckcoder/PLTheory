module Data.List.Util where

import qualified Data.List as L

uniqSorted :: (Ord a) => [a] -> [a]
uniqSorted [] = []
uniqSorted [a] = [a]
uniqSorted (a1:a2:as)
    | a1 == a2 = uniqSorted (a2:as)
    | otherwise = a1 : uniqSorted (a2:as)

unique :: (Ord a) => [a] -> [a]
unique xs = uniqSorted $ L.sort xs