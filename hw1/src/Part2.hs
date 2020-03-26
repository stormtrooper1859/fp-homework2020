{-# LANGUAGE InstanceSigs #-}

module Part2
       ( splitOn
       , joinWith
       ) where

import Data.List.NonEmpty (NonEmpty (..))


-- task1 is in Part1


-- task2
splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn delimiter list = foldr worker ([] :| []) list
    where
        worker cur (x :| xs) =
            if cur == delimiter
                then ([] :| (x : xs))
                else ((cur : x) :| xs)

joinWith :: a -> NonEmpty [a] -> [a]
joinWith delimiter (x :| xs) = foldl (\acc cur -> acc ++ [delimiter] ++ cur) x xs
