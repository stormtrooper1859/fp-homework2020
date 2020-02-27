module Part4
       ( iterateElement
       , fibonacci
       , factorial
       , mapFix
       ) where

import Data.Function (fix)


iterateElement :: a -> [a]
iterateElement = fix $ \f a -> a : (f a)

fibonacci :: Integer -> Integer
fibonacci = fix $ \f num ->
  case num of
    0 -> 0
    1 -> 1
    a -> (f (a - 1)) + (f (a - 2))

factorial :: Integer -> Integer
factorial = fix $ \f num ->
  case num of
    0 -> 1
    a -> a * (f (a - 1))

mapFix :: (a -> b) -> [a] -> [b]
mapFix g = fix $ \f l ->
  case l of
    []       -> []
    (x : xs) -> (g x) : (f xs)
