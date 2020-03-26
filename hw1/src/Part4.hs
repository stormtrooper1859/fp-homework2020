{-# LANGUAGE InstanceSigs #-}

module Part4
       ( stringSum
       , Tree(..)
       ) where

import Text.Read

-- task 1
stringSum :: String -> Maybe Int
stringSum a = fmap sum $ traverse readMaybe $ words a


--task 2
data Tree a
    = Branch (Tree a) (Tree a)
    | Leaf a

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f (Leaf x)     = Leaf $ f x
    fmap f (Branch a b) = Branch (fmap f a) (fmap f b)

instance Applicative Tree where
    pure :: a -> Tree a
    pure = Leaf

    (<*>) :: Tree (a -> b) -> Tree a -> Tree b
    (<*>) (Leaf f) node     = fmap f node
    (<*>) (Branch f g) node = Branch (f <*> node) (g <*> node)

instance Foldable Tree where
    foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr f acc (Leaf x)     = f x acc
    foldr f acc (Branch a b) = foldr f (foldr f acc b) a

instance Traversable Tree where
    traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
    traverse f (Leaf x)     = fmap Leaf $ f x
    traverse f (Branch a b) = Branch <$> (traverse f a) <*> (traverse f b)
