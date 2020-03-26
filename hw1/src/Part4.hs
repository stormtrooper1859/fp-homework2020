{-# LANGUAGE InstanceSigs #-}

module Part4
       ( stringSum
       ) where

import Text.Read

-- task 1
stringSum :: String -> Maybe Int
stringSum a = fmap sum $ traverse readMaybe $ words a

