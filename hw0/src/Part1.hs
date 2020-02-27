{-# LANGUAGE TypeOperators #-}

module Part1
       ( distributivity
       , associator
       , eitherAssoc
       ) where

distributivity
    :: Either a (b, c)
    -> (Either a b, Either a c)
distributivity (Left a)       = (Left a, Left a)
distributivity (Right (b, c)) = (Right b, Right c)

associator
    :: (a, (b, c))
    -> ((a, b), c)
associator (a, (b, c)) = ((a, b), c)

type (<->) a b = (a -> b, b -> a)

assocLeft :: Either a (Either b c) -> Either (Either a b) c
assocLeft (Left a)          = Left $ Left a
assocLeft (Right (Left a))  = Left $ Right a
assocLeft (Right (Right a)) = Right a

assocRight ::  Either (Either a b) c -> Either a (Either b c)
assocRight (Left (Left a))  = Left a
assocRight (Left (Right a)) = Right $ Left a
assocRight (Right a)        = Right $ Right a

eitherAssoc
    :: Either a (Either b c)
    <-> Either (Either a b) c
eitherAssoc = (assocLeft, assocRight)
