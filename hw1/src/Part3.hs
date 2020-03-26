{-# LANGUAGE InstanceSigs #-}

module Part3
       ( maybeConcat
       , eitherConcat
       , NonEmpty (..)
       , ThisOrThat (..)
       , Name (..)
       , Endo (..)
       ) where


-- task 1.1
maybeConcat :: [(Maybe [a])] -> [a]
maybeConcat = foldMap (maybe [] id)


--task 1.2
eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat = foldMap (either (\left -> (left, mempty)) (\right -> (mempty, right)))


--task 2
data NonEmpty a = a :| [a] deriving (Show, Eq)

instance Semigroup (NonEmpty a) where
    (<>) :: (NonEmpty a) -> (NonEmpty a) -> (NonEmpty a)
    (<>) (x :| xs) (b :| bs) = (x :| (xs <> [b] <> bs))


data ThisOrThat a b = This a | That b | Both a b deriving (Show, Eq)

instance (Semigroup a, Semigroup b) => Semigroup (ThisOrThat a b) where
    (<>) :: (ThisOrThat a b) -> (ThisOrThat a b) -> (ThisOrThat a b)
    (<>) (This a1) (This a2)       = This (a1 <> a2)
    (<>) (This a) (That b)         = Both a b
    (<>) (This a1) (Both a2 b)     = Both (a1 <> a2) b
    (<>) (That b) (This a)         = Both a b
    (<>) (That b1) (That b2)       = That (b1 <> b2)
    (<>) (That b1) (Both a b2)     = Both a (b1 <> b2)
    (<>) (Both a1 b) (This a2)     = Both (a1 <> a2) b
    (<>) (Both a b1) (That b2)     = Both a (b1 <> b2)
    (<>) (Both a1 b1) (Both a2 b2) = Both (a1 <> a2) (b1 <> b2)


-- task 2.2
data Name = Name String deriving (Show, Eq)

instance Semigroup Name where
    (<>) (Name a) (Name b) = Name $ a <> "." <> b

instance Monoid Name where
    mempty = Name ""
    mappend = (<>)


newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup (Endo a) where
    (<>) end1 end2 = Endo $ (getEndo end2) . (getEndo end1)

instance Monoid (Endo a) where
    mempty = Endo id
    mappend = (<>)

