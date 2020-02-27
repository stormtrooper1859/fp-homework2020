module Part2
       ( doubleNeg
       , excludedNeg
       , pierce
       , doubleNegElim
       , thirdNegElim
       ) where

import Data.Void (Void)

type Neg a = a -> Void


-- a -> (a -> Void) -> Void
doubleNeg :: a -> Neg (Neg a)
doubleNeg a b = b a


axiom1 :: a -> b -> a
axiom1 = const

axiom6 :: a -> Either a (Neg a)
axiom6 = Left

axiom7 :: Neg a -> Either a (Neg a)
axiom7 = Right

axiom9 :: (a -> b) -> (a -> Neg b) -> Neg a
axiom9 a b c = b c (a c)

contraposition :: (a -> b) -> (Neg b -> Neg a)
contraposition aImplB notB = axiom9 aImplB (axiom1 notB)

auxiliary1 :: Neg (Either a (Neg a)) -> Neg a
auxiliary1 = contraposition axiom6

auxiliary2 :: Neg (Either a (Neg a)) -> Neg (Neg a)
auxiliary2 = contraposition axiom7

excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg = axiom9 auxiliary1 auxiliary2

-- не заселяется
pierce :: ((a -> b) -> a) -> a
pierce = undefined

-- не заселяется, аналогичен 10ой аксиоме ИВ
doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim = contraposition doubleNeg
