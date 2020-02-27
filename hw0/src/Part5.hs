module Part5
       ( Nat
       , zero
       , churchMult
       , churchPlus
       , churchToInt
       , succChurch
       ) where


type Nat a = (a -> a) -> a -> a

zero :: Nat a
zero _ x = x

succChurch :: Nat a -> Nat a
succChurch num f x = f $ num f x

churchPlus :: Nat a -> Nat a -> Nat a
churchPlus m n f x = n f (m f x)

churchMult :: Nat a -> Nat a -> Nat a
churchMult m n f = m (n f)

churchToInt :: Nat Integer -> Integer
churchToInt num = num (+ 1) 0
