module Part1
       ( DayOfWeek(..)
       , nextDay
       , afterDays
       , isWeekend
       , daysToParty

       , Nat(..)
       , plus
       , mul
       , sub
       , fromIntToNat
       , fromNatToInt
       , isEven
       , divide
       , remainder
       ) where


-- task1
data DayOfWeek = Monday | Tuesday | Wednsday | Thursday | Friday | Saturday | Sunday deriving (Show)

instance Enum DayOfWeek where
    toEnum x = case x of
        0 -> Monday
        1 -> Tuesday
        2 -> Wednsday
        3 -> Thursday
        4 -> Friday
        5 -> Saturday
        6 -> Sunday
        _ -> error "illegal day of week"

    fromEnum x = case x of
        Monday   -> 0
        Tuesday  -> 1
        Wednsday -> 2
        Thursday -> 3
        Friday   -> 4
        Saturday -> 5
        Sunday   -> 6

nextDay :: DayOfWeek -> DayOfWeek
nextDay Sunday = Monday
nextDay day    = succ day

afterDays :: DayOfWeek -> Int -> DayOfWeek
afterDays day 0    = day
afterDays day left = afterDays (nextDay day) $ mod (left - 1) 7

isWeekend :: DayOfWeek -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

daysToParty :: DayOfWeek -> Int
daysToParty Friday = 0
daysToParty day    = 1 + daysToParty (nextDay day)


-- task2
data Nat = Z | S Nat deriving (Show)

plus :: Nat -> Nat -> Nat
plus a Z     = a
plus a (S b) = S (a `plus` b)

mul :: Nat -> Nat -> Nat
mul _ Z     = Z
mul a (S b) = a `plus` (a `mul` b)

sub :: Nat -> Nat -> Nat
sub Z _         = Z
sub a Z         = a
sub (S a) (S b) = a `sub` b

fromIntToNat :: Int -> Nat
fromIntToNat 0 = Z
fromIntToNat a
    | a < 0 = error "Nat must be non-negative"
    | otherwise = S $ fromIntToNat $ a - 1

fromNatToInt :: Nat -> Int
fromNatToInt Z     = 0
fromNatToInt (S a) = 1 + (fromNatToInt a)

instance Eq Nat where
    (==) Z Z         = True
    (==) (S a) (S b) = a == b
    (==) _ _         = False

instance Ord Nat where
    (<=) Z _         = True
    (<=) _ Z         = False
    (<=) (S a) (S b) = a <= b

isEven :: Nat -> Bool
isEven Z         = True
isEven (S Z)     = False
isEven (S (S a)) = isEven a

divide :: Nat -> Nat -> Nat
divide _ Z = error "Divide by zero"
divide a b = (iterSub Z (S a)) `sub` (S Z)
    where iterSub acc t
            | t == Z    = acc
            | otherwise = iterSub (S acc) (t `sub` b)

remainder :: Nat -> Nat -> Nat
remainder a b = sub a ((a `divide` b) `mul` b)
