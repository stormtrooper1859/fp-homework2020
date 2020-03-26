{-# LANGUAGE InstanceSigs #-}

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

       , Tree(..)
       , treeIsEmpty
       , treeSize
       , findInTree
       , insertInTree
       , fromList
       , removeFromTree
       ) where


import Data.List.NonEmpty (NonEmpty (..))

-- task1
data DayOfWeek = Monday | Tuesday | Wednsday | Thursday | Friday | Saturday | Sunday deriving (Show)

instance Enum DayOfWeek where
    toEnum :: Int -> DayOfWeek
    toEnum x = case x of
        0 -> Monday
        1 -> Tuesday
        2 -> Wednsday
        3 -> Thursday
        4 -> Friday
        5 -> Saturday
        6 -> Sunday
        _ -> error "illegal day of week"

    fromEnum :: DayOfWeek -> Int
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
    (==) :: Nat -> Nat -> Bool
    (==) Z Z         = True
    (==) (S a) (S b) = a == b
    (==) _ _         = False

instance Ord Nat where
    (<=) :: Nat -> Nat -> Bool
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


-- task3
data Tree a
    = Leaf
    | Node (NonEmpty a) (Tree a) (Tree a)
    deriving (Show)

treeIsEmpty :: Tree a -> Bool
treeIsEmpty Leaf = True
treeIsEmpty _    = False

treeSize :: Tree a -> Int
treeSize Leaf                        = 0
treeSize (Node (_ :| xs) left right) = 1 + (length xs) + (treeSize left) + (treeSize right)

findInTree :: Ord a => Tree a -> a -> Maybe (NonEmpty a)
findInTree Leaf _ = Nothing
findInTree (Node current@(x :| _) left right) value =
    case compare value x of
        LT -> findInTree left value
        EQ -> Just current
        GT -> findInTree right value


insertInTree :: Ord a => Tree a -> a -> Tree a
insertInTree Leaf value = Node (value :| []) Leaf Leaf
insertInTree (Node val@(x :| xs) left right) value =
    case compare value x of
        LT -> Node val (insertInTree left value) right
        EQ -> Node (value :| (x : xs)) left right
        GT -> Node val left (insertInTree right value)

fromList :: Ord a => [a] -> Tree a
fromList []       = Leaf
fromList (x : xs) = insertInTree (fromList xs) x


-- находит самый левый элемент в поддереве и возвращает пару
-- из значения этой вершины и поддерева с удаленной самой левой вершиной
extractLeftNode :: Tree a -> (NonEmpty a, Tree a)
extractLeftNode (Node val Leaf right) = (val, right)
extractLeftNode (Node val left right) =
    let (v, tree) = extractLeftNode left
    in (v, Node val tree right)
extractLeftNode _ = error "Unreachable"

removeFromTree :: Ord a => Tree a -> a -> Tree a
removeFromTree Leaf _ = Leaf
removeFromTree (Node (x :| []) left Leaf) value
    | x == value = left
removeFromTree (Node (x :| []) left right) value
    | x == value =
        let (v, tree) = extractLeftNode right
        in Node v left tree
removeFromTree (Node (x :| x2 : xs) left right) value
    | x == value = Node (x2 :| xs) left right
removeFromTree node@(Node val@(x :| _) left right) value =
    case compare value x of
        LT -> Node val (removeFromTree left value) right
        EQ -> node
        GT -> Node val left (removeFromTree right value)


tempTree :: Tree Int
tempTree = Node (9 :| [9, 9]) (Node (2 :| []) Leaf (Node (3 :| [3, 3, 3, 3, 3]) Leaf Leaf)) (Leaf)
