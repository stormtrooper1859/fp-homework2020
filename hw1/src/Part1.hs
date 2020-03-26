module Part1
       ( DayOfWeek(..)
       , nextDay
       , afterDays
       , isWeekend
       , daysToParty
       ) where


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

