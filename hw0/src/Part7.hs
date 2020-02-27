module Part7
       ( p1t20_all
       ) where


p1t1 :: String
p1t1 = " Grey"

p1t2 :: String
p1t2 = "Dorian "

p1t3 :: String -> String -> String
p1t3 = (++)

p1t4 :: String -> String
-- p1t4 = (++) "Dorian "
p1t4 = p1t3 p1t2

p1t5 :: (String -> String, String)
-- p1t5 = ((++) "Dorian ", " Grey")
p1t5 = (p1t4, p1t1)

p1t6 :: [(String -> String, String)]
-- p1t6 = [((++) "Dorian ", " Grey")]
p1t6 = [p1t5]

p1t7 :: (String -> String) -> (String -> String)
p1t7 = id

p1t8 :: ((String -> String) -> (String -> String)) -> (String -> String, String) -> String
p1t8 = uncurry

p1t9 :: (String -> String, String) -> String
-- p1t9 = uncurry id
p1t9 = p1t8 p1t7

p1t10 :: ((String -> String, String) -> String) -> [(String -> String, String)] -> [String]
p1t10 = map

p1t11 :: [(String -> String, String)] -> [String]
-- p1t11 = map (uncurry id)
p1t11 = p1t10 p1t9

p1t12 :: [String]
-- p1t12 = map (uncurry id) [((++) "Dorian ", " Grey")]
p1t12 = p1t11 p1t6

p1t13 :: String -> Bool
p1t13 = null

p1t14 :: [String] -> String
p1t14 = head

p1t15 :: (String -> Bool) -> ([String] -> String) -> [String] -> Bool
p1t15 = (.)

p1t16 :: ([String] -> String) -> [String] -> Bool
-- p1t16 = null .
p1t16 = (p1t13 `p1t15`)

p1t17 :: [String] -> Bool
-- p1t17 = null . head
p1t17 = p1t16 p1t14

p1t18 :: ([String] -> Bool) -> [String] -> Bool
p1t18 = ($)

p1t19 :: [String] -> Bool
-- p1t19 = null . head $
p1t19 = (p1t17 `p1t18`)

p1t20_all :: Bool
-- p1t20_all = null . head $ map (uncurry id) [((++) "Dorian ", " Grey")]
p1t20_all = p1t19 p1t12



-- temp1 = (\x -> zip (lefts x) (rights x)) [Left (1 + 2), Right (2 ^ 6)]

-- temp2 = let impl = \x y -> not x || y in
    -- let isMod2 = \x -> x `mod` 2 == 0 in
    -- let isMod4 = \x -> x `mod` 4 == 0 in
    -- \x -> (isMod4 x) `impl` (isMod2 x)
