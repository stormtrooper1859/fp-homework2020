module Part6
       ( first
       , firstWHNF
       , second
       , secondWHNF
       ) where


import Data.Maybe (mapMaybe)
import Part1 (distributivity)

first :: (Either String a, Either String b)
first = distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain"))

firstWHNF :: (Either String a, Either String b)
firstWHNF = (Left ("harold" ++ " hide " ++ "the " ++ "pain"), Left ("harold" ++ " hide " ++ "the " ++ "pain"))


foo :: Char -> Maybe Double
foo char =
    case char == 'o' of
      True  -> Just $ exp pi
      False -> Nothing

second :: Bool
second = null $ mapMaybe foo "pole chudes ochen' chudesno"

secondWHNF :: Bool
secondWHNF = False
