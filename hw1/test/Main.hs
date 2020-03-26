import Test.Tasty (defaultMain, testGroup)

import Part1.Unit (part1UnitTestTree)
import Part2.PropertyBased (part2PropertyBased)
import Part2.Unit (part2UnitTestTree)

main :: IO ()
main = do
    unitTests1 <- part1UnitTestTree
    unitTests2 <- part2UnitTestTree
    let allTests = testGroup "HW1 tests" [unitTests1, unitTests2, part2PropertyBased]
    defaultMain allTests
