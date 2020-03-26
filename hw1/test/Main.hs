import Test.Tasty (defaultMain, testGroup)

import Part1.Unit (testSuite)
import Part2.PropertyBased (testSuite)
import Part2.Unit (testSuite)
import Part3.Unit (testSuite)
import Part4.Unit (testSuite)

main :: IO ()
main = do
    unitTests1 <- Part1.Unit.testSuite
    unitTests2 <- Part2.Unit.testSuite
    unitTests3 <- Part3.Unit.testSuite
    unitTests4 <- Part4.Unit.testSuite
    let allTests = testGroup "HW1 tests" [unitTests1, unitTests2, Part2.PropertyBased.testSuite, unitTests3, unitTests4]
    defaultMain allTests
