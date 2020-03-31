import Test.Tasty (defaultMain, testGroup)

import Part6.Unit (testSuite)

main :: IO ()
main = do
    unitTests6 <- Part6.Unit.testSuite
    let allTests = testGroup "HW1-second-chance tests" [unitTests6]
    defaultMain allTests
