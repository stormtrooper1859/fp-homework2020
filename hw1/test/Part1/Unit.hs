module Part1.Unit (testSuite) where

import Part1 (DayOfWeek (..), Nat (..), afterDays, daysToParty, divide, fromIntToNat, fromNatToInt,
              isEven, isWeekend, mul, nextDay, plus, remainder, sub)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

testSuite :: IO TestTree
testSuite = testSpec "Part1 unit tests" part1Unit

part1Unit :: Spec
part1Unit = do
    describe "DayOfWeek" $ do
        describe "nextDay" $ do
            it "Tuesday -> Wednsday" $ do
                nextDay Tuesday `shouldBe` Wednsday

            it "Sunday -> Monday" $ do
                nextDay Sunday `shouldBe` Monday

        describe "afterDays" $ do
            it "afterDays work correctly loop" $ do
                afterDays Friday 3 `shouldBe` Monday

            it "afterDays should loop" $ do
                afterDays Friday 7 `shouldBe` Friday

            it "afterDays work fast on large arguments" $ do
                afterDays Friday 1000000007 `shouldBe` Thursday

        describe "isWeekend" $ do
            it "is weekend" $ do
                isWeekend Saturday `shouldBe` True

            it "isn't weekend" $ do
                isWeekend Thursday `shouldBe` False

        describe "daysToParty" $ do
            it "from Monday" $ do
                daysToParty Monday `shouldBe` 4

            it "from Saturday" $ do
                daysToParty Saturday `shouldBe` 6

    describe "Nat" $ do
        describe "Nat to Nnt" $ do
            it "of 3 is correct" $ do
                fromNatToInt (S (S (S Z))) `shouldBe` 3

            it "of 0 is correct" $ do
                fromNatToInt Z `shouldBe` 0

        describe "Int to Nat" $ do
            it "of 4 is correct" $ do
                fromIntToNat 4 `shouldBe` (S $ S $ S $ S Z)

            it "of 0 is correct" $ do
                fromIntToNat 0 `shouldBe` Z

        describe "isEven" $ do
            it "of 5 is correct" $ do
                isEven (S $ S $ S $ S $ S Z) `shouldBe` False

            it "of 4 is correct" $ do
                isEven (S $ S $ S $ S Z) `shouldBe` True

            it "of 0 is correct" $ do
                isEven Z `shouldBe` True

        describe "plus" $ do
            it "2 + 3 == 5" $ do
                plus (S $ S Z) (S $ S $ S Z) `shouldBe` (S $ S $ S $ S $ S Z)

            it "3 + 2 == 2 + 3" $ do
                plus (S $ S $ S Z) (S $ S Z) `shouldBe`  plus (S $ S Z) (S $ S $ S Z)

            it "7 + 0 == 7" $ do
                plus (S $ S $ S $ S $ S $ S $ S Z) Z `shouldBe` (S $ S $ S $ S $ S $ S $ S Z)

        describe "mul" $ do
            it "2 * 3 == 6" $ do
                mul (S $ S Z) (S $ S $ S Z) `shouldBe` (S $ S $ S $ S $ S $ S Z)

            it "3 * 2 == 2 * 3" $ do
                mul (S $ S $ S Z) (S $ S Z) `shouldBe` mul (S $ S Z) (S $ S $ S Z)

            it "7 * 0 == 7" $ do
                mul (S $ S $ S $ S $ S $ S $ S Z) Z `shouldBe` Z

            it "0 * 6 == 6" $ do
                mul Z (S $ S $ S $ S $ S $ S Z) `shouldBe` Z

        describe "sub" $ do
            it "2 - 3 == 0" $ do
                (S $ S Z) `sub` (S $ S $ S Z) `shouldBe` Z

            it "3 - 2 == 1" $ do
                (S $ S $ S Z) `sub` (S $ S Z) `shouldBe` S Z

            it "5 - 0 == 5" $ do
                (S $ S $ S $ S $ S Z) `sub` Z `shouldBe` (S $ S $ S $ S $ S Z)

        describe "divide" $ do
            it "2 / 3 == 0" $ do
                (S $ S Z) `divide` (S $ S $ S Z) `shouldBe` Z

            it "5 / 2 == 2" $ do
                (S $ S $ S $ S $ S Z) `divide` (S $ S Z) `shouldBe` (S $ S Z)

            it "6 / 2 == 3" $ do
                (S $ S $ S $ S $ S $ S Z) `divide` (S $ S Z) `shouldBe` (S $ S $ S Z)

        describe "remainder" $ do
            it "2 / 3 == 2" $ do
                (S $ S Z) `remainder` (S $ S $ S Z) `shouldBe` (S $ S Z)

            it "5 / 2 == 1" $ do
                (S $ S $ S $ S $ S Z) `remainder` (S $ S Z) `shouldBe` S Z

            it "6 / 2 == 0" $ do
                (S $ S $ S $ S $ S $ S Z) `remainder` (S $ S Z) `shouldBe` Z
