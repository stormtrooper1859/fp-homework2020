module Part3.Unit (testSuite) where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Part3 (maybeConcat, eitherConcat, NonEmpty(..),ThisOrThat (..), Name(..), Endo(..))

testSuite :: IO TestTree
testSuite = testSpec "Part3 unit tests" part3Unit

part3Unit :: Spec
part3Unit = do
    describe "maybeConcat" $ do
        it "maybeConcat work correctly" $ do
            maybeConcat ([Just [3, 4], Just [7, 0, 0], Nothing, Just [1, 1], Nothing, Nothing, Just [5, -5]] :: [Maybe [Int]]) `shouldBe` [3, 4, 7, 0, 0, 1, 1, 5, -5]

        it "maybeConcat on empty list work correctly " $ do
            maybeConcat ([] :: [Maybe [Int]]) `shouldBe` []

        it "maybeConcat on list with Nothing work correctly " $ do
            maybeConcat ([Nothing, Nothing] :: [Maybe [Int]]) `shouldBe` []

    describe "eitherConcat" $ do
        it "eitherConcat work correctly" $ do
            eitherConcat ([Left ("a", [3.0]), Right [1.0, 2.0, 3.0], Left ("c", [5.0]), Right [4.0, 5.0]] :: [Either (String, [Double]) [Double]]) `shouldBe` (("ac", [3.0, 5.0]), [1.0, 2.0, 3.0, 4.0, 5.0])

    describe "NonEmpty semigroup" $ do
        it "NonEmpty (<>) is correct" $ do
            ("abac" :| [] ) <> ("check" :| ["dac", "quak"]) `shouldBe` ("abac" :| ["check", "dac", "quak"])

    describe "ThisOrThat semigroup" $ do
        it "This and That" $ do
            This "str" <> That "rts" `shouldBe` Both "str" "rts"

        it "This and This" $ do
            This "str" <> This "rts" `shouldBe` (This "strrts" :: (ThisOrThat String [Int]))

        it "Both and That" $ do
            Both "say no" "to " <> That "kek" `shouldBe` Both "say no" "to kek"
            
        it "This and Both" $ do
            This "and " <> Both "what?" "nothing" `shouldBe` Both "and what?" "nothing"

        it "Both and Both" $ do
            Both "say no: " "say yes: " <> Both "no" "kek" `shouldBe` Both "say no: no" "say yes: kek"

    describe "Name semigroup and monoid instances" $ do
        it "Name (<>) is correctt" $ do
            Name "str" <> Name "rts" `shouldBe` Name "str.rts"

        it "Name mappend is correctt" $ do
            Name "127" `mappend` Name "0" `mappend` Name "0" `mappend` Name "1" `shouldBe` Name "127.0.0.1"

        it "Name mempty is correctt" $ do
            Name "127" `mappend` mempty `mappend` mempty `mappend` Name "1" `shouldBe` Name "127...1"

    describe "Endo semigroup and monoid instances" $ do
        it "Endo (<>) is correctt" $ do
            (getEndo (Endo (+1) <> Endo (*2))) 3 `shouldBe` (8 :: Integer)

        it "Endo mempty is correctt" $ do
            (getEndo mempty) 14 `shouldBe` (14 :: Integer)
