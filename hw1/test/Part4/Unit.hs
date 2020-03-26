module Part4.Unit (testSuite) where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Part4 (stringSum)

testSuite :: IO TestTree
testSuite = testSpec "Part4 unit tests" part4Unit

part4Unit :: Spec
part4Unit = do
    describe "stringSum" $ do
        it "stringSum calculated result" $ do
            stringSum "  18 84 8 -9" `shouldBe` Just 101

        it "stringSum return Nothing" $ do
            stringSum "1aa8 43" `shouldBe` Nothing
