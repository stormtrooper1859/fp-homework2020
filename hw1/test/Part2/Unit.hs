module Part2.Unit (testSuite) where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Data.List.NonEmpty (NonEmpty (..))

import Part2 (joinWith, splitOn)

testSuite :: IO TestTree
testSuite = testSpec "Part2 unit tests" part2Unit

part2Unit :: Spec
part2Unit = do
    describe "splitOn" $ do
        it "Split empty string correctly" $ do
            (splitOn '/' "") `shouldBe` ([] :| [])

        it "Split string correctly " $ do
            (splitOn '/' "path/to/file") `shouldBe` ("path" :| ["to", "file"])

        it "Split and join string correctly " $ do
            joinWith ':' (splitOn '/' "path/to/file") `shouldBe` "path:to:file"
