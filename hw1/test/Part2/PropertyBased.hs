module Part2.PropertyBased ( testSuite ) where

import Hedgehog

import qualified Data.List as List
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Part1 (fromList, toList)
import Part2 (joinWith, splitOn)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

genIntList :: Gen [Int]
genIntList =
    let listLength = Range.linear 0 1000
    in  Gen.list listLength Gen.enumBounded

sortListByBinaryTree :: Property
sortListByBinaryTree = property $
    forAll genIntList >>= \xs ->
    toList (fromList xs) === List.sort xs

testTree :: TestTree
testTree = testProperty "Sorting by binary tree" sortListByBinaryTree


genString :: Gen String
genString =
    let listLength = Range.linear 0 10000
    in  Gen.string listLength Gen.enumBounded

splitAndJoinString :: Property
splitAndJoinString = property $
    forAll genString >>= \str ->
    joinWith 'a' (splitOn 'a' str) === str

testSplit :: TestTree
testSplit = testProperty "Split and join string" splitAndJoinString


testSuite :: TestTree
testSuite = testGroup "Part2 Property-based tests" [testTree, testSplit]
