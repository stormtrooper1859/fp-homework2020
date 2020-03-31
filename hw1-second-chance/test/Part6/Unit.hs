module Part6.Unit (testSuite) where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Part6 (Parser (..), listlistParser, parseBBS, parseInteger)

testSuite :: IO TestTree
testSuite = testSpec "Part6 unit tests" part4Unit

justGetResult :: Maybe (a, b) -> Maybe a
justGetResult = fmap fst

part4Unit :: Spec
part4Unit = do
    describe "parseBBS" $ do
        it "() should parse" $ do
            runParser parseBBS "()" `shouldBe` Just ("()", "")

        it "()()(()((()))) should parse" $ do
            justGetResult (runParser parseBBS "()()(()((())))") `shouldBe` Just "()()(()((())))"

        it "()) shouldn't parse" $ do
            justGetResult (runParser parseBBS "())") `shouldBe` Nothing

        it "\"\" should parse" $ do
            justGetResult (runParser parseBBS "") `shouldBe` Just ""

        it ")( shouldn't parse" $ do
            justGetResult (runParser parseBBS ")(") `shouldBe` Nothing

        it "(()() shouldn't parse" $ do
            justGetResult (runParser parseBBS "(()()") `shouldBe` Nothing


    describe "parseInteger" $ do
        it "145 should parse" $ do
            runParser parseInteger "145" `shouldBe` Just (145, "")

        it "+0 should parse" $ do
            justGetResult (runParser parseInteger "+0") `shouldBe` Just 0

        it "+145 should parse" $ do
            justGetResult (runParser parseInteger "+145") `shouldBe` Just 145

        it "-10 should parse" $ do
            justGetResult (runParser parseInteger "-10") `shouldBe` Just (-10)

        it "-+10 shouldn't parse" $ do
            justGetResult (runParser parseInteger "-+10") `shouldBe` Nothing

        it "--10 shouldn't parse" $ do
            justGetResult (runParser parseInteger "--10") `shouldBe` Nothing

        it "+++10 shouldn't parse" $ do
            justGetResult (runParser parseInteger "+++10") `shouldBe` Nothing

        it "-1,0 shouldn't parse" $ do
            justGetResult (runParser parseInteger "-1,0") `shouldBe` Nothing

        it "1.0 shouldn't parse" $ do
            justGetResult (runParser parseInteger "1.0") `shouldBe` Nothing

        it "99999999 should parse" $ do
            justGetResult (runParser parseInteger "99999999") `shouldBe` Just 99999999

        it "9aa9999999 shouldn't parse" $ do
            justGetResult (runParser parseInteger "9aa9999999") `shouldBe` Nothing

        it "+ 9 shouldn't parse" $ do
            justGetResult (runParser parseInteger "+ 9") `shouldBe` Nothing

    describe "listlistParser" $ do
        it "\"2, 1,+10  , 3,5,-7, 2\" should parse correctly" $ do
            runParser listlistParser "2, 1,+10  , 3,5,-7, 2" `shouldBe` Just ([[1, 10], [5, -7, 2]], "")

        it "\"  0  \" should parse correctly" $ do
            justGetResult (runParser listlistParser "  0  ") `shouldBe` Just [[]]

        it "\"0, 0, 0, 1, 0\" should parse correctly" $ do
            justGetResult (runParser listlistParser "0, 0, 0, 1, 0") `shouldBe` Just [[], [], [], [0]]

        it "\"1, -1  ,2,+2    ,2,3 ,  3,3  ,+3,4,4, 4 , 4 , 4\" should parse" $ do
            justGetResult (runParser listlistParser "1, -1  ,2,+2    ,2,3 ,  3,3  ,+3,4,4, 4 , 4 , 4") `shouldBe` Just [[-1], [2, 2], [3, 3, 3], [4, 4, 4, 4]]         

        it "\"1 1\" should return Nothing" $ do
            justGetResult (runParser listlistParser "1 1") `shouldBe` Nothing

        it "\"0, 1, 2, 3, 4, 5\" should return Nothing" $ do
            justGetResult (runParser listlistParser "0, 1, 2, 3, 4, 5") `shouldBe` Nothing

        it "\"-1, +1\" should return Nothing" $ do
            justGetResult (runParser listlistParser "-1, +1") `shouldBe` Nothing  

        it "\"+1, -1\" should parse correctly" $ do
            justGetResult (runParser listlistParser "+1, -1") `shouldBe` Just [[-1]]  
            
        it "\"1,a\" should return Nothing" $ do
            justGetResult (runParser listlistParser "1,a") `shouldBe` Nothing  
