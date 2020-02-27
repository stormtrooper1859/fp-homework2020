import Part4 (factorial, fibonacci)
import Part5 (Nat, churchMult, churchPlus, churchToInt, succChurch, zero)
import Test.Hspec

sevenNat :: Nat a
sevenNat = succChurch $ succChurch $ succChurch $ succChurch $ succChurch $ succChurch $ succChurch $ zero

nineNat :: Nat a
nineNat = succChurch $ succChurch $ sevenNat

main :: IO ()
main = hspec $ do
  describe "churchToInt" $ do
    it "zero is correct" $ do
      churchToInt zero `shouldBe` 0

    it "succChurch is correct" $ do
      churchToInt (succChurch sevenNat) `shouldBe` 1 + churchToInt sevenNat

    it "churchPlus  is correct" $ do
      churchToInt (churchPlus sevenNat nineNat) `shouldBe` churchToInt sevenNat + churchToInt nineNat

    it "churchMult is correct" $ do
      churchToInt (churchMult sevenNat nineNat) `shouldBe` churchToInt sevenNat * churchToInt nineNat

  describe "Part4" $ do
    it "fibonacci is correct" $ do
      fibonacci 7 `shouldBe` 13

    it "factorial is correct" $ do
      factorial 5 `shouldBe` 120
