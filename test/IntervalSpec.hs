module IntervalSpec (spec) where

import           Control.Exception (evaluate)
import           Data.ExtendedReal
import           Test.Hspec

spec :: Spec
spec = parallel $ do
  -- as per figure 5.17 in "Program Analysis: an Appetizer"
  it "extended addition" $ do
    NegInf + NegInf `shouldBe` NegInf
    NegInf + Finite 5 `shouldBe` NegInf
    evaluate (NegInf + PosInf) `shouldThrow` anyException
    Finite 3 + NegInf `shouldBe` NegInf
    Finite 8 + Finite (-2) `shouldBe` Finite 6
    Finite 1 + PosInf `shouldBe` PosInf
    evaluate (PosInf + NegInf) `shouldThrow` anyException
    PosInf + Finite 6 `shouldBe` PosInf
    PosInf + PosInf `shouldBe` PosInf

  it "extended subtraction" $ do
    evaluate (NegInf - NegInf) `shouldThrow` anyException
    NegInf - Finite 5 `shouldBe` NegInf
    NegInf - PosInf `shouldBe` NegInf
    Finite 3 - NegInf `shouldBe` PosInf
    Finite 8 - Finite (-2) `shouldBe` Finite 10
    Finite 1 - PosInf `shouldBe` NegInf
    PosInf - NegInf `shouldBe` PosInf
    PosInf - Finite 6 `shouldBe` PosInf
    evaluate (PosInf - PosInf) `shouldThrow` anyException

  it "extended multiplication" $ do
    NegInf * NegInf `shouldBe` PosInf
    NegInf * Finite 5 `shouldBe` NegInf
    NegInf * Finite (-5) `shouldBe` PosInf
    NegInf * PosInf `shouldBe` NegInf
    Finite 3 * NegInf `shouldBe` NegInf
    Finite (-3) * NegInf `shouldBe` PosInf
    Finite 8 * Finite (-2) `shouldBe` Finite (-16)
    Finite 1 * PosInf `shouldBe` PosInf
    Finite (-1) * PosInf `shouldBe` NegInf
    PosInf * NegInf `shouldBe` NegInf
    PosInf * Finite 6 `shouldBe` PosInf
    PosInf * Finite (-6) `shouldBe` NegInf
    PosInf * PosInf `shouldBe` PosInf
    PosInf * 0 `shouldBe` 0
    NegInf * 0 `shouldBe` 0
    0 * PosInf `shouldBe` 0
    0 * NegInf `shouldBe` 0
