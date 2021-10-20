module IntervalSpec (spec) where

import           Control.Exception                (evaluate)
import           MicroC.Analysis.IntervalAnalysis
import           Test.Hspec

spec :: Spec
spec = parallel $ do
  -- as per figure 5.17 in "Program Analysis: an Appetizer"
  it "extended addition" $ do
    NegInf + NegInf `shouldBe` NegInf
    NegInf + Int 5 `shouldBe` NegInf
    evaluate (NegInf + Inf) `shouldThrow` anyException
    Int 3 + NegInf `shouldBe` NegInf
    Int 8 + Int (-2) `shouldBe` Int 6
    Int 1 + Inf `shouldBe` Inf
    evaluate (Inf + NegInf) `shouldThrow` anyException
    Inf + Int 6 `shouldBe` Inf
    Inf + Inf `shouldBe` Inf

  it "extended subtraction" $ do
    evaluate (NegInf - NegInf) `shouldThrow` anyException
    NegInf - Int 5 `shouldBe` NegInf
    NegInf - Inf `shouldBe` NegInf
    Int 3 - NegInf `shouldBe` Inf
    Int 8 - Int (-2) `shouldBe` Int 10
    Int 1 - Inf `shouldBe` NegInf
    Inf - NegInf `shouldBe` Inf
    Inf - Int 6 `shouldBe` Inf
    evaluate (Inf - Inf) `shouldThrow` anyException

  it "extended multiplication" $ do
    NegInf * NegInf `shouldBe` Inf
    NegInf * Int 5 `shouldBe` NegInf
    NegInf * Int (-5) `shouldBe` Inf
    NegInf * Inf `shouldBe` NegInf
    Int 3 * NegInf `shouldBe` NegInf
    Int (-3) * NegInf `shouldBe` Inf
    Int 8 * Int (-2) `shouldBe` Int (-16)
    Int 1 * Inf `shouldBe` Inf
    Int (-1) * Inf `shouldBe` NegInf
    Inf * NegInf `shouldBe` NegInf
    Inf * Int 6 `shouldBe` Inf
    Inf * Int (-6) `shouldBe` NegInf
    Inf * Inf `shouldBe` Inf
    Inf * 0 `shouldBe` 0
    NegInf * 0 `shouldBe` 0
    0 * Inf `shouldBe` 0
    0 * NegInf `shouldBe` 0
