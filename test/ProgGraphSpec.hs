module ProgGraphSpec (spec) where

import           ArbitraryInstances           ()
import qualified Data.Set                     as S
import           Language.MicroC.AST
import           Language.MicroC.ProgramGraph
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = describe "Program graph" $ do
  it "should work for an empty program" $ toPG (Program [] []) `shouldSatisfy` null
  prop "should process single declaration" $ \d -> do
    let [(0, DeclAction x, -1)] = toPG (Program [d] [])
    x `shouldBe` d
  prop "should process single statements" $ \s -> do
    let pg = toPG (Program [] [s])
    -- anything but assigning to (a nonexistent) record should generate some edges
    pg `shouldSatisfy` case s of
      RecordAssignment _ _ -> null
      _                    -> not . null
    -- states should be continuous
    let states = S.toList $ allStates pg
    states `shouldBe` [-1 .. length states - 2]

-- IfThenElse (Literal True) [] []
--        expected: [-1,0,1]
--         but got: [0,1,2]
