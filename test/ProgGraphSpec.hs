module ProgGraphSpec (spec) where

import           ArbitraryInstances    ()
import           Control.Lens
import           Control.Monad
import qualified Data.Set              as S
import           MicroC.AST
import           MicroC.ProgramGraph
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck       ()

spec :: Spec
spec = parallel $ do
  describe "unit" $ do
    it "empty program" $ let Right pg = toPG (Program [] []) in pg `shouldSatisfy` null
    forM_ testCases $ \(msg, Right pg, ex) ->
      it msg $ do
        wellFormed $ Right pg
        pg `shouldBe` ex

  describe "properties" $ do
    prop "well-formed declarations" $ \ds -> do
      unless (null ds) . wellFormed . toPG $ Program ds []
    prop "well-formed statements" $ \ss -> do
      unless (null ss) . wellFormed . toPG $ Program [] ss
    prop "well-formed programs" $ \prog@(Program ds ss) -> do
      unless (null ds && null ss) . wellFormed . toPG $ prog

wellFormed :: Either a PG -> Expectation
wellFormed (Right pg) = do
  let states = S.toList $ allStates pg
  pg `shouldSatisfy` not . null
  states `shouldBe` [-1 .. length states - 2]

testCases :: [(String, Either Diagnostics PG, PG)]
testCases = cases & traverse . _2 %~ toPG
  where
    x = Variable "x"
    cases = [ ("assignment"
              , Program [] [Assignment x (Literal 5)]
              , [(0, AssignAction x (Literal 5), -1)])
            , ("record assignment"
              , Program [RecordDecl "x" ["fst", "snd"]] [RecordAssignment "x" [Literal 1, Literal 2]]
              , [ (0, DeclAction (RecordDecl "x" ["fst", "snd"]), 1)
                , (1, AssignAction (FieldAccess "x" "fst") (Literal 1), 2)
                , (2, AssignAction (FieldAccess "x" "snd") (Literal 2), -1)])
            , ("empty if-then"
              , Program [] [IfThen (Literal True) []]
              , [ (0, BoolAction (Not (Literal True)), -1)
                , (0, BoolAction (Literal True), -1)])
            , ("if-then"
              , Program [] [IfThen (Literal True) [Read x]]
              , [ (0, BoolAction (Not (Literal True)), -1)
                , (0, BoolAction (Literal True), 1)
                , (1, ReadAction x, -1)])
            , ("if-then-else w/o then"
              , Program [] [IfThenElse (Literal True) [] [Read x]]
              , [ (0, BoolAction (Literal True), -1)
                , (0, BoolAction (Not (Literal True)), 1)
                , (1, ReadAction x, -1)])
            , ("if-then-else w/o else"
              , Program [] [IfThenElse (Literal True) [Read x] []]
              , [ (0, BoolAction (Literal True), 1)
                , (1, ReadAction x, -1)
                , (0, BoolAction (Not (Literal True)), -1)])
            , ("empty if-then-else"
              , Program [] [IfThenElse (Literal True) [] []]
              , [ (0, BoolAction (Literal True), -1)
                , (0, BoolAction (Not (Literal True)), -1)])
            , ("if-then-else"
              , Program [] [IfThenElse (Literal True) [Read x] [Write (Literal 4)]]
              , [ (0, BoolAction (Literal True), 1)
                , (1, ReadAction x, -1)
                , (0, BoolAction (Not (Literal True)), 2)
                , (2, WriteAction (Literal 4), -1)])
            , ("empty while"
              , Program [] [While (Literal True) []]
              , [ (0, BoolAction (Not (Literal True)), -1)
                , (0, BoolAction (Literal True), 0)])
            , ("while"
              , Program [] [While (Literal True) [Read x]]
              , [ (0, BoolAction (Not (Literal True)), -1)
                , (0, BoolAction (Literal True), 1)
                , (1, ReadAction x, 0)])
            ]
