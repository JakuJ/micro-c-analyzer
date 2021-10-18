module ProgGraphSpec (spec) where

import           ArbitraryInstances    ()
import           Control.Lens
import           Control.Monad         (forM_, unless)
import           Data.Either           (fromRight, isLeft, isRight)
import qualified Data.Set              as S
import           MicroC.AST
import           MicroC.Parser         (parseProgram)
import           MicroC.ProgramGraph
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (discard)

spec :: Spec
spec = parallel $ do
  describe "unit" $ do
    it "empty program" $ let Right pg = toPG (Program [] []) in pg `shouldSatisfy` null
    forM_ testCases $ \(msg, Right pg, ex) ->
      it msg $ do
        wellFormed $ Right pg
        pg `shouldBe` ex
    it "assignment to undefined record" $ do
      let prog = "R := (1, 2);"
          Right ast = parseProgram prog
      toPG ast `shouldSatisfy` isLeft
    it "assignment to undefined record field" $ do
      let prog = "R.fst := 42;"
          Right ast = parseProgram prog
      toPG ast `shouldSatisfy` isLeft

  describe "properties" $ do
    prop "well-formed declarations" $ \ds -> do
      unless (null ds) . wellFormed . toPG $ Program ds []
    prop "well-formed programs" $ \prog@(Program ds ss) -> do
      if null ds && null ss
        then discard
        else wellFormed . toPG $ prog

wellFormed :: Either Diagnostics PG -> Expectation
wellFormed rpg = do
  rpg `shouldSatisfy` isRight
  let Right pg = rpg
      states = S.toList $ allStates pg
  pg `shouldSatisfy` not . null
  states `shouldBe` [-1 .. length states - 2]

testCases :: [(String, Either Diagnostics PG, PG)]
testCases = cases & traverse . _2 %~ toPG . fromRight undefined . parseProgram
  where
    x = Variable "x"
    cases = [ ("assignment"
              , "x := 5;"
              , [(0, AssignAction x (Literal 5), -1)])
            , ("record assignment"
              , "{int fst; int snd} x; x := (1,2);"
              , [ (0, DeclAction (RecordDecl "x" ["fst", "snd"]), 1)
                , (1, AssignAction (FieldAccess "x" "fst") (Literal 1), 2)
                , (2, AssignAction (FieldAccess "x" "snd") (Literal 2), -1)])
            , ("empty if-then"
              , "if (true) {}"
              , [ (0, BoolAction (Not (Literal True)), -1)
                , (0, BoolAction (Literal True), -1)])
            , ("if-then"
              , "if (true) {read x;}"
              , [ (0, BoolAction (Not (Literal True)), -1)
                , (0, BoolAction (Literal True), 1)
                , (1, ReadAction x, -1)])
            , ("if-then-else w/o then"
              , "if (true) {} else {read x;}"
              , [ (0, BoolAction (Literal True), -1)
                , (0, BoolAction (Not (Literal True)), 1)
                , (1, ReadAction x, -1)])
            , ("if-then-else w/o else"
              , "if (true) {read x;} else {}"
              , [ (0, BoolAction (Literal True), 1)
                , (1, ReadAction x, -1)
                , (0, BoolAction (Not (Literal True)), -1)])
            , ("empty if-then-else"
              , "if (true) {} else {}"
              , [ (0, BoolAction (Literal True), -1)
                , (0, BoolAction (Not (Literal True)), -1)])
            , ("if-then-else"
              , "if (true) {read x;} else {write 4;}"
              , [ (0, BoolAction (Literal True), 1)
                , (1, ReadAction x, -1)
                , (0, BoolAction (Not (Literal True)), 2)
                , (2, WriteAction (Literal 4), -1)])
            , ("empty while"
              , "while (true) {}"
              , [ (0, BoolAction (Not (Literal True)), -1)
                , (0, BoolAction (Literal True), 0)])
            , ("while"
              , "while (true) {read x;}"
              , [ (0, BoolAction (Not (Literal True)), -1)
                , (0, BoolAction (Literal True), 1)
                , (1, ReadAction x, 0)])
            ]
