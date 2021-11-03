{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AnalysisSpec (spec) where

import           ArbitraryInstances                  ()
import           Common
import           Control.Lens                        ((^.))
import           Control.Monad
import           Control.Monad.IO.Class              (liftIO)
import           Data.Function                       (on)
import           Data.IntegerInterval                (member)
import           Data.List                           (subsequences)
import qualified Data.Map.Lazy                       as M
import           Data.String.Interpolate             (i)
import           MicroC.Analysis
import           MicroC.Analysis.DangerousVariables  (DV)
import           MicroC.Analysis.DetectionOfSigns    (DS)
import           MicroC.Analysis.FaintVariables      (FV)
import           MicroC.Analysis.IntervalAnalysis
import           MicroC.Analysis.LiveVariables       (LV)
import           MicroC.Analysis.ReachingDefinitions (RD)
import           MicroC.ID                           (lval2ID)
import           MicroC.Parser                       (parseFile)
import           MicroC.ProgramGraph                 (PG, toPG)
import           MicroC.Worklist                     (solution, worklist)
import           MicroC.Worklist.ChaoticIteration    (Chaotic)
import           MicroC.Worklist.Queue               (Queue)
import           MicroC.Worklist.RoundRobin          (roundRobin)
import           MicroC.Worklist.Stack               (Stack)
import           System.IO.Silently                  (silence)
import           Test.Hspec
import           Test.Hspec.QuickCheck               (prop)

spec :: Spec
spec = do
  graphs <- runIO programGraphs
  testAnalysis @RD graphs "Reaching Definitions"
  testAnalysis @DV graphs "Dangerous Variables"
  testAnalysis @LV graphs "Live Variables"
  testAnalysis @FV graphs "Faint Variables"
  testAnalysis @DS graphs "Detection of Signs"
  testAnalysis @IA graphs "Interval Analysis"
  testIACorrectness

testAnalysis :: forall m. (Show (Result m), Eq (Result m), Analysis m) => [PG] -> String -> Spec
testAnalysis graphs name = describe name $ do
  forM_ (zip names algos) $ \(algoName, algo) -> describe algoName $ do
    it "terminates on test sources" $ do
      forM_ graphs $ \pg -> do
        let sol = algo pg ^. solution
        silence (print sol) `shouldReturn` ()
    prop "terminates on arbitrary programs" $ \prog -> do
      let pg = toPG prog
          sol = algo pg ^. solution
      silence (print sol) `shouldReturn` ()
  parallel $ describe "different worklist algos return the same results" $ do
    let results = map (`map` graphs) algos
        comp = zipWithM_ sameResult
        combinations = filter ((==2) . length) . subsequences $ zip names results
    forM_ combinations $ \[(n1, r1), (n2, r2)] -> it [i|#{n1} = #{n2}|] $ comp r1 r2
  where
    sameResult = shouldBe `on` (^. solution)
    names = ["Round Robin", "Stack", "Queue", "Chaotic Iteration"]
    algos = [roundRobin @m, worklist @Stack @m, worklist @Queue @m, worklist @Chaotic @m]

testIACorrectness :: Spec
testIACorrectness = describe "memory after running consistent with Interval Analysis" $ do
  forM_ programs $ \prog -> do
    it [i|#{prog}.c|] $ do
      Right ast <- liftIO $ parseFile [i|sources/#{prog}.c|]
      let pg = toPG ast
          Abs lastState = (roundRobin @IA pg ^. solution) M.! (-1)
          mem = fst $ runWithInput ast "42"
      forM_ (M.assocs mem) $ \(lv, v) -> do
        v `shouldSatisfy` (\k -> toInteger k `member` (lastState M.! lval2ID lv))
