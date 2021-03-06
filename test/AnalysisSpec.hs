{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AnalysisSpec (spec) where

import           ArbitraryInstances      ()
import           Benchmark
import           Control.Lens            ((^.))
import           Control.Monad
import           Data.Function           (on)
import           Data.String.Interpolate (i)
import           MicroC
import           MicroC.ProgramGraph     (PG, toPG)
import           System.IO.Silently      (silence)
import           Test.Hspec
import           Test.Hspec.QuickCheck   (modifyMaxSize, prop)

spec :: Spec
spec = do
  graphs <- runIO $ programGraphs allPrograms
  testAnalysis @RD graphs "Reaching Definitions"
  testAnalysis @DV graphs "Dangerous Variables"
  testAnalysis @LV graphs "Live Variables"
  testAnalysis @FV graphs "Faint Variables"
  testAnalysis @DS graphs "Detection of Signs"
  testAnalysis @IA graphs "Interval Analysis"

testAnalysis :: forall m. (Show (Result m), Eq (Result m), Analysis m) => [PG] -> String -> Spec
testAnalysis graphs name = describe name $ do
  forM_ (zip names algos) $ \(algoName, algo) -> describe algoName $ do
    it "terminates on test sources" $ do
      forM_ graphs $ \pg -> do
        let sol = algo pg ^. solution
        silence (print sol) `shouldReturn` ()
    modifyMaxSize (const 50) $ prop "terminates on arbitrary programs" $ \prog -> do
        let pg = toPG prog
            sol = algo pg ^. solution
        silence (print sol) `shouldReturn` ()
  parallel $ describe "different worklist algos return the same results" $ do
    let results = map (`map` graphs) algos
        comp = zipWithM_ sameResult
        combinations = let xs = zip names results in zip xs (tail xs)
    forM_ combinations $ \((n1, r1), (n2, r2)) -> it [i|#{n1} = #{n2}|] $ comp r1 r2
  where
    sameResult = shouldBe `on` (^. solution)
    names = ["Naive Round Robin", "Stack", "Queue", "Chaotic Iteration", "Simple Post-Order", "Post-Order with Pending Set"]
    algos = [naiveIterative @m, worklist @Stack @m, worklist @Queue @m, worklist @Chaotic @m, worklist @PostOrder @m, worklist @PendingSet @m]
