{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AnalysisSpec (spec) where

import           ArbitraryInstances                  ()
import           Common
import           Control.Lens                        ((^.))
import           Control.Monad
import           Control.Monad.IO.Class              (liftIO)
import           Data.IntegerInterval                (member)
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
  it "terminates on test sources" $ do
    forM_ graphs $ \pg -> do
      let sol = roundRobin @m pg ^. solution
      silence (print sol) `shouldReturn` ()
  prop "terminates on arbitrary programs" $ \prog -> do
    let pg = toPG prog
        sol = roundRobin @m pg ^. solution
    silence (print sol) `shouldReturn` ()
  it "different worklist algos return the same results" $ do
    forM_ graphs $ \pg -> do
      let solRR = roundRobin @m pg ^. solution
          solStack = worklist @m @Stack pg ^. solution
          solQueue = worklist @m @Queue pg ^. solution
          solChaotic = worklist @m @Chaotic pg ^. solution
      solRR `shouldBe` solStack
      solRR `shouldBe` solQueue
      solRR `shouldBe` solChaotic

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
