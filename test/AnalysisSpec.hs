{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AnalysisSpec (spec) where

import           ArbitraryInstances                  ()
import           Common
import           Control.Monad                       (forM_)
import           Control.Monad.IO.Class              (liftIO)
import qualified Data.Map.Lazy                       as M
import           Data.String.Interpolate             (i)
import           MicroC.Analysis                     (Analysis (..))
import           MicroC.Analysis.DangerousVariables  (DV)
import           MicroC.Analysis.FaintVariables      (FV)
import           MicroC.Analysis.IntervalAnalysis
import           MicroC.Analysis.LiveVariables       (LV)
import           MicroC.Analysis.ReachingDefinitions (RD)
import           MicroC.ID                           (lval2ID)
import           MicroC.Parser                       (parseFile)
import           MicroC.ProgramGraph                 (PG, toPG)
import           MicroC.Worklist                     (roundRobin)
import           System.IO.Silently                  (silence)
import           Test.Hspec
import           Test.Hspec.QuickCheck               (prop)

spec :: Spec
spec = do
  graphs <- runIO programGraphs
  testTermination @RD graphs "Reaching Definitions"
  testTermination @DV graphs "Dangerous Variables"
  testTermination @LV graphs "Live Variables"
  testTermination @FV graphs "Faint Variables"
  testTermination @IA graphs "Interval Analysis"

  testIACorrectness

testTermination :: forall m. (Show (Result m), Analysis m) => [PG] -> String -> Spec
testTermination graphs name = describe name $ do
  it "terminates on test sources" $ do
    forM_ graphs $ \pg -> do
      let solution = roundRobin @m pg
      silence (print solution) `shouldReturn` ()
  prop "terminates on arbitrary programs" $ \prog -> do
    let pg = toPG prog
        solution = roundRobin @m pg
    silence (print solution) `shouldReturn` ()

testIACorrectness :: Spec
testIACorrectness = describe "memory after running consistent with Interval Analysis" $ do
  forM_ programs $ \prog -> do
    it [i|#{prog}.c|] $ do
      Right ast <- liftIO $ parseFile [i|sources/#{prog}.c|]
      let pg = toPG ast
          Union lastState = roundRobin @IA pg M.! (-1)
          mem = fst $ runWithInput ast "42"
      forM_ (M.assocs mem) $ \(lv, v) -> do
        let id' = lval2ID lv
        v `shouldSatisfy` inside (lastState M.! id')
  where
    inside :: Interval -> Int -> Bool
    inside Bottom _        = False
    inside (Between a b) x = a <= Int x && Int x <= b
