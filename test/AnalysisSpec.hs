{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AnalysisSpec (spec) where

import           ArbitraryInstances                  ()
import           Control.Monad                       (forM_)
import           MicroC.Analysis                     (Analysis (..))
import           MicroC.Analysis.DangerousVariables  (DV)
import           MicroC.Analysis.FaintVariables      (FV)
import           MicroC.Analysis.IntervalAnalysis    (IA)
import           MicroC.Analysis.LiveVariables       (LV)
import           MicroC.Analysis.ReachingDefinitions (RD)
import           MicroC.Parser                       (parseProgram)
import           MicroC.ProgramGraph                 (PG, toPG)
import           MicroC.Worklist                     (roundRobin)
import           System.IO.Silently                  (silence)
import           Test.Hspec
import           Test.Hspec.QuickCheck               (prop)

spec :: Spec
spec = do
  graphs <- runIO pgs
  testAnalysis @RD graphs "Reaching Definitions"
  testAnalysis @DV graphs "Dangerous Variables"
  testAnalysis @LV graphs "Live Variables"
  testAnalysis @FV graphs "Faint Variables"
  testAnalysis @IA graphs "Interval Analysis"

testAnalysis :: forall m. (Show (Result m), Analysis m) => [PG] -> String -> Spec
testAnalysis graphs name = describe name $ do
  it "terminates on test sources" $ do
    forM_ graphs $ \pg -> do
      let solution = roundRobin @m pg
      silence (print solution) `shouldReturn` ()
  prop "terminates on arbitrary programs" $ \prog -> do
    let solution = roundRobin @m $ toPG prog
    silence (print solution) `shouldReturn` ()

programs :: [String]
programs = ["danger", "even", "factorial", "faint", "fibonacci", "ifte", "intervals", "precedence", "records"]

pgs :: IO [PG]
pgs = map (\(Right a) -> toPG a) <$> mapM (\p -> parseProgram $ "sources/" <> p <> ".c") programs
