{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MicroC.Benchmark
( benchmark
) where

import           Control.Monad
import           Data.List                           (intercalate)
import           Data.String.Interpolate             (i)
import           MicroC.Analysis
import           MicroC.Analysis.DangerousVariables  (DV)
import           MicroC.Analysis.DetectionOfSigns    (DS)
import           MicroC.Analysis.FaintVariables      (FV)
import           MicroC.Analysis.IntervalAnalysis    (IA)
import           MicroC.Analysis.LiveVariables       (LV)
import           MicroC.Analysis.ReachingDefinitions (RD)
import           MicroC.Parser
import           MicroC.ProgramGraph
import           MicroC.Worklist.ChaoticIteration    (Chaotic)
import           MicroC.Worklist.PostOrder
import           MicroC.Worklist.PendingSet    (PendingSet)
import           MicroC.Worklist.Queue               (Queue)
import           MicroC.Worklist.Stack               (Stack)

benchmark :: IO ()
benchmark = do
  graphs <- programGraphs
  forM_ graphs $ \(name, pg) -> do
    putStrLn [i|===== #{name}.c =====\n|]
    benchAnalysis @RD pg "Reaching Definitions"
    benchAnalysis @DV pg "Dangerous Variables"
    benchAnalysis @LV pg "Live Variables"
    benchAnalysis @FV pg "Faint Variables"
    benchAnalysis @DS pg "Detection of Signs"
    benchAnalysis @IA pg "Interval Analysis"

benchAnalysis :: forall m. Analysis m => PG -> String -> IO ()
benchAnalysis pg analysis = do
  putStrLn analysis
  forM_ (zip names algos) $ \(algoName, algo) -> do
    let Solution _ its = algo pg
    putStrLn [i|#{its}\t#{algoName}|]
  putStrLn ""
  where
    names = ["Stack", "Queue", "Chaotic Iteration", "Post-Order Worklist with pending set"]
    algos = [worklist @Stack @m, worklist @Queue @m, worklist @Chaotic @m, worklist @PendingSet @m]

-- | Returns a list of program graphs of the example programs from the "sources" folder.
programGraphs :: IO [(String, PG)]
programGraphs = do
  asts <- zip programs <$> mapM (\p -> parseFile [i|sources/#{p}.c|]) programs
  forM asts $ \(name, ast) -> case ast of
    Right a   -> pure (name, toPG a)
    Left errs -> fail (intercalate "\n" errs)

programs :: [String]
programs =  [ "even"
            , "factorial"
            , "insertion_sort"
            ]
