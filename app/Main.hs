{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           ArgParse
import           Control.Lens                        ((^.))
import           Control.Monad                       (forM_)
import qualified Data.Map                            as M
import           MicroC.Analysis
import           MicroC.Analysis.DangerousVariables  (DV)
import           MicroC.Analysis.DetectionOfSigns    (DS)
import           MicroC.Analysis.FaintVariables      (FV)
import           MicroC.Analysis.IntervalAnalysis    (IA)
import           MicroC.Analysis.LiveVariables       (LV)
import           MicroC.Analysis.ReachingDefinitions (RD)
import           MicroC.Benchmark                    (benchmark)
import           MicroC.DFS
import           MicroC.Parser                       (parseFile)
import           MicroC.ProgramGraph                 (Edge, PG, toPG)
import           MicroC.Worklist.ChaoticIteration    (Chaotic)
import           MicroC.Worklist.Naive               (naiveIterative)
import           MicroC.Worklist.PendingSet          (PendingSet)
import           MicroC.Worklist.PostOrder           (PostOrder)
import           MicroC.Worklist.Queue               (Queue)
import           MicroC.Worklist.Stack

analyseFile :: forall m. (Analysis m, Show (Result m)) => (PG -> Solution m) -> FilePath -> IO ()
analyseFile algo path = do
  prog <- parseFile path
  case prog of
    Left errs -> mapM_ putStrLn errs
    Right ast -> do
      let pg = toPG ast
          Solution sol its = algo pg
          s0 = if direction @m == Forward then 0 else -1
          tree = dfs s0 pg
      putStrLn "AST:"
      print ast
      putStrLn "PG:"
      mapM_ (putStrLn . printEdge) pg
      putStrLn "Reverse postorder:"
      print $ orderStates tree
      putStrLn "Depth First Spanning Tree:"
      print $ tree ^. edges
      putStrLn "SOLUTION: "
      case M.toList sol of
        []      -> print "Program is empty"
        (h : t) -> forM_ (t ++ [h]) $ \(st, lv) -> putStrLn $ show st <> "\t" <> show lv
      putStr "Number of iterations: "
      print its
  where
    printEdge :: Edge -> String
    printEdge (qs, a, qe) = show qs <> " -> " <> show qe <> " :: " <> show a

-- TODO: Refactor this wall of shame
main :: IO ()
main = do
  mode <- parseArgs
  case mode of
    Benchmark               -> benchmark
    Analyse an algoT path   -> case algoT of
      NaiveAlgorithm        -> case an of
        DetectionOfSigns    -> analyseFile (naiveIterative @DS) path
        FaintVariables      -> analyseFile (naiveIterative @FV) path
        DangerousVariables  -> analyseFile (naiveIterative @DV) path
        IntervalAnalysis    -> analyseFile (naiveIterative @IA) path
        LiveVariables       -> analyseFile (naiveIterative @LV) path
        ReachingDefinitions -> analyseFile (naiveIterative @RD) path
      ChaoticWorklist       -> case an of
        DetectionOfSigns    -> analyseFile (worklist @Chaotic @DS) path
        FaintVariables      -> analyseFile (worklist @Chaotic @FV) path
        DangerousVariables  -> analyseFile (worklist @Chaotic @DV) path
        IntervalAnalysis    -> analyseFile (worklist @Chaotic @IA) path
        LiveVariables       -> analyseFile (worklist @Chaotic @LV) path
        ReachingDefinitions -> analyseFile (worklist @Chaotic @RD) path
      StackWorklist         -> case an of
        DetectionOfSigns    -> analyseFile (worklist @Stack @DS) path
        FaintVariables      -> analyseFile (worklist @Stack @FV) path
        DangerousVariables  -> analyseFile (worklist @Stack @DV) path
        IntervalAnalysis    -> analyseFile (worklist @Stack @IA) path
        LiveVariables       -> analyseFile (worklist @Stack @LV) path
        ReachingDefinitions -> analyseFile (worklist @Stack @RD) path
      QueueWorklist         -> case an of
        DetectionOfSigns    -> analyseFile (worklist @Queue @DS) path
        FaintVariables      -> analyseFile (worklist @Queue @FV) path
        DangerousVariables  -> analyseFile (worklist @Queue @DV) path
        IntervalAnalysis    -> analyseFile (worklist @Queue @IA) path
        LiveVariables       -> analyseFile (worklist @Queue @LV) path
        ReachingDefinitions -> analyseFile (worklist @Queue @RD) path
      PostOrderWorklist     -> case an of
        DetectionOfSigns    -> analyseFile (worklist @PostOrder @DS) path
        FaintVariables      -> analyseFile (worklist @PostOrder @FV) path
        DangerousVariables  -> analyseFile (worklist @PostOrder @DV) path
        IntervalAnalysis    -> analyseFile (worklist @PostOrder @IA) path
        LiveVariables       -> analyseFile (worklist @PostOrder @LV) path
        ReachingDefinitions -> analyseFile (worklist @PostOrder @RD) path
      PendingSetWorklist    -> case an of
        DetectionOfSigns    -> analyseFile (worklist @PendingSet @DS) path
        FaintVariables      -> analyseFile (worklist @PendingSet @FV) path
        DangerousVariables  -> analyseFile (worklist @PendingSet @DV) path
        IntervalAnalysis    -> analyseFile (worklist @PendingSet @IA) path
        LiveVariables       -> analyseFile (worklist @PendingSet @LV) path
        ReachingDefinitions -> analyseFile (worklist @PendingSet @RD) path
