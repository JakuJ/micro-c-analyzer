{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RunAnalysis
( runAnalysis
) where

import           ArgParse
import           Benchmark               (benchmark)
import           Control.Lens            ((^.))
import           Control.Monad           (forM_, when)
import qualified Data.Map                as M
import           Data.String.Interpolate (i)
import           MicroC
import           MicroC.DFS              (dfs, edges, orderStates)
import           MicroC.Parser           (parseFile)
import           MicroC.ProgramGraph     (Edge, toPG)

runAnalysis :: Args -> IO ()
runAnalysis Benchmark = benchmark
runAnalysis (Analyse an algo path) = case an of
  FaintVariables      -> analyse @FV
  DangerousVariables  -> analyse @DV
  DetectionOfSigns    -> analyse @DS
  IntervalAnalysis    -> analyse @IA
  LiveVariables       -> analyse @LV
  ReachingDefinitions -> analyse @RD
  where
    analyse :: forall m. (Analysis m, Show (Result m)) => IO ()
    analyse = analyseFile @m algo path

analyseFile :: forall m. (Analysis m, Show (Result m)) => AlgorithmType -> FilePath -> IO ()
analyseFile algoT path = do
  let algo = chooseAlgorithm @m algoT
  prog <- parseFile path
  case prog of
    Left errs -> do
      putStrLn [i|Errors occurred in the source file #{path}\n|]
      forM_ errs $ \e -> putStrLn [i|- #{e}|]
    Right ast -> do
      let pg = toPG ast
          Solution sol its = algo pg
          s0 = if direction @m == Forward then 0 else -1
          pg' = if direction @m == Forward then pg else map (\(a, b, c) -> (c, b, a)) pg
          tree = dfs s0 pg'
      putStrLn "Program Graph:"
      mapM_ (putStrLn . printEdge) pg
      when (algoT `elem` [PostOrderWorklist, PendingSetWorklist]) $ do
        putStrLn "\nDepth First Spanning Tree:"
        print $ tree ^. edges
        putStrLn "\nReverse postorder:"
        print $ orderStates tree
      putStrLn "\nSOLUTION:"
      case M.toList sol of
        []      -> print "Program is empty"
        (h : t) -> forM_ (t ++ [h]) $ \(st, lv) -> putStrLn $ show st <> "\t" <> show lv
      when (its /= -1) $ do
        putStr "Number of iterations: "
        print its
  where
    printEdge :: Edge -> String
    printEdge (qs, a, qe) = show qs <> " -> " <> show qe <> " :: " <> show a

chooseAlgorithm :: AlgorithmType -> WorklistAlgorithm m
chooseAlgorithm = \case
  NaiveAlgorithm     -> naiveIterative
  ChaoticWorklist    -> worklist @Chaotic
  QueueWorklist      -> worklist @Queue
  StackWorklist      -> worklist @Stack
  PostOrderWorklist  -> worklist @PostOrder
  PendingSetWorklist -> worklist @PendingSet
