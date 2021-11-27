{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Benchmark
( benchmark
, allPrograms
, programGraphs
) where

import           Control.Monad           (forM, forM_)
import           Data.List               (intercalate)
import qualified Data.Map.Lazy           as M
import           Data.String.Interpolate (i)
import           MicroC
import           MicroC.Parser           (parseFile)
import           MicroC.ProgramGraph     (PG, toPG)

benchmark :: IO ()
benchmark = do
  graphs <- programGraphs chosenPrograms
  forM_ (zip chosenPrograms graphs) $ \(name, pg) -> do
    putStrLn [i|===== #{name}.c =====\n|]
    benchAnalysis @RD pg "Reaching Definitions"
    benchAnalysis @LV pg "Live Variables"
    benchAnalysis @FV pg "Faint Variables"
    benchAnalysis @DV pg "Dangerous Variables"
    benchAnalysis @DS pg "Detection of Signs"
    benchAnalysis @IA pg "Interval Analysis"

  counts <- sequence [averageCount @DS, averageCount @FV, averageCount @LV, averageCount @DV, averageCount @IA, averageCount @RD]
  let total = M.map (`div` 6) $ M.unionsWith (+) counts

  putStrLn "========== ALL SOURCE FILES ==========\n"
  putStrLn "Average iteration count per algorithm:"
  forM_ (M.assocs total) $ \(name, avg) -> putStrLn [i|#{avg}\t#{name}|]

averageCount :: forall m. Analysis m => IO (M.Map String Int)
averageCount = do
  graphs <- programGraphs allPrograms
  pairs <- forM (zip algoNames (algos @m)) $ \(an, algo) -> do
    its <- forM graphs $ \pg -> let Solution _ its = algo pg in pure its
    pure (an, sum its)
  pure $ M.fromList pairs

benchAnalysis :: forall m. Analysis m => PG -> String -> IO ()
benchAnalysis pg analysis = do
  putStrLn analysis
  forM_ (zip algoNames (algos @m)) $ \(algoName, algo) -> do
    let Solution _ its = algo pg
    putStrLn [i|#{its}\t#{algoName}|]
  putStrLn ""

algoNames :: [String]
algoNames = ["Stack", "Queue", "Chaotic Iteration", "Post-Order Worklist", "Post-Order Worklist with pending set"]

algos :: forall m. Analysis m => [PG -> Solution m]
algos = [worklist @Stack @m, worklist @Queue @m, worklist @Chaotic @m, worklist @PostOrder @m, worklist @PendingSet @m]

-- | Returns a list of program graphs of the example programs from the "sources" folder.
programGraphs :: [String] -> IO [PG]
programGraphs programs = do
  asts <- mapM (\p -> parseFile [i|sources/#{p}.c|]) programs
  forM asts $ \case
    Right a   -> pure (toPG a)
    Left errs -> fail (intercalate "\n" errs)

-- | Programs chosen for detailed benchmarking.
chosenPrograms :: [String]
chosenPrograms =  [ "determine_sign"
                  , "even"
                  , "insertion_sort"
                  ]

-- | A list of names of the example programs from the "sources" folder.
allPrograms :: [String]
allPrograms = [ "arr_range"
              , "dangerEven"
              , "determine_sign"
              , "dfs"
              , "division"
              , "even"
              , "factorial"
              , "faint"
              , "faintEven"
              , "fibonacci"
              , "ifte"
              , "insertion_sort"
              , "intervals"
              , "monte_carlo"
              , "pos_avg"
              , "precedence"
              , "records"
              , "sum_arr"
              ]
