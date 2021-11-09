{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Lens                     ((^.))
import           Control.Monad                    (forM_, void)
import           Control.Monad.IO.Class           (MonadIO (..))
import qualified Data.Map                         as M
import           MicroC.Analysis                  (Analysis (Result))
import           MicroC.Analysis.DetectionOfSigns (DS)
import           MicroC.Benchmark                 (benchmark)
import           MicroC.DFS
import           MicroC.Interpreter               (MonadEval (..), evalProgram)
import           MicroC.Parser                    (parseFile)
import           MicroC.ProgramGraph              (Edge, toPG)
import           MicroC.Worklist.PendingSet

newtype IOEval a = IOEval {runIO :: IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadEval IOEval where
  evalRead = liftIO readLn
  evalWrite = IOEval . print

analyseFile :: forall m. (Analysis m, Show (Result m)) => FilePath -> IO ()
analyseFile path = do
  prog <- parseFile $ "sources/" <> path <> ".c"
  case prog of
    Left errs -> mapM_ putStrLn errs
    Right ast -> do
      let pg = toPG ast
          Solution sol its = worklist @PendingSet @m pg
          tree = dfs 0 pg -- 0 because it's a forward analysis
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
      putStrLn "Interpreter:"
      void $ runIO (evalProgram ast)
  where
    printEdge :: Edge -> String
    printEdge (qs, a, qe) = show qs <> " -> " <> show qe <> " :: " <> show a

main :: IO ()
main = benchmark >> analyseFile @DS "insertion_sort"
