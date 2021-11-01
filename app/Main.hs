{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Monad                    (forM_, void)
import           Control.Monad.IO.Class           (MonadIO (..))
import qualified Data.Map                         as M
import           MicroC.Analysis                  (Analysis (Result))
import           MicroC.Analysis.DetectionOfSigns (DS)
import           MicroC.Interpreter               (MonadEval (..), evalProgram)
import           MicroC.Parser                    (parseFile)
import           MicroC.ProgramGraph              (Edge, toPG)
import           MicroC.Worklist
import           MicroC.Worklist.ChaoticIteration
import           MicroC.Worklist.Stack
import           MicroC.Worklist.Queue



newtype IOEval a = IOEval {runIO :: IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadEval IOEval where
  evalRead = liftIO readLn
  evalWrite = IOEval . print

analyseFile :: forall m. (Analysis m, Show (Result m), Eq (Result m)) => FilePath -> IO ()
analyseFile path = do
  prog <- parseFile $ "sources/" <> path <> ".c"
  case prog of
    Left errs -> mapM_ putStrLn errs
    Right ast -> do
      let pg = toPG ast
          Solution solution iters = worklist @m @Queue pg
      putStrLn "AST:"
      print ast
      putStrLn "PG:"
      mapM_ (putStrLn . printEdge) pg
      putStrLn "SOLUTION: "
      case M.toList solution of
        []      -> print "Program is empty"
        (h : t) -> forM_ (t ++ [h]) $ \(st, lv) -> putStrLn $ show st <> "\t" <> show lv
      putStr "Number of iterations: "
      print iters
      putStrLn "Interpreter:"
      void $ runIO (evalProgram ast)
  where
    printEdge :: Edge -> String
    printEdge (qs, a, qe) = show qs <> " -> " <> show qe <> " :: " <> show a

main :: IO ()
main = analyseFile @DS "pos_avg"
