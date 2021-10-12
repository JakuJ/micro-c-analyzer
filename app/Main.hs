{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Monad                             (forM_, void)
import           Control.Monad.IO.Class                    (MonadIO (..))
import qualified Data.Map                                  as M
import           Language.MicroC.Analysis                  (Analysis (Result))
import           Language.MicroC.Analysis.IntervalAnalysis (IA)
import           Language.MicroC.Interpreter               (MonadEval (..),
                                                            evalProgram)
import           Language.MicroC.Parser                    (parseProgram)
import           Language.MicroC.ProgramGraph              (Edge, toPG)
import           Language.MicroC.Worklist                  (roundRobin)

newtype IOEval a = IOEval {runIO :: IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadEval IOEval where
  evalRead = liftIO readLn
  evalWrite = IOEval . print

analyseFile :: forall m. (Analysis m, Show (Result m)) => FilePath -> IO ()
analyseFile path = do
  prog <- parseProgram $ "sources/" <> path <> ".c"
  case prog of
    Left err  -> putStrLn $ "ERROR :: " <> err
    Right ast -> do
      let pg = toPG ast
          solution = roundRobin @m pg
      putStrLn "AST:"
      print ast
      putStrLn "PG:"
      mapM_ (putStrLn . printEdge) pg
      putStrLn "SOLUTION: "
      case M.toList solution of
        []      -> print "Program is empty"
        (h : t) -> forM_ (t ++ [h]) $ \(st, lv) -> putStrLn $ show st <> "\t" <> show lv
      putStrLn "Interpreter:"
      void $ runIO (evalProgram ast)
  where
    printEdge :: Edge -> String
    printEdge (qs, a, qe) = show qs <> " -> " <> show qe <> " :: " <> show a

main :: IO ()
main = analyseFile @IA "even"
