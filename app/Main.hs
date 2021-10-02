{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import           Control.Monad                          (forM_)
import           Control.Monad.IO.Class                 (MonadIO (..))
import           Data.Foldable                          (toList)
import qualified Data.Map                               as M
import           Language.MicroC.Analysis               (Analysis (gen, kill))
import           Language.MicroC.Analysis.LiveVariables (runLV)
import           Language.MicroC.Interpreter            (MonadEval (..))
import           Language.MicroC.Parser                 (parseProgram)
import           Language.MicroC.ProgramGraph           (Edge, toPG)
import           Language.MicroC.Worklist               (roundRobin)

newtype IOEval a = IOEval (IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadEval IOEval where
  evalRead = liftIO readLn
  evalWrite = IOEval . print

main :: IO ()
main = do
  prog <- parseProgram "sources/even.c"
  case prog of
    Left err  -> putStrLn $ "ERROR :: " <> err
    Right ast -> do
      let pg = toPG ast
          solution = runLV $ roundRobin pg (-1)
      putStrLn "AST:" >> print ast
      putStrLn "PG:"
      forM_ pg $ \e -> do
        putStrLn $ printEdge e
        putStr "KILL: "
        print . toList . runLV . kill $ e
        putStr "GEN: "
        print . toList . runLV . gen $ e
        putStrLn ""
      putStrLn "SOLUTION: "
      forM_ (M.toList solution) $ \(st, lv) -> putStrLn $ show st <> "\t" <> show (toList lv)
      -- putStrLn "Interpreter:"
      -- runIO (evalProgram ast)
      -- pure ()
  where
    printEdge :: Edge -> String
    printEdge (qs, a, qe) = show qs <> " -> " <> show qe <> " :: " <> show a
