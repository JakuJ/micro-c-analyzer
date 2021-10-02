{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Control.Monad.IO.Class
import           Language.MicroC.Interpreter
import           Language.MicroC.Parser
import           Language.MicroC.ProgramGraph (Edge, toPG)

newtype IOEval a = IOEval {runIO :: IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadEval IOEval where
  evalRead = liftIO readLn
  evalWrite = IOEval . print

main :: IO ()
main = do
  prog <- parseProgram "sources/ifte.c"
  case prog of
    Left err  -> putStrLn $ "ERROR :: " <> err
    Right ast -> do
      putStrLn "AST:"
      print ast
      putStrLn "PG:"
      mapM_ (putStrLn . printEdge) $ toPG ast
      putStrLn "Interpreter:"
      runIO (evalProgram ast)
      pure ()
  where
    printEdge :: Edge -> String
    printEdge (qs, a, qe) = show qs <> " -> " <> show qe <> " :: " <> show a
