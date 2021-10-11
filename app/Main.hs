module Main (main) where

import           Control.Monad                           (forM_, void)
import           Control.Monad.IO.Class                  (MonadIO (..))
import           Data.Foldable                           (toList)
import qualified Data.Map                                as M
import           Language.MicroC.Analysis.FaintVariables (FV)
import           Language.MicroC.Analysis.DangerousVariables (DV)
import           Language.MicroC.Interpreter             (MonadEval (..),
                                                          evalProgram)
import           Language.MicroC.Parser                  (parseProgram)
import           Language.MicroC.ProgramGraph            (Edge, toPG)
import           Language.MicroC.Worklist                (roundRobin)
import Language.MicroC.Analysis.LiveVariables (LV)

newtype IOEval a = IOEval {runIO :: IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadEval IOEval where
  evalRead = liftIO readLn
  evalWrite = IOEval . print

analyseFile :: FilePath -> IO ()
analyseFile path = do
  prog <- parseProgram $ "sources/" <> path <> ".c"
  case prog of
    Left err  -> putStrLn $ "ERROR :: " <> err
    Right ast -> do
      let pg = toPG ast
          solution = roundRobin @DV pg (-1)
      putStrLn "AST:"
      print ast
      putStrLn "PG:"
      mapM_ (putStrLn . printEdge) pg
      putStrLn "SOLUTION: "
      forM_ (M.toList solution) $ \(st, lv) -> putStrLn $ show st <> "\t" <> show (toList lv)
      putStrLn "Interpreter:"
      void $ runIO (evalProgram ast)
  where
    printEdge :: Edge -> String
    printEdge (qs, a, qe) = show qs <> " -> " <> show qe <> " :: " <> show a

main :: IO ()
main = analyseFile "danger"