{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Control.Monad.IO.Class
import           Language.MicroC.Interpreter
import           Language.MicroC.Parser

newtype IOEval a = IOEval {runIO :: IO a}
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
            print ast >> putStrLn ""
            runIO (evalProgram ast)
            pure ()
