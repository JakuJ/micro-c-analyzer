{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Language.MicroC.Interpreter
( Memory
, evalProgram
) where

import           Control.Monad.State.Lazy
import qualified Data.Map.Lazy            as M
import           Language.MicroC.AST

-- | The type of the internal state of the interpreter - a mapping from variable names to their values.
type Memory = M.Map Identifier Int

{- |
    Interpret a 'Program' assuming some initial state of the 'Memory'.
    Returns the state of the 'Memory' after evaluation.
-}
evalProgram :: Program -> Memory -> Memory
evalProgram (Program decls stats) = execState (evalDecls decls >> evalStats stats)

-- Helpers
evalStats :: [Statement] -> State Memory ()
evalStats = mapM_ evalStat

evalDecls :: [Declaration] -> State Memory ()
evalDecls = mapM_ evalDecl

-- Declarations
evalDecl :: Declaration -> State Memory ()
evalDecl (VariableDecl name) = modify (M.insertWith (\ _ x -> x) name 0) -- set to 0 if not present

-- Statements
evalStat :: Statement -> State Memory ()
evalStat (Assignment (Variable name) rval) = do
    r <- evalR rval
    modify $ M.insert name r
evalStat (IfThen cond body) = do
    true <- evalR cond
    when true $ evalStats body
evalStat loop@(While cond body) = do
    true <- evalR cond
    when true $ do
        evalStats body
        evalStat loop

-- R-values
evalR :: RValue t -> State Memory (TypeRepr t)
evalR (Reference (Variable name)) = gets (M.! name)
evalR (Literal v) = pure v
evalR (OpA l op r) = op2fun op <$> evalR l <*> evalR r
    where
        op2fun = \case
            Add -> (+)
            Sub -> (-)
            Mult -> (*)
evalR (OpR l op r) = op2fun op <$> evalR l <*> evalR r
    where
        op2fun = \case
            Lt -> (<)
            Neq -> (/=)
