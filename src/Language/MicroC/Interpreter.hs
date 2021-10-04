{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE DataKinds        #-}

module Language.MicroC.Interpreter
( Memory
, MonadEval(..)
, evalProgram
) where

import           Control.Monad.State.Lazy
import qualified Data.Map.Lazy            as M
import           Language.MicroC.AST

-- | The type of the internal state of the interpreter - a mapping from variable names to their values.
type Memory = M.Map (LValue 'CInt) Int

-- | The interpretation monad defining abstract IO operations.
class Monad m => MonadEval m where
    -- | Implements the __read__ statement.
    evalRead :: Read (TypeRepr t) => m (TypeRepr t)
    -- | Implements the __write__ statement.
    evalWrite :: Show (TypeRepr t) => TypeRepr t -> m ()

-- | The monadic stack consisting of 'StateT' (variable store) and 'MonadEval' (__read__ / __write__).
type Env m x = StateT Memory m x

-- | Interprets a 'Program' and returns the computation in the 'MonadEval' monad.
-- This can be then executes resulting in the state of the 'Memory' after program completion.
evalProgram :: MonadEval m => Program -> m Memory
evalProgram (Program decls stats) = execStateT (evalDecls decls >> evalStats stats) M.empty

-- Helpers
evalStats :: MonadEval m => Statements -> Env m ()
evalStats = mapM_ evalStat

evalDecls :: Monad m => Declarations -> Env m ()
evalDecls = mapM_ evalDecl

-- Declarations
evalDecl :: Monad m => Declaration -> Env m ()
evalDecl (VariableDecl name) = modify $ M.insert (Variable name) 0
evalDecl (RecordDecl name) = do
  modify $ M.insert (FieldAccess name "fst") 0
  modify $ M.insert (FieldAccess name "snd") 0

-- Statements
evalStat :: MonadEval m => Statement -> Env m ()
evalStat (Assignment lval rval) = do
    r <- evalR rval
    modify $ M.insert lval r
evalStat (IfThen cond body) = do
    true <- evalR cond
    when true $ evalStats body
evalStat (IfThenElse cond body els) = do
    true <- evalR cond
    if true then evalStats body
    else evalStats els
evalStat loop@(While cond body) = do
    true <- evalR cond
    when true $ do
        evalStats body
        evalStat loop
evalStat (Read lval) = modify . M.insert lval =<< lift evalRead
evalStat (Write rval) = lift . evalWrite =<< evalR rval

-- R-values
evalR :: Monad m => RValue t -> Env m (TypeRepr t)
evalR (Reference lval) = gets (M.! lval)
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
            Le -> (<=)
