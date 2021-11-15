{-# LANGUAGE TemplateHaskell #-}

module MicroC.Interpreter
( -- * Types
  ProgramMemory
, MonadEval(..)
  -- * Functions
, evalProgram
) where

import           Control.Lens               hiding (op)
import           Control.Monad.State.Strict
import           Data.Bits                  ((.&.), (.|.))
import           Data.Map.Strict            (Map, empty)
import           MicroC.AST

-- | The type used to represent the runtime memory of the program.
type ProgramMemory = Map (LValue 'CInt) Int

-- | The type of the internal state of the interpreter - a mapping from variable names to their values.
data Memory = Memory
  { _memory :: ProgramMemory
  -- ^ The memory, maps L-Values to their runtime values. Arrays are represented with `ArrayIndex` and a `Literal` index.
  , _fields :: Map Identifier [Identifier]
  -- ^ A mapping from record names to lists of their field names.
  }

makeLenses ''Memory

-- | Get the current value of an 'LValue' from the memory.
-- An exception is raised if we are accessing an array and the index is out of range.
-- Reference to an undefined variable or record field returns 0.
referTo :: Monad m => LValue 'CInt -> Env m Int
referTo = \case
  ArrayIndex arr i -> do
    i' <- evalR i
    use $ memory . at (ArrayIndex arr (Literal i')) . non (outOfBounds arr i')
  lval -> do
    mem <- use memory
    pure $! mem ^. at lval . non 0

-- | Throw an exception with a message and terminate the program.
outOfBounds :: Identifier -> Int -> a
outOfBounds arr i = error $ "Index " <> show i <> " out of range for array " <> arr

-- | The interpretation monad defining abstract IO operations.
class Monad m => MonadEval m where
  -- | Implements the __read__ statement.
  evalRead :: m (TypeRepr 'CInt)
  -- | Implements the __write__ statement.
  evalWrite :: TypeRepr 'CInt -> m ()

-- | Monadic stack for the interpreter. m is usually some MonadEval.
type Env m x = StateT Memory m x

-- | Interprets a 'Program' and returns the computation in some 'MonadEval'.
-- This can be then executes resulting in the state of the 'ProgramMemory' after program completion.
evalProgram :: MonadEval m => Program -> m ProgramMemory
evalProgram (Program decls stats) = _memory <$> execStateT (evalDecls decls >> evalStats stats) (Memory empty empty)

-- Helper functions
evalStats :: MonadEval m => Statements -> Env m ()
evalStats = mapM_ evalStat

evalDecls :: Monad m => Declarations -> Env m ()
evalDecls = mapM_ evalDecl

-- Declarations
evalDecl :: Monad m => Declaration -> Env m ()
evalDecl (VariableDecl name) = memory . at (Variable name) ?= 0
evalDecl (ArrayDecl size name) = forM_ [0 .. size - 1] $ \i -> memory . at (ArrayIndex name (Literal i)) ?= 0
evalDecl (RecordDecl name fs) = do
  fields . at name ?= fs
  forM_ fs $ \f -> memory . at (FieldAccess name f) ?= 0

-- Statements
evalStat :: MonadEval m => Statement -> Env m ()
evalStat (Assignment lval rval) = case lval of
  ArrayIndex arr i -> do
      i' <- evalR i
      memory . at (ArrayIndex arr (Literal i')) <~ Just <$> evalR rval
  _ -> memory . at lval <~ Just <$> evalR rval
evalStat (RecordAssignment i rs) = do
  fs <- use (fields . ix i)
  forM_ (zip fs rs) $ \(f, r) ->
    memory . at (FieldAccess i f) <~ Just <$> evalR r
evalStat (IfThen cond body) = do
  true <- evalR cond
  when true $ evalStats body
evalStat (IfThenElse cond body els) = do
  true <- evalR cond
  if true then evalStats body else evalStats els
evalStat loop@(While cond body) = do
  true <- evalR cond
  when true $ do
    evalStats body
    evalStat loop
evalStat (Read lval) = case lval of
  ArrayIndex arr i -> do
    i' <- evalR i
    memory . at (ArrayIndex arr (Literal i')) <~ Just <$> lift evalRead
  _ -> memory . at lval <~ Just <$> lift evalRead
evalStat (Write rval) = lift . evalWrite =<< evalR rval
evalStat Break = error "Interpreter does not support break statements!"
evalStat Continue = error "Interpreter does not support continue statements!"

-- R-values
evalR :: Monad m => RValue t -> Env m (TypeRepr t)
evalR (Reference lval) = referTo lval
evalR (Literal v) = pure v
evalR (OpA l op r) = op2fun op <$> evalR l <*> evalR r
  where
    op2fun = \case
      Add    -> (+)
      Sub    -> (-)
      Mult   -> (*)
      Div    -> div
      Mod    -> mod
      BitAnd -> (.&.)
      BitOr  -> (.|.)
evalR (OpB l op r) = op2fun op <$> evalR l <*> evalR r
  where
    op2fun = \case
      And -> (&&)
      Or  -> (||)
evalR (Not b) = not <$> evalR b
evalR (OpR l op r) = op2fun op <$> evalR l <*> evalR r
  where
    op2fun = \case
      Lt  -> (<)
      Gt  -> (>)
      Ge  -> (>=)
      Le  -> (<=)
      Neq -> (/=)
      Eq  -> (==)
