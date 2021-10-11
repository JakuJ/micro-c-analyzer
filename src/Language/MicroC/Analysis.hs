{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.MicroC.Analysis
( Analysis(..)
, AnalysisDirection(..)
, ID(..)
, forward
, backward
, lval2ID
, def2IDs
) where

import           Language.MicroC.AST          hiding (LValue (Variable))
import qualified Language.MicroC.AST          as AST
import           Language.MicroC.ProgramGraph (Edge, PG, StateNum)
import Data.Lattice
import Data.Set (Set)

data AnalysisDirection = Forward | Backward
  deriving (Eq)

-- | An abstract analysis monad.
--   Results need to have an instance of `Ord` since we are using `Set`.
class Lattice (Result m) => Analysis m where
  type Result m
  direction :: AnalysisDirection
  -- ^ The type of the elements of the sets returned by the analysis.
  initialValue :: PG -> Result m
  -- ^ The value assigned as an initial solution for the first state at the start of any worklist algorithm.
  analyze :: PG -> Edge -> Result m -> Result m
  -- ^ An analysis function. For bit-vector frameworks defined as S(edge, X) = (X \ kill(edge)) + gen(edge)
  stateOrder :: Edge -> (StateNum, StateNum)
  -- ^ The order of states in constraints, either `forward` or `backward`.

  default direction :: AnalysisDirection
  direction = Forward

  default stateOrder :: Edge -> (StateNum, StateNum)
  stateOrder = case direction @m of
    Forward  -> forward
    Backward -> backward

forward, backward :: Edge -> (StateNum, StateNum)
forward (qs, _, qe) = (qs, qe)
-- ^ Edge state order for forward analyses.
backward (qs, _, qe) = (qe, qs)
-- ^ Edge state order for backward analyses.

-- | Result type for multiple analyses.
data ID
  = Variable Identifier
  -- ^ Name of a variable.
  | Array Identifier
  -- ^ Name of an array, we amalgamate those.
  | RecordField Identifier Identifier
  -- ^ Name of a record and its field.
    deriving (Eq, Ord)

instance Show ID where
  show (Variable i)      = i
  show (Array i)         = i
  show (RecordField r f) = r <> "." <> f

lval2ID :: AST.LValue a -> ID
lval2ID (AST.Variable x)  = Variable x
lval2ID (ArrayIndex n _)  = Array n
lval2ID (FieldAccess r f) = RecordField r f

def2IDs :: Declaration -> [ID]
def2IDs = \case
  VariableDecl name   -> [Variable name]
  ArrayDecl _ name    -> [Array name]
  RecordDecl r fields -> map (RecordField r) fields
