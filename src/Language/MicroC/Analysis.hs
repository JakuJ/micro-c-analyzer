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
) where

import           Data.Set                     (Set, isSubsetOf)
import           Language.MicroC.AST          (Identifier,
                                               LValue (ArrayIndex, FieldAccess))
import qualified Language.MicroC.AST          as AST
import           Language.MicroC.ProgramGraph (Edge, PG, StateNum)

data AnalysisDirection = Forward | Backward
  deriving (Eq)

-- | An abstract analysis monad.
--   Results need to have an instance of `Ord` since we are using `Set`.
class Ord (Result m) => Analysis m where
  type Result m
  direction :: AnalysisDirection
  -- ^ The type of the elements of the sets returned by the analysis.
  bottomValue :: Set (Result m)
  -- ^ The value assigned as an initial solution to all states but the first at the start of any worklist algorithm.
  initialValue :: PG -> Set (Result m)
  -- ^ The value assigned as an initial solution for the first state at the start of any worklist algorithm.
  constraint :: Set (Result m) -> Set (Result m) -> Bool
  -- ^ The constraint function, either `isSubsetOf` or `isSupersetOf`.
  analyze :: PG -> Edge -> Set (Result m) -> Set (Result m)
  -- ^ An analysis function. For bit-vector frameworks defined as S(edge, X) = (X \ kill(edge)) + gen(edge)
  stateOrder :: Edge -> (StateNum, StateNum)
  -- ^ The order of states in constraints, either `forward` or `backward`.

  default direction :: AnalysisDirection
  direction = Forward

  default constraint :: Set (Result m) -> Set (Result m) -> Bool
  constraint = isSubsetOf

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

lval2ID :: LValue a -> ID
lval2ID (AST.Variable x)  = Variable x
lval2ID (ArrayIndex n _)  = Array n
lval2ID (FieldAccess r f) = RecordField r f
