{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MicroC.Analysis
( Analysis(..)
, Direction(..)
, forward
, backward
) where

import           Data.Lattice        (Lattice)
import           MicroC.ProgramGraph (Edge, PG, StateNum)

data Direction = Forward | Backward
  deriving (Eq)

-- | An abstract analysis monad. Results of the analysis must form a complete 'Lattice'.
class Lattice (Result m) => Analysis m where
  type Result m
  -- ^ The type of the result of the analysis for a given state in the program graph.
  direction :: Direction
  -- ^ Direction of the analysis, either 'Forward' or 'Backward'.
  initialValue :: PG -> Result m
  -- ^ The value assigned as an initial solution for the first state at the start of any worklist algorithm.
  analyze :: PG -> Edge -> Result m -> Result m
  -- ^ An analysis function. For bit-vector frameworks defined as S(edge, X) = (X \ kill(edge)) + gen(edge)
  stateOrder :: Edge -> (StateNum, StateNum)
  -- ^ The order of states in constraints, either `forward` or `backward`.

  default direction :: Direction
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
