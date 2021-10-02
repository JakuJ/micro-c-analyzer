{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Language.MicroC.Analysis
( Analysis(..)
, forward, backward
, subset, superset
) where

import           Data.Set                     (Set, isSubsetOf)
import           Language.MicroC.ProgramGraph (Edge, StateNum)

-- | An abstract analysis monad.
--   Results need to have an instance of `Ord` since we are using `Set`.
class (Monad m, Ord (Result m)) => Analysis m where
  type Result m = r | r -> m
  -- ^ The type of the elements of the sets returned by the analysis.
  bottomValue :: m (Set (Result m))
  -- ^ The value assigned as an initial solution to all states but the first at the start of any worklist algorithm.
  initialValue :: m (Set (Result m))
  -- ^ The value assigned as an initial solution for the first state at the start of any worklist algorithm.
  constraint :: Set (Result m) -> Set (Result m) -> m Bool
  -- ^ The constraint function, either `subset` or `superset`.
  kill :: Edge -> m (Set (Result m))
  -- ^ Get a kill set of an edge.
  gen :: Edge -> m (Set (Result m))
  -- ^ Get a gen set of an edge.
  stateOrder :: Edge -> m (StateNum, StateNum)
  -- ^ The order of states in constraints, either `forward` or `backward`.

  default stateOrder :: Edge -> m (StateNum, StateNum)
  stateOrder = forward

  default constraint :: Set (Result m) -> Set (Result m) -> m Bool
  constraint = subset

subset, superset :: Analysis m => Set (Result m) -> Set (Result m) -> m Bool
subset a b = pure $ isSubsetOf a b
-- ^ Subset constraint.
superset a b = pure $ isSubsetOf b a
-- ^ Superset constraint.

forward, backward :: Analysis m => Edge -> m (StateNum, StateNum)
forward (qs, _, qe) = pure (qs, qe)
-- ^ Edge state order for forward analyses.
backward (qs, _, qe) = pure (qe, qs)
-- ^ Edge state order for backward analyses.
