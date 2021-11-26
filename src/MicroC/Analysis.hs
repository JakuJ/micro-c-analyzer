{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}

module MicroC.Analysis
( Analysis(..)
, Direction(..)
) where

import           Data.Lattice        (SemiLattice)
import           MicroC.ProgramGraph (Edge, PG)

data Direction = Forward | Backward
  deriving (Eq)

-- | An abstract analysis monad. Results of the analysis must form a 'SemiLattice'.
class SemiLattice (Result m) => Analysis m where
  type Result m
  -- ^ The type of the result of the analysis for a given state in the program graph.
  direction :: Direction
  -- ^ Direction of the analysis, either 'Forward' or 'Backward'.
  initialValue :: PG -> Result m
  -- ^ The value assigned as an initial solution for the first state at the start of any worklist algorithm.
  analyze :: PG -> Edge -> Result m -> Result m
  -- ^ An analysis function. For bit-vector frameworks defined as S(edge, X) = (X \ kill(edge)) + gen(edge)
