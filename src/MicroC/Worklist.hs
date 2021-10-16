{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MicroC.Worklist
( Solution
, WorklistAlgorithm
, roundRobin
) where

import           Control.Monad.State.Lazy
import           Data.Lattice             (Lattice (bottom, order, supremum))
import qualified Data.Map.Lazy            as M
import           Data.Set                 (singleton, (\\))
import           MicroC.Analysis          (Analysis (..), Direction (..))
import           MicroC.ProgramGraph      (PG, StateNum, allStates)

-- | A solution to an analysis is a mapping from states to sets of `Result`s.
type Solution m = M.Map StateNum (Result m)

-- | An algorithm works for any Program Graph and starting state and produces a `Solution`.
type WorklistAlgorithm m = Analysis m => PG -> Solution m

-- | An implementation of the Round Robin worklist algorithm.
roundRobin :: forall m. WorklistAlgorithm m
roundRobin pg = flip execState M.empty $ do
  -- all states except the first one
  let s0 = if direction @m == Forward then 0 else -1
      qq = allStates pg \\ singleton s0

  -- set bottom value to all states
  forM_ qq $ \s -> modify (M.insert s bottom)

  -- set initial value to state s0
  modify (M.insert s0 (initialValue @m pg))

  -- iterate until False is returned for every element
  whileM $ anyM pg $ \e -> do
    -- get order of states (reversed for backward problems)
    let (q, q') = stateOrder @m e
    -- get current solution for q and q'
    aq <- gets (M.! q)
    aq' <- gets (M.! q')
    -- calculate left side of the constraint
    let leftSide = analyze @m pg e aq
        satisfied = leftSide `order` aq'
    if not satisfied then do
      -- if not satisfied, we set update the solution for state q'
      modify $ M.insert q' (supremum aq' leftSide)
      -- and indicate something has changed
      pure True
    else
      pure False

-- HELPER FUNCTIONS

-- | Check if any element of a list maps to `True` under a monadic predicate.
anyM :: Monad m => [a] -> (a -> m Bool) -> m Bool
anyM xs p = or <$> mapM p xs

-- | Repeat a monadic computation until it returns `False`.
whileM :: Monad m => m Bool -> m ()
whileM expr = do
  cond <- expr
  when cond $ whileM expr
