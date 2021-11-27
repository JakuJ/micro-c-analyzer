{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module MicroC.Worklist.Naive
( naiveIterative
) where

import           Control.Lens
import           Control.Monad.State.Lazy
import           Data.Lattice
import qualified Data.Map.Lazy            as M
import qualified Data.Set                 as S
import           MicroC.Analysis
import           MicroC.ProgramGraph
import           MicroC.Worklist

data Memory m = Memory
  { _changed :: Bool
  , _output  :: Assignment m
  }

makeLenses ''Memory

-- | An implementation of the Round Robin worklist algorithm.
naiveIterative :: forall m. WorklistAlgorithm m
naiveIterative pg = Solution mem (-1) -- We do not care about iteration counting
  where
    mem :: Assignment m
    mem = execState go (Memory False M.empty) ^. output

    go :: State (Memory m) ()
    go = do
      -- all states except the first one
      let s0 = if direction @m == Forward then 0 else -1
          qq = allStates pg S.\\ S.singleton s0

      -- set bottom value to all states
      forM_ qq $ \q -> output . at q ?= bottom

      -- set initial value to state s0
      output . at s0 ?= initialValue @m pg

      -- iterate until 'changed' is no longer True
      let loop = do
            forM_ pg $ \e -> do
              -- get order of states (reversed for backward problems)
              let (q, q') = stateOrder @m e
              -- get current solution for q and q'
              aq <- uses output (M.! q)
              aq' <- uses output (M.! q')
              -- calculate left side of the constraint
              let leftSide = analyze @m pg e aq
                  satisfied = leftSide `order` aq'
              unless satisfied $ do
                -- if not satisfied, we set update the solution for state q'
                output . at q' ?= supremum aq' leftSide
                -- and indicate something has changed
                changed .= True
            c <- changed <<.= False
            when c loop

      loop

-- HELPER FUNCTIONS

-- | Flip the order of states in an edge for backward analyses.
stateOrder :: forall m. Analysis m => Edge -> (StateNum, StateNum)
stateOrder (qs, _, qe) = case direction @m of
  Forward  -> (qs, qe)
  Backward -> (qe, qs)
