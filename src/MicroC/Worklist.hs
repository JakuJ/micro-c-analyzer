{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module MicroC.Worklist where

import qualified Data.Map            as M
import           MicroC.Analysis
import           MicroC.ProgramGraph
import Control.Monad.State
import Control.Lens
import Data.Lattice
-- | A solution to an analysis is a mapping from states to sets of `Result`s.
type Solution m = M.Map StateNum (Result m)

-- | An algorithm works for any Program Graph and starting state and produces a `Solution`.
type WorklistAlgorithm m = Analysis m => PG -> Solution m

data Memory w m = Memory {
  _wl     :: w,
  _output :: Solution m
}

makeLenses ''Memory

class Worklist t s where
  empty :: t s
  insert :: s -> t s -> t s
  extract :: t s -> Maybe (s, t s)

worklist :: forall m w. (Worklist w StateNum, Eq (Result m), Eq (w StateNum)) => WorklistAlgorithm m
worklist pg = _output $ flip execState (Memory (empty @w @StateNum) M.empty) $ do

  -- Initialize all states in the output to the bottom value
  forM_ (allStates pg) $ \s -> do
    output . at s .= Just bottom
    wl %= insert s

  let s0 = if direction @m == Forward then 0 else -1

  output . at s0 .= Just (initialValue @m pg)

  let pred' :: State (Memory (w StateNum) m ) Bool
      pred' = do
        w <- use wl
        return $ w /= empty

  whileM' pred' $ do
    q0 <- extract'

    let qs = filter (\(q, _, _) -> q == q0) pg

    forM_ qs $ \e -> do
      -- get order of states (reversed for backward problems)
      let (q0', qe) = stateOrder @m e

      -- get current solution for q and q'
      aq0 <- use (output . at q0' . non bottom)
      aqe <- use (output . at qe . non bottom)

      -- calculate left side of the constraint
      let leftSide = analyze @m pg e aq0
          satisfied = leftSide `order` aqe
          
      unless satisfied $ do
        output . at qe .= Just (aqe `supremum` leftSide)
        wl %= insert qe

whileM' :: Monad m => m Bool -> m () -> m ()
whileM' pred' body = do
  cond <- pred'
  when cond $ do
    body
    whileM' pred' body

extract' :: forall m w. Worklist w StateNum =>  State (Memory (w StateNum) m ) StateNum
extract' = do
  w <- use wl
  let Just (x, w') = extract w
  wl .= w'
  return x