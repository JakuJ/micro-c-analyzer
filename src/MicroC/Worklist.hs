{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module MicroC.Worklist where

import           Control.Lens
import           Control.Monad.State
import           Data.Lattice
import qualified Data.Map            as M
import           MicroC.Analysis
import           MicroC.ProgramGraph

-- | A solution to an analysis is a mapping from states to sets of `Result`s.
type Solution m = M.Map StateNum (Result m)

-- | An algorithm works for any Program Graph and starting state and produces a `Solution`.
type WorklistAlgorithm m = Analysis m => PG -> Solution m

data Memory w m = Memory
  { _wl     :: w
  , _output :: Solution m
  }

makeLenses ''Memory

class Worklist f a where
  empty :: f a
  insert :: a -> f a -> f a
  extract :: f a -> Maybe (a, f a)

worklist :: forall m w. (Worklist w StateNum, Eq (Result m), Eq (w StateNum)) => WorklistAlgorithm m
worklist pg = _output $ execState go $ Memory empty M.empty
  where
    go :: State (Memory (w StateNum) m) ()
    go = do
      -- Initialize all states in the output to the bottom value
      forM_ (allStates pg) $ \q -> do
        output . at q .= Just bottom
        wl %= insert q

      -- Initial constraint for 0 (first state in Forward analyses)
      output . at 0 .= Just (initialValue @m pg)

      whileM' (uses wl (/= empty)) $ do
        q0 <- extract'

        let edges = filter (\(q, _, _) -> q == q0) pg
        forM_ edges $ \e@(q, _, q') -> do
          -- get current solution for q and q'
          aq <- use (output . at q . non bottom)
          aq' <- use (output . at q' . non bottom)

          -- calculate left side of the constraint
          let leftSide = analyze @m pg e aq
              satisfied = leftSide `order` aq'

          unless satisfied $ do
            output . at q' .= Just (aq' `supremum` leftSide)
            wl %= insert q'

whileM' :: Monad m => m Bool -> m () -> m ()
whileM' pred' body = do
  cond <- pred'
  when cond $ do
    body
    whileM' pred' body

extract' :: forall m w. Worklist w StateNum => State (Memory (w StateNum) m ) StateNum
extract' = do
  w <- use wl
  let Just (x, w') = extract w
  wl .= w'
  return x
