{-# LANGUAGE TemplateHaskell #-}

module MicroC.DFS
( SpanningTree(..)
, dfs
, orderStates
-- * Lenses
, edges
, numbering
, state0
) where

import           Control.Lens
import           Control.Monad.State.Lazy
import           Data.List                (sortOn)
import qualified Data.Map                 as M
import qualified Data.Set                 as S
import           MicroC.ProgramGraph

data SpanningTree = SpanningTree
  { _edges     :: S.Set (StateNum, StateNum)
  , _numbering :: M.Map StateNum Int
  , _state0    :: StateNum
  }
  deriving (Show)

makeLenses ''SpanningTree

data Memory = Memory
  { _tree    :: SpanningTree
  , _visited :: S.Set StateNum
  , _k       :: Int
  }

makeLenses ''Memory

dfs :: StateNum -> PG -> SpanningTree
dfs s0 pg = view tree $ flip execState initMemory $ go s0 -- assuming 0 is the first state
  where
    states :: S.Set StateNum
    states = allStates pg

    initMemory :: Memory
    initMemory = Memory (SpanningTree S.empty M.empty s0) S.empty (S.size states)

    go :: Int -> State Memory ()
    go q = do
      -- mark node as seen
      seen <- visited <%= S.insert q
      -- forall unseen edges from this node
      let unseen = filter (\(q1, _, q2) -> q1 == q && S.notMember q2 seen) pg
      forM_ unseen $ \(_, _, q') -> do
        tree . edges %= S.insert (q, q')
        go q'
      k' <- k <<-= 1
      tree . numbering . at q ?= k'

orderStates :: SpanningTree -> [StateNum]
orderStates (SpanningTree _ nb _) = map fst . sortOn snd . M.assocs $ nb
