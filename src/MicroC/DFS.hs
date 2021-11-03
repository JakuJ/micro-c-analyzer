{-# LANGUAGE TemplateHaskell #-}

module MicroC.DFS
( SpanningTree(..)
, dfs
, orderStates
-- * Lenses
, edges
, numbering
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
  }
  deriving (Show)

makeLenses ''SpanningTree

data Memory = Memory
  { _tree    :: SpanningTree
  , _visited :: S.Set StateNum
  , _k       :: Int
  }

makeLenses ''Memory

dfs :: PG -> SpanningTree
dfs pg = view tree $ flip execState initMemory $ go 0 -- assuming 0 is the first state
  where
    states :: S.Set StateNum
    states = allStates pg

    initMemory :: Memory
    initMemory = Memory (SpanningTree S.empty M.empty) S.empty (S.size states)

    go :: Int -> State Memory ()
    go s0 = do
      -- mark node as seen
      seen <- visited <%= S.insert s0
      -- forall unseen edges from this node
      let unseen = filter (\(q1, _, q2) -> q1 == s0 && S.notMember q2 seen) pg
      forM_ unseen $ \(q1, _, q2) -> do
        tree . edges %= S.insert (q1, q2)
        go q2
      k' <- k <<-= 1
      tree . numbering . at s0 ?= k'

orderStates :: SpanningTree -> [StateNum]
orderStates (SpanningTree _ nb) = map fst . sortOn snd . M.assocs $ nb
