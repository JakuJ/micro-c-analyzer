{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module MicroC.Worklist where

import           Control.Lens
import           Control.Monad.State
import           Data.Lattice
import qualified Data.Map            as M
import           Data.Maybe          (fromJust)
import           MicroC.Analysis
import           MicroC.DFS          (SpanningTree, dfs)
import           MicroC.ProgramGraph

-- | An analysis assignment maps a `StateNum` to the analysis' `Result`.
type Assignment m = M.Map StateNum (Result m)

-- | An algorithm works for any Program Graph and produces a `Solution`.
type WorklistAlgorithm m = Analysis m => PG -> Solution m

-- | A solution to an analysis combines the results and the step count of the algorithm.
data Solution m = Solution
  { _solution   :: Assignment m
  , _iterations :: Int
  }

makeLenses ''Solution

data Memory w m = Memory
  { _wl     :: w
  , _output :: M.Map StateNum (Result m)
  , _iters  :: Int
  }

makeLenses ''Memory

-- | A worklist is an abstract data structure that holds `StateNum`s.
class Worklist w where
  empty :: w
  insert :: StateNum -> w -> w
  extract :: SpanningTree -> w -> Maybe (StateNum, w)

-- | An implementation of the worklist algorithm polymorphic over the worklist representation.
worklist :: forall w m. Worklist w => WorklistAlgorithm m
worklist forwardPG = Solution (mem ^. output) (mem ^. iters)
  where
    mem :: Memory w m
    mem = execState go $ Memory empty M.empty 0

    pg :: PG
    pg = case direction @m of
      Forward  -> forwardPG
      Backward -> map (\(a, b, c) -> (c, b, a)) forwardPG

    st :: SpanningTree
    st = dfs initialState pg

    initialState :: StateNum
    initialState = case direction @m of
      Forward  -> 0
      Backward -> -1

    go :: State (Memory w m) ()
    go = do
      -- Initialize all states in the output to the bottom value
      forM_ (allStates pg) $ \q -> do
        output . at q ?= bottom
        wl %= insert q

      -- Initial constraint
      output . at initialState ?= initialValue @m pg

      -- While worklist is not empty, extract q
      whileWL st $ \q -> do
        -- For all edges starting with state q
        let edges = filter (\(x, _, _) -> x == q) pg
        forM_ edges $ \e@(_, _, q') -> do

          -- get current solution for q and q'
          aq <- fmap fromJust . use $ output . at q
          aq' <- fmap fromJust . use $ output . at q'

          -- calculate left side of the constraint
          let leftSide = analyze @m pg e aq
              satisfied = leftSide `order` aq'

          unless satisfied $ do
            iters += 1
            output . at q' ?= aq' `supremum` leftSide
            wl %= insert q'

whileWL :: (Analysis m, Worklist w) => SpanningTree -> (StateNum -> State (Memory w m) ()) -> State (Memory w m) ()
whileWL st process = do
  v <- extract' st
  case v of
    Nothing -> pure ()
    Just v' -> process v' >> whileWL st process

extract' :: Worklist w => SpanningTree -> State (Memory w m) (Maybe StateNum)
extract' st = do
  w <- use wl
  case extract st w of
    Nothing      -> pure Nothing
    Just (x, w') -> wl .= w' >> pure (Just x)
