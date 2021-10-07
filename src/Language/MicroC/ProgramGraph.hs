{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.MicroC.ProgramGraph
( toPG
, Edge
, PG
, StateNum
, Action(..)
) where

import           Control.Lens             (makeLenses, uses, (%=), (<<+=))
import           Control.Monad            (replicateM)
import qualified Control.Monad.State.Lazy as S
import qualified Data.Map.Lazy            as M
import qualified Language.MicroC.AST      as AST

-- | Different states are represented using integers.
type StateNum = Int

-- | Represents a basic action in our analysis framework.
data Action
  = DeclAction AST.Declaration
  | AssignAction (AST.LValue 'AST.CInt) (AST.RValue 'AST.CInt)
  | ReadAction (AST.LValue 'AST.CInt)
  | WriteAction (AST.RValue 'AST.CInt)
  | BoolAction (AST.RValue 'AST.CBool)
    deriving (Show)

-- | An edge between two states is labeled with an action.
type Edge = (StateNum, Action, StateNum)

-- | A program graph is a list of edges.
type PG = [Edge]

data Memory = Memory
  { _nextInt :: Int
  , _fields  :: M.Map AST.Identifier [AST.Identifier]
  }

makeLenses ''Memory

class Monad m => MonadNode m where
  newState :: m StateNum
  setFields :: AST.Identifier -> [AST.Identifier] -> m ()
  getFields :: AST.Identifier -> m [AST.Identifier]

instance MonadNode (S.State Memory) where
  newState = nextInt <<+= 1
  getFields r = uses fields (M.! r)
  setFields r fs = fields %= M.insert r fs

toPG :: AST.Program -> PG
toPG p = S.evalState (toPG' 0 (-1) p) (Memory 1 M.empty)

toPG' :: MonadNode m => StateNum -> StateNum ->  AST.Program -> m PG
toPG' qs qe (AST.Program ds ss) = do
  q <- newState
  decs <- decsToPG qs q ds
  stats <- statsToPG q qe ss
  return $ decs <> stats

decsToPG :: MonadNode m => StateNum -> StateNum -> [AST.Declaration] -> m PG
decsToPG _ _ [] = return []
decsToPG qs qe [x] = do
  d <- decToPG qs qe x
  return [d]
decsToPG qs qe (d : ds) = do
  q <- newState
  (:) <$> decToPG qs q d <*> decsToPG q qe ds

decToPG :: MonadNode m => StateNum -> StateNum -> AST.Declaration -> m Edge
decToPG qs qe d = do
  case d of
    AST.RecordDecl i ds -> setFields i ds
    _                   -> pure ()
  pure (qs, DeclAction d, qe)

statsToPG :: MonadNode m => StateNum -> StateNum -> [AST.Statement] -> m PG
statsToPG _ _ [] = return []
statsToPG qs qe [x] = stmToPG qs qe x
statsToPG qs qe (s : ss) = do
  q <- newState
  (++) <$> stmToPG qs q s <*> statsToPG q qe ss

stmToPG :: MonadNode m => StateNum -> StateNum -> AST.Statement -> m PG
stmToPG qs qe (AST.Write r) = return [(qs, WriteAction r, qe)]
stmToPG qs qe (AST.Read l) = return [(qs, ReadAction l, qe)]
stmToPG qs qe (AST.Assignment l r) = return [(qs, AssignAction l r, qe)]

stmToPG qs qe (AST.RecordAssignment i rs) = do
  fs <- getFields i
  states' <- replicateM (length rs - 1) newState
  let states = qs : states' ++ [qe]
      triples = zip3 fs rs (zip states (tail states))
      mkEdge (f, v, (q1, q2)) = (q1, AssignAction (AST.FieldAccess i f) v, q2)
  pure $ map mkEdge triples

stmToPG qs qe (AST.IfThen cond body) = do
  q <- newState
  let (pos, neg) = branch cond
  rest <- statsToPG q qe body
  return $ [(qs, pos, q), (qs, neg, qe)] ++ rest

stmToPG qs qe (AST.IfThenElse cond body els) = do
  qPos <- newState
  qNeg <- newState
  let (pos, neg) = branch cond
  restBody <- statsToPG qPos qe body
  restEls <- statsToPG qNeg qe els
  return $ [(qs, pos, qPos), (qs, neg, qNeg)] ++ restBody ++ restEls

stmToPG qs qe (AST.While cond body) = do
  q <- newState
  let (pos, neg) = branch cond
  rest <- statsToPG q qs body
  return $ [(qs, pos, q), (qs, neg, qe)] ++ rest

branch :: AST.RValue 'AST.CBool -> (Action, Action)
branch v = (BoolAction v, BoolAction (AST.Not v))
