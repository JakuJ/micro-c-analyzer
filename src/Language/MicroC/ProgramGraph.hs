{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.MicroC.ProgramGraph
( toPG
, Edge
) where

import qualified Control.Monad.State.Lazy as S
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

class Monad m => MonadNode m where
  newState :: m StateNum

instance MonadNode (S.State Int) where
  newState = do
    c <- S.get
    S.modify (+1)
    return c

toPG :: AST.Program -> PG
toPG p = S.evalState (toPG' 0 (-1) p) (1 :: Int)

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
decToPG qs qe d = return (qs, DeclAction d, qe)

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
stmToPG qs qe (AST.RecordAssignment i r1 r2) = do
  q <- newState
  return [ (qs, AssignAction (AST.FieldAccess i "fst") r1, q)
         , (q, AssignAction (AST.FieldAccess i "snd") r2, qe) ]
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
