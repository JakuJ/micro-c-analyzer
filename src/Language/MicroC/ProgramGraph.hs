{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.MicroC.ProgramGraph where

import qualified Control.Monad.State.Lazy as S
import           GHC.IO                   (unsafePerformIO)
import qualified Language.MicroC.AST      as AST

type StateNum = Int

data Action
  = DeclAction AST.Declaration
  | AssignAction (AST.LValue 'AST.CInt) (AST.RValue 'AST.CInt)
  | ReadAction (AST.LValue 'AST.CInt)
  | WriteAction (AST.RValue 'AST.CInt)
  | BoolAction (AST.RValue 'AST.CBool)
    deriving (Show)

type Edge = (StateNum, Action, StateNum)

type PG = [Edge]

class Monad m => MonadNode m where
  newState :: m StateNum

instance MonadNode (S.State StateNum) where
  newState :: S.State StateNum StateNum
  newState = do
    c <- S.get
    S.modify (+1)
    return c -- State StateNum StateNum

modify' :: S.MonadState s m => (s -> s) -> m ()
modify' f = do
  state <- S.get
  S.put (f state)

instance MonadNode IO where
  newState = do
    contents <- readFile "state.txt"
    let number = read contents  -- read :: Read t => String -> t
    writeFile "state.txt" (show (number + 1))
    return number

toPG :: AST.Program -> PG
toPG p = S.evalState (toPG' 0 (-1) p) (1 :: Int)

testImpl :: MonadNode m => m PG
testImpl = toPG' 0 100 (AST.Program [] [])

test :: PG
test = S.evalState testImpl (0 :: Int)

-- unsafePerformIO :: IO a -> a  !!!! FORBIDDEN !!!!
testIO :: PG
testIO = unsafePerformIO testImpl

toPG' :: MonadNode m => StateNum -> StateNum -> AST.Program -> m PG
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
stmToPG qs qe (AST.While cond body) = do
  q <- newState
  let (pos, neg) = branch cond
  rest <- statsToPG q qs body
  return $ [(qs, pos, q), (qs, neg, qe)] ++ rest

-- TODO: Implement [if - then - else] and remove this line:
stmToPG _ _ _ = return []

branch :: AST.RValue 'AST.CBool -> (Action, Action)
branch v = (BoolAction v, BoolAction (AST.Not v))
