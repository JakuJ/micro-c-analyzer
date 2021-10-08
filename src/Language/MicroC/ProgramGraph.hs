{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

module Language.MicroC.ProgramGraph
( toPG
, Edge
, PG
, StateNum
, Action(..)
) where

import           Control.Lens             (makeLenses, uses, (%=), (<<+=))
import           Control.Monad.State.Lazy
import qualified Data.Map.Lazy            as M
import           Language.MicroC.AST

-- | Different states are represented using integers.
type StateNum = Int

-- | Represents a basic action in our analysis framework.
data Action
  = DeclAction Declaration
  | AssignAction (LValue 'CInt) (RValue 'CInt)
  | ReadAction (LValue 'CInt)
  | WriteAction (RValue 'CInt)
  | BoolAction (RValue 'CBool)
    deriving (Show)

-- | An edge between two states is labeled with an action.
type Edge = (StateNum, Action, StateNum)

-- | A program graph is a list of edge
type PG = [Edge]

data Memory = Memory
  { _nextInt :: Int
  , _fields  :: M.Map Identifier [Identifier]
  }

makeLenses ''Memory

-- | The monad used for constructing the program graph.
type NodeM = State Memory

-- | Return the nextInt field and then increment it.
newState :: NodeM StateNum
newState = nextInt <<+= 1

-- | Remember the field names of a record with the given name.
setFields :: Identifier -> [Identifier] -> NodeM ()
setFields r fs = fields %= M.insert r fs

-- | Get the field names of a record with the given name.
getFields :: Identifier -> NodeM [Identifier]
getFields r = uses fields (M.! r)

toPG :: Program -> PG
toPG p = evalState (toPG' 0 (-1) p) (Memory 1 M.empty)

toPG' :: StateNum -> StateNum ->  Program -> NodeM PG
toPG' qs qe (Program ds ss) = do
  q <- newState
  (++) <$> decsToPG qs q ds <*> stmsToPG q qe ss

decsToPG :: StateNum -> StateNum -> [Declaration] -> NodeM PG
decsToPG _ _ [] = pure []
decsToPG qs qe [x] = pure <$> decToPG qs qe x
decsToPG qs qe (d : ds) = do
  q <- newState
  (:) <$> decToPG qs q d <*> decsToPG q qe ds

decToPG :: StateNum -> StateNum -> Declaration -> NodeM Edge
decToPG qs qe d = do
  case d of
    RecordDecl i ds -> setFields i ds
    _               -> pure ()
  pure (qs, DeclAction d, qe)

stmsToPG :: StateNum -> StateNum -> [Statement] -> NodeM PG
stmsToPG _ _ [] = pure []
stmsToPG qs qe [x] = stmToPG qs qe x
stmsToPG qs qe (s : ss) = do
  q <- newState
  (++) <$> stmToPG qs q s <*> stmsToPG q qe ss

test :: RValue 'CBool -> (Action, Action)
test v = (BoolAction v, BoolAction (Not v))

stmToPG :: StateNum -> StateNum -> Statement -> NodeM PG
stmToPG qs qe (Write r) = pure [(qs, WriteAction r, qe)]
stmToPG qs qe (Read l) = pure [(qs, ReadAction l, qe)]
stmToPG qs qe (Assignment l r) = pure [(qs, AssignAction l r, qe)]

stmToPG qs qe (RecordAssignment i rs) = do
  fs <- getFields i
  states' <- replicateM (length rs - 1) newState
  let states = qs : states' ++ [qe]
      triples = zip3 fs rs (zip states (tail states))
      mkEdge (f, v, (q1, q2)) = (q1, AssignAction (FieldAccess i f) v, q2)
  pure $ map mkEdge triples

stmToPG qs qe (IfThen (test -> (yes, no)) body) = do
  q <- newState
  rest <- stmsToPG q qe body
  pure $ [(qs, yes, q), (qs, no, qe)] ++ rest

stmToPG qs qe (IfThenElse (test -> (yes, no)) body els) = do
  qYes <- newState
  qNo <- newState
  bodyStms <- stmsToPG qYes qe body
  elseStms <- stmsToPG qNo qe els
  pure $ [(qs, yes, qYes), (qs, no, qNo)] ++ bodyStms ++ elseStms

stmToPG qs qe (While (test -> (yes, no)) body) = do
  q <- newState
  rest <- stmsToPG q qs body
  pure $ [(qs, yes, q), (qs, no, qe)] ++ rest
