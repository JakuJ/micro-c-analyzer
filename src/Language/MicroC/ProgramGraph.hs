{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

module Language.MicroC.ProgramGraph
( PG
, Edge
, StateNum
, Action(..)
, toPG
, allStates
) where

import           Control.Lens
import           Control.Monad.State.Lazy
import           Data.Data                (Data)
import           Data.Map.Lazy            (Map)
import           Data.Map.Lens            (toMapOf)
import           Data.Set                 (Set, fromList)
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
    deriving (Show, Data)

-- | An edge between two states is labeled with an action.
type Edge = (StateNum, Action, StateNum)

-- | A program graph is a list of edge
type PG = [Edge]

data Memory = Memory
  { _nextInt :: Int
  , _fields  :: Map Identifier [Identifier]
  }

makeLenses ''Memory

-- | The monad used for constructing the program graph.
type NodeM = State Memory

-- | Traverse over record declarations and make a `Map` from their names to their field names.
declaredRecords :: Declarations -> Map Identifier [Identifier]
declaredRecords = toMapOf $ traverse . _RecordDecl . itraversed

-- | Get all states from a list of edges.
allStates :: PG -> Set StateNum
allStates = fromList . concatMap (\(q1, _, q2) -> [q1, q2])

-- | Return the nextInt field and then increment it.
newState :: NodeM StateNum
newState = nextInt <<+= 1

-- | Get the field names of a record with the given name.
getFields :: Identifier -> NodeM [Identifier]
getFields r = use $ fields . ix r

toPG :: Program -> PG
toPG p@(Program ds _) = evalState (toPG' 0 (-1) p) $ Memory 1 (declaredRecords ds)

toPG' :: StateNum -> StateNum ->  Program -> NodeM PG
toPG' qs qe (Program [] ss)  = stmsToPG qs qe ss
toPG' qs qe (Program [d] []) = decsToPG qs qe [d]
toPG' qs qe (Program ds ss)  = do
  -- have declarations end in a state with this temporary number
  let flag = -2
  decs <- decsToPG qs flag ds
  -- a correctly ordered state to put after declarations
  q <- newState
  -- update all edges that end in 'flag' to end in 'q'
  let decsToQ = decs & traverse . _3 . filtered (== flag) .~ q
  -- generate edges from statements starting from q
  stats <- stmsToPG q qe ss
  pure $ decsToQ ++ stats

decsToPG :: StateNum -> StateNum -> [Declaration] -> NodeM PG
decsToPG _ _ [] = pure []
decsToPG qs qe [d] = pure [(qs, DeclAction d, qe)]
decsToPG qs qe (d : ds) = do
  q <- newState
  rest <- decsToPG q qe ds
  pure $ (qs, DeclAction d, q) : rest

stmsToPG :: StateNum -> StateNum -> [Statement] -> NodeM PG
stmsToPG _ _ [] = pure []
stmsToPG qs qe [x] = stmToPG qs qe x
stmsToPG qs qe (s : ss) = do
  q <- newState
  (++) <$> stmToPG qs q s <*> stmsToPG q qe ss

test :: RValue 'CBool -> (Action, Action)
test v = (v, Not v) & each %~ BoolAction

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
  pure $ (qs, yes, q) : (qs, no, qe) : rest

stmToPG qs qe (IfThenElse (test -> (yes, no)) body els) = do
  qYes <- newState
  qNo <- newState
  bodyStms <- stmsToPG qYes qe body
  elseStms <- stmsToPG qNo qe els
  pure $ (qs, yes, qYes) : (qs, no, qNo) : bodyStms ++ elseStms

stmToPG qs qe (While (test -> (yes, no)) body) = do
  q <- newState
  rest <- stmsToPG q qs body
  pure $ (qs, yes, q) : (qs, no, qe) : rest
