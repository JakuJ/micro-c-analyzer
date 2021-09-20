{-# LANGUAGE DataKinds #-}

module Language.MicroC.ProgramGraph where

import qualified Language.MicroC.AST as AST
import qualified Control.Monad.State.Lazy as S
import Control.Lens

type State = Int

data Action 
  = DeclAction AST.Declaration
  | AssignAction (AST.LValue 'AST.CInt) (AST.RValue 'AST.CInt)
  | ReadAction (AST.LValue 'AST.CInt)
  | WriteAction (AST.RValue 'AST.CInt)
  | BoolAction (AST.RValue 'AST.CBool)

type Edge = (State, Action, State)

type PG = [Edge]

instance Show Action where
  show (DeclAction ident) = "int " ++ ident ++ "\n" -- TODO: Finish the pretty printer

data Memory = Memory
  { _nextFree :: State
  }

makeLenses ''Memory

type Comp a = S.State Memory a

toPG :: State -> State ->  AST.Program -> Comp PG
toPG qs qe (AST.Program ds ss) = do
  decs <- decsToPG qs qe ds
  stats <- statsToPG qe ss
  return $ concat $ decs ++ stats

decsToPG :: State -> State -> [AST.Declaration] -> Comp PG
decsToPG qs qe [] = return []
decsToPG qs qe (d : ds) = do
  counter <- S.gets counter
  let q = "s_" ++ show counter
  counter += 1
  d1 <- decToPG qs q d
  rest <- decsToPG q qe ds
  return $ d1 ++ rest
  
decToPG :: State -> State -> AST.Declaration -> Comp PG
decToPG qs qe d = do
  end .= qe
  return [(qs, DeclAction d, qe)]

-- stmToPG :: State -> State -> [AST.Statement] -> Comp PG
-- stmToPG qs qe (AST.Write r) = return [(qs, WriteAction r, qe)]
statsToPG :: State -> State 
statsToPG = undefined
