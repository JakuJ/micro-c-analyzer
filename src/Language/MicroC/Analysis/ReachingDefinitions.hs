{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Language.MicroC.Analysis.ReachingDefinitions
( LV
, LVResult(..)
) where

import qualified Data.Set                     as S
import           Language.MicroC.Analysis
import           Language.MicroC.AST          hiding (Variable)
import qualified Language.MicroC.AST          as AST
import           Language.MicroC.ProgramGraph


-- | An empty data type for instantiating the analysis.
data RD

type RDResult  = (ID, StateNum, StateNum)

instance Analysis RD where
  type Result RD = RDResult
  bottomValue = S.empty
  initialValue = S.fromList ([(name, -2, 0) | name <- getAllNames pg])
  stateOrder = forward
  analyze e s = (s S.\\ kill e) `S.union` gen e

-- Missing record dec: How to refer to a record itself, and not an access?
-- Need to pass PG to analysis and kill/gen
kill :: Edge  -> S.Set RDResult
kill (_, action, _) = case action of
  DeclAction (VariableDecl x)             -> S.fromList killDefinition Variable x
  DeclAction (RecordDecl x _)             -> S.empty
  DeclAction (ArrayDecl _ x)              -> S.fromList killDefinition Array x
  AssignAction (AST.Variable x) _         -> S.fromList killDefinition Variable x
  AssignAction (AST.FieldAccess i i') _   -> S.fromList killDefinition RecordField i i'
  ReadAction (AST.Variable x)             -> S.fromList killDefinition Variable x
  ReadAction (AST.FieldAccess i i')       -> S.fromList killDefinition RecordField i i'
  _                                       -> S.empty
  
gen :: Edge  -> S.Set RDResult
gen (qs, action, qe) = case action of
  DeclAction (VariableDecl x)             -> S.singleton (Variable x, qs, qe)
  DeclAction (RecordDecl x _)             -> S.empty
  DeclAction (ArrayDecl _ x)              -> S.singleton (Array x, qs, qe)
  AssignAction (AST.Variable x) _         -> S.singleton (Variable x, qs, qe)
  AssignAction (AST.ArrayIndex x _) _     -> S.singleton (Array x, qs, qe)
  AssignAction (AST.FieldAccess i i') _   -> S.singleton (RecordField i i', qs, qe)
  ReadAction (AST.Variable x)             -> S.singleton (Variable x, qs, qe)
  AssignAction (AST.ArrayIndex x _) _     -> S.singleton (Array x, qs, qe)
  ReadAction (AST.FieldAccess i i')       -> S.singleton (RecordField i i', qs, qe)
  _                                       -> S.empty


-- Missing record dec: How to refer to a record itself, and not an access?
getAllNames :: PG -> [ID]
getAllNames [] = []
getAllNames ((_,action,_):rest) = case action of 
  DeclAction (VariableDecl x) -> (Variable x : getAllNames rest)
  DeclAction (ArrayDecl _ x) -> (Array x : getAllNames rest)
  DeclAction (RecordDecl x _) ->  []
  _                           -> getAllNames rest


killDefinition :: ID -> PG -> [RDResult]
killDefinition x pg  = (x, -2, 0) : map (\(qs, qe) -> (x,qs,qe)) statePairs
  where
  statePairs :: [(StateNum, StateNum)]
  statePairs = map (\(qs,a,qe) -> (qs,qe)) pg
