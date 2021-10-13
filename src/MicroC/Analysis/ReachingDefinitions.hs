{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Language.MicroC.Analysis.ReachingDefinitions
( RD
, RDResult
, getAllNames
) where
import           Control.Lens                 ((^..))
import           Data.Data.Lens               (biplate)
import           Data.Lattice
import qualified Data.Set                     as S
import           Language.MicroC.AST          hiding (Variable)
import qualified Language.MicroC.AST          as AST
import           Language.MicroC.Analysis
import           Language.MicroC.ID
import           Language.MicroC.ProgramGraph


-- | An empty data type for instantiating the analysis.
data RD

type RDResult  = (ID, StateNum, StateNum)

instance Analysis RD where
  type Result RD = Poset RDResult
  direction = Forward
  initialValue pg = Poset $ S.mapMonotonic (,-2, 0) $ getAllNames pg
  analyze pg e (Poset s) = Poset $ (s S.\\ kill e pg) `S.union` gen e

-- Missing record dec: How to refer to a record itself, and not an access?
-- Need to pass PG to analysis and kill/gen
kill :: Edge -> PG  -> S.Set RDResult
kill (_, action, _) pg = case action of
  DeclAction (VariableDecl x)             -> S.fromList (killDefinition (Variable x) pg)
  DeclAction (RecordDecl r ids)           -> S.fromList (concatMap (\ i -> killDefinition (RecordField r i) pg) ids)
  DeclAction (ArrayDecl _ x)              -> S.fromList (killDefinition (Array x) pg)
  AssignAction (AST.Variable x) _         -> S.fromList (killDefinition (Variable x) pg)
  AssignAction (AST.FieldAccess i i') _   -> S.fromList (killDefinition (RecordField i i') pg)
  ReadAction (AST.Variable x)             -> S.fromList (killDefinition (Variable x) pg)
  ReadAction (AST.FieldAccess i i')       -> S.fromList (killDefinition (RecordField i i') pg)
  _                                       -> S.empty

gen :: Edge  -> S.Set RDResult
gen (qs, action, qe) = case action of
  DeclAction (VariableDecl x)             -> S.singleton (Variable x, qs, qe)
  DeclAction (RecordDecl r ids)           -> S.fromList (map (\ i -> (RecordField r i,qs,qe)) ids)
  DeclAction (ArrayDecl _ x)              -> S.singleton (Array x, qs, qe)
  AssignAction (AST.Variable x) _         -> S.singleton (Variable x, qs, qe)
  AssignAction (AST.ArrayIndex x _) _     -> S.singleton (Array x, qs, qe)
  AssignAction (AST.FieldAccess i i') _   -> S.singleton (RecordField i i', qs, qe)
  ReadAction (AST.Variable x)             -> S.singleton (Variable x, qs, qe)
  ReadAction (AST.ArrayIndex x _)         -> S.singleton (Array x, qs, qe)
  ReadAction (AST.FieldAccess i i')       -> S.singleton (RecordField i i', qs, qe)
  _                                       -> S.empty

getAllNames :: PG -> S.Set ID
getAllNames pg = S.fromList $ defs ++ usages
  where
    usages = map lval2ID (pg ^.. biplate :: [LValue 'CInt])
    defs = concatMap def2IDs (pg ^.. biplate :: [Declaration])

killDefinition :: ID -> PG -> [RDResult]
killDefinition x pg = (x, -2, 0) : map (\(qs, qe) -> (x, qs, qe)) statePairs
  where
  statePairs :: [(StateNum, StateNum)]
  statePairs = map (\(qs, _, qe) -> (qs, qe)) pg
