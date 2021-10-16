{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module MicroC.Analysis.ReachingDefinitions
( RD
, RDResult
, getAllNames
) where
import           Control.Lens        ((^..))
import           Data.Data.Lens      (biplate)
import           Data.Lattice
import qualified Data.Set            as S
import           MicroC.AST
import           MicroC.Analysis
import           MicroC.ID
import           MicroC.ProgramGraph


-- | An empty data type for instantiating the analysis.
data RD

type RDResult  = (ID, StateNum, StateNum)

instance Analysis RD where
  type Result RD = Poset RDResult
  direction = Forward
  initialValue pg = Poset $ S.mapMonotonic (,-2, 0) $ getAllNames pg
  analyze pg e (Poset s) = Poset $ (s S.\\ kill e pg) `S.union` gen e

kill :: Edge -> PG  -> S.Set RDResult
kill (_, action, _) pg = case action of
  DeclAction (VariableDecl x)       -> S.fromList (killDefinition (VariableID x) pg)
  DeclAction (RecordDecl r ids)     -> S.fromList (concatMap (\ i -> killDefinition (FieldID r i) pg) ids)
  DeclAction (ArrayDecl _ x)        -> S.fromList (killDefinition (ArrayID x) pg)
  AssignAction (Variable x) _       -> S.fromList (killDefinition (VariableID x) pg)
  AssignAction (FieldAccess i i') _ -> S.fromList (killDefinition (FieldID i i') pg)
  ReadAction (Variable x)           -> S.fromList (killDefinition (VariableID x) pg)
  ReadAction (FieldAccess i i')     -> S.fromList (killDefinition (FieldID i i') pg)
  _                                 -> S.empty

gen :: Edge  -> S.Set RDResult
gen (qs, action, qe) = case action of
  DeclAction (VariableDecl x)       -> S.singleton (VariableID x, qs, qe)
  DeclAction (RecordDecl r ids)     -> S.fromList (map (\ i -> (FieldID r i,qs,qe)) ids)
  DeclAction (ArrayDecl _ x)        -> S.singleton (ArrayID x, qs, qe)
  AssignAction (Variable x) _       -> S.singleton (VariableID x, qs, qe)
  AssignAction (ArrayIndex x _) _   -> S.singleton (ArrayID x, qs, qe)
  AssignAction (FieldAccess i i') _ -> S.singleton (FieldID i i', qs, qe)
  ReadAction (Variable x)           -> S.singleton (VariableID x, qs, qe)
  ReadAction (ArrayIndex x _)       -> S.singleton (ArrayID x, qs, qe)
  ReadAction (FieldAccess i i')     -> S.singleton (FieldID i i', qs, qe)
  _                                 -> S.empty

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
