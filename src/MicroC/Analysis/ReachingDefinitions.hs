module MicroC.Analysis.ReachingDefinitions
( RD
, RDResult
, getAllNames
) where

import           Control.Lens        ((^..))
import           Data.Data.Lens      (biplate)
import           Data.Lattice        (Poset (..))
import           Data.Set            hiding (map)
import           MicroC.AST
import           MicroC.Analysis
import           MicroC.ID
import           MicroC.ProgramGraph

-- | An empty data type for instantiating the analysis.
data RD

-- | The result of the Reaching Definitions analysis.
type RDResult = (ID, StateNum, StateNum)

instance Analysis RD where
  type Result RD = Poset RDResult
  direction = Forward
  initialValue pg = Poset $ mapMonotonic (,-2, 0) $ getAllNames pg
  analyze pg e (Poset s) = Poset $ (s \\ kill e pg) `union` gen e

kill :: Edge -> PG -> Set RDResult
kill (_, action, _) pg = case action of
  DeclAction (VariableDecl x)      -> killDefinition (VariableID x)
  DeclAction (RecordDecl r ids)    -> mconcat $ map (killDefinition . FieldID r) ids
  DeclAction (ArrayDecl _ x)       -> killDefinition (ArrayID x)
  AssignAction (Variable x) _      -> killDefinition (VariableID x)
  AssignAction (FieldAccess r f) _ -> killDefinition (FieldID r f)
  ReadAction (Variable x)          -> killDefinition (VariableID x)
  ReadAction (FieldAccess i i')    -> killDefinition (FieldID i i')
  _                                -> empty
  where
    killDefinition :: ID -> Set RDResult
    killDefinition x = fromList $ (x, -2, 0) : map (\(qs, _, qe) -> (x, qs, qe)) pg

gen :: Edge -> Set RDResult
gen (qs, action, qe) = case action of
  DeclAction (VariableDecl x)      -> singleton (VariableID x, qs, qe)
  DeclAction (RecordDecl r ids)    -> fromList $ map ((, qs, qe) . FieldID r) ids
  DeclAction (ArrayDecl _ x)       -> singleton (ArrayID x, qs, qe)
  AssignAction (Variable x) _      -> singleton (VariableID x, qs, qe)
  AssignAction (ArrayIndex x _) _  -> singleton (ArrayID x, qs, qe)
  AssignAction (FieldAccess r f) _ -> singleton (FieldID r f, qs, qe)
  ReadAction (Variable x)          -> singleton (VariableID x, qs, qe)
  ReadAction (ArrayIndex x _)      -> singleton (ArrayID x, qs, qe)
  ReadAction (FieldAccess r f)     -> singleton (FieldID r f, qs, qe)
  _                                -> empty

getAllNames :: PG -> Set ID
getAllNames pg = fromList $ defs ++ usages
  where
    usages = map lval2ID (pg ^.. biplate :: [LValue 'CInt])
    defs = concatMap def2IDs (pg ^.. biplate :: [Declaration])
