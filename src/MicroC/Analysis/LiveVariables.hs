module MicroC.Analysis.LiveVariables
( LV
, kill
, gen
, fv
) where

import           Data.Lattice        (Poset (..))
import qualified Data.Set            as S
import           MicroC.AST
import           MicroC.Analysis
import           MicroC.ID           (ID (..))
import           MicroC.ProgramGraph (Action (..))

-- | An empty data type for instantiating the analysis.
data LV

instance Analysis LV where
  type Result LV = Poset ID
  direction = Backward
  initialValue _ = Poset S.empty
  analyze _ e (Poset s) = Poset $ (s S.\\ kill e) `S.union` gen e

kill :: (a, Action, c) -> S.Set ID
kill (_, action, _) = case action of
  DeclAction (VariableDecl i)       -> S.singleton $ VariableID i
  DeclAction (RecordDecl i fs)      -> S.fromList  $ map (FieldID i) fs
  DeclAction (ArrayDecl _ i)        -> S.singleton $ ArrayID i
  AssignAction (Variable i) _       -> S.singleton $ VariableID i
  AssignAction (FieldAccess i i') _ -> S.singleton $ FieldID i i'
  ReadAction (Variable i)           -> S.singleton $ VariableID i
  ReadAction (FieldAccess i i')     -> S.singleton $ FieldID i i'
  _                                 -> S.empty

gen :: (a, Action, c) -> S.Set ID
gen (_, action, _) = case action of
  AssignAction (ArrayIndex _ ix) rv -> fv ix `S.union` fv rv
  AssignAction _ rv                 -> fv rv
  WriteAction rv                    -> fv rv
  ReadAction (ArrayIndex _ ix)      -> fv ix
  ReadAction _                      -> S.empty
  BoolAction rv                     -> fv rv
  DeclAction _                      -> S.empty

fv :: RValue a -> S.Set ID
fv (Reference lv)  = fv' lv
fv (OpA rva _ rvb) = fv rva `S.union` fv rvb
fv (OpR rva _ rvb) = fv rva `S.union` fv rvb
fv (OpB rva _ rvb) = fv rva `S.union` fv rvb
fv (Not rv)        = fv rv
fv (Literal _)     = S.empty

fv' :: LValue a -> S.Set ID
fv' (Variable i)       = S.singleton $ VariableID i
fv' (ArrayIndex i rv)  = S.insert (ArrayID i) $ fv rv
fv' (FieldAccess i i') = S.singleton $ FieldID i i'
