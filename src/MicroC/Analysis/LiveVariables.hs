module MicroC.Analysis.LiveVariables
( LV
, kill
, gen
, fv
) where

import           Data.Lattice        (Poset (..))
import           Data.Set
import           MicroC.AST
import           MicroC.Analysis
import           MicroC.ID           (ID (..))
import           MicroC.ProgramGraph (Action (..))

-- | An empty data type for instantiating the analysis.
data LV

instance Analysis LV where
  type Result LV = Poset ID
  direction = Backward
  initialValue _ = Poset empty
  analyze _ e (Poset s) = Poset $ (s \\ kill e) `union` gen e

kill :: (a, Action, c) -> Set ID
kill (_, action, _) = case action of
  DeclAction (VariableDecl i)       -> singleton $ VariableID i
  DeclAction (RecordDecl i fs)      -> fromList  $ fmap (FieldID i) fs
  DeclAction (ArrayDecl _ i)        -> singleton $ ArrayID i
  AssignAction (Variable i) _       -> singleton $ VariableID i
  AssignAction (FieldAccess i i') _ -> singleton $ FieldID i i'
  ReadAction (Variable i)           -> singleton $ VariableID i
  ReadAction (FieldAccess i i')     -> singleton $ FieldID i i'
  _                                 -> empty

gen :: (a, Action, c) -> Set ID
gen (_, action, _) = case action of
  AssignAction (ArrayIndex _ ix) rv -> fv ix `union` fv rv
  AssignAction _ rv                 -> fv rv
  WriteAction rv                    -> fv rv
  ReadAction (ArrayIndex _ ix)      -> fv ix
  ReadAction _                      -> empty
  BoolAction rv                     -> fv rv
  _                                 -> empty

fv :: RValue a -> Set ID
fv (Reference lv)  = fv' lv
fv (OpA rva _ rvb) = fv rva `union` fv rvb
fv (OpR rva _ rvb) = fv rva `union` fv rvb
fv (OpB rva _ rvb) = fv rva `union` fv rvb
fv (Not rv)        = fv rv
fv (Literal _)     = empty

fv' :: LValue a -> Set ID
fv' (Variable i)       = singleton $ VariableID i
fv' (ArrayIndex i rv)  = insert (ArrayID i) $ fv rv
fv' (FieldAccess i i') = singleton $ FieldID i i'
