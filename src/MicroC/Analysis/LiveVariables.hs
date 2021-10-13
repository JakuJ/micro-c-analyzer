module MicroC.Analysis.LiveVariables
( LV
, kill
, gen
, fv
) where

import           Data.Lattice        (Poset (..))
import qualified Data.Set            as S
import           MicroC.AST          hiding (Variable)
import qualified MicroC.AST          as AST
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
  DeclAction (VariableDecl i)           -> S.singleton $ Variable i
  DeclAction (RecordDecl i fs)          -> S.fromList  $ map (RecordField i) fs
  DeclAction (ArrayDecl _ i)            -> S.singleton $ Array i
  AssignAction (AST.Variable i) _       -> S.singleton $ Variable i
  AssignAction (AST.FieldAccess i i') _ -> S.singleton $ RecordField i i'
  ReadAction (AST.Variable i)           -> S.singleton $ Variable i
  ReadAction (AST.FieldAccess i i')     -> S.singleton $ RecordField i i'
  _                                     -> S.empty

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
fv' (AST.Variable i)   = S.singleton $ Variable i
fv' (ArrayIndex i rv)  = S.insert (Array i) $ fv rv
fv' (FieldAccess i i') = S.singleton $ RecordField i i'
