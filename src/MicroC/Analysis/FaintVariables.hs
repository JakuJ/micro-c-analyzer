module MicroC.Analysis.FaintVariables
( FV
) where

import           Data.Lattice                  (Poset (..))
import qualified Data.Set                      as S
import           MicroC.AST
import           MicroC.Analysis
import           MicroC.Analysis.LiveVariables (fv)
import           MicroC.ID                     (ID (..))
import           MicroC.ProgramGraph

-- | An empty data type for instantiating the analysis.
data FV

instance Analysis FV where
  type Result FV = Poset ID
  direction = Backward
  initialValue _ = Poset S.empty
  analyze _ (_, action, _) (Poset s) = Poset $ case action of
    DeclAction de -> case de of
      VariableDecl n ->
        let x = VariableID n
        in
          if x `S.member` s then S.delete x s else s
      ArrayDecl _ _ -> s
      RecordDecl r fs ->
        let xs = map (FieldID r) fs
        in
          s S.\\ S.fromList (filter (`S.member` s) xs)

    AssignAction lv rv -> case lv of
      Variable n ->
        let x = VariableID n
            rhs = fv rv
        in
          if x `S.member` s then S.delete x s `S.union` rhs else s
      ArrayIndex n ix ->
        let a = ArrayID n
            ixfvs = fv ix
            rhsfvs = fv rv
        in
          if a `S.member` s then s `S.union` ixfvs `S.union` rhsfvs else s
      FieldAccess n field ->
        let x = FieldID n field
            rhs = fv rv
        in
          if x `S.member` s then S.delete x s `S.union` rhs else s

    ReadAction lv -> case lv of
      Variable n ->
        let x = VariableID n
        in
          if x `S.member` s then S.delete x s else s
      ArrayIndex _ _ -> s
      FieldAccess n field ->
        let x = FieldID n field
        in
          if x `S.member` s then S.delete x s else s

    WriteAction r -> s `S.union` fv r
    BoolAction r -> s `S.union` fv r
