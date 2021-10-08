module Language.MicroC.Analysis.FaintVariables
( FV
) where

import qualified Data.Set                               as S
import           Language.MicroC.AST                    hiding (Variable)
import qualified Language.MicroC.AST                    as AST
import           Language.MicroC.Analysis
import           Language.MicroC.Analysis.LiveVariables (fv)
import           Language.MicroC.ProgramGraph

-- | An empty data type for instantiating the analysis.
data FV

instance Analysis FV where
  type Result FV = ID
  bottomValue = S.empty
  initialValue = S.empty
  stateOrder = backward
  analyze (_, action, _) s = case action of
    DeclAction de -> case de of
      VariableDecl n ->
        let x = Variable n
        in
          if x `S.member` s then S.delete x s else s
      ArrayDecl _ _ -> s
      RecordDecl r fs ->
        let xs = map (RecordField r) fs
        in
          s S.\\ S.fromList (filter (`S.member` s) xs)

    AssignAction lv rv -> case lv of
      AST.Variable n ->
        let x = Variable n
            rhs = fv rv
        in
          if x `S.member` s then S.delete x s `S.union` rhs else s
      ArrayIndex n ix ->
        let a = Array n
            ixfvs = fv ix
            rhsfvs = fv rv
        in
          if a `S.member` s then s `S.union` ixfvs `S.union` rhsfvs else s
      FieldAccess n field ->
        let x = RecordField n field
            rhs = fv rv
        in
          if x `S.member` s then S.delete x s `S.union` rhs else s

    ReadAction lv -> case lv of
      AST.Variable n ->
        let x = Variable n
        in
          if x `S.member` s then S.delete x s else s
      ArrayIndex _ _ -> s
      FieldAccess n field ->
        let x = RecordField n field
        in
          if x `S.member` s then S.delete x s else s

    WriteAction r -> s `S.union` fv r
    BoolAction r -> s `S.union` fv r
