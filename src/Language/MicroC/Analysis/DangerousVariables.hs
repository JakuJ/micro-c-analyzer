module Language.MicroC.Analysis.DangerousVariables
( DV
) where

import qualified Data.Set                               as S
import           Language.MicroC.AST                    hiding (Variable)
import qualified Language.MicroC.AST                    as AST
import           Language.MicroC.Analysis
import           Language.MicroC.Analysis.LiveVariables (fv)
import           Language.MicroC.ProgramGraph

-- | An empty data type for instantiating the analysis.
data DV

instance Analysis DV where
  type Result DV = ID
  bottomValue = S.empty
  initialValue = S.empty
  stateOrder = forward
  analyze (_, action, _) s = case action of
    DeclAction de -> case de of
        VariableDecl n  ->
            let x = Variable n
            in
              if S.singleton x `S.intersection` s == S.empty then S.delete x s else s
        ArrayDecl _ _   -> s
        RecordDecl r fs ->
            let xs = map (RecordField r) fs
            in
              s S.\\ S.fromList (filter (`S.member` s) xs)
    AssignAction lv rv -> case lv of
        AST.Variable n -> 
            let x = Variable n
                rhs = fv rv
            in
              if rhs `S.intersection` s == S.empty then S.delete x s else s `S.union` S.singleton x
        ArrayIndex n ix ->
            let _ = Array n
                ixfvs = fv ix
                rhsfvs = fv rv
                fvs = ixfvs `S.union` rhsfvs
            in
               if fvs `S.intersection` s == S.empty then s else s `S.union` S.singleton (Array n)
        FieldAccess n field ->
            let x = RecordField n field
                rhs = fv rv
            in
               if rhs `S.intersection` s == S.empty then S.delete x s else s `S.union` S.singleton x
    ReadAction lv -> case lv of
      AST.Variable n ->
        let x = Variable n
        in
          if S.singleton x `S.intersection` s == S.empty then S.delete x s else s
      ArrayIndex _ _ -> s
      FieldAccess n field ->
        let x = RecordField n field
        in
          if S.singleton x `S.intersection` s == S.empty then S.delete x s else s
    WriteAction r -> s `S.union` fv r
    BoolAction r -> s `S.union` fv r
