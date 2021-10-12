module Language.MicroC.Analysis.DangerousVariables
( DV
) where

import           Data.Lattice
import qualified Data.Set                                     as S
import           Language.MicroC.AST                          hiding (Variable)
import qualified Language.MicroC.AST                          as AST
import           Language.MicroC.Analysis
import           Language.MicroC.Analysis.LiveVariables       (fv)
import           Language.MicroC.Analysis.ReachingDefinitions (getAllNames)
import           Language.MicroC.ProgramGraph


-- | An empty data type for instantiating the analysis.
data DV

instance Analysis DV where
  type Result DV = Poset ID
  direction = Forward
  initialValue = Poset . getAllNames
  analyze _ (_, action, _) (Poset s) = Poset $ case action of
    DeclAction de -> case de of
        VariableDecl n  -> S.delete (Variable n) s
        ArrayDecl _ n   -> S.delete (Variable n) s
        RecordDecl r fs ->
            let xs = map (RecordField r) fs
            in
              s S.\\ S.fromList xs
    AssignAction lv rv -> case lv of
        AST.Variable n ->
            let x = Variable n
                rhs = fv rv
            in
              if rhs `S.disjoint` s then S.delete x s else S.insert x s
        ArrayIndex n ix ->
            let fvs = fv ix `S.union`  fv rv
            in
               if fvs `S.disjoint` s then s else S.insert (Array n) s
        FieldAccess n field ->
            let x = RecordField n field
            in
               if fv rv `S.disjoint` s then S.delete x s else S.insert x s
    ReadAction lv -> case lv of
      AST.Variable n      -> S.delete (Variable n) s
      ArrayIndex _ _      -> s
      FieldAccess n field -> S.delete (RecordField n field) s
    WriteAction _ -> s
    BoolAction _ -> s
