{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Language.MicroC.Analysis.FaintVariables
( FV
) where

import qualified Data.Set                               as S
import           Language.MicroC.Analysis
import           Language.MicroC.Analysis.LiveVariables (fv)
import           Language.MicroC.AST                    hiding (Variable)
import qualified Language.MicroC.AST                    as AST
import           Language.MicroC.ProgramGraph

-- | A result of a Faint Variables analysis.
newtype FVResult = FVResult ID
  deriving (Eq, Ord, Show) via ID

-- | An empty data type for instantiating the analysis.
data FV

instance Analysis FV where
  type Result FV = FVResult
  bottomValue = S.empty
  initialValue = S.empty
  stateOrder = backward
  analyze (_, action, _) s = case action of
    DeclAction de      -> case de of
      AST.VariableDecl n ->
        let x = FVResult (Variable n)
        in
          if x `S.member` s then S.delete x s else s
      ArrayDecl _ _ -> s
      RecordDecl r fs ->
        let xs = map (FVResult . RecordField r) fs
        in
          s S.\\ S.fromList (filter (`S.member` s) xs)

    AssignAction lv rv -> case lv of
      AST.Variable n ->
        let x = FVResult (Variable n)
            rhs = S.map FVResult (fv rv)
        in
          if x `S.member` s then S.delete x s `S.union` rhs else s
      ArrayIndex n ix ->
        let a = FVResult (Array n)
            ixfvs = S.map FVResult $ fv ix
            rhsfvs = S.map FVResult $ fv rv
        in
          if a `S.member` s then s `S.union` ixfvs `S.union` rhsfvs else s
      FieldAccess n field ->
        let x = FVResult (RecordField n field)
            rhs = S.map FVResult (fv rv)
        in
          if x `S.member` s then S.delete x s `S.union` rhs else s
    ReadAction lv -> case lv of
      AST.Variable n ->
        let x = FVResult (Variable n)
        in
          if x `S.member` s then S.delete x s else s
      ArrayIndex _ _ -> s
      FieldAccess n field ->
        let x = FVResult (RecordField n field)
        in
          if x `S.member` s then S.delete x s else s
    BoolAction r -> s `S.union` S.map FVResult (fv r)
    WriteAction r -> s `S.union` S.map FVResult (fv r)
