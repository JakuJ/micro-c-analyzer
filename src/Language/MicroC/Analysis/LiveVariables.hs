{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Language.MicroC.Analysis.LiveVariables
( LV
, LVResult(..)
) where

import qualified Data.Set                     as S
import           Language.MicroC.Analysis
import           Language.MicroC.AST          hiding (Variable)
import qualified Language.MicroC.AST          as AST
import           Language.MicroC.ProgramGraph

-- | A result of a Live Variables analysis.
data LVResult
  = Variable Identifier
  -- ^ Name of a variable.
  | Array Identifier
  -- ^ Name of an array, we amalgamate those.
  | RecordField Identifier Identifier
  -- ^ Name of a record and its field.
    deriving (Eq, Ord, Show)

-- | An empty data type for instantiating the analysis.
data LV

instance Analysis LV where
  type Result LV = LVResult
  bottomValue = S.empty
  initialValue = S.empty
  stateOrder = backward
  -- Missing double-record assignment R := (a,b)
  kill (_, action, _) = case action of
    DeclAction (VariableDecl i)             -> S.singleton $ Variable i
    DeclAction (RecordDecl i)               -> S.fromList [RecordField i "fst", RecordField i "snd"]
    DeclAction (ArrayDecl _ i)              -> S.singleton $ Array i
    AssignAction (AST.Variable i) _         -> S.singleton $ Variable i
    AssignAction (AST.FieldAccess i i') _   -> S.singleton $ RecordField i i'
    ReadAction (AST.Variable i)             -> S.singleton $ Variable i
    ReadAction (AST.FieldAccess i i')       -> S.singleton $ RecordField i i'
    _                                       -> S.empty

  gen (_, action, _) = case action of
    AssignAction lv rv -> fv'' lv `S.union` fv rv
    WriteAction rv     -> fv rv
    BoolAction rv      -> fv rv
    _                  -> S.empty

fv :: RValue a -> S.Set LVResult
fv (Reference lv)  = fv' lv
fv (OpA rva _ rvb) = fv rva `S.union` fv rvb
fv (OpR rva _ rvb) = fv rva `S.union` fv rvb
fv (OpB rva _ rvb) = fv rva `S.union` fv rvb
fv (Not rv)        = fv rv
fv _               = S.empty

fv' :: LValue a -> S.Set LVResult
fv' (AST.Variable i)   = S.singleton $ Variable i
fv' (ArrayIndex i rv)  = S.singleton (Array i) `S.union` fv rv
fv' (FieldAccess i i') = S.singleton $ RecordField i i'

fv'' :: LValue a -> S.Set LVResult
fv'' (ArrayIndex i rv) = S.singleton (Array i) `S.union` fv rv
fv'' _                 = S.empty
