{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Language.MicroC.Analysis.LiveVariables
( LV
, LVResult(..)
) where

import qualified Data.Set                     as S
import           Language.MicroC.AST          hiding (Variable)
import qualified Language.MicroC.AST          as AST
import           Language.MicroC.Analysis
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
  -- TODO: Kill and gen functions
  kill _ = S.empty
  gen (_, action, _) = case action of
    DeclAction _       -> S.empty
    AssignAction lv rv -> fv'' lv `S.union` fv rv
    ReadAction _       -> S.empty
    WriteAction rv     -> fv rv
    BoolAction rv      -> fv rv

fv :: RValue a -> S.Set LVResult
fv (Reference lv)  = fv' lv
fv (OpA rva _ rvb) = fv rva `S.union` fv rvb
fv (OpR rva _ rvb) = fv rva `S.union` fv rvb
fv (OpB rva _ rvb) = fv rva `S.union` fv rvb
fv (Not rv)        = fv rv
fv _               = S.empty

fv' :: LValue a -> S.Set LVResult
fv' (AST.Variable i)   = S.singleton (Variable i)
fv' (ArrayIndex i rv)  = S.singleton (Array i) `S.union` fv rv
fv' (FieldAccess i i') = S.singleton (RecordField i i')

fv'' :: LValue a -> S.Set LVResult
fv'' (ArrayIndex i rv) = S.singleton (Array i) `S.union` fv rv
fv'' _                 = S.empty