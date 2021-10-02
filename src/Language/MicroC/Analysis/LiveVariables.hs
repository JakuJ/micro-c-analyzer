{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Language.MicroC.Analysis.LiveVariables
( LV
, LVResult(..)
) where

import qualified Data.Set                     as S
import           Language.MicroC.Analysis
import           Language.MicroC.AST hiding ( Variable )
import qualified Language.MicroC.AST as AST
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
  gen (_, DeclAction _, _) = S.empty 
  gen (_, AssignAction _ r, _) = fv r
  gen (_, ReadAction _, _) = S.empty 
  gen (_, WriteAction r, _) = fv r
  gen (_, BoolAction r, _) = fv r

fv :: RValue a -> S.Set LVResult
fv (Reference l) = fv' l
fv (OpA x _ y) = fv x `S.union` fv y
fv (OpR x _ y) = fv x `S.union` fv y
fv (OpB x _ y) = fv x `S.union` fv y
fv (Not r) = fv r
fv _ = S.empty

fv' :: LValue a -> S.Set LVResult
fv' (AST.Variable i) = S.singleton (Variable i)
fv' (ArrayIndex i r) = S.singleton (Array i) `S.union` fv r
fv' (FieldAccess i i') = S.singleton (RecordField i i')