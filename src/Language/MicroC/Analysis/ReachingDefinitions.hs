{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Language.MicroC.Analysis.ReachingDefinitions
( LV
, LVResult(..)
) where

import qualified Data.Set                     as S
import           Language.MicroC.Analysis
import           Language.MicroC.AST          hiding (Variable)
import qualified Language.MicroC.AST          as AST
import           Language.MicroC.ProgramGraph

-- | A result of a Reaching Definitions analysis.
data ID
  = Variable Identifier
  -- ^ Name of a variable.
  | Array Identifier
  -- ^ Name of an array, we amalgamate those.
  | RecordField Identifier Identifier
  -- ^ Name of a record and its field.
    deriving (Eq, Ord, Show)

-- | An empty data type for instantiating the analysis.
data RD

type RDResult  = (ID, StateNum, StateNum)

-- Not complete
instance Analysis RD where
  type Result RD = RDResult
  bottomValue = S.empty
  initialValue = S.fromList ([(name, -2, 0) | name <- getAllNames pg])
  stateOrder = forward
  kill (_, action, _) = case action of
    DeclAction (VariableDecl x)             -> S.singleton (Variable x)
    DeclAction (RecordDecl x)               -> S.fromList [RecordField x "fst", RecordField x "snd"]
    DeclAction (ArrayDecl _ a)              -> S.singleton (Array a)
    AssignAction (AST.Variable x) _         -> S.singleton (Variable x)
    AssignAction (AST.FieldAccess i i') _   -> S.singleton (RecordField i i')
    ReadAction (AST.Variable x)             -> S.singleton (Variable x)
    ReadAction (AST.FieldAccess i i')       -> S.singleton (RecordField i i')
    _                                       -> S.empty

  gen (_, action, _) = case action of
    AssignAction lv rv -> fv'' lv `S.union` fv rv
    WriteAction rv     -> fv rv
    BoolAction rv      -> fv rv
    _                  -> S.empty


-- TODO
getAllNames :: PG -> [Identifier]
getAllNames pg = []