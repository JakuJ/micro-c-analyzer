{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}

module Language.MicroC.Analysis where

import qualified Data.Set                     as S
import           Language.MicroC.AST          (Identifier)
import           Language.MicroC.ProgramGraph

type WorkListAlgo r = Analysis r => PG -> StateNum -> r

trivial :: WorkListAlgo r
trivial = undefined -- TODO

class Analysis r where
  bottomValue :: S.Set r
  initialValue :: S.Set r
  constraint :: S.Set r -> S.Set r -> Bool
  kill :: Edge -> S.Set r
  gen :: Edge -> S.Set r

data LiveDefResult
  = Variable Identifier
  | Array Identifier
  | RecordField Identifier Identifier
    deriving (Eq, Ord)

newtype LiveDef = LiveDef {result :: LiveDefResult}
  deriving (Eq, Ord)

instance Analysis LiveDef where
  bottomValue = S.empty
  initialValue = S.empty
  constraint = S.isSubsetOf
  kill _ = S.empty -- TODO
  gen _ = S.empty

