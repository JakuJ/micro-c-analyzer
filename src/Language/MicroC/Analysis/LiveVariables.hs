{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Language.MicroC.Analysis.LiveVariables
( LV
, LVResult(..)
, runLV
) where

import           Control.Monad.Identity       (Identity (runIdentity))
import qualified Data.Set                     as S
import           Language.MicroC.Analysis
import           Language.MicroC.AST          (Identifier)
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

-- | A monadic wrapper for the Live Variables analysis.
newtype LV a = LV {getResult :: Identity a}
  deriving (Functor, Applicative, Monad)

-- | Extract a value from the `LV` wrapper.
runLV :: LV a -> a
runLV = runIdentity . getResult

instance Analysis LV where
  type Result LV = LVResult
  bottomValue = pure S.empty
  initialValue = pure S.empty
  stateOrder = backward
  -- TODO: Kill and gen functions
  kill _ = pure S.empty
  gen _ = pure S.empty
