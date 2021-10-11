module Language.MicroC.Analysis.IntervalAnalysis
where

import qualified Data.Set                 as S
import           Language.MicroC.Analysis

data Int' = NegInf | Int Integer | Inf
  deriving (Eq, Ord)

instance Num Int' where
  (Int x) + (Int y) = Int $ x + y
  Inf + NegInf      = undefined
  NegInf + Inf      = undefined
  NegInf + _        = NegInf
  _ + NegInf        = NegInf
  Inf + _           = Inf
  _ + Inf           = Inf

  (Int x) * (Int y) = Int $ x * y
  NegInf * _        = NegInf
  _ * NegInf        = NegInf
  Inf * _           = Inf
  _ * Inf           = Inf

  abs = \case
    Inf    -> Inf
    NegInf -> Inf
    Int n  -> Int $ abs n

  signum = \case
    Inf    -> Int 1
    NegInf -> Int (-1)
    Int n  -> Int $ signum n

  fromInteger = Int
  negate = \case
    Inf    -> NegInf
    NegInf -> Inf
    Int n  -> Int $ negate n

instance Bounded Int' where
  minBound = NegInf
  maxBound = Inf

instance Show Int' where
  show Inf     = "Infinity"
  show NegInf  = "-Infinity"
  show (Int n) = show n

-- | The type of the sign
data Interval
  = Bottom
  | Between { _min :: Int', _max :: Int' }
  deriving (Eq, Ord)

-- | An empty data type for instantiating the analysis.
data IA

instance Analysis IA where
  type Result IA = Interval
  direction = Forward
  bottomValue = S.empty
  initialValue _ = S.empty
  analyze _ _ _ = undefined
