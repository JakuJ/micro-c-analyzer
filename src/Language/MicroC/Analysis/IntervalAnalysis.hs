module Language.MicroC.Analysis.IntervalAnalysis
( IA
) where

import qualified Data.Set                 as S
import           Language.MicroC.Analysis

-- | The type of the sign
data Interval
  = Bottom
  | Between { _min :: Int, _max :: Int }
  deriving (Eq, Ord)

-- | An empty data type for instantiating the analysis.
data IA

instance Analysis IA where
  type Result IA = Interval
  direction = Forward
  bottomValue = S.empty
  initialValue _ = S.empty
  analyze _ _ _ = undefined
