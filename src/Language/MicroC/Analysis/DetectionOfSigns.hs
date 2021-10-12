module Language.MicroC.Analysis.DetectionOfSigns
( DS
) where

import           Data.Lattice
import qualified Data.Set                 as S
import           Language.MicroC.Analysis

-- | The type of the sign
data Sign = Minus | Zero | Plus
  deriving (Eq, Ord)

-- | An empty data type for instantiating the analysis.
data DS

instance Analysis DS where
  type Result DS = Poset Sign
  direction = Forward
  initialValue _ = Poset S.empty
  analyze _ _ _ = undefined
