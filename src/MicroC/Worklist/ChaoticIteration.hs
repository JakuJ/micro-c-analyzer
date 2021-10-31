module MicroC.Worklist.ChaoticIteration where

import qualified Data.Set        as S
import           MicroC.Worklist

instance Worklist S.Set where
    empty = S.empty
    insert x s = S.insert x s
    extract s = if S.null s then Nothing else Just (S.elemAt 0 s, s S.\\ S.elemAt 0 s)
