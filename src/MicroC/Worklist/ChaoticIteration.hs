{-# LANGUAGE FlexibleInstances #-}

module MicroC.Worklist.ChaoticIteration where

import qualified Data.Set        as S
import           MicroC.Worklist

newtype Chaotic a = Chaotic (S.Set a)
  deriving (Eq)

instance Ord a => Worklist Chaotic a where
    empty = Chaotic S.empty
    insert x (Chaotic s) = Chaotic $ S.insert x s
    extract (Chaotic s) = if S.null s then Nothing else
      let x = S.elemAt 0 s in Just (x, Chaotic $ S.delete x s)
