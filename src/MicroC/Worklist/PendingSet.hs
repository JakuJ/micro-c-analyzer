{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module MicroC.Worklist.PendingSet
( PendingSet(..)
) where

import qualified Data.Set            as S
import           MicroC.DFS          (orderStates)
import           MicroC.ProgramGraph (StateNum)
import           MicroC.Worklist     (Worklist (..))

newtype PendingSet = PS ([StateNum], S.Set StateNum, S.Set StateNum)

instance Worklist PendingSet where
  empty = PS ([], S.empty, S.empty)
  insert q (PS (vrp, v, p)) = PS . (vrp, v,) $
    if q `S.member` v then p else S.insert q p
  extract st (PS ([], _,  p))
    | S.null p = Nothing
    | otherwise = Just (q, PS (v', S.fromList v', S.empty))
    where
      q : v' = filter (`S.member` p) $ orderStates st
  extract _ (PS (q : v', v, p)) = Just (q, PS (v', S.delete q v, p))
