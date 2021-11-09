module MicroC.Worklist.PendingSet
( PendingSet(..)
, module MicroC.Worklist
) where

import qualified Data.Set            as S
import           MicroC.DFS
import           MicroC.ProgramGraph (StateNum)
import           MicroC.Worklist

newtype PendingSet = PendingSet ([StateNum], S.Set StateNum)

instance Worklist PendingSet where
  empty = PendingSet ([], S.empty)
  insert q (PendingSet (v, p)) = if q `elem` v then PendingSet (v, p) else PendingSet (v, S.insert q p)
  extract st (PendingSet ([], p))
    | S.null p = Nothing
    | otherwise = Just (q, PendingSet (v', S.empty))
    where
      q : v' = filter (`S.member` p) $ orderStates st
  extract _ (PendingSet (v, p)) = Just (q, PendingSet (v', p))
    where
      q : v' = v
