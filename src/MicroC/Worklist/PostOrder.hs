module MicroC.Worklist.PostOrder
( PostOrder(..)
, module MicroC.Worklist
) where

import           Control.Lens        ((^.))
import           MicroC.DFS
import           MicroC.ProgramGraph (StateNum)
import           MicroC.Worklist

newtype PostOrder = PostOrder ([StateNum], Bool)

instance Worklist PostOrder where
  empty = PostOrder ([], False)
  insert _ (PostOrder (v, _)) = PostOrder (v, True)
  extract _ (PostOrder ([], False)) = Nothing
  extract st (PostOrder ([], True)) =
    let s0 = st ^. state0
    in Just (s0, PostOrder (filter (/= s0) (orderStates st), False))
  extract _ (PostOrder (n : ns, t)) = Just (n, PostOrder (ns, t))
