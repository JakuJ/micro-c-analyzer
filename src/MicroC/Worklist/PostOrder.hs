module MicroC.Worklist.PostOrder
( PostOrder(..)
) where

import           Control.Lens        ((^.))
import           MicroC.DFS          (orderStates, state0)
import           MicroC.ProgramGraph (StateNum)
import           MicroC.Worklist     (Worklist (..))

newtype PostOrder = Post ([StateNum], Bool)

instance Worklist PostOrder where
  empty = Post ([], False)
  insert _ (Post (v, _)) = Post (v, True)
  extract _ (Post ([], False)) = Nothing
  extract st (Post ([], True)) =
    let s0 = st ^. state0
        sorted = filter (/= s0) (orderStates st)
    in Just (s0, Post (sorted, False))
  extract _ (Post (n : ns, t)) = Just (n, Post (ns, t))
