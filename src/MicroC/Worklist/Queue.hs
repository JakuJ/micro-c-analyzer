module MicroC.Worklist.Queue
( Queue(..)
) where

import           Deque.Lazy          (Deque, snoc, uncons)
import           MicroC.ProgramGraph (StateNum)
import           MicroC.Worklist     (Worklist (..))

newtype Queue = Queue (Deque StateNum)
  deriving (Eq, Semigroup, Monoid)

instance Worklist Queue where
  empty = mempty
  insert x (Queue q) = Queue (snoc x q)
  extract _ (Queue q) = fmap Queue <$> uncons q
