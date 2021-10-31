module MicroC.Worklist.Queue where

import           Deque.Lazy
import           MicroC.Worklist
import           Prelude         hiding (head, null, tail)

newtype Queue a = Queue (Deque a)
  deriving (Monoid, Semigroup)

instance Worklist Queue where
    empty = mempty
    insert x (Queue q) = Queue (cons x q)
    extract (Queue q) = case head q of
                            Nothing -> Nothing
                            Just a  -> Just (a, Queue (tail q))
