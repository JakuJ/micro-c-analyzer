module MicroC.Worklist.Queue
( Queue(..)
, module MicroC.Worklist
) where

import qualified Deque.Lazy          as D
import           MicroC.ProgramGraph (StateNum)
import           MicroC.Worklist

newtype Queue = Queue (D.Deque StateNum)
  deriving (Eq, Semigroup, Monoid)

instance Worklist Queue where
  empty = mempty
  insert x (Queue q) = Queue (D.snoc x q)
  extract (Queue q) = fmap Queue <$> D.uncons q
