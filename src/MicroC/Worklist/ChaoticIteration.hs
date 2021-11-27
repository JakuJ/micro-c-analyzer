module MicroC.Worklist.ChaoticIteration
( Chaotic(..)
) where

import           Data.Set            as S
import           Data.Set.Internal   (Set (Bin, Tip))
import           MicroC.ProgramGraph (StateNum)
import           MicroC.Worklist     (Worklist (..))

newtype Chaotic = Chaotic (Set StateNum)

instance Worklist Chaotic where
  empty = Chaotic S.empty
  insert x (Chaotic s) = Chaotic $ S.insert x s
  extract _ (Chaotic s) = (\x -> (x, Chaotic $ delete x s)) <$> getAny s

-- | O(1) - returns an arbitrary element from the set.
-- | It is the root of the balanced binary tree used internally by Data.Set.
getAny :: Set a -> Maybe a
getAny = \case
  Bin _ a _ _ -> Just a
  Tip         -> Nothing
