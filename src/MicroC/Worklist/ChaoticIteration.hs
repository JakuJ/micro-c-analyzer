module MicroC.Worklist.ChaoticIteration
( Chaotic(..)
) where

import qualified Data.Set            as S
import qualified Data.Set.Internal   as S
import           MicroC.ProgramGraph (StateNum)
import           MicroC.Worklist     (Worklist (..))

newtype Chaotic = Chaotic (S.Set StateNum)
  deriving (Eq)

instance Worklist Chaotic where
  empty = Chaotic S.empty
  insert x (Chaotic s) = Chaotic $ S.insert x s
  extract _ (Chaotic s) = (\x -> (x, Chaotic $ S.delete x s)) <$> getAny s

-- | O(1) - returns an arbitrary element from the set.
-- | It is the root of the balanced binary tree used internally by Data.Set.
getAny :: S.Set a -> Maybe a
getAny = \case
  S.Bin _ a _ _ -> Just a
  S.Tip         -> Nothing
