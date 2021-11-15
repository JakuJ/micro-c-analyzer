module MicroC.Analysis.DangerousVariables
( DV
) where

import           Data.Lattice                        (Poset (..))
import qualified Data.Set                            as S
import           MicroC.AST
import           MicroC.Analysis
import           MicroC.Analysis.LiveVariables       (fv)
import           MicroC.Analysis.ReachingDefinitions (getAllNames)
import           MicroC.ID                           (ID (..), def2IDs, lval2ID)
import           MicroC.ProgramGraph

-- | An empty data type for instantiating the analysis.
data DV

instance Analysis DV where
  type Result DV = Poset ID
  direction = Forward
  initialValue = Poset . getAllNames
  analyze _ (_, action, _) (Poset s) = Poset $ case action of
    -- everything is safe if it's just declared
    DeclAction de -> foldr S.delete s (def2IDs de)
    AssignAction lv@(lval2ID -> x) a -> case lv of
      -- amalgamated arrays can easily be tainted, but we can't un-taint them
      ArrayIndex _ ix -> if (fv ix `S.union` fv a) `S.disjoint` s then s else S.insert x s
      -- variables and record fields are safe iff they do not use unsafe variables
      _               -> if fv a `S.disjoint` s then S.delete x s else S.insert x s
    -- reading a value from the user is considered safe
    ReadAction lv -> case lv of
      ArrayIndex _ _ -> s
      _              -> S.delete (lval2ID lv) s
    -- writing, booleans and break/continue do not affect memory
    _ -> s
