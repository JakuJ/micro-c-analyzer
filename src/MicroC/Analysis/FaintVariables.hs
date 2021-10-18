module MicroC.Analysis.FaintVariables
( FV
) where

import           Data.Lattice                  (Poset (..))
import           Data.Set                      (delete, empty, member, union)
import           MicroC.AST
import           MicroC.Analysis
import           MicroC.Analysis.LiveVariables (fv)
import           MicroC.ID                     (ID (..), def2IDs, lval2ID)
import           MicroC.ProgramGraph

-- | An empty data type for instantiating the analysis.
data FV

instance Analysis FV where
  type Result FV = Poset ID
  direction = Backward
  initialValue _ = Poset empty
  analyze _ (_, action, _) (Poset s) = Poset $ case action of
    -- declaring something makes it faint
    DeclAction de -> foldr delete s (def2IDs de)
    AssignAction lv@(lval2ID -> x) a -> case lv of
      -- amalgamated arrays can never be proven to be fully faint
      ArrayIndex _ ix -> if x `member` s then s `union` fv ix `union` fv a else s
      -- unless a variable or record field was faint, whatever was used to compute the new value is now live
      _               -> if x `member` s then delete x s `union` fv a else s
    ReadAction lv@(lval2ID -> x) -> case lv of
      -- stuff used to compute the index is marked live
      ArrayIndex _ ix -> if x `member` s then s `union` fv ix else s
      -- overwriting some variable/field makes it faint
      _               -> delete x s
    -- using stuff for writing / testing makes it live
    WriteAction a -> s `union` fv a
    BoolAction b  -> s `union` fv b
