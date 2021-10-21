{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleInstances #-}
module MicroC.Analysis.DetectionOfSigns
( DS
) where
import           Control.Monad.Writer                (All (getAll))
import qualified Data.IntMap.Lazy                    as Set
import           Data.Lattice
import           Data.List
import qualified Data.Map                            as M
import qualified Data.Map.Lazy                       as M
import qualified Data.Set                            as S
import           Data.String.Interpolate
import           GHC.Show                            (Show)
import           MicroC.AST                          hiding (Variable)
import qualified MicroC.AST                          as AST
import           MicroC.Analysis
import           MicroC.Analysis.ReachingDefinitions (getAllNames)
import           MicroC.ID
import           MicroC.ProgramGraph






-- | The type of the sign
data Sign = Minus | Zero | Plus
  deriving (Eq, Ord)

-- | An empty data type for instantiating the analysis.
data DS

newtype State = State (M.Map ID (Poset Sign))
  deriving (Eq)

instance Show State where
  show (State (M.assocs -> assocs))
    = intercalate ", " (map (\(a, b) -> [i|#{a} ∈ #{b}|]) assocs)

instance Show Sign where
  show Plus  = "+"
  show Zero  = "0"
  show Minus = "-"

instance SemiLattice State where
  bottom = State M.empty
  (State m1) `order` (State m2)
    = M.foldrWithKey (\k signs1 acc ->
        case M.lookup k m2 of
          Nothing       -> False
          (Just signs2) -> acc && signs1 `order` signs2) True m1
  supremum (State a) (State b) = State $ M.unionWith supremum a b



instance Analysis DS where
  type Result DS = State
  direction = Forward
  initialValue pg = State $ M.fromList $ map (\ v -> (v, Poset S.empty)) $ S.toList $ (getAllNames pg)
  analyze _ (_, action, _) (State result) = case action of
    DeclAction (VariableDecl x) -> State $ M.insert (VariableID x) (Poset (S.singleton Zero)) result
    DeclAction (ArrayDecl _ a) -> State $ M.insert (ArrayID a) (Poset (S.singleton Zero)) result
    DeclAction (RecordDecl r fields) -> State $ foldr (\ field acc -> M.insert (FieldID r field) (Poset (S.singleton Zero)) acc) result fields
    AssignAction (AST.ArrayIndex a i) rval -> State $ M.insertWith supremum (ArrayID a) (arithmeticSign rval (State result)) result
    AssignAction x rval -> State $ M.insert (lval2ID x) (arithmeticSign rval (State result)) result
    ReadAction lval -> State $ M.insert (lval2ID lval) (Poset (S.fromList [Minus, Zero, Plus])) result
    _ -> State result



instance Lattice (Poset Sign) where
  infimum (Poset s1) (Poset s2) = Poset $ S.intersection s1 s2
  top = Poset $ S.fromList [Plus, Zero, Minus]

arithmeticSign :: RValue 'CInt -> State -> Poset Sign
arithmeticSign (Literal x) _
  | x > 0 = Poset (S.singleton Plus)
  | x == 0 = Poset (S.singleton Zero)
  | x < 0 = Poset (S.singleton Minus)
arithmeticSign (Reference (AST.ArrayIndex a rval)) (State state) = if (arithmeticSign rval  (State state) `infimum` Poset (S.fromList [Zero, Plus])) == Poset S.empty
                                                                  then state M.!  ArrayID a
                                                                  else Poset S.empty
arithmeticSign (Reference x) (State state) = state M.!  lval2ID x
arithmeticSign (OpA left  op right) (State state) =  absOpA op (arithmeticSign left (State state)) (arithmeticSign right (State state))
-- missing negation

absOpA :: OpArith -> Poset Sign -> Poset Sign -> Poset Sign
absOpA op (Poset left) (Poset right) = case op of
  Add ->  Poset $ foldMap S.union  (S.map  (\ (leftSign, rightSign) -> absPlus leftSign rightSign) (S.cartesianProduct left right)) S.empty
  Sub ->  Poset $ foldMap S.union  (S.map  (\ (leftSign, rightSign) -> absMinus leftSign rightSign) (S.cartesianProduct left right)) S.empty
  Mult ->  Poset $ foldMap S.union  (S.map  (\ (leftSign, rightSign) -> absMult leftSign rightSign) (S.cartesianProduct left right)) S.empty
  Div ->  Poset $ foldMap S.union  (S.map  (\ (leftSign, rightSign) -> absDiv leftSign rightSign) (S.cartesianProduct left right)) S.empty
  Mod -> Poset $ foldMap S.union  (S.map  (\ (leftSign, rightSign) -> absMod leftSign rightSign) (S.cartesianProduct left right)) S.empty






absPlus :: Sign -> Sign -> S.Set Sign
absPlus s1 s2 = case s1 of
  Minus -> if s2 == Plus then  S.fromList [Minus, Zero, Plus] else  S.singleton Minus
  Zero ->  S.singleton s2
  Plus -> if s2 == Minus then  S.fromList [Minus, Zero, Plus] else  S.singleton Plus


absMinus :: Sign -> Sign -> S.Set Sign
absMinus Minus Minus = S.fromList [Minus, Zero, Plus]
absMinus Minus _     = S.singleton Minus
absMinus Zero Plus   = S.singleton Minus
absMinus Zero Zero   = S.singleton Zero
absMinus Zero Minus  = S.singleton Plus
absMinus Plus Plus   = S.fromList [Minus, Zero, Plus]
absMinus Plus _      = S.singleton Plus


absMult :: Sign -> Sign -> S.Set Sign
absMult Minus Minus = S.singleton Plus
absMult Minus Plus  = S.singleton Minus
absMult _ Zero      = S.singleton Zero
absMult Zero _      = S.singleton Zero
absMult Plus Minus  = S.singleton Minus
absMult Plus Plus   = S.singleton Plus

absDiv :: Sign -> Sign -> S.Set Sign
absDiv Minus Minus = S.singleton Plus
absDiv Minus Plus  = S.singleton Minus
absDiv _ Zero      = S.empty
absDiv Zero _      = S.singleton Zero
absDiv Plus Minus  = S.singleton Minus
absDiv Plus Plus   = S.singleton Plus


absMod :: Sign -> Sign -> S.Set Sign
absMod Minus Minus = S.fromList [Zero, Minus]
absMod Minus Plus = S.fromList [Zero, Minus]
absMod _ Zero      = S.empty
absMod Zero _      = S.singleton Zero
absMod Plus Plus = S.fromList [Zero, Plus]
absMod Plus Minus = S.fromList [Zero, Plus]

