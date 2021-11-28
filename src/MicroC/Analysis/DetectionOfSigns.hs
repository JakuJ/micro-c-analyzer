{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists   #-}

module MicroC.Analysis.DetectionOfSigns
( DS
) where

import           Control.Lens                        ((^..))
import           Data.Data.Lens                      (biplate)
import           Data.Lattice
import           Data.List                           (intercalate)
import qualified Data.Map                            as M
import           Data.Maybe                          (fromMaybe)
import qualified Data.Set                            as S
import           Data.String.Interpolate
import           MicroC.AST                          hiding (Variable)
import qualified MicroC.AST                          as AST
import           MicroC.Analysis
import           MicroC.Analysis.ReachingDefinitions (getAllNames)
import           MicroC.ID
import           MicroC.ProgramGraph
import Debug.Trace (traceId, traceShow, trace, traceShowId)

data Sign = Minus | Zero | Plus
  deriving (Eq, Ord, Bounded, Enum)

-- | An empty data type for instantiating the analysis.
data DS

newtype State = State (M.Map ID (Poset Sign))
  deriving (Eq)

instance Show State where
  show (State m) = "[" ++ intercalate ", " (map (\(a, b) -> [i|#{a} : #{b}|]) (M.assocs m)) ++ "]"

instance Show Sign where
  show Plus  = "+"
  show Zero  = "0"
  show Minus = "-"

instance PartialOrder State where
  (State m1) `order` (State m2)
    = M.foldrWithKey (\k signs1 acc ->
        case M.lookup k m2 of
          Nothing       -> False
          (Just signs2) -> acc && signs1 `order` signs2) True m1

instance SemiLattice State where
  bottom = State M.empty
  supremum (State a) (State b) = State $ M.unionWith supremum a b

instance Analysis DS where
  type Result DS = State
  direction = Forward
  initialValue pg = State $ M.fromList $ map (, top) $ S.toList (getAllNames pg)
  analyze _ (_, action, _) (State result) = case action of
    DeclAction (VariableDecl x) -> State $ M.insert (VariableID x) (Poset [Zero]) result
    DeclAction (ArrayDecl _ a) -> State $ M.insert (ArrayID a) (Poset [Zero]) result
    DeclAction (RecordDecl r fields) -> State $ foldr (\field acc -> M.insert (FieldID r field) (Poset [Zero]) acc) result fields
    AssignAction (AST.ArrayIndex arr a1) a2 -> if (arithmeticSign a1 (State result) `infimum` Poset [Zero, Plus]) /= Poset [] then State $ M.insertWith supremum (ArrayID arr) (arithmeticSign a2 (State result)) result else bottom
    AssignAction x rval -> State $ M.insert (lval2ID x) (arithmeticSign rval (State result)) result
    ReadAction lval -> State $ M.insert (lval2ID lval) top result
    BoolAction rval -> if (foldr supremum bottom (filter (\s -> let Poset s' = boolSign rval s in S.member True s') $ basic (State $ M.filterWithKey (\k _ -> k `S.member` (traceShowId  usedIDs)) result))) == bottom then bottom else   State (M.filterWithKey (\k _ -> k `S.notMember` usedIDs) result) `supremum` foldr supremum bottom (filter (\s -> let Poset s' = boolSign rval s in S.member True s') $ basic (State $ M.filterWithKey (\k _ -> k `S.member` (traceShowId  usedIDs)) result))
      where
        usedIDs = S.fromList $ map lval2ID (action ^.. biplate :: [LValue 'CInt])
    _ -> State result -- write, jump

-- ARITHMETIC

arithmeticSign :: RValue 'CInt -> State -> Poset Sign
arithmeticSign (Literal x) _
  | x > 0 = Poset [Plus]
  | x == 0 = Poset [Zero]
  | x < 0 = Poset [Minus]
arithmeticSign (Reference (AST.ArrayIndex a rval)) (State state) =
  if (arithmeticSign rval (State state) `infimum` Poset [Zero, Plus]) /= bottom
    then fromMaybe bottom $ M.lookup (ArrayID a) state
    else bottom
arithmeticSign (Reference x) (State state) = fromMaybe bottom $ M.lookup (lval2ID x) state
arithmeticSign (OpA left op right) (State state) = absOpA op (arithmeticSign left (State state)) (arithmeticSign right (State state))
arithmeticSign _ _ = top

absOpA :: OpArith -> Poset Sign -> Poset Sign -> Poset Sign
absOpA op (Poset left) (Poset right) = Poset . S.unions . S.map (uncurry operator) $ S.cartesianProduct left right
  where
    operator = case op of
      Add  -> absPlus
      Sub  -> absMinus
      Mult -> absMult
      Div  -> absDiv
      Mod  -> absMod

absPlus :: Sign -> Sign -> S.Set Sign
absPlus s1 s2 = case s1 of
  Minus -> if s2 == Plus then [Minus, Zero, Plus] else [Minus]
  Zero  -> [s2]
  Plus  -> if s2 == Minus then [Minus, Zero, Plus] else [Plus]

absMinus :: Sign -> Sign -> S.Set Sign
absMinus Minus Minus = [Minus, Zero, Plus]
absMinus Minus _     = [Minus]
absMinus Zero Plus   = [Minus]
absMinus Zero Zero   = [Zero]
absMinus Zero Minus  = [Plus]
absMinus Plus Plus   = [Minus, Zero, Plus]
absMinus Plus _      = [Plus]

absMult :: Sign -> Sign -> S.Set Sign
absMult Minus Minus = [Plus]
absMult Minus Plus  = [Minus]
absMult _ Zero      = [Zero]
absMult Zero _      = [Zero]
absMult Plus Minus  = [Minus]
absMult Plus Plus   = [Plus]

absDiv :: Sign -> Sign -> S.Set Sign
absDiv Minus Minus = [Plus]
absDiv Minus Plus  = [Minus]
absDiv _ Zero      = []
absDiv Zero _      = [Zero]
absDiv Plus Minus  = [Minus]
absDiv Plus Plus   = [Plus]

absMod :: Sign -> Sign -> S.Set Sign
absMod Minus Minus = [Zero, Minus]
absMod Minus Plus  = [Zero, Minus]
absMod _ Zero      = []
absMod Zero _      = [Zero]
absMod Plus Plus   = [Zero, Plus]
absMod Plus Minus  = [Zero, Plus]

-- BOOLEANS

boolSign :: RValue 'CBool -> State -> Poset Bool
boolSign (Literal b) _  = Poset [b]
boolSign (OpB left  op right) s = absOpB op (boolSign left s) (boolSign right s)
boolSign (OpR left  op right) s = absOpR op (arithmeticSign left s) (arithmeticSign right s)
boolSign (Not rval) s = absNot (boolSign rval s)

absNot :: Poset Bool -> Poset Bool
absNot (Poset bSigns) = Poset $ S.map not bSigns

absOpB :: OpBool -> Poset Bool -> Poset Bool -> Poset Bool
absOpB op (Poset left) (Poset right) = Poset . S.unions . S.map (uncurry operator) $ S.cartesianProduct left right
  where
    operator b1 b2 = case op of
      And -> [b1 && b2]
      Or  -> [b1 || b2]

-- RELATIONAL OPERATORS

absOpR :: OpRel  -> Poset Sign -> Poset Sign -> Poset Bool
absOpR op (Poset left) (Poset right) = Poset . S.unions . S.map (uncurry operator) $ S.cartesianProduct left right
  where
    operator = case op of
      Lt  -> absLt
      Gt  -> absGt
      Le  -> absLe
      Ge  -> absGe
      Eq  -> absEq
      Neq -> absNeq

absLt :: Sign -> Sign -> S.Set Bool
absLt Minus Minus = [True, False]
absLt Minus Plus  = [True]
absLt Minus Zero  = [True]
absLt Zero Zero   = [False]
absLt Zero Minus  = [False]
absLt Zero Plus   = [True]
absLt Plus Plus   = [True, False]
absLt Plus Zero   = [False]
absLt Plus Minus  = [False]

absGt :: Sign -> Sign -> S.Set Bool
absGt Minus Minus = [True, False]
absGt Minus Plus  = [False]
absGt Minus Zero  = [False]
absGt Zero Zero   = [False]
absGt Zero Minus  = [True]
absGt Zero Plus   = [False]
absGt Plus Plus   = [True, False]
absGt Plus Zero   = [True]
absGt Plus Minus  = [True]

absLe :: Sign -> Sign -> S.Set Bool
absLe Minus Minus = [True, False]
absLe Minus Plus  = [True]
absLe Minus Zero  = [True]
absLe Zero Zero   = [True]
absLe Zero Minus  = [False]
absLe Zero Plus   = [True]
absLe Plus Plus   = [True, False]
absLe Plus Zero   = [False]
absLe Plus Minus  = [False]

absGe :: Sign -> Sign -> S.Set Bool
absGe Minus Minus = [True, False]
absGe Minus Plus  = [False]
absGe Minus Zero  = [False]
absGe Zero Zero   = [True]
absGe Zero Minus  = [True]
absGe Zero Plus   = [False]
absGe Plus Plus   = [True, False]
absGe Plus Zero   = [True]
absGe Plus Minus  = [True]

absEq :: Sign -> Sign -> S.Set Bool
absEq Zero Zero = [True]
absEq Zero _    = [False]
absEq _ Zero    = [False]
absEq _ _       = [True, False]

absNeq :: Sign -> Sign -> S.Set Bool
absNeq Zero Zero = [False]
absNeq Zero _    = [True]
absNeq _ Zero    = [True]
absNeq _ _       = [True, False]

-- BASIC MEMORY GENERATION

mapProduct :: M.Map ID (Poset Sign) -> [M.Map ID (Poset Sign)]
mapProduct = map M.fromList . mapM choose . M.assocs
  where
    choose :: (ID, Poset Sign) -> [(ID, Poset Sign)]
    choose (ArrayID arr, p) = [(ArrayID arr, p)]
    choose (x, Poset vs)    = [(x, Poset [v]) | v <- S.toList vs]

basic :: State -> [State]
basic (State m) = map State $ mapProduct m
