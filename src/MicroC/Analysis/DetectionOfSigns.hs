{-# LANGUAGE FlexibleInstances #-}

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


data Sign = Minus | Zero | Plus
  deriving (Eq, Ord, Bounded, Enum)

-- | An empty data type for instantiating the analysis.
data DS

newtype State = State (M.Map ID (Poset Sign))
  deriving (Eq)

instance Show State where
  show (State (M.assocs -> assocs))
    = "[" ++ intercalate ", " (map (\(a, b) -> [i|#{a} : #{b}|]) assocs) ++ "]"

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

only :: Sign -> Poset Sign
only = Poset . S.singleton

instance Analysis DS where
  type Result DS = State
  direction = Forward
  initialValue pg = State $ M.fromList $ map (, top) $ S.toList (getAllNames pg)
  analyze _ (_, action, _) (State result) = case action of
    DeclAction (VariableDecl x) -> State $ M.insert (VariableID x) (only Zero) result
    DeclAction (ArrayDecl _ a) -> State $ M.insert (ArrayID a) (only Zero) result
    DeclAction (RecordDecl r fields) -> State $ foldr (\field acc -> M.insert (FieldID r field) (only Zero) acc) result fields
    -- TODO: Bounds check
    AssignAction (AST.ArrayIndex a _) rval -> State $ M.insertWith supremum (ArrayID a) (arithmeticSign rval (State result)) result
    AssignAction x rval -> State $ M.insert (lval2ID x) (arithmeticSign rval (State result)) result
    ReadAction lval -> State $ M.insert (lval2ID lval) top result
    BoolAction rval -> (State $ M.filterWithKey (\k _ -> k `S.notMember` usedIDs) result) `supremum` (foldr supremum bottom
                      $ filter (\s -> let Poset s' = boolSign rval s in S.member True s') $ basic (State $ M.filterWithKey (\k _ -> k `S.member` usedIDs) result))
      where
        usedIDs = S.fromList $ map lval2ID (action ^.. biplate :: [LValue 'CInt])
    _ -> State result


arithmeticSign :: RValue 'CInt -> State -> Poset Sign
arithmeticSign (Literal x) _
  | x > 0 = Poset (S.singleton Plus)
  | x == 0 = Poset (S.singleton Zero)
  | x < 0 = Poset (S.singleton Minus)
arithmeticSign (Reference (AST.ArrayIndex a rval)) (State state) = if (arithmeticSign rval (State state) `infimum` Poset (S.fromList [Zero, Plus])) /= bottom
                                                                  -- then fromMaybe bottom $ M.lookup (ArrayID a) state
                                                                  then fromMaybe bottom $ M.lookup (ArrayID a) state
                                                                  else bottom
-- arithmeticSign (Reference x) (State state) = fromMaybe (error $ show x <> " is not in the map") $ M.lookup (lval2ID x) state
arithmeticSign (Reference x) (State state) = fromMaybe bottom $ M.lookup (lval2ID x) state
-- arithmeticSign (Reference x) (State state) = state M.! lval2ID x
arithmeticSign (OpA left op right) (State state) = absOpA op (arithmeticSign left (State state)) (arithmeticSign right (State state))
arithmeticSign _ _ = top

absOpA :: OpArith -> Poset Sign -> Poset Sign -> Poset Sign
absOpA op (Poset left) (Poset right) = case op of
  Add ->  Poset $ foldMap S.union  (S.map  (uncurry absPlus) (S.cartesianProduct left right)) S.empty
  Sub ->  Poset $ foldMap S.union  (S.map  (uncurry absMinus) (S.cartesianProduct left right)) S.empty
  Mult ->  Poset $ foldMap S.union  (S.map  (uncurry absMult) (S.cartesianProduct left right)) S.empty
  Div ->  Poset $ foldMap S.union  (S.map  (uncurry absDiv) (S.cartesianProduct left right)) S.empty
  Mod -> Poset $ foldMap S.union  (S.map  (uncurry absMod) (S.cartesianProduct left right)) S.empty
  _ -> top -- BitAnd and BitOr

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
absMod Minus Plus  = S.fromList [Zero, Minus]
absMod _ Zero      = S.empty
absMod Zero _      = S.singleton Zero
absMod Plus Plus   = S.fromList [Zero, Plus]
absMod Plus Minus  = S.fromList [Zero, Plus]



boolSign :: RValue 'CBool -> State -> Poset Bool
boolSign (Literal b) _  = Poset (S.singleton b)
boolSign (OpB left  op right) s = absOpB op (boolSign left s) (boolSign right s)
boolSign (OpR left  op right) s = absOpR op (arithmeticSign left s) (arithmeticSign right s)
boolSign (Not rval) s = absNot (boolSign rval s)


absNot :: Poset Bool -> Poset Bool
absNot (Poset bSigns) = Poset $ S.map not bSigns

absOpB :: OpBool -> Poset Bool -> Poset Bool -> Poset Bool
absOpB op (Poset left) (Poset right) = case op of
  And -> Poset $ foldMap S.union  (S.map  (uncurry absAnd) (S.cartesianProduct left right)) S.empty
  Or -> Poset $ foldMap S.union  (S.map  (uncurry absOr) (S.cartesianProduct left right)) S.empty

absAnd :: Bool -> Bool -> S.Set Bool
absAnd b1 b2  = S.singleton (b1 && b2)

absOr :: Bool -> Bool -> S.Set Bool
absOr b1 b2 = S.singleton (b1 || b2)


absOpR :: OpRel  -> Poset Sign -> Poset Sign -> Poset Bool
absOpR op (Poset left) (Poset right) = case op of
  Lt -> Poset $ foldr S.union S.empty  (S.map  (uncurry absLt) (S.cartesianProduct left right))
  Gt -> Poset $ foldr S.union S.empty  (S.map  (uncurry absGt) (S.cartesianProduct left right))
  Le -> Poset $ foldr S.union S.empty  (S.map  (uncurry absLe) (S.cartesianProduct left right))
  Ge -> Poset $ foldr S.union S.empty  (S.map  (uncurry absGe) (S.cartesianProduct left right))
  Eq -> Poset $ foldr S.union S.empty  (S.map  (uncurry absEq) (S.cartesianProduct left right))
  Neq -> Poset $ foldr S.union S.empty (S.map  (uncurry absNeq) (S.cartesianProduct left right))


absLt :: Sign -> Sign -> S.Set Bool
absLt Minus Minus = S.fromList [True, False]
absLt Minus Plus  = S.singleton True
absLt Minus Zero  = S.singleton True
absLt Zero Zero   = S.singleton False
absLt Zero Minus  = S.singleton False
absLt Zero Plus   = S.singleton True
absLt Plus Plus   =  S.fromList [True, False]
absLt Plus Zero   =  S.singleton False
absLt Plus Minus  =  S.singleton False

absGt :: Sign -> Sign -> S.Set Bool
absGt Minus Minus = S.fromList [True, False]
absGt Minus Plus  = S.singleton False
absGt Minus Zero  = S.singleton False
absGt Zero Zero   = S.singleton False
absGt Zero Minus  = S.singleton True
absGt Zero Plus   = S.singleton False
absGt Plus Plus   =  S.fromList [True, False]
absGt Plus Zero   =  S.singleton True
absGt Plus Minus  =  S.singleton True

absLe :: Sign -> Sign -> S.Set Bool
absLe Minus Minus = S.fromList [True, False]
absLe Minus Plus  = S.singleton True
absLe Minus Zero  = S.singleton True
absLe Zero Zero   = S.singleton True
absLe Zero Minus  = S.singleton False
absLe Zero Plus   = S.singleton True
absLe Plus Plus   =  S.fromList [True, False]
absLe Plus Zero   =  S.singleton False
absLe Plus Minus  =  S.singleton False


absGe :: Sign -> Sign -> S.Set Bool
absGe Minus Minus = S.fromList [True, False]
absGe Minus Plus  = S.singleton False
absGe Minus Zero  = S.singleton False
absGe Zero Zero   = S.singleton True
absGe Zero Minus  = S.singleton True
absGe Zero Plus   = S.singleton False
absGe Plus Plus   =  S.fromList [True, False]
absGe Plus Zero   =  S.singleton True
absGe Plus Minus  =  S.singleton True

absEq :: Sign -> Sign -> S.Set Bool
absEq Zero Zero = S.singleton True
absEq Zero _    = S.singleton False
absEq _ Zero    = S.singleton False
absEq _ _       = S.fromList [True, False]


absNeq :: Sign -> Sign -> S.Set Bool
absNeq Zero Zero = S.singleton False
absNeq Zero _    = S.singleton True
absNeq _ Zero    = S.singleton True
absNeq _ _       = S.fromList [True, False]

mapProduct :: M.Map ID (Poset Sign) -> [M.Map ID (Poset Sign)]
mapProduct = map (M.fromList . posetify) . mapM choose . M.assocs
  where
    choose :: (a, Poset b) -> [(a, b)]
    choose (k, Poset vs) = [(k, v) | v <- S.toList vs]

    posetify :: [(ID, Sign)] -> [(ID, Poset Sign)]
    posetify = map (fmap (Poset . S.singleton))

basic :: State -> [State]
basic (State m) = map State $ mapProduct m
