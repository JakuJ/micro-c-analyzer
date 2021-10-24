{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleInstances #-}
module MicroC.Analysis.DetectionOfSigns
( DS
) where
import           Data.Lattice
import           Data.List
import qualified Data.Map                            as M
import qualified Data.Set                            as S
import           Data.String.Interpolate
import           MicroC.AST                          hiding (Variable)
import qualified MicroC.AST                          as AST
import           MicroC.Analysis
import           MicroC.Analysis.ReachingDefinitions (getAllNames)
import           MicroC.ID
import           MicroC.ProgramGraph



data Sign = Minus | Zero | Plus
  deriving (Eq, Ord)

data BoolSign = Tt | Ff
  deriving (Eq, Ord)

-- | An empty data type for instantiating the analysis.
data DS

newtype State = State (M.Map ID (Poset Sign))
  deriving (Eq)

instance Show State where
  show (State (M.assocs -> assocs))
    = intercalate ", " (map (\(a, b) -> [i|#{a} : #{b}|]) assocs)

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


instance Lattice (Poset Sign) where
  infimum (Poset s1) (Poset s2) = Poset $ S.intersection s1 s2
  top = Poset $ S.fromList [Plus, Zero, Minus]



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
    BoolAction rval -> foldr (\ s1 s2 -> s1 `supremum` s2) (State M.empty) $ filter (\ s -> (Poset (S.singleton Tt)) `order` (boolSign rval s)) (basic (State result))
    _ -> State result


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
absMod Minus Plus  = S.fromList [Zero, Minus]
absMod _ Zero      = S.empty
absMod Zero _      = S.singleton Zero
absMod Plus Plus   = S.fromList [Zero, Plus]
absMod Plus Minus  = S.fromList [Zero, Plus]



boolSign :: RValue 'CBool -> State -> Poset BoolSign
boolSign (Literal b) _ = if b then Poset (S.singleton Tt) else  Poset (S.singleton Ff)
boolSign (OpB left  op right) (State state) = absOpB op (boolSign left (State state)) (boolSign right (State state))
boolSign (OpR left  op right) (State state) = absOpR op (arithmeticSign left (State state)) (arithmeticSign right (State state))
boolSign (Not rval) (State state) = absNot  (boolSign rval (State state))


absNot :: Poset BoolSign -> Poset BoolSign
absNot (Poset bSigns) = Poset $ S.map (\ sign -> if sign == Tt then Ff else Tt) bSigns


absOpB :: OpBool -> Poset BoolSign -> Poset BoolSign -> Poset BoolSign
absOpB op (Poset left) (Poset right) = case op of
  And -> Poset $ foldMap S.union  (S.map  (\ (leftBool, rightBool) -> absAnd leftBool rightBool) (S.cartesianProduct left right)) S.empty
  Or -> Poset $ foldMap S.union  (S.map  (\ (leftBool, rightBool) -> absOr leftBool rightBool) (S.cartesianProduct left right)) S.empty

absAnd :: BoolSign -> BoolSign -> S.Set BoolSign
absAnd Tt Tt = S.singleton Tt
absAnd _ _   = S.singleton Ff

absOr :: BoolSign -> BoolSign -> S.Set BoolSign
absOr Ff Ff = S.singleton Ff
absOr _ _   = S.singleton Tt


absOpR :: OpRel  -> Poset Sign -> Poset Sign -> Poset BoolSign
absOpR op (Poset left) (Poset right) = case op of
  Lt -> Poset $ foldMap S.union  (S.map  (\ (leftBool, rightBool) -> absLt leftBool rightBool) (S.cartesianProduct left right)) S.empty
  Gt -> Poset $ foldMap S.union  (S.map  (\ (leftBool, rightBool) -> absGt leftBool rightBool) (S.cartesianProduct left right)) S.empty
  Le -> Poset $ foldMap S.union  (S.map  (\ (leftBool, rightBool) -> absLe leftBool rightBool) (S.cartesianProduct left right)) S.empty
  Ge -> Poset $ foldMap S.union  (S.map  (\ (leftBool, rightBool) -> absGe leftBool rightBool) (S.cartesianProduct left right)) S.empty
  Eq -> Poset $ foldMap S.union  (S.map  (\ (leftBool, rightBool) -> absEq leftBool rightBool) (S.cartesianProduct left right)) S.empty
  Neq -> Poset $ foldMap S.union  (S.map  (\ (leftBool, rightBool) -> absNeq leftBool rightBool) (S.cartesianProduct left right)) S.empty


absLt :: Sign -> Sign -> S.Set BoolSign
absLt Minus Minus = S.fromList [Tt, Ff]
absLt Minus Plus  = S.singleton Tt
absLt Minus Zero  = S.singleton Tt
absLt Zero Zero   = S.singleton Ff
absLt Zero Minus  = S.singleton Ff
absLt Zero Plus   = S.singleton Tt
absLt Plus Plus   =  S.fromList [Tt, Ff]
absLt Plus Zero   =  S.singleton Ff
absLt Plus Minus  =  S.singleton Ff

absGt :: Sign -> Sign -> S.Set BoolSign
absGt Minus Minus = S.fromList [Tt, Ff]
absGt Minus Plus  = S.singleton Ff
absGt Minus Zero  = S.singleton Ff
absGt Zero Zero   = S.singleton Ff
absGt Zero Minus  = S.singleton Tt
absGt Zero Plus   = S.singleton Ff
absGt Plus Plus   =  S.fromList [Tt, Ff]
absGt Plus Zero   =  S.singleton Tt
absGt Plus Minus  =  S.singleton Tt

absLe :: Sign -> Sign -> S.Set BoolSign
absLe Minus Minus = S.fromList [Tt, Ff]
absLe Minus Plus  = S.singleton Tt
absLe Minus Zero  = S.singleton Tt
absLe Zero Zero   = S.singleton Tt
absLe Zero Minus  = S.singleton Ff
absLe Zero Plus   = S.singleton Tt
absLe Plus Plus   =  S.fromList [Tt, Ff]
absLe Plus Zero   =  S.singleton Ff
absLe Plus Minus  =  S.singleton Ff


absGe :: Sign -> Sign -> S.Set BoolSign
absGe Minus Minus = S.fromList [Tt, Ff]
absGe Minus Plus  = S.singleton Ff
absGe Minus Zero  = S.singleton Ff
absGe Zero Zero   = S.singleton Tt
absGe Zero Minus  = S.singleton Tt
absGe Zero Plus   = S.singleton Ff
absGe Plus Plus   =  S.fromList [Tt, Ff]
absGe Plus Zero   =  S.singleton Tt
absGe Plus Minus  =  S.singleton Tt

absEq :: Sign -> Sign -> S.Set BoolSign
absEq Zero Zero = S.singleton Tt
absEq Zero _    = S.singleton Ff
absEq _ Zero    = S.singleton Ff
absEq _ _       = S.fromList [Tt, Ff]


absNeq :: Sign -> Sign -> S.Set BoolSign
absNeq Zero Zero = S.singleton Ff
absNeq Zero _    = S.singleton Tt
absNeq _ Zero    = S.singleton Tt
absNeq _ _       = S.fromList [Tt, Ff]



mapProduct :: M.Map ID (Poset Sign) -> [M.Map ID (Poset Sign)]
mapProduct = map (M.fromList . posetify) . mapM choose . M.assocs
  where
    choose :: (a, Poset b) -> [(a, b)]
    choose (k, Poset vs) = do
      v <- S.toList vs
      pure (k, v)

    posetify :: [(ID, Sign)] -> [(ID, Poset Sign)]
    posetify = map (fmap (Poset . S.singleton))

basic :: State -> [State]
basic (State m) =  map (\x -> State x) $ mapProduct m
