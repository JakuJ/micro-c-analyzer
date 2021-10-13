{-# LANGUAGE TemplateHaskell #-}

module MicroC.Analysis.IntervalAnalysis where

import           Control.Lens                        hiding (op)
import           Control.Monad.State.Lazy
import           Data.Lattice                        (Lattice (..))
import           Data.List                           (intercalate)
import qualified Data.Map.Lazy                       as M
import qualified Data.Set                            as S
import           MicroC.AST
import           MicroC.Analysis
import           MicroC.Analysis.ReachingDefinitions (getAllNames)
import           MicroC.ID
import           MicroC.ProgramGraph                 (Action (..))

data Int'
  = NegInf
  | Int Int
  | Inf
  deriving (Eq)

instance Num Int' where
  (Int x) + (Int y) = Int $ x + y
  Inf + NegInf      = undefined
  NegInf + Inf      = undefined
  NegInf + _        = NegInf
  _ + NegInf        = NegInf
  Inf + _           = Inf
  _ + Inf           = Inf

  (Int x) * (Int y) = Int $ x * y
  _ * (Int 0)       = Int 0
  (Int 0) * _       = Int 0
  NegInf * x        = if signum x == -1 then Inf else NegInf
  x * NegInf        = NegInf * x
  Inf * x           = if signum x == -1 then NegInf else Inf
  x * Inf           = Inf * x

  abs = \case
    Inf    -> Inf
    NegInf -> Inf
    Int n  -> Int $ abs n

  signum = \case
    Inf    -> Int 1
    NegInf -> Int (-1)
    Int n  -> Int $ signum n

  fromInteger = Int . fromInteger
  negate = \case
    Inf    -> NegInf
    NegInf -> Inf
    Int n  -> Int $ negate n

instance Ord Int' where
  NegInf <= _        = True
  _ <= NegInf        = False
  _ <= Inf           = True
  Inf <= _           = False
  (Int a) <= (Int b) = a <= b

instance Bounded Int' where
  minBound = NegInf
  maxBound = Inf

instance Show Int' where
  show Inf     = "Infinity"
  show NegInf  = "-Infinity"
  show (Int n) = show n

-- | The type of the sign
data Interval
  = Bottom
  | Between Int' Int'
  deriving (Eq)

makePrisms ''Interval

instance Ord Interval where
  Bottom <= Bottom           = True
  Bottom <= Between _ _      = True
  Between _ _ <= Bottom      = False
  Between a b <= Between c d
    | a > b || c > d = error $ "Invalid intervals: " <> show (Between a b) <> " and " <> show (Between c d)
    | otherwise = c <= a && a <= d && c <= b && b <= d

instance Lattice Interval where
  bottom = Bottom
  order = (<=)
  supremum Bottom x                    = x
  supremum x Bottom                    = x
  supremum (Between a b) (Between c d) = Between (min a c) (max b d)
  infimum Bottom _                    = Bottom
  infimum _ Bottom                    = Bottom
  infimum i1@(Between a b) i2@(Between c d)
    | b < c || a > d = Bottom
    -- case 1 :: [a    (c   b]   d)
    | c <= b && a <= c && b <= d = Between c b
    -- case 2 :: [a  (c d)  b]
    | i2 <= i1 = i2
    -- case 3 :: (c  [a  b]  d)
    | i1 <= i2 = i1
    -- case 4 :: (c  [a  d)  b]
    | c <= a && a <= d && d <= b = Between a d
    | otherwise = Bottom

instance Show Interval where
  show Bottom        = "_|_"
  show (Between a b) = "[" <> show a <> ", " <> show b <> "]"

-- | An empty data type for instantiating the analysis.
data IA

newtype Union = Union (M.Map ID Interval)
  deriving (Eq)

instance Lattice Union where
  bottom = Union M.empty
  (Union m1) `order` (Union m2)
      = M.foldrWithKey (\k int acc ->
          case M.lookup k m2 of
            Nothing     -> False
            (Just int2) -> acc && int `order` int2) True m1
  supremum (Union a) (Union b) = Union $ M.unionWith supremum a b
  infimum (Union a) (Union b) = Union $ M.unionWith infimum a b

instance Show Union where
  show (Union (M.assocs -> lst))
    = "[" <> intercalate ", " (map (\(a, b) -> show a <> " -> " <> show b) lst) <> "]"

instance Analysis IA where
  type Result IA = Union
  direction = Forward
  initialValue pg = Union $ M.fromDistinctAscList $ map (, Between NegInf Inf) $ S.toAscList (getAllNames pg)
  analyze _ (_, action, _) (Union results) = Union $ execState (evalAction action) results

-- TODO: Make these not constant
min', max' :: Int'
min' = Int (-100)
max' = Int 100

normalizeMin, normalizeMax :: Int' -> Int'
normalizeMin a
  | min' <= a && a <= max' = a
  | a < min' = NegInf
  | a > max' = max'
  | otherwise = error "Unreachable"

normalizeMax b
  | min' <= b && b <= max' = b
  | b > max' = Inf
  | b < min' = min'
  | otherwise = error "Unreachable"

type Eval = State (M.Map ID Interval)

evalExpr :: RValue 'CInt -> Eval Interval
evalExpr (Literal (Int -> x))
  | x < min'  = pure $ Between NegInf min'
  | x > max'  = pure $ Between max' Inf
  | otherwise = pure $ Between x x
evalExpr (Reference (lval2ID -> i)) = use (at i . non (Between NegInf Inf))
evalExpr (OpA left op right) = evalOp op <$> evalExpr left <*> evalExpr right

evalOp :: OpArith -> Interval -> Interval -> Interval
evalOp _ Bottom _ = Bottom
evalOp _ _ Bottom = Bottom
evalOp op (Between a1 b1) (Between a2 b2) = case op of
  Add -> let
    a | a1 == NegInf || a2 == NegInf = NegInf
      | otherwise = normalizeMin $ a1 + a2
    b | b1 == Inf || b2 == Inf = Inf
      | otherwise = normalizeMax $ b1 + b2
    in Between a b
  Sub -> Between (normalizeMin (a1 - b2)) $ normalizeMax (b1 - a2)
  Mult ->
    let combos = [a1 * a2, a1 * b2, b1 * a2, b1 * b2]
    in Between (normalizeMin (minimum combos)) (normalizeMax (maximum combos))
  _ -> Between NegInf Inf -- TODO: Other operators

evalAction :: Action -> Eval ()
evalAction = \case
  DeclAction de -> forM_ (def2IDs de) $ \i -> at i <~ Just <$> evalExpr (Literal 0)
  AssignAction (lval2ID -> lv) rv -> at lv <~ Just <$> evalExpr rv
  ReadAction (lval2ID -> lv) -> at lv .= Just (Between NegInf Inf)
  WriteAction _ -> pure ()
  BoolAction action -> processB action
    where
      processB :: RValue 'CBool -> Eval ()
      processB = \case
        Literal True -> pure ()
        Literal False -> modify (M.map (const Bottom))
        OpR (Reference _) _ (Reference _) -> pure ()
        OpR l op r@(Reference _) -> evalAction $ BoolAction $ OpR r (flipRelation op) l
        OpR l@(Reference (lval2ID -> ref)) op r -> do
          li <- evalExpr l
          ri <- evalExpr r
          case op of
            Lt  -> at ref .= Just (infimum li (extendLeft (ri & _Between . both -~ 1)))
            Gt  -> pure ()
            Le  -> at ref .= Just (infimum li (extendLeft ri))
            Ge  -> at ref .= Just (infimum li (extendRight ri))
            Eq  -> at ref .= Just (infimum li ri)
            Neq -> pure ()
        OpR {} -> pure ()
        OpB l And r -> processB l >> processB r
        OpB _ Or _ -> pure ()
        Not (OpR a op b) -> processB (OpR a (inverseRelation op) b)
        Not (OpB a And b) -> processB $ OpB (Not a) Or (Not b)
        Not (OpB a Or b) -> processB $ OpB (Not a) And (Not b)
        Not (Not b) -> processB b
        Not (Literal b) -> processB $ Literal (not b)

      extendLeft, extendRight :: Interval -> Interval
      extendLeft Bottom        = Bottom
      extendLeft (Between _ x) = Between NegInf x

      extendRight Bottom        = Bottom
      extendRight (Between x _) = Between x Inf

      inverseRelation, flipRelation :: OpRel -> OpRel
      inverseRelation = \case
        Lt  -> Ge
        Le  -> Gt
        Ge  -> Lt
        Gt  -> Le
        Eq  -> Neq
        Neq -> Eq

      flipRelation = \case
        Lt  -> Gt
        Gt  -> Lt
        Ge  -> Le
        Le  -> Ge
        Eq  -> Eq
        Neq -> Neq
