{-# LANGUAGE TemplateHaskell #-}

module Language.MicroC.Analysis.IntervalAnalysis where

import           Control.Lens                                 hiding (op)
import           Control.Monad.State.Lazy
import           Data.Lattice
import           Data.List                                    (intercalate)
import qualified Data.Map.Lazy                                as M
import qualified Data.Set                                     as S
import           Language.MicroC.AST
import           Language.MicroC.Analysis
import           Language.MicroC.Analysis.ReachingDefinitions (getAllNames)
import           Language.MicroC.ProgramGraph

data Int'
  = NegInf
  | Int Int
  | Inf
  deriving (Eq, Ord)

instance Num Int' where
  (Int x) + (Int y) = Int $ x + y
  Inf + NegInf      = undefined
  NegInf + Inf      = undefined
  NegInf + _        = NegInf
  _ + NegInf        = NegInf
  Inf + _           = Inf
  _ + Inf           = Inf

  (Int x) * (Int y) = Int $ x * y
  NegInf * _        = NegInf
  _ * NegInf        = NegInf
  Inf * _           = Inf
  _ * Inf           = Inf

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

instance Ord Interval where
  Bottom <= Bottom           = True
  Bottom <= Between _ _      = True
  Between _ _ <= Bottom      = False
  Between a b <= Between c d = a >= c && b <= d

instance Lattice Interval where
  bottom = Bottom
  order = (<=)
  supremum Bottom x                    = x
  supremum x Bottom                    = x
  supremum (Between a b) (Between c d) = Between (min a c) (max b d)

instance Show Interval where
  show Bottom        = "_|_"
  show (Between a b) = "[" <> show a <> ", " <> show b <> "]"

-- | An empty data type for instantiating the analysis.
data IA

newtype Union = Union (M.Map ID (S.Set Interval))
  deriving (Eq)

instance Lattice Union where
  bottom = Union M.empty
  (Union m1) `order` (Union m2)
      = M.foldrWithKey (\k ints acc ->
          let ints2 = M.findWithDefault S.empty k m2
          in acc && all (\i -> any (i <=) ints2) ints) True m1
  supremum (Union a) (Union b) = Union $ M.unionWith S.union a b

instance Show Union where
  show (Union (M.assocs -> lst))
    = "[" <> intercalate ", " (map (\(a, b) -> show a <> " -> " <> show b) lst) <> "]"

instance Analysis IA where
  type Result IA = Union
  direction = Forward
  initialValue pg = Union $ M.fromDistinctAscList $ map (, S.singleton (Between NegInf Inf)) $ S.toAscList (getAllNames pg)
  analyze _ (_, action, _) (Union results) = Union $ M.map S.fromList $ execState (evalAction action) (M.map S.toList results)

-- TODO: Make these not constant
min', max' :: Int'
min' = Int (-100)
max' = Int 100

type Eval = State (M.Map ID [Interval])

evalExpr :: RValue 'CInt -> Eval [Interval]
evalExpr (Literal (Int -> x))
  | x < min' = pure [Between NegInf min']
  | x > max' = pure [Between max' Inf]
  | otherwise = pure [Between x x]
evalExpr (Reference (lval2ID -> i)) = use (ix i)
evalExpr (OpA left op right) = do
  lefts <- evalExpr left
  rights <- evalExpr right
  pure $ evalOp op <$> lefts <*> rights

evalOp :: OpArith -> Interval -> Interval -> Interval
evalOp _ Bottom _ = Bottom
evalOp _ _ Bottom = Bottom
evalOp op (Between a1 b1) (Between a2 b2) = case op of
  Add -> let
    a | (min' <= a1 + a2) && (a1 + a2 <= max') = a1 + a2
      | a1 + a2 < min' || a1 == NegInf || a2 == NegInf = NegInf
      | a1 + a2 > max' = max'
      | otherwise = error "Impossible happened" -- TODO: Instructive error message?
    b | (min' <= b1 + b2) && (b1 + b2 <= max') = b1 + b2
      | b1 + b2 > max' || b1 == Inf || b2 == Inf = Inf
      | b1 + b2 < min' = min'
      | otherwise = error "Impossible happened"
    in Between a b
  _ -> Between NegInf Inf -- TODO: Other operators

evalAction :: Action -> Eval ()
evalAction = \case
  DeclAction de -> forM_ (def2IDs de) $ \i -> at i <~ Just <$> evalExpr (Literal 0)
  AssignAction (lval2ID -> lv) rv -> at lv <~ Just <$> evalExpr rv
  ReadAction (lval2ID -> lv) -> at lv .= Just [Between NegInf Inf]
  WriteAction _ -> pure ()
  BoolAction _ -> pure () -- TODO: We can improve precision here
