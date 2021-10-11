{-# LANGUAGE TemplateHaskell #-}

module Language.MicroC.Analysis.IntervalAnalysis where

import           Control.Lens                                 hiding (op)
import           Control.Monad.State.Lazy
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

instance Show Interval where
  show Bottom        = "_|_"
  show (Between a b) = "[" <> show a <> ", " <> show b <> "]"

-- | An empty data type for instantiating the analysis.
data IA

instance Analysis IA where
  type Result IA = (ID, Interval)
  direction = Forward
  bottomValue = S.empty
  constraint (S.toList -> s1) (S.toList -> s2)
    = flip all s1 $ \(x, rng1) -> flip any s2 $ \(y, rng2) -> x == y && rng1 <= rng2

  initialValue pg = S.mapMonotonic (, Between NegInf Inf) $ getAllNames pg
  analyze _ (_, action, _) results = map2set $ execState (evalAction action) (set2map results)

-- TODO: Make these not constant
min', max' :: Int'
min' = Int (-100)
max' = Int 100

set2map :: Ord a => S.Set (a, b) -> M.Map a [b]
set2map = M.fromListWith (++) . map (fmap pure) . S.elems

map2set :: (Ord a, Ord b) => M.Map a [b] -> S.Set (a, b)
map2set = S.fromList . concatMap (\(a, bs) -> map (a,) bs) . M.assocs

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
