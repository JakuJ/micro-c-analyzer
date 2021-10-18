{-# LANGUAGE TemplateHaskell #-}

module MicroC.Analysis.IntervalAnalysis where

import           Control.Lens                        hiding (ix, op)
import           Control.Monad.State.Lazy
import           Data.Lattice
import           Data.List                           (intercalate)
import qualified Data.Map.Lazy                       as M
import           Data.Map.Lens                       (toMapOf)
import qualified Data.Set                            as S
import           Data.String.Interpolate             (i)
import           MicroC.AST
import           MicroC.Analysis
import           MicroC.Analysis.ReachingDefinitions (getAllNames)
import           MicroC.ID
import           MicroC.ProgramGraph

data Int'
  = NegInf
  | Int Int
  | Inf
  deriving (Eq)

instance Num Int' where
  Inf + NegInf      = error [i|Int' :: attempted to add #{Inf} to #{NegInf}|]
  NegInf + Inf      = error [i|Int' :: attempted to add #{NegInf} to #{Inf}|]
  NegInf + _        = NegInf
  _ + NegInf        = NegInf
  Inf + _           = Inf
  _ + Inf           = Inf
  (Int x) + (Int y) = Int $ x + y

  (Int x) * (Int y) = Int $ x * y
  _ * (Int 0)       = Int 0 -- includes infinities which makes sense for intervals
  (Int 0) * _       = Int 0 -- (inf * 0 = 0) since inf just means (unknowingly big), but 0 is known
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

-- | The type of an interval between two (potentially infinite) integers.
data Interval
  = Bottom
  | Between Int' Int'
  deriving (Eq)

makePrisms ''Interval

instance Ord Interval where
  Bottom      <= _      = True
  _           <= Bottom = False
  Between a b <= Between c d
    | a > b || c > d = error [i|Interval :: One of the intervals is invalid: #{Between a b} and #{Between c d}|]
    | otherwise = c <= a && b <= d

instance SemiLattice Interval where
  bottom = Bottom
  order = (<=)
  supremum Bottom x                    = x
  supremum x Bottom                    = x
  supremum (Between a b) (Between c d) = Between (min a c) (max b d)

instance Lattice Interval where
  top = Between NegInf Inf
  infimum (Between a b) (Between c d)
    | kmin <= kmax = Between kmin kmax
    | otherwise = Bottom
      where
        kmin = max a c
        kmax = min b d
  infimum _ _                     = Bottom

instance Show Interval where
  show Bottom        = "_|_"
  show (Between a b) = [i|[#{a}, #{b}]|]

-- | State of the computation.
data Memory = Memory
  { _arrays    :: M.Map Identifier Int
  , _intervals :: M.Map ID Interval
  }

makeLenses ''Memory

type Eval = State Memory

-- | An empty data type for instantiating the analysis.
data IA

newtype Union = Union (M.Map ID Interval)
  deriving (Eq)

instance SemiLattice Union where
  bottom = Union M.empty
  (Union m1) `order` (Union m2)
      = M.foldrWithKey (\k int acc ->
          case M.lookup k m2 of
            Nothing     -> False
            (Just int2) -> acc && int `order` int2) True m1
  supremum (Union a) (Union b) = Union $ M.unionWith supremum a b

instance Show Union where
  show (Union (M.assocs -> assocs))
    = "[" <> intercalate ", " (map (\(a, b) -> [i|#{a} -> #{b}|]) assocs) <> "]"

instance Analysis IA where
  type Result IA = Union
  direction = Forward
  initialValue pg = Union $ M.fromDistinctAscList $ map (, Between NegInf Inf) $ S.toAscList (getAllNames pg)
  analyze pg (_, action, _) (Union results) = Union $ _intervals $ execState (evalAction action) (Memory (declaredArrays pg) results)

-- | Traverse over array declarations and make a `Map` from their names to their sizes.
declaredArrays :: PG -> M.Map Identifier Int
declaredArrays = toMapOf $ traverse . _2 . _DeclAction . _ArrayDecl . swapped . itraversed

-- TODO: Make these not constant
min', max' :: Int'
min' = Int (-100)
max' = Int 100

normalizeMin, normalizeMax :: Int' -> Int'
normalizeMin a
  | a < min' = NegInf
  | a > max' = max'
  | otherwise = a

normalizeMax b
  | b > max' = Inf
  | b < min' = min'
  | otherwise = b

intOf :: ID -> Lens' Memory (Maybe Interval)
intOf x = intervals . at x

evalExpr :: RValue 'CInt -> Eval Interval
evalExpr (Literal (Int -> x)) = pure $ Between (normalizeMin x) (normalizeMax x)
evalExpr (Reference lv@(lval2ID -> v)) = case lv of
  ArrayIndex name ix -> do
    arrlen <- use (arrays . at name . non 0)
    indexRange <- evalExpr ix
    if indexRange `infimum` Between (normalizeMin 0) (normalizeMax (Int arrlen - 1)) /= Bottom
      -- array have to be declared
      then use (intOf v . non Bottom)
      -- array index out of possible bounds
      else pure Bottom
  -- unknown variables are initialized with garbage
  _ -> use (intOf v . non top)
evalExpr (OpA left op right) = evalOp op <$> evalExpr left <*> evalExpr right

evalOp :: OpArith -> Interval -> Interval -> Interval
evalOp _ Bottom _ = Bottom
evalOp _ _ Bottom = Bottom
evalOp op (Between a1 b1) (Between a2 b2) = case op of
  Add -> Between (normalizeMin $ a1 + a2) (normalizeMax $ b1 + b2)
  Sub -> Between (normalizeMin $ a1 - b2) (normalizeMax $ b1 - a2)
  Mult -> let combos = (*) <$> [a1, a2] <*> [b1, b2]
          in Between (normalizeMin $ minimum combos) (normalizeMax $ maximum combos)
  _ -> Between NegInf Inf -- TODO: Other operators

evalAction :: Action -> Eval ()
evalAction = \case
  DeclAction   de                 -> forM_ (def2IDs de) $ \d -> intOf d <~ Just <$> evalExpr (Literal 0)
  AssignAction (lval2ID -> lv) rv -> case lv of
    -- amalgamated arrays - we don't know which element changed
    ArrayID _ -> do
      rval <- evalExpr rv
      intOf lv %= fmap (supremum rval)
    -- anything else - we know for sure we changed the entire value
    _         -> intOf lv <~ Just <$> evalExpr rv
  ReadAction   (lval2ID -> lv)    -> intOf lv .= Just top
  WriteAction  _                  -> pure ()
  BoolAction   action             -> processB action
    where
      processB :: RValue 'CBool -> Eval ()
      processB = \case
        Literal True  -> pure ()
        Literal False -> intervals %= M.map (const Bottom)
        OpR (Reference _) _ (Reference _) -> pure ()
        OpR l op r@(Reference _)          -> evalAction $ BoolAction $ OpR r (flipRelation op) l
        OpR l@(Reference (lval2ID -> ref)) op r -> do
          li <- evalExpr l
          ri <- evalExpr r
          case op of
            Lt  -> intOf ref .= Just (infimum li (extendLeft (ri & _Between . both -~ 1)))
            Gt  -> pure ()
            Le  -> intOf ref .= Just (infimum li (extendLeft ri))
            Ge  -> intOf ref .= Just (infimum li (extendRight ri))
            Eq  -> intOf ref .= Just (infimum li ri)
            Neq -> case (li, ri) of
              -- if we have [a, a] != [a, a], then we have unreachable code
              (Between a a', Between a'' a''') | a == a' && a' == a'' && a'' == a''' ->
                intOf ref .= Just Bottom
              -- only if we have something like x in [2, 5] != 2 or != 5, then we can conclude x in [3, 5] or [2, 4]
              -- doesn't work with infinities
              (Between a b, Between x x') | x == x' && a /= NegInf && b /= Inf ->
                intOf ref .= Just (if | a == x -> Between (a + 1) b
                                      | b == x -> Between a (b - 1)
                                      | otherwise -> Between a b)
              _ -> pure ()
        OpR {}            -> pure ()
        OpB l And r       -> processB l >> processB r
        OpB _ Or _        -> pure ()
        Not (OpR a op b)  -> processB (OpR a (inverseRelation op) b)
        Not (OpB a And b) -> processB $ OpB (Not a) Or (Not b)
        Not (OpB a Or b)  -> processB $ OpB (Not a) And (Not b)
        Not (Not b)       -> processB b
        Not (Literal b)   -> processB $ Literal (not b)

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
