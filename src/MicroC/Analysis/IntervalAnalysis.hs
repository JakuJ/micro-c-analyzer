{-# LANGUAGE TemplateHaskell #-}

module MicroC.Analysis.IntervalAnalysis
( IA
, AbsMemory(..)
, idiv
, between
) where

import           Control.Lens                        hiding (ix, op)
import           Control.Monad.State.Lazy
import           Data.Data.Lens                      (biplate)
import           Data.IntegerInterval
import qualified Data.Interval                       as I
import           Data.Lattice
import           Data.List                           (intercalate)
import qualified Data.Map.Lazy                       as M
import           Data.Map.Lens                       (toMapOf)
import           Data.Maybe                          (catMaybes)
import           Data.Ratio                          (Ratio, (%))
import qualified Data.Set                            as S
import           Data.String.Interpolate             (i)
import           GHC.Real                            (Ratio ((:%)))
import           MicroC.AST
import           MicroC.Analysis
import           MicroC.Analysis.ReachingDefinitions (getAllNames)
import           MicroC.ID
import           MicroC.ProgramGraph
import           Prelude                             hiding (null)

-- | Integers extended with negative and positive infinity.
type Int' = Extended Integer

-- | An interval over extended integers.
type Interval = IntegerInterval

-- | State of the computation.
data Memory = Memory
  { _arrays    :: M.Map Identifier Int
  , _intervals :: M.Map ID Interval
  }

makeLenses ''Memory

type Eval = State Memory

-- | A data type representing the Inteval analysis.
data IA

-- | The abstract memory type used in the analysis.
newtype AbsMemory = Abs (M.Map ID Interval)
  deriving (Eq)

instance SemiLattice AbsMemory where
  bottom = Abs M.empty
  (Abs m1) `order` (Abs m2)
      = M.foldrWithKey (\k int acc ->
          case M.lookup k m2 of
            Nothing     -> False
            (Just int2) -> acc && int `order` int2) True m1
  supremum (Abs a) (Abs b) = Abs $ M.unionWith supremum a b

instance Analysis IA where
  type Result IA = AbsMemory
  direction = Forward
  initialValue pg = Abs $ M.fromDistinctAscList $ map (, top) $ S.toAscList (getAllNames pg)
  analyze pg (_, action, _) (Abs results) = Abs $ _intervals $ execState (evalAction action) (Memory (declaredArrays pg) results)

-- EVALUATION FUNCTIONS

evalExpr :: RValue 'CInt -> Eval Interval
evalExpr (Literal x) = pure . normalize . singleton . toInteger $ x
evalExpr (Reference lv) = case lv of
  ArrayIndex name ix -> do
    -- we default to 0 as the size here - for undefined arrays this will cause the range to be bottom every time
    arrlen <- toInteger <$> use (arrays . at name . non 0)
    indexRange <- evalExpr ix
    if indexRange `infimum` between 0 (Finite arrlen - 1) /= bottom
      -- arrays have to be declared
      then use (intOf' lv . non bottom)
      -- array index out of possible bounds
      else pure bottom
  -- unknown variables are initialized with garbage
  _ -> use (intOf' lv . non top)
evalExpr (OpA left op right) = evalOp op <$> evalExpr left <*> evalExpr right

evalOp :: OpArith -> Interval -> Interval -> Interval
evalOp _ (null -> True) _ = bottom
evalOp _ _ (null -> True) = bottom
evalOp op i1 i2 = normalize $ case op of
  Add  -> i1 + i2
  Sub  -> i1 - i2
  Mult -> i1 * i2
  Div  -> i1 `idiv` i2
  Mod  -> i1 `imod` i2
  _    -> top -- TODO: Other operators

data Branches
  = Impossible -- {}
  | No         -- {ff}
  | Yes        -- {tt}
  | Dunno      -- {ff, tt}
  deriving (Eq)

evalAction :: Action -> Eval ()
evalAction = \case
  DeclAction   de     -> forM_ (def2IDs de) $ \d -> intOf d <~ Just <$> evalExpr (Literal 0)
  AssignAction lv rv  -> lv <~~ evalExpr rv
  ReadAction   lv     -> lv .== top
  WriteAction  _      -> pure ()
  BoolAction   action -> do
    current <- use intervals
    let usedIDs = map lval2ID (action ^.. biplate :: [LValue 'CInt])
        possibilities = mapProduct usedIDs current
    states <- forM possibilities $ \m -> do
      withState (intervals .~ m) $ do
        outcomes <- processB action
        if outcomes == Yes || outcomes == Dunno
          then Just <$> use intervals
          else pure Nothing
    let (Abs union) = foldr (supremum . Abs) (Abs M.empty) . catMaybes $ states
    intervals .= union
    where
      mapProduct :: [ID] -> M.Map ID Interval -> [M.Map ID Interval]
      mapProduct ids = map M.fromList . traverse (choose ids) . M.assocs
        where
          choose :: [ID] -> (ID, Interval) -> [(ID, Interval)]
          choose _ (ArrayID a, int) = [(ArrayID a, int)]
          choose usedIds (k, int)
            | k `notElem` usedIds = [(k, int)]
            | otherwise = map (k,) basics
                where
                  basics = other <> map (\x -> between (Finite x) (Finite x)) [toInteger' a' .. toInteger' b']
                  ((a', b'), other) = case bounds int of
                    (NegInf, PosInf) -> ((min', max'), [between NegInf min', between max' PosInf])
                    (NegInf, Finite x) -> ((min', Finite x), [between NegInf min'])
                    (Finite x, PosInf) -> ((Finite x, max'), [between max' PosInf])
                    (x, y) -> ((x, y), [])

          toInteger' (Finite x) = x
          toInteger' _          = error "toInteger' :: Unreachable"

      processB :: RValue 'CBool -> Eval Branches
      processB = \case
        Literal True  -> pure Yes
        Literal False -> pure No
        OpR l op r -> processOp op <$> evalExpr l <*> evalExpr r
        OpB l And r   -> do
          l' <- processB l
          r' <- processB r
          case l' of
            Impossible -> pure Impossible
            Yes -> pure r'
            No -> pure No
            Dunno -> pure $ case r' of
              Impossible -> Impossible
              No         -> No
              Dunno      -> Dunno
              Yes        -> Dunno
        OpB l Or r        -> do
          l' <- processB l
          r' <- processB r
          case l' of
            Impossible -> pure Impossible
            Yes -> pure Yes
            No -> pure r'
            Dunno -> pure $ case r' of
              Impossible -> Impossible
              No         -> Dunno
              Dunno      -> Dunno
              Yes        -> Yes
        Not someOp -> do
          branches <- processB someOp
          pure $ case branches of
            Yes -> No
            No  -> Yes
            x   -> x

      processOp :: OpRel -> Interval -> Interval -> Branches
      processOp op l@(bounds -> (a, b)) r@(bounds -> (c, d))
        | null l || null r = Impossible
        | otherwise = case op of
            Lt -> if  | b < c     -> Yes
                      | a >= d    -> No
                      | otherwise -> Dunno
            Gt -> if  | a > d     -> Yes
                      | b <= c    -> No
                      | otherwise -> Dunno
            Le -> if  | b <= c    -> Yes
                      | a > d     -> No
                      | otherwise -> Dunno
            Ge -> if  | a >= d    -> Yes
                      | b < c     -> No
                      | otherwise -> Dunno
            Eq -> if  | a == b && b == c && c == d -> Yes
                      | null $ l `infimum` r       -> No
                      | otherwise                  -> Dunno
            Neq -> if | a == b && b == c && c == d -> No
                      | null $ l `infimum` r       -> Yes
                      | otherwise                  -> Dunno

-- INTERVAL DIVISION

idiv :: Interval -> Interval -> Interval
idiv i1 = fromRatioInterval . I.hulls . map (toRatioInterval i1 *) . inverse . toRatioInterval

inverse :: I.Interval (Ratio Integer) -> [I.Interval (Ratio Integer)]
inverse iv
  | iv == 0 = []
  | 0 `I.notMember` iv = pure $ inv b I.<=..<= inv a
  | a == 0 = pure $ inv b I.<=..<= 1
  | b == 0 = pure $ -1 I.<=..<= inv a
  | otherwise = [-1 I.<=..<= inv a, inv b I.<=..<= 1]
  where
    (a, b) = (I.lowerBound iv, I.upperBound iv)

    inv :: Extended (Ratio Integer) -> Extended (Ratio Integer)
    inv = \case
      NegInf -> 0
      PosInf -> 0
      Finite x -> case x of
        0 :% k -> Finite (signum k % 1) * PosInf
        k :% j -> Finite $ j % k

toRatioInterval :: Interval -> I.Interval (Ratio Integer)
toRatioInterval (bounds -> (a, b)) = ar I.<=..<= br
  where
    (ar, br) = (a, b) & each %~ \case
                                  NegInf   -> NegInf
                                  PosInf   -> PosInf
                                  Finite x -> Finite $ x % 1

fromRatioInterval :: I.Interval (Ratio Integer) -> Interval
fromRatioInterval iv = between (toI a) (toI b)
  where
    (a, b) = (I.lowerBound iv, I.upperBound iv)

    toI :: Extended (Ratio Integer) -> Int'
    toI = \case
      (Finite x) -> toI' x
      NegInf     -> NegInf
      PosInf     -> PosInf

    toI' :: Ratio Integer -> Int'
    toI' (x :% y) = Finite $ x `quot` y

-- INTERVAL MODULO

imod :: Interval -> Interval -> Interval
imod i1@(bounds -> (a, b)) i2@(bounds -> (m, n))
  -- (1): empty intervals
  | null i1 || null i2 = bottom
  -- (2): compute modulo with positive interval and negate
  | b < 0 = -imod (-i1) i2
  -- (3): split into negative and non-negative interval, compute, and join
  | a < 0 = imod (between a (-1)) i2 `hull` imod (between 0 b) i2
  -- (4): use the simpler function
  | m == n = imod1 i1 m
  -- (5): use only non-negative m and n
  | n <= 0 = imod i1 (-i2)
  -- (6): similar to (5), make modulus non-negative
  | m <= 0 = imod i1 $ between 1 (max (-m) n)
  -- (7): compare to (4) in mod1, check b-a < |modulus|
  | b - a >= n = between 0 (n - 1)
  -- (8): similar to (7), split interval, compute, and join
  | b - a >= m = between 0 (b - a - 1) `hull` imod i1 (between (b - a + 1) n)
  -- (9): modulo has no effect
  | m > b = i1
  -- (10): there is some overlapping of [a,b] and [n,m]
  | n > b = between 0 b
  -- (11): either compute all possibilities and join, or be imprecise
  | otherwise = between 0 (n - 1) -- imprecise

imod1 :: Interval -> Int' -> Interval
imod1 x@(bounds -> (a, b)) m
  -- (1): empty interval or division by zero
  | m == 0 || null x = bottom
  -- (2): compute modulo with positive interval and negate
  | b < 0 = - imod1 (-x) m
  -- (3): split into negative and non-negative interval, compute and join
  | a < 0 = imod1 (between a (-1)) m `hull` imod1 (between 0 b) m
  -- (4): there is no k > 0 such that a < k * m <= b
  | b - a < abs m && a `erem` m <= b `erem` m = between (a `erem` m) (b `erem` m)
  -- (5): we can't do better than that
  | otherwise = between 0 (abs m - 1)
  where
    erem :: Int' -> Int' -> Int'
    erem = curry $ \case
      (Finite a', Finite m') -> Finite $ a' `rem` m'
      (infty, Finite _)      -> infty
      (k, _)                 -> k

-- HELPER FUNCTIONS

-- | Traverse over array declarations and make a `Map` from their names to their sizes.
declaredArrays :: PG -> M.Map Identifier Int
declaredArrays = toMapOf $ traverse . _2 . _DeclAction . _ArrayDecl . swapped . itraversed

-- TODO: Make these not constant
min', max' :: Int'
min' = -100
max' = 100

-- | Smart constructor for Intervals bounded between the min and max values.
between :: Int' -> Int' -> Interval
between a b = a' <=..<= b'
  where
    a'
      | a < min' = NegInf
      | a > max' = max'
      | otherwise = a
    b'
      | b > max' = PosInf
      | b < min' = min'
      | otherwise = b

-- | Interval normalization function.
normalize :: Interval -> Interval
normalize a = between (lowerBound a) (upperBound a)

-- | Focuses on the 'Interval' assigned to the provided 'ID' in the abstract memory.
intOf :: ID -> Lens' Memory (Maybe Interval)
intOf x = intervals . at x

-- | Focuses on the 'Interval' assigned an 'ID' of the provided 'LValue' in the abstract memory.
intOf' :: LValue 'CInt -> Lens' Memory (Maybe Interval)
intOf' = intOf . lval2ID

-- | Assign a new interval to an 'LValue' in the abstract memory.
--   Variables and records are overwritten, arrays are extended with `supremum`.
(.==) :: LValue 'CInt -> Interval -> Eval ()
lv .== r = case lv of
  ArrayIndex _ _ -> intOf' lv %= fmap (supremum r)
  _              -> intOf' lv .= Just r

-- | A version of '.==' that takes a monadic argument.
(<~~) :: LValue 'CInt -> Eval Interval -> Eval ()
(<~~) lv = ((lv .==) =<<)

bounds :: Interval -> (Int', Int')
bounds x = (lowerBound x, upperBound x)

-- INSTANCES

instance Show AbsMemory where
  show (Abs (M.assocs -> assocs))
    = intercalate ", " (map (\(a, b) -> [i|#{a} #{pprint b}|]) assocs)
    where
      pprint :: Interval -> String
      pprint iv
        | null iv = "∈ {}"
        | isSingleton iv = "= " <> pprint' (lowerBound iv)
        | otherwise = case (lowerBound iv, upperBound iv) of
          (NegInf, PosInf)     -> ": any"
          (NegInf, x)          -> [i|<= #{pprint' x}|]
          (x, PosInf)          -> [i|>= #{pprint' x}|]
          (Finite a, Finite b) -> [i|∈ { #{a}..#{b} }|]
          _                    -> error $ "Invalid interval: " <> show iv
      pprint' :: Int' -> String
      pprint' (Finite x) = show x
      pprint' NegInf     = "-∞"
      pprint' PosInf     = "∞"
