module Language.MicroC.Monads where

import           Control.Monad.State
import           Data.Monoid hiding  (Sum, Product)
import           Data.Semigroup      (Semigroup)

-- Reminder: A functor is a "box" that allows mapping over the values inside
class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

data Either' a b = Left' a | Right' b

canFail :: Int -> Int -> Either String Int
canFail n m
  | m == 0 = Left "wtf man"
  | otherwise = Right $ n `div` m

class Applicative' f where
  pure' :: x -> f x
  (<*>) :: f (a -> b) -> f a -> f b

instance Applicative' Maybe where
  Nothing <*> Nothing = Nothing
  Just _ <*> Nothing = Nothing
  Nothing <*> Just _ = Nothing
  Just f <*> Just v = Just (f v)

instance Applicative' [] where
  fs <*> vals = [f x | f <- fs, x <- vals]

class Monad' m where
  return' :: a -> m a
  (>>>=) :: m a -> (a -> m b) -> m b

instance Monad' Maybe where
  return' a = Just a
  Nothing >>>= f = Nothing
  (Just v) >>>= f = f v -- >>=

noZero 0 = Nothing
noZero n = Just n

addMaybe a = Just (a + 3)

divBy a b = if b == 0 then Nothing else Just $ a `div` b

program :: Maybe Int
program = do
  x <- noZero 3 :: Maybe Int -- x :: Int, x = 3
  y <- addMaybe x :: Maybe Int
  divBy 12 y

test = (noZero 3 >>= (\x -> addMaybe x >>= \y -> divBy 12 y))

instance Monad' [] where
  -- return :: a -> [a]
  return' a = [a]
  -- (>>>=) :: [a] -> (a -> [b]) -> [b]
  [] >>>= f = []
  (x : xs) >>>= f = f x ++ (xs >>>= f)
  vals >>>= f = concatMap f vals

diceRoll :: [(Int, Int)]
diceRoll = do
  x <- [1 .. 6] -- x is any number from 1 .. 6
  y <- [1 .. 6]
  if x == y then [] else [(x, y)]

canFail2 :: Int -> Maybe Int
canFail2 n = do
  y <- if n == 0 then Nothing else Just n
  let k = y + 3 :: Int
  pure $ k + 2

-- Functor => Applicative => Monad

type Stateful s a b = (a, s) -> (b, s)

sum l = sum' l 0
  where
    sum' [] acc       = acc
    sum' (x : xs) acc = sum' xs (acc + x)

-- class Monad m => MonadState s m where
--   get :: m s
--   put :: s -> m ()
--   modify :: (s -> s) -> m ()

statefulSum :: [Int] -> State Int ()
statefulSum [] = return ()

statefulSum (x : xs) = do
  currentState <- get
  let newState = currentState + x
  put newState
  -- modify (+x)
  statefulSum xs

type StateNum = Int

newState :: State StateNum String
newState = do
  k <- get
  -- get :: State Int Int
  modify (+1) -- modify :: (Int -> Int) -> State Int ()
  -- also     _ <- modify (+1)
  return $ "q" ++ show k
  -- return k in ProgramGraph

-- >>= :: m a -> (a -> m b) -> m b
-- >> :: m a -> m b -> m b

-- runState :: State s a -> s -> (a, s)
-- evalState :: State s a -> s -> a       | evalState x = fst (runState x)
-- execState :: State s a -> s -> s       | execState x = snd (runState x)

-- class Semigroup m where
--   (<>) :: m a -> m a -> m a

-- instance Semigroup [] where
--   (<>) = (++)

-- instance Semigroup Int where
--   a <> b = a + b
--   a <> b = a * b

newtype Sum = Sum {getSum :: Int}
  deriving (Show)
-- getSum :: Sum -> Int

newtype Product = Product {getProd :: Int}
  deriving (Show)
-- getProd :: Product -> Int

instance Semigroup Sum where
  (Sum a) <> (Sum b) = Sum (a + b)

instance Semigroup Product where
  (Product a) <> (Product b) = Product (a * b)

-- class Semigroup m => Monoid m where
--   mzero :: m a
--   mappend :: m a -> m a -> m a -- <>

instance Monoid Sum where
  mempty = Sum 0
  mappend a b = a <> b

instance Monoid Product where
  mempty = Product 1
  mappend a b = a <> b

concat' :: Monoid m => [m] -> m
concat' []       = mempty
concat' (x : xs) = x `mappend` concat' xs

product' :: [Int] -> Int
product' xs = fmap Product xs |> concat' |> getProd

a |> f = f a
