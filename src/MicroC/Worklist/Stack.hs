module MicroC.Worklist.Stack
( Stack(..)
, module MicroC.Worklist
) where

import           MicroC.ProgramGraph (StateNum)
import           MicroC.Worklist

newtype Stack = Stack [StateNum]
  deriving (Eq)

instance Worklist Stack where
  empty = Stack []
  insert x (Stack s) = Stack (x : s)
  extract _ (Stack [])       = Nothing
  extract _ (Stack (x : xs)) = Just (x, Stack xs)
