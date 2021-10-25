module MicroC.Worklist.Stack where

import           MicroC.Worklist

newtype Stack a = Stack [a]
  deriving (Eq)

instance Worklist Stack where
    empty = Stack []
    insert x (Stack s) = Stack (x : s)
    extract (Stack [])       = Nothing
    extract (Stack (x : xs)) = Just (x, Stack xs)
