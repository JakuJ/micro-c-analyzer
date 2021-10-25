{-# LANGUAGE FlexibleInstances #-}
module MicroC.Worklist.Stack where

import           MicroC.Worklist

newtype Stack a = Stack [a]

instance Worklist Stack a where
    empty = Stack []
    insert x (Stack s) = Stack (x : s)
    extract (Stack [])    = Nothing
    extract (Stack (x:s)) = Just (x, Stack s)
