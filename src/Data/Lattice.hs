module Data.Lattice
( Lattice(..)
, Poset(..)
) where

import           Data.Set

class Lattice a where
  bottom :: a
  order :: a -> a -> Bool
  supremum :: a -> a -> a
  infimum :: a -> a -> a

newtype Poset a = Poset (Set a)
  deriving (Eq, Show) via Set a

instance Ord a => Lattice (Poset a) where
  bottom = Poset empty
  Poset a `order` Poset b = a `isSubsetOf` b
  supremum (Poset a) (Poset b) = Poset $ a `union` b
  infimum (Poset a) (Poset b) = Poset $ a `intersection` b
