module Data.Lattice
( SemiLattice (..)
, Lattice(..)
, Poset(..)
) where

import           Data.Set

class SemiLattice a where
  bottom :: a
  order :: a -> a -> Bool
  supremum :: a -> a -> a

class SemiLattice a => Lattice a where
  top :: a
  infimum :: a -> a -> a

newtype Poset a = Poset (Set a)
  deriving (Eq, Show) via Set a

instance Ord a => SemiLattice (Poset a) where
  bottom = Poset empty
  Poset a `order` Poset b = a `isSubsetOf` b
  supremum (Poset a) (Poset b) = Poset $ a `union` b
