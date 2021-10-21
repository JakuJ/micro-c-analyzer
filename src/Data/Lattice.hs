module Data.Lattice
( SemiLattice (..)
, Lattice(..)
, Poset(..)
) where

import qualified Data.IntegerInterval as I
import qualified Data.Set             as S

class SemiLattice a where
  bottom :: a
  order :: a -> a -> Bool
  supremum :: a -> a -> a

class SemiLattice a => Lattice a where
  top :: a
  infimum :: a -> a -> a

newtype Poset a = Poset (S.Set a)
  deriving (Eq, Show) via S.Set a

instance Ord a => SemiLattice (Poset a) where
  bottom = Poset S.empty
  Poset a `order` Poset b = a `S.isSubsetOf` b
  supremum (Poset a) (Poset b) = Poset $ a `S.union` b

instance SemiLattice I.IntegerInterval where
  bottom = I.empty
  order = I.isSubsetOf
  supremum = I.hull

instance Lattice I.IntegerInterval where
  top = I.whole
  infimum = I.intersection
