{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Lattice
( SemiLattice (..)
, PartialOrder (..)
, Lattice(..)
, Poset(..)
) where

import qualified Data.IntegerInterval as I
import           Data.List            (intercalate)
import qualified Data.Set             as S

class PartialOrder a where
  order :: a -> a -> Bool

class PartialOrder a => SemiLattice a where
  bottom :: a
  supremum :: a -> a -> a

class SemiLattice a => Lattice a where
  top :: a
  infimum :: a -> a -> a

newtype Poset a = Poset (S.Set a)
  deriving (Eq)

instance Show a => Show (Poset a) where
  show (Poset s) = "{" <> intercalate ", " (S.toList (S.map show s)) <> "}"

-- Constraint on `Ord` for the elements in only here because
-- S.Set uses a binary tree inside.
instance Ord a => PartialOrder (Poset a) where
  Poset a `order` Poset b = a `S.isSubsetOf` b

instance Ord a => SemiLattice (Poset a) where
  bottom = Poset S.empty
  supremum (Poset a) (Poset b) = Poset $ a `S.union` b

instance (Ord a, Bounded a, Enum a) => Lattice (Poset a) where
  top = Poset . S.fromList $ enumFrom minBound
  infimum (Poset a) (Poset b) = Poset $ S.intersection a b

instance PartialOrder I.IntegerInterval where
  order = I.isSubsetOf

instance SemiLattice I.IntegerInterval where
  bottom = I.empty
  supremum = I.hull

instance Lattice I.IntegerInterval where
  top = I.whole
  infimum = I.intersection
