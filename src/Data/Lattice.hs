module Data.Lattice
( SemiLattice (..)
, Lattice(..)
, Poset(..)
) where

import qualified Data.IntegerInterval as I
import           Data.List            (intercalate)
import qualified Data.Set             as S

class SemiLattice a where
  bottom :: a
  order :: a -> a -> Bool
  supremum :: a -> a -> a

class SemiLattice a => Lattice a where
  top :: a
  infimum :: a -> a -> a

newtype Poset a = Poset (S.Set a)
  deriving (Eq)

instance Show a => Show (Poset a) where
  show (Poset s) = "{" <> intercalate ", " (S.toList (S.map show s)) <> "}"

instance Ord a => SemiLattice (Poset a) where
  bottom = Poset S.empty
  Poset a `order` Poset b = a `S.isSubsetOf` b
  supremum (Poset a) (Poset b) = Poset $ a `S.union` b

instance (Ord a, Enum a) => Lattice (Poset a) where
  top = Poset $ S.fromList $ enumFrom (toEnum minBound)
  infimum (Poset a) (Poset b) = Poset $ S.intersection a b

instance SemiLattice I.IntegerInterval where
  bottom = I.empty
  order = I.isSubsetOf
  supremum = I.hull

instance Lattice I.IntegerInterval where
  top = I.whole
  infimum = I.intersection
