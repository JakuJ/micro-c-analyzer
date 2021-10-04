{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Data.Lattice where

import qualified Data.Set as S

class Ord a => Lattice a where
  supremum :: a -> a -> a
  infimum :: a -> a -> a

class Lattice a => CompleteLattice a where
  maxElement :: a
  minElement :: a

newtype Poset a = Poset (S.Set a)
  deriving (Eq, Show)

instance Ord a => Ord (Poset a) where
  Poset a <= Poset b = a `S.isSubsetOf` b

instance Ord a => Lattice (Poset a) where
  supremum (Poset a) (Poset b) = Poset $ S.union a b
  infimum (Poset a) (Poset b) = Poset $ S.intersection a b

-- Example of a bounded poset

data Sign = Minus | Zero | Plus
  deriving (Eq, Ord, Enum, Show)

generateEnumValues :: (Enum a) => [a]
generateEnumValues = enumFrom (toEnum 0)

instance Bounded (Poset Sign) where
  minBound = Poset S.empty
  maxBound = Poset $ S.fromList generateEnumValues
