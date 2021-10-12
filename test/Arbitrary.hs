{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Arbitrary where

import           Generic.Random
import           Language.MicroC.AST
import           Test.QuickCheck

branch :: [Gen a] -> [Gen a] -> Gen a
branch base go = do
  n <- getSize
  if n <= 1 then oneof base else resize (n - 1) . oneof $ go

instance Arbitrary Declaration where
  arbitrary = genericArbitrary uniform

instance Arbitrary Statement where
  arbitrary = do
    r <- genericArbitrary uniform
    case r of
      RecordAssignment x _ -> RecordAssignment x <$> listOf1 arbitrary
      _                    -> pure r

instance Arbitrary OpArith where
  arbitrary = genericArbitrary uniform

instance Arbitrary OpRel where
  arbitrary = genericArbitrary uniform

instance Arbitrary OpBool where
  arbitrary = genericArbitrary uniform

instance Arbitrary (LValue 'CInt) where
  arbitrary = do
    let base = [Variable <$> arbitrary, FieldAccess <$> arbitrary <*> arbitrary]
    branch base $ (ArrayIndex <$> arbitrary <*> arbitrary) : base

instance Arbitrary (RValue 'CInt) where
  arbitrary = do
    let base = [Reference <$> arbitrary, Literal <$> arbitrary]
    branch base $ (OpA <$> arbitrary <*> arbitrary <*> arbitrary) : base

instance Arbitrary (RValue 'CBool) where
  arbitrary = do
    let base = [Literal <$> arbitrary]
        other = [ Not <$> arbitrary
                , OpR <$> arbitrary <*> arbitrary <*> arbitrary
                , OpB <$> arbitrary <*> arbitrary <*> arbitrary]
    branch base $ base ++ other
