{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module ArbitraryInstances () where

import           Generic.Random
import           Language.MicroC.AST
import           Test.QuickCheck

branch :: [Gen a] -> [Gen a] -> Gen a
branch base go = do
  n <- getSize
  if n <= 1 then oneof base else resize (n - 1) . oneof $ go

identifier :: Gen Identifier
identifier = (:) <$> letter <*> listOf idChar
  where
    letter = oneof [chooseEnum ('a', 'z'), chooseEnum ('A', 'Z')]
    idChar = oneof [letter, pure '_', chooseEnum ('0', '9')]

instance Arbitrary Declaration where
  arbitrary = oneof [ VariableDecl <$> identifier
                    , ArrayDecl <$> arbitrarySizedNatural <*> identifier
                    , RecordDecl <$> identifier <*> listOf1 identifier]

instance Arbitrary Statement where
  arbitrary = do
    n <- getSize
    r <- resize (max 0 (n - 1)) $ genericArbitrary uniform
    case r of
      RecordAssignment _ _ -> RecordAssignment <$> identifier <*> listOf1 arbitrary
      _                    -> pure r

instance Arbitrary OpArith where
  arbitrary = genericArbitrary uniform

instance Arbitrary OpRel where
  arbitrary = genericArbitrary uniform

instance Arbitrary OpBool where
  arbitrary = genericArbitrary uniform

instance Arbitrary (LValue 'CInt) where
  arbitrary = do
    let base = [Variable <$> identifier, FieldAccess <$> identifier <*> identifier]
    branch base $ (ArrayIndex <$> identifier <*> arbitrary) : base

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
