{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module ArbitraryInstances () where

import           Generic.Random
import           Language.MicroC.AST
import           Test.QuickCheck

step :: Int -> Gen a -> Gen a
step n = resize (max 0 (n - 1))

(./) :: Gen a -> Int -> Gen a
g ./ n = scale (`div` n) g

branch :: Int -> [Gen a] -> [Gen a] -> Gen a
branch n base go = if n <= 1 then oneof base else oneof go

identifier :: Gen Identifier
identifier = (:) <$> letter <*> listOf idChar
  where
    letter = oneof [chooseEnum ('a', 'z'), chooseEnum ('A', 'Z')]
    idChar = oneof [letter, pure '_', chooseEnum ('0', '9')]

instance Arbitrary Program where
  shrink (Program ds ss) = [Program ds' ss' | (ds', ss') <- shrink (ds, ss)]
  arbitrary = Program <$> listOf' arbitrary <*> listOf' arbitrary

instance Arbitrary Declaration where
  arbitrary = oneof [ VariableDecl <$> identifier
                    , ArrayDecl <$> arbitrarySizedNatural <*> identifier
                    , RecordDecl <$> identifier <*> listOf1 identifier]

instance Arbitrary Statement where
  arbitrary = sized $ \n ->
    let other = [ Assignment <$> arbitrary ./ 2 <*> arbitrary ./ 2
                , RecordAssignment <$> identifier <*> listOf1' arbitrary
                , IfThen <$> step n arbitrary <*> listOf' arbitrary
                , IfThenElse <$> step n arbitrary <*> listOf' arbitrary <*> listOf' arbitrary
                , While <$> step n arbitrary <*> listOf' arbitrary
                , Read <$> step n arbitrary
                , Write <$> step n arbitrary]
    in branch n [Read <$> step n arbitrary] other

instance Arbitrary OpArith where
  arbitrary = genericArbitrary uniform

instance Arbitrary OpRel where
  arbitrary = genericArbitrary uniform

instance Arbitrary OpBool where
  arbitrary = genericArbitrary uniform

instance Arbitrary (LValue 'CInt) where
  arbitrary = sized $ \n -> do
    let base = [Variable <$> identifier, FieldAccess <$> identifier <*> identifier]
    branch n base $ (ArrayIndex <$> identifier <*> step n arbitrary) : base

instance Arbitrary (RValue 'CInt) where
  shrink = \case
    OpA l o r -> [l, r] ++ [OpA l' o r' | (l', r') <- shrink (l, r)]
    _         -> []
  arbitrary = sized $ \n -> do
    let base = [Reference <$> step n arbitrary, Literal <$> step n arbitrary]
    branch n base $ (OpA <$> arbitrary ./ 2 <*> arbitrary <*> arbitrary ./ 2 ) : base

instance Arbitrary (RValue 'CBool) where
  shrink = \case
    Not b     -> b : [Not b' | b' <- shrink b]
    OpB l o r -> [l, r] ++ [OpB l' o r' | (l', r') <- shrink (l, r)]
    _         -> []
  arbitrary = sized $ \n -> do
    let base = [Literal <$> step n arbitrary]
        other = [ Not <$> step n arbitrary
                , OpR <$> arbitrary ./ 2  <*> arbitrary <*> arbitrary ./ 2
                , OpB <$> arbitrary ./ 2  <*> arbitrary <*> arbitrary ./ 2 ]
    branch n base $ base ++ other
