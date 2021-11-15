{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module ArbitraryInstances () where

import           Control.Lens
import           Control.Monad       (replicateM)
import           Data.Data.Lens      (biplate)
import           Data.List           (nub)
import qualified Data.Map.Lazy       as M
import           Data.Maybe          (fromMaybe)
import           Generic.Random
import           MicroC.AST
import           MicroC.ProgramGraph
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
  arbitrary = do
    ss <- listOf' arbitrary
    ds <- listOf' arbitrary

    -- find existing record declarations
    let m1 = declaredRecords ds
    -- fill in the missing record declarations based on field accesses
        accs = M.fromListWith (\a b -> nub (a ++ b)) $ map (fmap pure) $ ss ^.. biplate @_ @(LValue 'CInt) . _FieldAccess
        m2 = M.unionWith (\a b -> nub (a ++ b)) m1 accs
    -- fill in the missing record declarations based on record assignments
        rass = map (fmap length) $ concatMap universe ss ^.. traverse . _RecordAssignment
        m3 = foldr (\(r, n) m -> M.alter (\v -> Just $ take n $ fromMaybe [] v ++ repeat "???") r m) m2 rass
    -- filter out all record declarations
        ds' = filter (isn't _RecordDecl) ds
    -- and add all based on what we need (m3)
        ds'' = ds' ++ map (uncurry RecordDecl) (M.toList m3)

    pure $ Program ds'' ss

instance Arbitrary Declaration where
  arbitrary = oneof [ VariableDecl <$> identifier
                    , ArrayDecl <$> arbitrarySizedNatural <*> identifier
                    , RecordDecl <$> identifier <*> listOf1 identifier]

instance Arbitrary Statement where
  arbitrary = sized $ \n ->
    let other = [ Assignment <$> arbitrary ./ 2 <*> arbitrary ./ 2
                , RecordAssignment <$> identifier <*> replicateM 3 (arbitrary ./ 3) -- always 3 fields for the sake of simplicity
                , IfThen <$> step n arbitrary <*> listOf' arbitrary
                , IfThenElse <$> step n arbitrary <*> listOf' arbitrary <*> listOf' arbitrary
                , While <$> step n arbitrary <*> listOf' arbitraryWithBreaks
                , Read <$> step n arbitrary
                , Write <$> step n arbitrary]
    in branch n [Read <$> step n arbitrary] other

arbitraryWithBreaks :: Gen Statement
arbitraryWithBreaks = frequency [(8, arbitrary), (1, pure Break), (1, pure Continue)]

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

instance Arbitrary Action where
  arbitrary = sized $ \n -> do
    let base =  [ DeclAction <$> step n arbitrary
                , AssignAction <$> arbitrary ./ 2 <*> arbitrary ./ 2
                , ReadAction <$> step n arbitrary
                , WriteAction <$> step n arbitrary
                , BoolAction <$> step n arbitrary
                ]
    branch n base base
