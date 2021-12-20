{-# LANGUAGE ScopedTypeVariables #-}

module Language.Fortran.Repr.TypeSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Language.Fortran.Repr.Type

instance Arbitrary FTypeInt where
  arbitrary = elements (enumFrom (toEnum 0))

spec :: Spec
spec = do
  describe "Properties of `joinType`" $ do
    prop "Reflexive"  (\(x :: FTypeInt)     -> joinType x x `shouldBe` (Just x))
    prop "Symmetric"  (\(x :: FTypeInt) y   -> joinType x y `shouldBe` joinType y x)
    prop "Transitive" (\(x :: FTypeInt) y z -> (joinType x y >>= flip joinType z) `shouldBe` (joinType y z >>= joinType x))
