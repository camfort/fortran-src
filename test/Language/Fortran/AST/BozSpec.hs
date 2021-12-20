{-# LANGUAGE TypeApplications #-}

module Language.Fortran.AST.BozSpec where

import Test.Hspec

import Language.Fortran.AST.Boz
import Numeric.Natural ( Natural )

spec :: Spec
spec = do
  describe "BOZ literal constants" $ do
    it "parses a prefix and suffix BOZ constant identically" $ do
      parseBoz "z'123abc'" `shouldBe` parseBoz "'123abc'z"

    it "parses nonstandard X as Z (hex)" $ do
      parseBoz "x'09af'" `shouldBe` parseBoz "z'09af'"

    it "resolves a BOZ as a natural" $ do
      bozAsNatural @Natural (parseBoz "x'FF'") `shouldBe` 255
