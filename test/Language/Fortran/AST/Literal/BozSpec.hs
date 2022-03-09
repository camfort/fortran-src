{-# LANGUAGE TypeApplications #-}

module Language.Fortran.AST.Literal.BozSpec where

import Test.Hspec

import Language.Fortran.AST.Literal.Boz
import Numeric.Natural ( Natural )

spec :: Spec
spec = do
  describe "BOZ literal constants" $ do
    it "parses single and double quotes identically" $ do
      parseBoz "o'017'" `shouldBe` parseBoz "o\"017\""

    it "parses postfix BOZ constant as explicitly nonconforming" $ do
      parseBoz "'010'b" `shouldBe` Boz BozPrefixB "010" Nonconforming

    it "parses a prefix and postfix BOZ constant identically" $ do
      (parseBoz "z'123abc'" `bozIdentical` parseBoz "'123abc'z") `shouldBe` True

    it "parses nonstandard X as Z (hex)" $ do
      (parseBoz "x'09af'" `bozIdentical` parseBoz "z'09af'") `shouldBe` True

    it "resolves a BOZ as a natural" $ do
      bozAsNatural @Natural (parseBoz "x'FF'") `shouldBe` 255
