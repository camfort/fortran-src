{-# LANGUAGE TypeApplications #-}

module Language.Fortran.AST.Literal.BozSpec where

import Test.Hspec

import Language.Fortran.AST.Literal.Boz
import Numeric.Natural ( Natural )
import Data.Int ( Int8, Int16, Int32 )

spec :: Spec
spec = do
  describe "BOZ literal constants" $ do
    it "parses single and double quotes identically" $ do
      parseBoz "o'017'" `shouldBe` parseBoz "o\"017\""

    it "parses postfix BOZ constant as explicitly nonconforming" $ do
      parseBoz "'010'b" `shouldBe` Boz BozPrefixB "010" Nonconforming

    it "parses a prefix and postfix BOZ constant identically (ignoring conformance flags)" $ do
      parseBoz "z'123abc'" `shouldBe` parseBoz "'123abc'z"

    it "parses nonstandard X as Z (hex)" $ do
      parseBoz "x'09af'" `shouldBe` parseBoz "z'09af'"

    it "resolves a BOZ as a natural" $ do
      bozAsNatural @Natural (parseBoz "x'00'") `shouldBe` 0
      bozAsNatural @Natural (parseBoz "x'7F'") `shouldBe` 127
      bozAsNatural @Natural (parseBoz "x'80'") `shouldBe` 128
      bozAsNatural @Natural (parseBoz "x'FF'") `shouldBe` 255

    it "resolves a BOZ as a two's complement integer (INT(1))" $ do
      bozAsTwosComp @Int8  (parseBoz "x'00'") `shouldBe` 0
      bozAsTwosComp @Int8  (parseBoz "x'7F'") `shouldBe` 127
      bozAsTwosComp @Int8  (parseBoz "x'80'") `shouldBe` (-128)
      bozAsTwosComp @Int8  (parseBoz "x'FF'") `shouldBe` (-1)

    it "resolves a BOZ as a two's complement integer (INT(2))" $ do
      bozAsTwosComp @Int16 (parseBoz "x'00'")   `shouldBe` 0
      bozAsTwosComp @Int16 (parseBoz "x'7F'")   `shouldBe` 127
      bozAsTwosComp @Int16 (parseBoz "x'80'")   `shouldBe` 128
      bozAsTwosComp @Int16 (parseBoz "x'FF'")   `shouldBe` 255
      bozAsTwosComp @Int16 (parseBoz "x'7FFF'") `shouldBe` 32767
      bozAsTwosComp @Int16 (parseBoz "x'8000'") `shouldBe` (-32768)
      bozAsTwosComp @Int16 (parseBoz "x'FFFF'") `shouldBe` (-1)

    it "resolves a BOZ as a two's complement integer (INT(4))" $ do
      bozAsTwosComp @Int32 (parseBoz "x'FFFFFFFF'") `shouldBe` (-1)
