module Language.Fortran.Parser.UtilsSpec where

import Test.Hspec

import Language.Fortran.Parser.Utils

spec :: Spec
spec =
  describe "Fortran Parser Utils" $ do

    describe "readReal" $ do
      it "tests" $ do
        readReal "+12"       `shouldBe` Just 12
        readReal "-1.2"      `shouldBe` Just (-1.2)
        readReal "1.2d3"     `shouldBe` Just 1200
        readReal "1.e2"      `shouldBe` Just 100
        readReal "1.e-2"     `shouldBe` Just 0.01
        readReal ".12"       `shouldBe` Just 0.12
        readReal "-.12"      `shouldBe` Just (-0.12)
        readReal "1_f"       `shouldBe` Just 1

    describe "readInteger" $ do
      it "tests" $ do
        readInteger "b'101'" `shouldBe` Just 5
        readInteger "o'22'"  `shouldBe` Just 18
        readInteger "z'AF'"  `shouldBe` Just 175
        readInteger "1_f"    `shouldBe` Just 1
        readInteger "+123"   `shouldBe` Just 123
        readInteger "-123"   `shouldBe` Just (-123)
