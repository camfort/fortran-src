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

    describe "parseRealLiteral" $ do
      it "parses various well-formed valid real literals" $ do
        prl "1"         `shouldBe` rl "1"    n n
        prl "1."        `shouldBe` rl "1."   n n
        prl ".0"        `shouldBe` rl ".0"   n n
        prl "1e0"       `shouldBe` rl "1"    (jExp expE n 0) n
        prl "1e0_4"     `shouldBe` rl "1"    (jExp expE n 0) (j 4)
        --prl "1e0_k"     `shouldBe` rl "1" _ _
        prl "1.0e0_4"   `shouldBe` rl "1.0"  (jExp expE n 0) (j 4)
        prl "+1.0e0_4"  `shouldBe` rl "+1.0" (jExp expE n 0) (j 4)
        prl "-1.0e0_4"  `shouldBe` rl "-1.0" (jExp expE n 0) (j 4)
        prl "-1.0e+0_4" `shouldBe` rl "-1.0" (jExp expE (j SignPos) 0) (j 4)
        prl "-1.0e-0_4" `shouldBe` rl "-1.0" (jExp expE (j SignNeg) 0) (j 4)
        prl "-1.0d-0_4" `shouldBe` rl "-1.0" (jExp expD (j SignNeg) 0) (j 4)

      -- Literals we gladly parse, but that most Fortran specs consider invalid.
      -- These will prompt an error during type analysis.
      it "parses various well-formed invalid real literals" $ do
        -- only exponent letter e allows kind param
        -- even if you use kind 8 (== what d sets), it should be considered
        -- invalid
        prl "1d0_8"   `shouldBe` rl "1" (jExp expD n 0) (j 8)
        prl "1d0_4"   `shouldBe` rl "1" (jExp expD n 0) (j 4)

      -- parseRealLiteral runtime errors on poorly-formed real literals because
      -- the parser should ensure we only ever receive well-formed ones.
      -- TODO: unable to test these while the parser uses 'error'
      it "fails to parse poorly-formed real literals" $ do
        pending
        {-
        -- exponent number can't be empty
        fails $ prl "1e"

        -- exponent number must be an integer
        fails $ prl "1ex"
        fails $ prl "1ex1"
        --fails $ prl "1e0.0"       -- not detected, we take the digits before
                                    -- the decimal point
        -}


      where
        prl = parseRealLiteral
        rl = RealLit
        n = Nothing
        j = Just
        jExp a b c = Just (Exponent a b c)
        expE = ExpLetterE
        expD = ExpLetterD
        fails test = return test `shouldThrow` anyException
