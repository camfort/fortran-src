module Language.Fortran.AST.Literal.RealSpec where

import Prelude hiding ( exp )

import Test.Hspec

import Language.Fortran.AST.Literal.Real

spec :: Spec
spec = do
  describe "Fortran real literals" $ do
    it "parses & normalizes various well-formed valid real literals" $ do
      prl "1.0"    `shouldBe` rl "1.0" expDef
      prl "1.0e0"  `shouldBe` rl "1.0" expDef
      prl "10e-1"  `shouldBe` rl "10.0" (exp e "-1")
      prl "-1.e-1" `shouldBe` rl "-1.0" (exp e "-1")
      prl "+1.e+1" `shouldBe` rl "1.0" (exp e "1")
      prl "1.e1"   `shouldBe` rl "1.0" (exp e "1")
      prl ".1"     `shouldBe` rl "0.1" expDef
      prl "1.0d0"  `shouldBe` rl "1.0" (exp d "0")
      prl "1.0q0"  `shouldBe` rl "1.0" (exp q "0")
    where
      prl = parseRealLit
      rl = RealLit
      exp = Exponent
      expDef = Exponent ExpLetterE "0"
      e = ExpLetterE
      d = ExpLetterD
      q = ExpLetterQ
