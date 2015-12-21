module Forpar.Parser.Fortran66Spec where

import Test.Hspec

-- import Forpar.Parser.Fortran66
-- import Forpar.Lexer.FixedForm
-- 
-- evalExpressionParser sourceCode = 
--     evalState expressionParser $ initParseState sourceCode Fortran66 "<unknown>"
-- 
spec :: Spec
spec = 
  describe "Fortran 66 Parser" $ do
    it "parses '3 + 2'" $ do
      pending
--       evalExpressionParser "      3 + 2" `shouldBe` 
--         ExpBinary () undefined Addition 
--           ExpValue  () undefined $ ValInteger () undefined Plus $ ConstNumeric a undefined "3"
--           ExpValue  () undefined $ ValInteger () undefined Plus $ ConstNumeric a undefined "2"
