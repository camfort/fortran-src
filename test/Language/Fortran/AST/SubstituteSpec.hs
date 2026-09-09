module Language.Fortran.AST.SubstituteSpec where

import Test.Hspec
import Language.Fortran.AST
import Language.Fortran.AST.AList
import Language.Fortran.Util.Position

spec :: Spec
spec = do
  describe "substitute" $ do
    it "replaces a variable in a simple expression" $ do
      let target = ExpValue () nullSpan (ValVariable "x")
          replacement = ExpValue () nullSpan (ValInteger "1" Nothing)
          result = substitute replacement "x" target
      result `shouldBe` replacement

    it "replaces nested variables in binary and function-call expressions" $ do
      let target =
            ExpBinary () nullSpan Addition
              (ExpValue () nullSpan (ValVariable "x"))
              (ExpFunctionCall () nullSpan
                (ExpValue () nullSpan (ValVariable "f"))
                (fromList () [Argument () nullSpan Nothing (ArgExpr (ExpValue () nullSpan (ValVariable "x")))]))
          replacement = ExpValue () nullSpan (ValInteger "2" Nothing)
          result = substitute replacement "x" target
      result `shouldBe`
        ExpBinary () nullSpan Addition
          replacement
          (ExpFunctionCall () nullSpan
            (ExpValue () nullSpan (ValVariable "f"))
            (fromList () [Argument () nullSpan Nothing (ArgExpr replacement)]))

    it "does not replace a different variable name" $ do
      let target = ExpValue () nullSpan (ValVariable "y")
          replacement = ExpValue () nullSpan (ValInteger "3" Nothing)
          result = substitute replacement "x" target
      result `shouldBe` target
