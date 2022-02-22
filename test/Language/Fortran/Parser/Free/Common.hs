{-| Common tests for free-form Fortran.

Fortran standards F90 and beyond are a lot more consistent than the
previous 2. As such, there is lots of shared parsing, and lots of shared
tests. This module encodes such shared/common tests, where no difference
in behaviour between parsers is be expected.
-}

module Language.Fortran.Parser.Free.Common ( specFreeCommon ) where

import           TestUtil
import           Test.Hspec

import           Language.Fortran.AST
import           Language.Fortran.AST.RealLit

specFreeCommon :: (String -> Statement A0) -> (String -> Expression A0) -> Spec
specFreeCommon sParser eParser =
  describe "Common Fortran 90+ tests" $ do
    describe "Literals" $ do
      describe "Logical" $ do
        it "parses logical literal without kind parameter" $ do
          eParser ".true." `shouldBe'` valTrue

        it "parses logical literal with kind parameter" $ do
          let kp = KindParamVar () u "kind"
          eParser ".false._kind" `shouldBe'` valFalse' kp

        it "parses mixed-case logical literal" $ do
          eParser ".tRUe." `shouldBe'` valTrue

      -- Main parse testing is performed in @Language.Fortran.AST.RealLitSpec@.
      -- Here we mainly want to test kind parameter and sign behaviour.
      describe "Real" $ do
        let realLitExp r mkp = ExpValue () u (ValReal (parseRealLit r) mkp)
        it "parses various REAL literals" $ do
          eParser "1."      `shouldBe'` realLitExp "1."    Nothing
          eParser ".1e20_8" `shouldBe'` realLitExp ".1e20" (Just (KindParamInt () u "8"))

        it "parses \"negative\" real literal (unary op)" $ do
          let lit = "-1.0d-1_k8"
              kp  = KindParamVar () u "k8"
           in eParser lit `shouldBe'` ExpUnary () u Minus (realLitExp "1.0d-1" (Just kp))

      describe "Kind parameters" $ do
        it "parses var kind parameter with underscore" $ do
          let lit = "123_a_1"
              kp  = KindParamVar () u "a_1"
           in eParser lit `shouldBe'` ExpValue () u (ValInteger "123" (Just kp))

        it "fails to parse invalid kind parameter with underscore after numeric" $ do
          {- TODO can't test here because we push parse errors into runtime ones
          let lit = "123_4_8"
           in eParser lit `shouldBe'` undefined -- should error "last parsed was the 2nd underscore"
          -}
          pending

      describe "Complex" $ do
        it "parses a basic complex literal" $ do
          let cr = ComplexPartReal () u (parseRealLit "1.0")  Nothing
              ci = ComplexPartReal () u (parseRealLit "-1.0") Nothing
          eParser "(1.0, -1.0)" `shouldBe'` complexGen cr ci

    describe "Statement" $ do
      describe "Declaration" $ do
        it "parses scalar declaration with nonstandard kind param (non-CHAR)" $ do
          let stStr    = "integer x*8"
              expected = StDeclaration () u typeSpec Nothing decls
              typeSpec = TypeSpec () u TypeInteger Nothing
              decls    = AList () u
                [ declVariable () u (varGen "x") (Just (intGen 8)) Nothing ]
          sParser stStr `shouldBe'` expected

        it "parses array declaration with nonstandard kind param (non-CHAR)" $ do
          let stStr    = "integer x(2)*8"
              expected = StDeclaration () u typeSpec Nothing decls
              typeSpec = TypeSpec () u TypeInteger Nothing
              decls    = AList () u
                [ declArray () u (varGen "x") dims (Just (intGen 8)) Nothing ]
              dims     = AList () u
                [ DimensionDeclarator () u Nothing (Just (intGen 2)) ]
          sParser stStr `shouldBe'` expected

        it "parses array declaration with nonstandard kind param (non-CHAR) and nonstandard dimension/charlen order" $ do
          let stStr    = "integer x*8(2)"
              expected = StDeclaration () u typeSpec Nothing decls
              typeSpec = TypeSpec () u TypeInteger Nothing
              decls    = AList () u
                [ declArray () u (varGen "x") dims (Just (intGen 8)) Nothing ]
              dims     = AList () u
                [ DimensionDeclarator () u Nothing (Just (intGen 2)) ]
          sParser stStr `shouldBe'` expected

      describe "Function call" $ do
        it "parses a simple function call" $ do
          let stStr    = "call double(i, i)"
              expected = StCall () u (varGen "double") (Just args)
              args     = AList () u [arg, arg]
              arg      = Argument () u Nothing (ArgExpr (varGen "i"))
          sParser stStr `shouldBe'` expected

        it "parses a parenthesized variable as a special indirect/copied variable reference" $ do
          let stStr    = "call double((i), i)"
              expected = StCall () u (varGen "double") (Just args)
              args     = AList () u [ genArg (ArgExprVar () u "i")
                                    , genArg (ArgExpr (varGen "i")) ]
              genArg   = Argument () u Nothing
          sParser stStr `shouldBe'` expected
