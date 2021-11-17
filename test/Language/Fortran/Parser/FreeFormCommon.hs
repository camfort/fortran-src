-- | Fortran standards F90 and beyond are a lot more consistent than the
--   previous 2. As such, there is lots of shared parsing, and lots of shared
--   tests. This module encodes such shared/common tests, where no difference
--   in behaviour between parsers is be expected.

module Language.Fortran.Parser.FreeFormCommon ( specFreeFormCommon ) where

import           TestUtil
import           Test.Hspec

import           Language.Fortran.AST
import           Language.Fortran.AST.RealLit

specFreeFormCommon :: (String -> Statement A0) -> (String -> Expression A0) -> Spec
specFreeFormCommon sParser eParser =
  describe "Common Fortran 90+ tests" $ do
    describe "Literals" $ do
      describe "Logical" $ do
        it "parses logical literal without kind parameter" $ do
          eParser ".true." `shouldBe'` valTrue

        it "parses logical literal with kind parameter" $ do
          let kp = ExpValue () u (ValVariable "kind")
          eParser ".false._kind" `shouldBe'` valFalse' kp

        it "parses mixed-case logical literal" $ do
          eParser ".tRUe." `shouldBe'` valTrue

      -- Main parse testing is performed in @Language.Fortran.AST.RealLitSpec@.
      -- Here we mainly want to test kind parameter and sign behaviour.
      describe "Real" $ do
        let realLitExp r mkp = ExpValue () u (ValReal (parseRealLit r) mkp)
        it "parses various REAL literals" $ do
          eParser "1."      `shouldBe'` realLitExp "1."    Nothing
          eParser ".1e20_8" `shouldBe'` realLitExp ".1e20" (Just (intGen 8))

        it "parses \"negative\" real literal (unary op)" $ do
          eParser "-1.0d-1_k8" `shouldBe'` ExpUnary () u Minus (realLitExp "1.0d-1" (Just (varGen "k8")))

    describe "Statement" $ do
      describe "Declaration" $ do
        it "parses scalar declaration with nonstandard kind param (non-CHAR)" $ do
          let stStr    = "integer x*8"
              expected = StDeclaration () u typeSpec Nothing decls
              typeSpec = TypeSpec () u TypeInteger Nothing
              decls    = AList () u
                [ DeclVariable () u (varGen "x") (Just (intGen 8)) Nothing ]
          sParser stStr `shouldBe'` expected

        it "parses array declaration with nonstandard kind param (non-CHAR)" $ do
          let stStr    = "integer x(2)*8"
              expected = StDeclaration () u typeSpec Nothing decls
              typeSpec = TypeSpec () u TypeInteger Nothing
              decls    = AList () u
                [ DeclArray () u (varGen "x") dims (Just (intGen 8)) Nothing ]
              dims     = AList () u
                [ DimensionDeclarator () u Nothing (Just (intGen 2)) ]
          sParser stStr `shouldBe'` expected

        it "parses array declaration with nonstandard kind param (non-CHAR) and nonstandard dimension/charlen order" $ do
          let stStr    = "integer x*8(2)"
              expected = StDeclaration () u typeSpec Nothing decls
              typeSpec = TypeSpec () u TypeInteger Nothing
              decls    = AList () u
                [ DeclArray () u (varGen "x") dims (Just (intGen 8)) Nothing ]
              dims     = AList () u
                [ DimensionDeclarator () u Nothing (Just (intGen 2)) ]
          sParser stStr `shouldBe'` expected
