module Language.Fortran.Parser.Fortran90PlusCommon ( specF90PlusCommon ) where

import TestUtil
import Test.Hspec

import Language.Fortran.AST

specF90PlusCommon :: (String -> Statement A0) -> Spec
specF90PlusCommon sParser =
  describe "Common Fortran 90+ tests" $ do
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
