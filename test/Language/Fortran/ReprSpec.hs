module Language.Fortran.ReprSpec ( spec ) where

import Test.Hspec
import TestUtil

import Language.Fortran.Repr
import Language.Fortran.AST
import Language.Fortran.Version

spec :: Spec
spec = do
  describe "Semantic Fortran type representation" $ do
    it "recovers DOUBLE PRECISION for REAL(8) in Fortran 77" $ do
      let ty = intrinsicTy BTyReal 8
          ts = TypeSpec () u TypeDoublePrecision Nothing
       in recoverTyTypeSpec () u Fortran77 ty `shouldBe` ts

    it "recovers DOUBLE COMPLEX for COMPLEX(16) in Fortran 77" $ do
      let ty = intrinsicTy BTyComplex 16
          ts = TypeSpec () u TypeDoubleComplex Nothing
       in recoverTyTypeSpec () u Fortran77 ty `shouldBe` ts

    it "recovers REAL(8) for REAL(8) in Fortran 90" $ do
      let ty = intrinsicTy BTyReal 8
          ts = TypeSpec () u TypeReal (Just (Selector () u Nothing (Just (ExpValue () u (ValInteger "8")))))
       in recoverTyTypeSpec () u Fortran90 ty `shouldBe` ts

-- TODO: star yet unsupported
{-
    it "recovers CHARACTER(*)" $ do
      let semtype  = scalarTy (BTyCharacter Nothing) 1
          typespec = TypeSpec () u TypeCharacter (Just (Selector () u (Just (ExpValue () u ValStar)) Nothing))
       in recoverTyTypeSpec () u Fortran90 semtype `shouldBe` typespec
-}

---

intrinsicTy :: BaseTy -> Kind -> Ty
intrinsicTy bTy k = TyScalarTy $ ScalarTyIntrinsic $ IntrinsicTy bTy k
