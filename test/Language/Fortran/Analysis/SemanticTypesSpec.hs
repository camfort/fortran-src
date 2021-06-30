module Language.Fortran.Analysis.SemanticTypesSpec where

import Test.Hspec
import TestUtil

import Language.Fortran.Analysis.SemanticTypes
import Language.Fortran.AST
import Language.Fortran.Version

spec :: Spec
spec = do
  describe "Semantic types" $ do
    it "recovers DOUBLE PRECISION for REAL(8) in Fortran 77" $ do
      let semtype  = TReal 8
          typespec = TypeSpec () u TypeDoublePrecision Nothing
       in recoverSemTypeTypeSpec () u Fortran77 semtype `shouldBe` typespec

    it "recovers DOUBLE COMPLEX for COMPLEX(16) in Fortran 77" $ do
      let semtype  = TComplex 16
          typespec = TypeSpec () u TypeDoubleComplex Nothing
       in recoverSemTypeTypeSpec () u Fortran77 semtype `shouldBe` typespec

    it "recovers REAL(8) for REAL(8) in Fortran 90" $ do
      let semtype  = TReal 8
          typespec = TypeSpec () u TypeReal (Just (Selector () u Nothing (Just (ExpValue () u (ValInteger "8")))))
       in recoverSemTypeTypeSpec () u Fortran90 semtype `shouldBe` typespec

    it "recovers CHARACTER(*)" $ do
      let semtype  = TCharacter CharLenStar 1
          typespec = TypeSpec () u TypeCharacter (Just (Selector () u (Just (ExpValue () u ValStar)) Nothing))
       in recoverSemTypeTypeSpec () u Fortran90 semtype `shouldBe` typespec
