{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fortran.Analysis.SemanticTypesSpec where

import Test.Hspec
import TestUtil

import Language.Fortran.Analysis.SemanticTypes
import Language.Fortran.AST
import Language.Fortran.Version

import Language.Fortran.PrettyPrint
import Text.PrettyPrint hiding ((<>))
import Text.PrettyPrint.GenericPretty

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
          typespec = TypeSpec () u TypeReal (Just (Selector () u Nothing (Just (intGen 8))))
       in recoverSemTypeTypeSpec () u Fortran90 semtype `shouldBe` typespec

    it "recovers CHARACTER(*)" $ do
      let semtype  = TCharacter CharLenStar 1
          typespec = TypeSpec () u TypeCharacter (Just (Selector () u (Just (ExpValue () u ValStar)) Nothing))
       in recoverSemTypeTypeSpec () u Fortran90 semtype `shouldBe` typespec

    it "prints semantic type with dimensions" $ do
      let dims = DimsExplicitShape ( [ Dim (Just 1) (Just 3), Dim (Just 1) (Just 4) ] )
      let semtype  = TArray (TReal 8) dims
      pprint Fortran90 semtype Nothing `shouldBe` "real(8)(1:3, 1:4)"

