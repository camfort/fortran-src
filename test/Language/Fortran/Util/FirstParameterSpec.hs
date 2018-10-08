{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Fortran.Util.FirstParameterSpec (spec) where

import           Test.Hspec

import           GHC.Generics                             ( Generic(..) )

import           Language.Fortran.Util.FirstParameter

data A = A Int deriving (Generic, Eq, Show)
data B = B Char Char Int Char deriving (Generic, Eq, Show)
data C = CA [Int] Char | CB [Int] Int deriving (Generic, Eq, Show)
data D = DA () | DB () | DC () | DD () | DE () deriving (Generic, Eq, Show)

instance FirstParameter A Int
instance FirstParameter B Char
instance FirstParameter C [Int]
instance FirstParameter D ()

spec :: Spec
spec = describe "First parameter accessor type class" $ do
  describe "data A" $ do
    it "retrieves first parameter from 'A 42'" $ getFirstParameter (A 42) `shouldBe` 42

    it "sets first parameter in 'A 42' to 24" $ setFirstParameter 24 (A 42) `shouldBe` A 24

  describe "data B" $ do
    it "retrieves first parameter from \"B 'x' 'y' 42 'z'\""
      $          getFirstParameter (B 'x' 'y' 42 'z')
      `shouldBe` 'x'

    it "sets first parameter in \"B 'x' 'y' 42 'z'\" to 'm'"
      $          setFirstParameter 'm' (B 'x' 'y' 42 'z')
      `shouldBe` B 'm' 'y' 42 'z'

  describe "data C" $ do
    it "retrieves first parameter from 'CA [1,2,3] 'a''"
      $          getFirstParameter (CA [1, 2, 3] 'a')
      `shouldBe` [1, 2, 3]

    it "retrieves first parameter from \"CB [1,2,3] 'a'\""
      $          getFirstParameter (CB [] 42)
      `shouldBe` []

    it "sets first parameter in \"CB [1,2,3] 'a'\" to '[]'"
      $          setFirstParameter [] (CA [1, 2, 3] 'a')
      `shouldBe` CA [] 'a'

  describe "data D" $ do
    it "retrieves first parameter from 'DB ()" $ getFirstParameter (DB ()) `shouldBe` ()

    it "retrieves first parameter from 'DD ()" $ getFirstParameter (DD ()) `shouldBe` ()

    it "retrieves first parameter from 'DE ()" $ getFirstParameter (DE ()) `shouldBe` ()
