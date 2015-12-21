{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Forpar.Util.FirstParameterSpec(spec) where

import Test.Hspec

import GHC.Generics (Generic(..))

import Forpar.Util.FirstParameter

data A = A Int deriving (Generic)
data B = B Char Char Int Char deriving (Generic)
data C = CA ([Int]) Char | CB ([Int]) Int deriving (Generic)
data D = DA () | DB () | DC () | DD () | DE () deriving (Generic)

instance FirstParameter A Int
instance FirstParameter B Char
instance FirstParameter C ([Int])
instance FirstParameter D ()

spec :: Spec
spec = 
  describe "First parameter retrieving type class" $ do
    describe "data A" $ do
      it "works on 'A 42'" $ do
        getFirstParameter (A 42) `shouldBe` 42

    describe "data B" $ do
      it "works on 'B 'x' 'y' 42 'z'" $ do
        getFirstParameter (B 'x' 'y' 42 'z') `shouldBe` 'x'

    describe "data C" $ do
      it "works on 'CA [1,2,3] 'a''" $ do 
        getFirstParameter (CA [1,2,3] 'a') `shouldBe` [1,2,3]

      it "works on 'CB [1,2,3] 'a''" $ do 
        getFirstParameter (CB [] 42) `shouldBe` []

    describe "data D" $ do
      it "works on 'DB ()" $ do 
        getFirstParameter (DB ()) `shouldBe` ()

      it "works on 'DD ()" $ do 
        getFirstParameter (DD ()) `shouldBe` ()

      it "works on 'DE ()" $ do 
        getFirstParameter (DE ()) `shouldBe` ()
