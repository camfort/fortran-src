{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Forpar.Util.SecondParameterSpec(spec) where

import Test.Hspec

import GHC.Generics (Generic(..))

import Forpar.Util.SecondParameter

data A = A Char Int deriving (Generic)
data B = B Int Int Int Int deriving (Generic)
data C = CA String [Char] | CB Int [Char] () deriving (Generic)
data D = DA () () | DB Int () Int Int Int Int Int Int Int | DC () () | DD () () Char deriving (Generic)

instance SecondParameter A Int
instance SecondParameter B Int
instance SecondParameter C [Char]
instance SecondParameter D ()

spec :: Spec
spec =
  describe "Second parameter retrieving type class" $ do
    describe "data A" $ do
      it "works on 'A 'a' 42'" $ do
        getSecondParameter (A 'x' 42) `shouldBe` 42

    describe "data B" $ do
      it "works on 'B 41 42 43 44'" $ do
        getSecondParameter (B 41 42 43 44) `shouldBe` 42

    describe "data C" $ do
      it "works on 'CA \"hello\" ['x', 'y']'" $ do
        getSecondParameter (CA "hello" ['x', 'y']) `shouldBe` ['x', 'y']

      it "works on 'CB 42 []'" $ do
        getSecondParameter (CB 42 [] ()) `shouldBe` []

    describe "data d" $ do
      it "works on 'DB 42 () 42 42 42 42 42 42 42'" $ do
        getSecondParameter (DB 42 () 42 42 42 42 42 42 42) `shouldBe` ()

      it "works on 'DD () () 'a'" $ do
        getSecondParameter (DD () () 'a') `shouldBe` ()
