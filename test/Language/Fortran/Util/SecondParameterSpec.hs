{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Fortran.Util.SecondParameterSpec(spec) where

import Test.Hspec

import GHC.Generics (Generic(..))

import Language.Fortran.Util.SecondParameter

data A = A Char Int deriving (Generic, Eq, Show)
data B = B Int Int Int Int deriving (Generic, Eq, Show)
data C = CA String String | CB Int String () deriving (Generic, Eq, Show)
data D = DA () () | DB Int () Int Int Int Int Int Int Int | DC () () | DD () () Char deriving (Generic, Eq, Show)

instance SecondParameter A Int
instance SecondParameter B Int
instance SecondParameter C String
instance SecondParameter D ()

spec :: Spec
spec =
  describe "Second parameter retrieving type class" $ do
    describe "data A" $ do
      it "retrieves second parameter from 'A 'a' 42'" $
        getSecondParameter (A 'x' 42) `shouldBe` 42

      it "sets second parameter in \"A 'a' 42\" to 24" $
        setSecondParameter 24 (A 'x' 42) `shouldBe` A 'x' 24

    describe "data B" $ do
      it "retrieves second parameter from 'B 41 42 43 44'" $
        getSecondParameter (B 41 42 43 44) `shouldBe` 42

      it "sets second parameter in \"B 41 42 43 44\" to 24" $
        setSecondParameter 24 (B 41 42 43 44) `shouldBe` B 41 24 43 44

    describe "data C" $ do
      it "retrieves second parameter from 'CA \"hello\" ['x', 'y']'" $
        getSecondParameter (CA "hello" ['x', 'y']) `shouldBe` ['x', 'y']

      it "retrieves second parameter from 'CB 42 [] ()'" $
        getSecondParameter (CB 42 [] ()) `shouldBe` []

      it "sets second parameter in \"CB 42 []\" to ['x','x','x']" $
        setSecondParameter "xxx" (CB 42 [] ()) `shouldBe` CB 42 "xxx" ()

    describe "data d" $ do
      it "retrieves second parameter from 'DB 42 () 42 42 42 42 42 42 42'" $
        getSecondParameter (DB 42 () 42 42 42 42 42 42 42) `shouldBe` ()

      it "retrieves second parameter from 'DD () () 'a'" $
        getSecondParameter (DD () () 'a') `shouldBe` ()
