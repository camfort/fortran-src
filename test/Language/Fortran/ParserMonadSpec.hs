{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Fortran.ParserMonadSpec where

import Test.Hspec

import Language.Fortran.ParserMonad
import Language.Fortran.Util.Position

vanillaParseState :: ParseState String
vanillaParseState = ParseState
  { psAlexInput = ""
  , psVersion = Fortran66
  , psFilename = "<unknown>"
  , psParanthesesCount = ParanthesesCount 0 False
  , psContext = [ ConStart ]
  }

instance Loc String where
  getPos = error "Never needed"

instance LastToken String String where
  getLastToken  = error "Never needed"

data SomeInput = SomeInput { p :: Position }

initPos :: Position
initPos = Position 5 1 2 ""

initSomeInput :: SomeInput
initSomeInput = SomeInput { p = initPos }

instance Loc SomeInput where
  getPos = p

instance LastToken SomeInput String where
  getLastToken  = error "Never needed"

vanillaSomeInput :: ParseState SomeInput
vanillaSomeInput = ParseState
  { psAlexInput = initSomeInput
  , psVersion = Fortran66
  , psFilename = "some.f"
  , psParanthesesCount = ParanthesesCount 0 False
  , psContext = [ ConStart ]
  }

spec :: Spec
spec =
  describe "ParserMonad" $ do
    describe "Parse" $ do
      it "should give out correct version" $
        evalParse getVersion vanillaParseState `shouldBe` Fortran66

      it "satisfies read after write equals to what is written" $
        let ai = evalParse (putAlex "l'enfer" >> getAlex) vanillaParseState in
          ai `shouldBe` "l'enfer"

      describe "Obtaining locations" $ do
        it "getPosition returns correct location" $
          let _expPosition = Position 6 2 3 "some.f"
              _exampleM = do
                _ai <- getAlex
                putAlex $ _ai { p = _expPosition }
                getPosition
              _loc = evalParse _exampleM vanillaSomeInput in
            _loc `shouldBe` _expPosition

        it "getSrcSpan return correct location span" $
          let _loc2 = Position 6 2 3 "some.f"
              _exampleM = do
                _ai <- getAlex
                _loc1 <- getPosition
                putAlex $ _ai { p = _loc2 }
                getSrcSpan _loc1
              _span = evalParse _exampleM vanillaSomeInput
              _expectation = SrcSpan initPos _loc2 in
            _span `shouldBe` _expectation

    describe "Lex" $ do
      it "reads the state correctly" $
        evalParse getAlex vanillaParseState `shouldBe` ""

      it "overrides the state correctly" $
        let ai = evalParse (putAlex "c'est" >> getAlex) vanillaParseState in
            ai `shouldBe` "c'est"

      it "mixes operations correctly" $
       let ai = evalParse (putAlex "hello" >> getAlex >>= \s -> putAlex (take 4 s) >> getAlex) vanillaParseState in
             ai `shouldBe` "hell"
