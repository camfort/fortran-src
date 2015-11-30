module Forpar.ParserMonadSpec where

import Forpar.ParserMonad
import Control.Monad.State.Lazy
import Test.Hspec

vanillaParseState :: ParseState String
vanillaParseState = ParseState { psAlexInput = "", psVersion = Fortran66, psFilename = "<unknown>" }

data SomeInput = SomeInput { p :: Position }

initPos :: Position
initPos = Position 5 1 2

initSomeInput :: SomeInput
initSomeInput = SomeInput { p = initPos }

instance Loc SomeInput where
  getPos = p

vanillaSomeInput :: ParseState SomeInput
vanillaSomeInput = ParseState { psAlexInput = initSomeInput, psVersion = Fortran66, psFilename = "some.f" }

spec :: Spec
spec =
  describe "ParserMonad" $ do
    describe "Parse" $ do
      it "should give out correct version" $ do
        evalState getVersionP vanillaParseState `shouldBe` Fortran66

      it "satisfies read after write equals to what is written" $ do
        let ai = evalState (putAlexP "l'enfer" >> getAlexP) vanillaParseState in
          ai `shouldBe` "l'enfer"

      describe "Obtaining locations" $ do
        it "getSrcLoc returns correct location" $ do
          let _expPosition = Position 6 2 3
              _exampleM = do
                _ai <- getAlexP
                putAlexP $ _ai { p = _expPosition }
                getSrcLoc
              _loc = evalState _exampleM vanillaSomeInput
              _expectation = SrcLoc { locFilename = "some.f" , locPosition = _expPosition } in
            _loc `shouldBe` _expectation

        it "getSrcSpan return correct location span" $ do
          let _loc2 = Position 6 2 3
              _exampleM = do
                _ai <- getAlexP
                _loc1 <- getSrcLoc
                putAlexP $ _ai { p = _loc2 }
                getSrcSpan _loc1
              _span = evalState _exampleM vanillaSomeInput 
              _expectation = (SrcLoc initPos "some.f", SrcLoc _loc2 "some.f") in 
            _span `shouldBe` _expectation

    describe "Lex" $ do
      it "reads the state correctly" $ do
        runLex getAlexL vanillaParseState `shouldBe` ""

      it "overrides the state correctly" $ do
        let ai = runLex (putAlexL "c'est" >> getAlexL) vanillaParseState in
            ai `shouldBe` "c'est"

      it "mixes operations correctly" $ do
       let ai = runLex (putAlexL "hello" >> getAlexL >>= \s -> (putAlexL $ take 4 s) >> getAlexL) vanillaParseState in
             ai `shouldBe` "hell"
