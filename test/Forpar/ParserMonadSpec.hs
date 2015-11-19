module Forpar.ParserMonadSpec where

import Forpar.ParserMonad
import Control.Monad.State.Lazy
import Test.Hspec
import Helper

vanillaParseState :: ParseState String
vanillaParseState = ParseState { rAlexInput = "", rVersion = Fortran66 }

spec :: Spec
spec =
  describe "ParserMonad" $ do
    describe "Parse" $ do
      it "should give out correct version" $ do
        evalState getVersionP vanillaParseState `shouldBe` Fortran66

      it "satisfies read after write equals to what is written" $ do
        let ai = evalState (putAlexP "l'enfer" >> getAlexP) vanillaParseState in
          ai `shouldBe` "l'enfer"

    describe "Lex" $ do
      it "reads the state correctly" $ do
        runLex getAlexL vanillaParseState `shouldBe` ""

      it "overrides the state correctly" $ do
        let ai = runLex (putAlexL "c'est") vanillaParseState in
            ai `shouldBe` "c'est"

      it "mixes operations correctly" $ do
       let ai = runLex (putAlexL "hello" >>= \s -> putAlexL $ take 4 s) vanillaParseState in
             ai `shouldBe` "hell"
