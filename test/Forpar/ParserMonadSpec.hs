module Forpar.ParserMonadSpec where

import Forpar.ParserMonad
import Control.Monad.State.Lazy
import Test.Hspec

vanillaParseState :: ParseState String
vanillaParseState = ParseState { alexInput = "", version = Fortran66 }

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
        runLex vanillaParseState getAlexL `shouldBe` ""

      it "overrides the state correctly" $ do
        let ai = runLex vanillaParseState (putAlexL "c'est") in
            ai `shouldBe` "c'est"

      it "mixes operations correctly" $ do
       let ai = runLex vanillaParseState (putAlexL "hello" >>= \s -> putAlexL $ take 4 s) in
             ai `shouldBe` "hell"
