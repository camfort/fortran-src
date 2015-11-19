module Forpar.Lexer.FixedFormSpec where

import Forpar.ParserMonad
import Forpar.Lexer.FixedForm
import Test.Hspec
import Helper
import Control.Monad.State.Lazy
import Control.Monad.Trans.Cont

vanillaParseState :: ParseState AlexInput
vanillaParseState = ParseState { rAlexInput = undefined, rVersion = Fortran66 }

setSourceInput :: String -> ParseState AlexInput
setSourceInput srcInput = vanillaParseState { rAlexInput = vanillaAlexInput { rSourceInput = srcInput } } 

collectFixedTokens :: ParseState AlexInput -> [Token]
collectFixedTokens = collectTokens TEOF lexer'

spec :: Spec
spec = 
  describe "Fortran Fixed Form Lexer" $ do
    describe "lexer" $ do
      it "should lex 'function'" $ do
        runLex lexer' (setSourceInput "function") `shouldBe` TFunction

      it "should lex 'end'" $ do
        runLex lexer' (setSourceInput "end") `shouldBe` TEnd
    
      it "should lex identifier" $ do
        runLex lexer' (setSourceInput "mistr") `shouldBe` TId "mistr"

      it "should lex comment if first column is C" $ do
        runLex lexer' (setSourceInput "c this is a comment\n") `shouldBe` TComment " this is a comment"

      it "should lex empty comment" $ do
        runLex lexer' (setSourceInput "c") `shouldBe` TComment ""

      it "should lex comment with one char" $ do
        runLex lexer' (setSourceInput "ca") `shouldBe` TComment "a"

      it "should not lex from the next line" $ do
        runLex lexer' (setSourceInput "cxxx\nselam") `shouldNotBe` TComment "xxxselam"

      it "should lex three tokens"  $ do
        collectFixedTokens (setSourceInput "function end format") `shouldBe` [TFunction, TEnd, TFormat, TEOF]

      it "should multiple comments in a line" $ do
        collectFixedTokens (setSourceInput "csomething\ncsomething else\n\nc\ncc") `shouldBe` 
          [TComment "something", TComment "something else", TComment "", TComment "c", TEOF]

      it "should lex example1" $ do
        collectFixedTokens (setSourceInput example1) `shouldBe` example1Expectation

    describe "lexer' >> lexer'" $ do
      it "should lex two tokens and return last one" $ do
        runLex (lexer' >> lexer') (setSourceInput "function end") `shouldBe` TEnd

    describe "lexN" $ do
      it "`lexN 5` parses lexes next five characters" $ do
        rMatch (evalState (runContT (lexN 5 >> getAlexL) return) (setSourceInput "helloWorld")) `shouldBe` reverse "hello"

    describe "lexHollerith" $ do
      it "lexes Hollerith '7hmistral'" $ do
        runLex lexer' (setSourceInput "7hmistral") `shouldBe` THollerith "mistral"

example1 = unwords [
  "      integer ix\n",
  "      ix = 42\n",
  "      ix = ix * ix\n",
  "      write (*,*), ix\n",
  "      end" ]

example1Expectation = [
  TType "integer", TId "ix",
  TId "ix", TOpAssign, TNum "42",
  TId "ix", TOpAssign, TId "ix", TStar , TId "ix",
  TWrite, TLeftPar, TStar, TComma, TStar, TRightPar, TComma, TId "ix",
  TEnd,
  TEOF]
