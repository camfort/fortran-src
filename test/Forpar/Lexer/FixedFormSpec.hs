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

singleLexer'App :: String -> Token
singleLexer'App str = runLex lexer' (setSourceInput str) 

spec :: Spec
spec = 
  describe "Fortran Fixed Form Lexer" $ do
    describe "lexer" $ do
      it "lexes 'function'" $ do
        singleLexer'App "      function" `shouldBe` TFunction

      it "lexes 'end'" $ do
        singleLexer'App "      end" `shouldBe` TEnd
    
      it "lexes identifier" $ do
        singleLexer'App "      mistr" `shouldBe` TId "mistr"

      it "lexes comment if first column is C" $ do
        singleLexer'App "c this is a comment\n" `shouldBe` TComment " this is a comment"

      it "lexes empty comment" $ do
        singleLexer'App "c" `shouldBe` TComment ""

      it "lexes comment with one char" $ do
        singleLexer'App "ca" `shouldBe` TComment "a"

      it "should not lex from the next line" $ do
        singleLexer'App "cxxx\nselam" `shouldNotBe` TComment "xxxselam"

      it "lexes three tokens"  $ do
        collectFixedTokens (setSourceInput "      function end format") `shouldBe` [TFunction, TEnd, TFormat, TEOF]

      it "lexes multiple comments in a line" $ do
        collectFixedTokens (setSourceInput "csomething\ncsomething else\n\nc\ncc") `shouldBe` 
          [TComment "something", TComment "something else", TComment "", TComment "c", TEOF]

      it "lexes example1" $ do
        collectFixedTokens (setSourceInput example1) `shouldBe` example1Expectation

      it "shouldn't lex anything apart from numbers/comments in the first 6 columns" $ do
        pending

    describe "lexer' >> lexer'" $ do
      it "lexes two tokens and return last one" $ do
        runLex (lexer' >> lexer') (setSourceInput "      function end") `shouldBe` TEnd

    describe "lexN" $ do
      it "`lexN 5` parses lexes next five characters" $ do
        rMatch (evalState (runContT (lexN 5 >> getAlexL) return) (setSourceInput "helloWorld")) `shouldBe` reverse "hello"

    describe "lexHollerith" $ do
      it "lexes Hollerith '7hmistral'" $ do
        singleLexer'App "      7hmistral" `shouldBe` THollerith "mistral"

example1 = unlines [
  "      intEGerix",
  "1         iX= 42",
  " 200    ix =IX* ix",
  " 10   wrITe (*,*), ix",
  "        EnD" ]

example1Expectation = [
  TType "integer", TId "ix",
  TLabel "1", TId "ix", TOpAssign, TNum "42",
  TLabel "200", TId "ix", TOpAssign, TId "ix", TStar , TId "ix",
  TLabel "10", TWrite, TLeftPar, TStar, TComma, TStar, TRightPar, TComma, TId "ix",
  TEnd,
  TEOF]
