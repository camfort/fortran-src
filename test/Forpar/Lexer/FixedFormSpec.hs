module Forpar.Lexer.FixedFormSpec where

import Forpar.ParserMonad
import Forpar.Lexer.FixedForm

import Test.Hspec
import Test.Hspec.QuickCheck

import Control.Monad.State.Lazy
import Control.Monad.Trans.Cont
import Control.Exception

import Data.List (isPrefixOf)

singleLexer'App :: String -> Maybe Token
singleLexer'App srcInput = runLex lexer' _parseState
  where
    _vanillaParseState = ParseState { rAlexInput = undefined, rVersion = Fortran66 }
    _parseState = _vanillaParseState { rAlexInput = vanillaAlexInput { rSourceInput = srcInput } } 

spec :: Spec
spec = 
  describe "Fortran Fixed Form Lexer" $ do
    describe "lexer" $ do
      prop "lexes Label, Comment, or EOF in the first six columns or returns Nothing " $
        \x -> isPrefixOf "      " x || case singleLexer'App x of
                Nothing -> True
                Just (TLabel _) -> True
                Just (TComment _) -> True
                Just TEOF -> True
                _ -> False
        
      it "lexes alphanumeric identifier" $ do
        singleLexer'App "      m42" `shouldBe` (Just $ TId "m42")

      it "lexes 'function'" $ do
        singleLexer'App "      function" `shouldBe` Just TFunction

      it "lexes 'end'" $ do
        singleLexer'App "      end" `shouldBe` Just TEnd
    
      it "lexes identifier" $ do
        singleLexer'App "      mistr" `shouldBe` (Just $ TId "mistr")

      it "lexes comment if first column is C" $ do
        singleLexer'App "c this is a comment\n" `shouldBe` (Just $ TComment " this is a comment")

      it "lexes empty comment" $ do
        singleLexer'App "c" `shouldBe` (Just $ TComment "")

      it "lexes comment with one char" $ do
        singleLexer'App "ca" `shouldBe` (Just $ TComment "a")

      it "should not lex from the next line" $ do
        singleLexer'App "cxxx\nselam" `shouldNotBe` (Just $ TComment "xxxselam")

      it "lexes three tokens"  $ do
        collectFixedFormTokens "      function end format" `shouldBe` Just [TFunction, TEnd, TFormat, TEOF]

      it "lexes multiple comments in a line" $ do
        collectFixedFormTokens "csomething\ncsomething else\n\nc\ncc" `shouldBe` 
          Just [TComment "something", TComment "something else", TComment "", TComment "c", TEOF]

      it "lexes example1" $ do
        collectFixedFormTokens example1 `shouldBe` Just example1Expectation
    
      it "lexes end of file" $ do
        singleLexer'App "" `shouldBe` Just TEOF

      it "should lex continuation lines properly" $ do
        collectFixedFormTokens continuationExample `shouldBe` Just [ TType "integer", TId "ix", TId "ix", TOpAssign, TNum "42", TEnd, TEOF ]

      describe "lexing format items" $ do
        it "lexes '10x'" $ do
          singleLexer'App "      10x" `shouldBe` (Just $ TFormatItem "10x")

        it "lexes '10a1'" $ do
          singleLexer'App "      10a1" `shouldBe` (Just $ TFormatItem "10a1")

        it "lexes 'a1'" $ do
          singleLexer'App "      a1" `shouldBe` (Just $ TFormatItem "a1")

        it "lexes '20g10.3'" $ do
          singleLexer'App "      20g10.3" `shouldBe` (Just $ TFormatItem "20g10.3")

    describe "lexN" $ do
      it "`lexN 5` parses lexes next five characters" $ do
        rMatch (evalState (runContT (lexN 5 >> getAlexL) return) (initParseState "helloWorld")) `shouldBe` reverse "hello"

    describe "lexHollerith" $ do
      it "lexes Hollerith '7hmistral'" $ do
        singleLexer'App "      7hmistral" `shouldBe` (Just $ THollerith "mistral")

example1 = unlines [
  "      intEGerix",
  "1         iX= 42",
  " 200    ix =IX* ix",
  " 10   wrITe (*,*), ix",
  "        EnD" ]

continuationExample = unlines [
  "      inte",
  "     .ger i",
  "     .x",
  "      ix = 4",
  "     .2",
  "      end"]

example1Expectation = [
  TType "integer", TId "ix",
  TLabel "1", TId "ix", TOpAssign, TNum "42",
  TLabel "200", TId "ix", TOpAssign, TId "ix", TStar , TId "ix",
  TLabel "10", TWrite, TLeftPar, TStar, TComma, TStar, TRightPar, TComma, TId "ix",
  TEnd,
  TEOF]
