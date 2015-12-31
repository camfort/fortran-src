module Forpar.Lexer.FixedFormSpec where

import Forpar.ParserMonad
import Forpar.Lexer.FixedForm

import Test.Hspec
import Test.Hspec.QuickCheck

import Control.Monad.State.Lazy
import Control.Exception

import Data.List (isPrefixOf)

singleLexer'App :: String -> Maybe Token
singleLexer'App srcInput = runParse lexer' _parseState
  where
    _parseState = initParseState srcInput Fortran66 "<unknown>"

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

      it "lexes '3 + 2'" $ do
        collectFixedFormTokens "      3 + 2" `shouldBe` Just [TInt "3", TOpPlus, TInt "2", TEOF]

      it "should lex continuation lines properly" $ do
        collectFixedFormTokens continuationExample `shouldBe` Just [ TType "integer", TId "ix", TId "ix", TOpAssign, TInt "42", TEnd, TEOF ]

      describe "lexing format items" $ do
        it "lexes '10x'" $ do
          singleLexer'App "      10x" `shouldBe` (Just $ TBlankDescriptor 10)

        it "lexes '10a1'" $ do
          singleLexer'App "      10a1" `shouldBe` (Just $ TFieldDescriptorAIL (Just 10) 'a' 1)

        it "lexes 'a1'" $ do
          singleLexer'App "      a1" `shouldBe` (Just $ TFieldDescriptorAIL Nothing 'a' 1)

        it "lexes '20g10.3'" $ do
          singleLexer'App "      20g10.3" `shouldBe` (Just $ TFieldDescriptorDEFG (Just 20) 'g' 10 3)

        it "lexes '-10p'" $ do
          singleLexer'App "      -10p" `shouldBe` (Just $ TScaleFactor $ -10)

    describe "lexN" $ do
      it "`lexN 5` parses lexes next five characters" $ do
        aiMatch (evalState (lexN 5 >> getAlexP) (initParseState "helloWorld" Fortran66 "")) `shouldBe` reverse "hello"

    describe "lexHollerith" $ do
      it "lexes Hollerith '7hmistral'" $ do
        singleLexer'App "      7hmistral" `shouldBe` (Just $ THollerith "mistral")

      it "becomes case sensitive" $ do
        singleLexer'App "      5h a= 1" `shouldBe` (Just $ THollerith " a= 1")

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
  TLabel "1", TId "ix", TOpAssign, TInt "42",
  TLabel "200", TId "ix", TOpAssign, TId "ix", TStar , TId "ix",
  TLabel "10", TWrite, TLeftPar, TStar, TComma, TStar, TRightPar, TComma, TId "ix",
  TEnd,
  TEOF]
