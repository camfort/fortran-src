module Forpar.Lexer.FixedFormSpec where

import Forpar.ParserMonad
import Forpar.Lexer.FixedForm
import Forpar.AST (resetSrcSpan)

import Test.Hspec
import Test.Hspec.QuickCheck

import Control.Monad.State.Lazy
import Control.Exception

import Data.List (isPrefixOf)

singleLexer'App :: String -> Maybe Token
singleLexer'App srcInput = evalParse lexer' _parseState
  where
    _parseState = initParseState srcInput Fortran66 "<unknown>"

u = undefined

spec :: Spec
spec = 
  describe "Fortran Fixed Form Lexer" $ do
    describe "lexer" $ do
      prop "lexes Label, Comment, Newline or EOF in the first six columns or returns Nothing " $
        \x -> isPrefixOf "      " x || case singleLexer'App x of
                Nothing -> True
                Just (TLabel _ _) -> True
                Just (TComment _ _) -> True
                Just (TEOF _) -> True
                Just (TNewline _) -> True
                _ -> False
        
      it "lexes alphanumeric identifier" $ do
        resetSrcSpan (singleLexer'App "      m42") `shouldBe` resetSrcSpan (Just $ TId u "m42")

      it "lexes 'function'" $ do
        resetSrcSpan (singleLexer'App "      function") `shouldBe` resetSrcSpan (Just $ TFunction u)

      it "lexes 'end'" $ do
        resetSrcSpan (singleLexer'App "      end") `shouldBe` resetSrcSpan (Just $ TEnd u)
    
      it "lexes identifier" $ do
        resetSrcSpan (singleLexer'App "      mistr") `shouldBe` resetSrcSpan (Just $ TId u "mistr")

      it "lexes comment if first column is C" $ do
        resetSrcSpan (singleLexer'App "c this is a comment\n") `shouldBe` resetSrcSpan (Just $ TComment u " this is a comment")

      it "lexes empty comment" $ do
        resetSrcSpan (singleLexer'App "c") `shouldBe` resetSrcSpan (Just $ TComment u "")

      it "lexes comment with one char" $ do
        resetSrcSpan (singleLexer'App "ca") `shouldBe` resetSrcSpan (Just $ TComment u "a")

      it "should not lex from the next line" $ do
        resetSrcSpan (singleLexer'App "cxxx\nselam") `shouldNotBe` resetSrcSpan (Just $ TComment u "xxxselam")

      it "lexes three tokens"  $ do
        resetSrcSpan (collectFixedFormTokens "      function end format") `shouldBe` resetSrcSpan (Just [TFunction u, TEnd u, TFormat u, TEOF u])

      it "lexes multiple comments in a line" $ do
        resetSrcSpan (collectFixedFormTokens "csomething\ncsomething else\n\nc\ncc\n") `shouldBe` 
          resetSrcSpan (Just [TComment u "something", TNewline u, TComment u "something else", TNewline u, TNewline u, TComment u "", TNewline u, TComment u "c", TNewline u, TEOF u])

      it "lexes example1" $ do
        resetSrcSpan (collectFixedFormTokens example1) `shouldBe` resetSrcSpan (Just example1Expectation)
    
      it "lexes end of file" $ do
        resetSrcSpan (singleLexer'App "") `shouldBe` resetSrcSpan (Just $ TEOF u)

      it "lexes '3 + 2'" $ do
        resetSrcSpan (collectFixedFormTokens "      3 + 2") `shouldBe` resetSrcSpan (Just [TInt u "3", TOpPlus u , TInt u "2", TEOF u])

      it "should lex continuation lines properly" $ do
        resetSrcSpan (collectFixedFormTokens continuationExample) `shouldBe` resetSrcSpan (Just [ TType u "integer", TId u "ix", TNewline u, TId u "ix", TOpAssign u, TInt u "42", TNewline u, TEnd u, TNewline u, TEOF u ])

      describe "lexing format items" $ do
        it "lexes '10x'" $ do
          resetSrcSpan (singleLexer'App "      10x") `shouldBe` resetSrcSpan (Just $ TBlankDescriptor u 10)

        it "lexes '10a1'" $ do
          resetSrcSpan (singleLexer'App "      10a1") `shouldBe` resetSrcSpan (Just $ TFieldDescriptorAIL u (Just 10) 'a' 1)

        it "lexes 'a1'" $ do
          resetSrcSpan (singleLexer'App "      a1") `shouldBe` resetSrcSpan (Just $ TFieldDescriptorAIL u Nothing 'a' 1)

        it "lexes '20g10.3'" $ do
          resetSrcSpan (singleLexer'App "      20g10.3") `shouldBe` resetSrcSpan (Just $ TFieldDescriptorDEFG u (Just 20) 'g' 10 3)

        it "lexes '-10p'" $ do
          resetSrcSpan (singleLexer'App "      -10p") `shouldBe` resetSrcSpan (Just $ TScaleFactor u (-10))

    describe "lexN" $ do
      it "`lexN 5` parses lexes next five characters" $ do
        (lexemeMatch . aiLexeme) (evalParse (lexN 5 >> getAlex) (initParseState "helloWorld" Fortran66 "")) `shouldBe` reverse "hello"

    describe "lexHollerith" $ do
      it "lexes Hollerith '7hmistral'" $ do
        resetSrcSpan (singleLexer'App "      7hmistral") `shouldBe` resetSrcSpan (Just $ THollerith u "mistral")

      it "becomes case sensitive" $ do
        resetSrcSpan (singleLexer'App "      5h a= 1") `shouldBe` resetSrcSpan (Just $ THollerith u " a= 1")

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
  TType u "integer", TId u "ix", TNewline u,
  TLabel u "1", TId u "ix", TOpAssign u, TInt u "42", TNewline u,
  TLabel u "200", TId u "ix", TOpAssign u, TId u "ix", TStar u, TId u "ix", TNewline u,
  TLabel u "10", TWrite u, TLeftPar u, TStar u, TComma u, TStar u, TRightPar u, TComma u, TId u "ix", TNewline u,
  TEnd u, TNewline u,
  TEOF u]
