module Forpar.Lexer.FixedFormSpec where

import Forpar.ParserMonad
import Forpar.Lexer.FixedForm
import Forpar.AST (resetSrcSpan)

import Test.Hspec
import Test.Hspec.QuickCheck

import Control.Monad.State.Lazy
import Control.Exception

import Data.List (isPrefixOf)

lex66 :: String -> Maybe Token
lex66 = collectToLex Fortran66

lex77 :: String -> Maybe Token
lex77 = collectToLex Fortran77 

collectToLex :: FortranVersion -> String -> Maybe Token
collectToLex version srcInput = do
  tokens <- collectFixedTokens version srcInput
  dropUntil2 tokens
  where
    dropUntil2 [] = Nothing
    dropUntil2 [_] = Nothing
    dropUntil2 [a,_] = Just a
    dropUntil2 (x:xs) = dropUntil2 xs

u = undefined

spec :: Spec
spec = 
  describe "Fortran Fixed Form Lexer" $ do
    describe "Fortran 77" $ do
      describe "String" $ do
        it "lexes 'hello'" $ do
          resetSrcSpan (lex77 "      c = 'hello'") `shouldBe` resetSrcSpan (Just $ TString u "hello")

        it "lexes 'he''llo'" $ do
          resetSrcSpan (lex77 "      c = 'he''llo'") `shouldBe` resetSrcSpan (Just $ TString u "he'llo")

        it "lexes 'he''''ll''o'" $ do
          resetSrcSpan (lex77 "      c = 'he''''ll''o'") `shouldBe` resetSrcSpan (Just $ TString u "he''ll'o")

        it "lexes '''hello'''" $ do
          resetSrcSpan (lex77 "      c = '''hello'''") `shouldBe` resetSrcSpan (Just $ TString u "'hello'")

        it "lexes 'hello world'" $ do
          resetSrcSpan (lex77 "      c = 'hello world'") `shouldBe` resetSrcSpan (Just $ TString u "hello world")

        it "lexes 'hello world'" $ do
          resetSrcSpan (collectFixedTokens Fortran77 "      c = 'x' // 'o'") `shouldBe` resetSrcSpan (Just $ [TId u "c", TOpAssign u, TString u "x", TSlash u, TSlash u, TString u "o", TEOF u])

    describe "Fortran 66" $ do
      prop "lexes Label, Comment, Newline or EOF in the first six columns or returns Nothing " $
        \x -> isPrefixOf "      " x || case lex66 x of
                Nothing -> True
                Just (TLabel _ _) -> True
                Just (TComment _ _) -> True
                Just (TEOF _) -> True
                Just (TNewline _) -> True
                _ -> False

      it "lexes alphanumeric identifier" $ do
        resetSrcSpan (collectFixedTokens Fortran66 "      e42 =") `shouldBe` resetSrcSpan (Just $ [TId u "e42", TOpAssign u, TEOF u] )

      it "lexes exponent" $ do
        resetSrcSpan (collectFixedTokens Fortran66 "      a = 42 e42") `shouldBe` resetSrcSpan (Just $ [TId u "a", TOpAssign u, TInt u "42", TExponent u "e42", TEOF u])

      it "lexes 'function'" $ do
        resetSrcSpan (lex66 "      function") `shouldBe` resetSrcSpan (Just $ TFunction u)

      it "lexes 'end'" $ do
        resetSrcSpan (lex66 "      end") `shouldBe` resetSrcSpan (Just $ TEnd u)
    
      it "lexes identifier" $ do
        resetSrcSpan (lex66 "      a = mistr") `shouldBe` resetSrcSpan (Just $ TId u "mistr")

      it "lexes comment if first column is C" $ do
        resetSrcSpan (lex66 "c this is a comment") `shouldBe` resetSrcSpan (Just $ TComment u " this is a comment")

      it "lexes empty comment" $ do
        resetSrcSpan (lex66 "c") `shouldBe` resetSrcSpan (Just $ TComment u "")

      it "lexes comment with one char" $ do
        resetSrcSpan (lex66 "ca") `shouldBe` resetSrcSpan (Just $ TComment u "a")

      it "should not lex from the next line" $ do
        resetSrcSpan (lex66 "cxxx\nselam") `shouldNotBe` resetSrcSpan (Just $ TComment u "xxxselam")

      it "lexes three tokens"  $ do
        resetSrcSpan (collectFixedTokens Fortran66 "      function end format") `shouldBe` resetSrcSpan (Just [TFunction u, TId u "endfor", TId u "mat", TEOF u])

      it "lexes multiple comments in a line" $ do
        resetSrcSpan (collectFixedTokens Fortran66 "csomething\ncsomething else\n\nc\ncc\n") `shouldBe` 
          resetSrcSpan (Just [TComment u "something", TNewline u, TComment u "something else", TNewline u, TNewline u, TComment u "", TNewline u, TComment u "c", TNewline u, TEOF u])

      it "lexes example1" $ do
        resetSrcSpan (collectFixedTokens Fortran66 example1) `shouldBe` resetSrcSpan (Just example1Expectation)
    
      it "lexes end of file" $ do
        resetSrcSpan (lex66 "") `shouldBe` Nothing

      it "lexes '3 + 2'" $ do
        resetSrcSpan (collectFixedTokens Fortran66 "      a = 3 + 2") `shouldBe` resetSrcSpan (Just [TId u "a", TOpAssign u, TInt u "3", TOpPlus u , TInt u "2", TEOF u])

      it "should lex continuation lines properly" $ do
        resetSrcSpan (collectFixedTokens Fortran66 continuationExample) `shouldBe` resetSrcSpan (Just [ TType u "integer", TId u "ix", TNewline u, TId u "ix", TOpAssign u, TInt u "42", TNewline u, TEnd u, TNewline u, TEOF u ])

      describe "lexing format items" $ do
        it "lexes '10x'" $ do
          resetSrcSpan (lex66 "      format 10x") `shouldBe` resetSrcSpan (Just $ TBlankDescriptor u 10)

        it "lexes '10a1'" $ do
          resetSrcSpan (lex66 "      format 10a1") `shouldBe` resetSrcSpan (Just $ TFieldDescriptorAIL u (Just 10) 'a' 1)

        it "lexes 'a1'" $ do
          resetSrcSpan (lex66 "      format a1") `shouldBe` resetSrcSpan (Just $ TFieldDescriptorAIL u Nothing 'a' 1)

        it "lexes '20g10.3'" $ do
          resetSrcSpan (lex66 "      format 20g10.3") `shouldBe` resetSrcSpan (Just $ TFieldDescriptorDEFG u (Just 20) 'g' 10 3)

        it "lexes '-10p'" $ do
          resetSrcSpan (lex66 "      format -10p") `shouldBe` resetSrcSpan (Just $ TScaleFactor u (-10))

    describe "lexN" $ do
      it "`lexN 5` parses lexes next five characters" $ do
        (lexemeMatch . aiLexeme) (evalParse (lexN 5 >> getAlex) (initParseState "helloWorld" Fortran66 "")) `shouldBe` reverse "hello"

    describe "lexHollerith" $ do
      it "lexes Hollerith '7hmistral'" $ do
        resetSrcSpan (lex66 "      format 7hmistral") `shouldBe` resetSrcSpan (Just $ THollerith u "mistral")

      it "becomes case sensitive" $ do
        resetSrcSpan (lex66 "      format 5h a= 1") `shouldBe` resetSrcSpan (Just $ THollerith u " a= 1")

    it "lexer if statement '        IF (IY) 5,6,6'" $ do
      resetSrcSpan (collectFixedTokens Fortran66 "      IF (IY) 5,6,6") `shouldBe` resetSrcSpan (Just [TIf u, TLeftPar u, TId u "iy", TRightPar u, TInt u "5", TComma u, TInt u "6", TComma u, TInt u "6", TEOF u])

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
