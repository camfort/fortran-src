module Language.Fortran.Lexer.FixedFormSpec where

import Language.Fortran.ParserMonad
import Language.Fortran.Lexer.FixedForm

import Test.Hspec
import Test.Hspec.QuickCheck
import TestUtil

import Control.Monad.State.Lazy
import Control.Exception

import Data.List (isPrefixOf)

lex66 :: String -> Maybe Token
lex66 = collectToLex Fortran66

safeLex66 :: String -> Maybe Token
safeLex66 = collectToLexSafe Fortran66

lex77 :: String -> Maybe Token
lex77 = collectToLex Fortran77

collectToLex :: FortranVersion -> String -> Maybe Token
collectToLex version srcInput = dropUntil2 $ collectFixedTokens version srcInput
  where
    dropUntil2 [] = Nothing
    dropUntil2 [_] = Nothing
    dropUntil2 [a,_] = Just a
    dropUntil2 (x:xs) = dropUntil2 xs

collectToLexSafe :: FortranVersion -> String -> Maybe Token
collectToLexSafe version srcInput = dropUntil2 $ collectFixedTokensSafe version srcInput
  where
    dropUntil2 (Just [a,_]) = Just a
    dropUntil2 (Just (x:xs)) = dropUntil2 $ Just xs
    dropUntil2 _ = Nothing

spec :: Spec
spec =
  describe "Fortran Fixed Form Lexer" $ do
    describe "Fortran 77" $
      describe "String" $ do
        it "lexes 'hello'" $
          resetSrcSpan (lex77 "      c = 'hello'") `shouldBe` resetSrcSpan (Just $ TString u "hello")

        it "lexes 'he''llo'" $
          resetSrcSpan (lex77 "      c = 'he''llo'") `shouldBe` resetSrcSpan (Just $ TString u "he'llo")

        it "lexes 'he''''ll''o'" $
          resetSrcSpan (lex77 "      c = 'he''''ll''o'") `shouldBe` resetSrcSpan (Just $ TString u "he''ll'o")

        it "lexes '''hello'''" $
          resetSrcSpan (lex77 "      c = '''hello'''") `shouldBe` resetSrcSpan (Just $ TString u "'hello'")

        it "lexes 'hello world'" $
          resetSrcSpan (lex77 "      c = 'hello world'") `shouldBe` resetSrcSpan (Just $ TString u "hello world")

        it "lexes 'hello world'" $
          resetSrcSpan (collectFixedTokens Fortran77 "      c = 'x' // 'o'") `shouldBe` resetSrcSpan [TId u "c", TOpAssign u, TString u "x", TSlash u, TSlash u, TString u "o", TEOF u]

    describe "Fortran 66" $ do
      prop "lexes Label, Comment, Newline or EOF in the first six columns or returns Nothing " $
        \x -> isPrefixOf "      " x || case safeLex66 x of
                Nothing -> True
                Just (TLabel _ _) -> True
                Just (TComment _ _) -> True
                Just (TEOF _) -> True
                Just (TNewline _) -> True
                _ -> False

      it "lexes alphanumeric identifier" $
        resetSrcSpan (collectFixedTokens Fortran66 "      e42 =") `shouldBe` resetSrcSpan [TId u "e42", TOpAssign u, TEOF u]

      it "lexes exponent" $
        resetSrcSpan (collectFixedTokens Fortran66 "      a = 42 e42") `shouldBe` resetSrcSpan [TId u "a", TOpAssign u, TInt u "42", TExponent u "e42", TEOF u]

      it "lexes 'function'" $
        resetSrcSpan (lex66 "      function") `shouldBe` resetSrcSpan (Just $ TFunction u)

      it "lexes 'end'" $
        resetSrcSpan (lex66 "      end") `shouldBe` resetSrcSpan (Just $ TEnd u)

      it "lexes identifier" $
        resetSrcSpan (lex66 "      a = mistr") `shouldBe` resetSrcSpan (Just $ TId u "mistr")

      it "lexes comment if first column is C" $
        resetSrcSpan (lex66 "c this is a comment") `shouldBe` resetSrcSpan (Just $ TComment u " this is a comment")

      it "lexes empty comment" $
        resetSrcSpan (lex66 "c") `shouldBe` resetSrcSpan (Just $ TComment u "")

      it "lexes comment with one char" $
        resetSrcSpan (lex66 "ca") `shouldBe` resetSrcSpan (Just $ TComment u "a")

      it "should not lex from the next line" $
        resetSrcSpan (safeLex66 "cxxx\nselam") `shouldNotBe` resetSrcSpan (Just $ TComment u "xxxselam")

      -- This is commented out as identifiers are longer than what the standard says.
      it "lexes three tokens"  $ do
        pending
        resetSrcSpan (collectFixedTokens Fortran66 "      function end format") `shouldBe` resetSrcSpan [TFunction u, TId u "endfor", TId u "mat", TEOF u]

      it "lexes multiple comments in a line" $
        resetSrcSpan (collectFixedTokens Fortran66 "csomething\ncsomething else\n\nc\ncc\n") `shouldBe`
          resetSrcSpan [TComment u "something", TNewline u, TComment u "something else", TNewline u, TNewline u, TComment u "", TNewline u, TComment u "c", TNewline u, TEOF u]

      it "lexes example1" $
        resetSrcSpan (collectFixedTokens Fortran66 example1) `shouldBe` resetSrcSpan example1Expectation

      it "lexes end of file" $
        resetSrcSpan (lex66 "") `shouldBe` Nothing

      it "lexes '3 + 2'" $
        resetSrcSpan (collectFixedTokens Fortran66 "      a = 3 + 2") `shouldBe` resetSrcSpan [TId u "a", TOpAssign u, TInt u "3", TOpPlus u , TInt u "2", TEOF u]

      it "should lex continuation lines properly" $
        resetSrcSpan (collectFixedTokens Fortran66 continuationExample) `shouldBe` resetSrcSpan [ TType u "integer", TId u "ix", TNewline u, TId u "ix", TOpAssign u, TInt u "42", TNewline u, TEnd u, TNewline u, TEOF u ]

      describe "lexing format items" $ do
        it "lexes '10x'" $
          resetSrcSpan (lex66 "      format 10x") `shouldBe` resetSrcSpan (Just $ TBlankDescriptor u 10)

        it "lexes '10a1'" $
          resetSrcSpan (lex66 "      format 10a1") `shouldBe` resetSrcSpan (Just $ TFieldDescriptorAIL u (Just 10) 'a' 1)

        it "lexes 'a1'" $
          resetSrcSpan (lex66 "      format a1") `shouldBe` resetSrcSpan (Just $ TFieldDescriptorAIL u Nothing 'a' 1)

        it "lexes '20g10.3'" $
          resetSrcSpan (lex66 "      format 20g10.3") `shouldBe` resetSrcSpan (Just $ TFieldDescriptorDEFG u (Just 20) 'g' 10 3)

        it "lexes '-10p'" $
          resetSrcSpan (lex66 "      format -10p") `shouldBe` resetSrcSpan (Just $ TScaleFactor u (-10))

    describe "lexN" $
      it "`lexN 5` parses lexes next five characters" $
        (lexemeMatch . aiLexeme) (evalParse (lexN 5 >> getAlex) (initParseState "helloWorld" Fortran66 "")) `shouldBe` reverse "hello"

    describe "lexHollerith" $ do
      it "lexes Hollerith '7hmistral'" $
        resetSrcSpan (lex66 "      format 7hmistral") `shouldBe` resetSrcSpan (Just $ THollerith u "mistral")

      it "becomes case sensitive" $
        resetSrcSpan (collectFixedTokens Fortran66 "      format (5h a= 1)") `shouldBe` resetSrcSpan [ TFormat u, TLeftPar u, THollerith u " a= 1", TRightPar u, TEOF u ]

    it "lexes if statement '        IF (IY) 5,6,6'" $
      resetSrcSpan (collectFixedTokens Fortran66 "      IF (IY) 5,6,6") `shouldBe` resetSrcSpan [TIf u, TLeftPar u, TId u "iy", TRightPar u, TInt u "5", TComma u, TInt u "6", TComma u, TInt u "6", TEOF u]

    it "lexes if then statement '      if (x) then'" $
      resetSrcSpan (collectFixedTokens Fortran77 "      if (x) then") `shouldBe` resetSrcSpan [TIf u, TLeftPar u, TId u "x", TRightPar u, TThen u, TEOF u]

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
