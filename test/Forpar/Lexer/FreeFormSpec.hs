module Forpar.Lexer.FreeFormSpec where

import Test.Hspec
import TestUtil

import Data.Maybe (fromJust)

import Forpar.ParserMonad (FortranVersion(..))
import Forpar.Lexer.FreeForm (collectFreeTokens, Token(..))

import Debug.Trace

collectF90 :: String -> [ Token ]
collectF90 = traceShowId . fromJust . collectFreeTokens Fortran90

spec :: Spec
spec =
  describe "Fortran Free Form Lexer" $
    describe "Fortran 90" $ do
      describe "Character sensitivity" $ do
        it "lexes lower case tokens" $
          shouldBe' (collectF90 "integer id") $
                    fmap ($u) [ TInteger, flip TId "id", TEOF ]

        it "lexes mixed case tokens" $
          shouldBe' (collectF90 "InTEgeR ID") $
                    fmap ($u) [ TInteger, flip TId "id", TEOF ]

      describe "Identifier" $ do
        it "lexes long ID names" $
          shouldBe' (collectF90 "program long_id_name") $
                    fmap ($u) [ TProgram, flip TId "long_id_name", TEOF ]

        it "treats 'if' as ID if used in assignment" $
          shouldBe' (collectF90 "if = 20") $
                    fmap ($u) [ flip TId "if", TOpAssign
                              , flip TIntegerLiteral "20", TEOF ]

        it "'result' is an identifier in spec. context" $
          shouldBe' (collectF90 "integer :: result") $
                    fmap ($u) [ TInteger, TDoubleColon , flip TId "result"
                              , TEOF ]

      describe "Types" $ do
        it "lexes simple type tokens" $
          shouldBe' (collectF90 "character x") $
                    fmap ($u) [ TCharacter, flip TId "x", TEOF ]

        it "lexes simple type tokens in function" $
          shouldBe' (collectF90 "character function x") $
                    fmap ($u) [ TCharacter, TFunction, flip TId "x", TEOF ]

        it "lexes character type with F77 length syntax (1)" $
          shouldBe' (collectF90 "character * (*) function x") $
                    fmap ($u) [ TCharacter, TStar, TLeftPar, TStar, TRightPar, TFunction, flip TId "x", TEOF ]

        it "lexes character type with F77 length syntax (2)" $
          shouldBe' (collectF90 "character * 20 function x") $
                    fmap ($u) [ TCharacter, TStar, flip TIntegerLiteral "20", TFunction, flip TId "x", TEOF ]

        it "lexes derived type tokens in function" $
          shouldBe' (collectF90 "type (x) function x") $
                    fmap ($u) [ TType, TLeftPar, flip TId "x", TRightPar
                              , TFunction, flip TId "x", TEOF ]

        it "lexes interleaved type recursive tokens" $
          shouldBe' (collectF90 "integer (KIND=10*2) recursive function x") $
                    fmap ($u) [ TInteger, TLeftPar, TKind, TOpAssign
                              , flip TIntegerLiteral "10" , TStar
                              , flip TIntegerLiteral "2", TRightPar, TRecursive
                              , TFunction, flip TId "x", TEOF ]

        it "lexes interleaved type recursive tokens (reversed)" $
          shouldBe' (collectF90 "recursive integer (KIND=10*2) function x") $
                    fmap ($u) [ TRecursive, TInteger, TLeftPar, TKind, TOpAssign
                              , flip TIntegerLiteral "10" , TStar
                              , flip TIntegerLiteral "2", TRightPar, TFunction
                              , flip TId "x", TEOF ]

      describe "Function" $ do
        it "lexes 'function fx ( a, b, c )'" $
          shouldBe' (collectF90 "function fx ( a, b )") $
                    fmap ($u) [ TFunction, flip TId "fx", TLeftPar, flip TId "a"
                              , TComma, flip TId "b", TRightPar, TEOF ]

        it "lexes functions with specific result" $
          shouldBe' (collectF90 "function fx (array) result (c_sum)") $
                    fmap ($u) [ TFunction, flip TId "fx", TLeftPar
                              , flip TId "array", TRightPar, TResult, TLeftPar
                              , flip TId "c_sum", TRightPar, TEOF ]

        it "lexes recursive functions" $
          shouldBe' (collectF90 "recursive function fx (array)") $
                    fmap ($u) [ TRecursive, TFunction, flip TId "fx", TLeftPar
                              , flip TId "array", TRightPar, TEOF ]

        it "lexes recursive functions with result specified" $
          shouldBe' (collectF90 "RECURSIVE FUNCTION FX (ARRAY) RESULT (C_SUM)") $
                    fmap ($u) [ TRecursive, TFunction, flip TId "fx", TLeftPar
                              , flip TId "array", TRightPar, TResult, TLeftPar
                              , flip TId "c_sum", TRightPar, TEOF ]
