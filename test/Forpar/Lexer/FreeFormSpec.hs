module Forpar.Lexer.FreeFormSpec where

import Test.Hspec
import TestUtil

import Data.Maybe (fromJust)

import Forpar.ParserMonad (FortranVersion(..))
import Forpar.Lexer.FreeForm (collectFreeTokens, Token(..))

import Debug.Trace

collectF90Tokens :: String -> [ Token ]
collectF90Tokens = traceShowId . fromJust . collectFreeTokens Fortran90

spec :: Spec
spec =
  describe "Fortran Free Form Lexer" $
    describe "Fortran 90" $ do
      describe "Character sensitivity" $ do
        it "lexes lower case tokens" $
          collectF90Tokens "integer id" `shouldBe'` fmap ($u) [ TInteger, flip TId "id", TEOF ]

        it "lexes mixed case tokens" $
          collectF90Tokens "InTEgeR ID" `shouldBe'` fmap ($u) [ TInteger, flip TId "id", TEOF ]

      describe "Identifier" $ do
        it "lexes long ID names" $
          collectF90Tokens "program long_id_name" `shouldBe'` fmap ($u) [ TProgram, flip TId "long_id_name", TEOF ]

        it "treats 'if' as ID if used in assignment" $
          collectF90Tokens "if = 20" `shouldBe'` fmap ($u) [ flip TId "if", TOpAssign , flip TIntegerLiteral "20", TEOF ]

        it "'result' is an identifier in spec. context" $
          collectF90Tokens "integer :: result" `shouldBe'` fmap ($u) [ TInteger, TDoubleColon , flip TId "result", TEOF ]

      describe "Types" $ do
        it "lexes simple type tokens" $
          collectF90Tokens "character x" `shouldBe'` fmap ($u) [ TCharacter, flip TId "x", TEOF ]

        it "lexes simple type tokens in function" $
          collectF90Tokens "character function x" `shouldBe'` fmap ($u) [ TCharacter, TFunction, flip TId "x", TEOF ]

        it "lexes derived type tokens in function" $
          collectF90Tokens "type (x) function x" `shouldBe'` fmap ($u) [ TType, TLeftPar, flip TId "x", TRightPar, TFunction, flip TId "x", TEOF ]

        it "lexes interleaved type recursive tokens" $
          collectF90Tokens "integer (KIND=10*2) recursive function x" `shouldBe'` fmap ($u) [ TInteger, TLeftPar, TKind, TOpAssign, flip TIntegerLiteral "10" , TStar, flip TIntegerLiteral "2", TRightPar, TRecursive, TFunction, flip TId "x", TEOF ]

        it "lexes interleaved type recursive tokens (reversed)" $
          collectF90Tokens "recursive integer (KIND=10*2) function x" `shouldBe'` fmap ($u) [ TRecursive, TInteger, TLeftPar, TKind, TOpAssign, flip TIntegerLiteral "10" , TStar, flip TIntegerLiteral "2", TRightPar, TFunction, flip TId "x", TEOF ]

      describe "Function" $ do
        it "lexes 'function fx ( a, b, c )'" $
          collectF90Tokens "function fx ( a, b )" `shouldBe'` fmap ($u) [ TFunction, flip TId "fx", TLeftPar, flip TId "a", TComma, flip TId "b", TRightPar, TEOF ]

        it "lexes functions with specific result" $
          collectF90Tokens "function fx (array) result (c_sum)" `shouldBe'` fmap ($u) [ TFunction, flip TId "fx", TLeftPar, flip TId "array", TRightPar, TResult, TLeftPar, flip TId "c_sum", TRightPar, TEOF ]

        it "lexes recursive functions" $
          collectF90Tokens "recursive function fx (array)" `shouldBe'` fmap ($u) [ TRecursive, TFunction, flip TId "fx", TLeftPar, flip TId "array", TRightPar, TEOF ]

        it "lexes recursive functions with result specified" $
          collectF90Tokens "RECURSIVE FUNCTION FX (ARRAY) RESULT (C_SUM)" `shouldBe'` fmap ($u) [ TRecursive, TFunction, flip TId "fx", TLeftPar, flip TId "array", TRightPar, TResult, TLeftPar, flip TId "c_sum", TRightPar, TEOF ]
