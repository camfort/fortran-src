module Language.Fortran.Lexer.FreeFormSpec where

import Test.Hspec
import TestUtil

import Language.Fortran.ParserMonad (FortranVersion(..))
import Language.Fortran.Lexer.FreeForm (collectFreeTokens, Token(..))
import Language.Fortran.Util.Position (SrcSpan)
import qualified Data.ByteString.Char8 as B

collectF90 :: String -> [ Token ]
collectF90 = collectFreeTokens Fortran90 . B.pack

collectF03 :: String -> [ Token ]
collectF03 = collectFreeTokens Fortran2003 . B.pack


pseudoAssign :: (SrcSpan -> Token) -> [Token]
pseudoAssign token = fmap ($u) [ flip TId "i", TOpAssign, token, TEOF ]

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
        it "lexes length and kind selectors" $
          shouldBe' (collectF90 "integer (KIND=1, LEN=1) :: kind, len") $
                    fmap ($u) [ TInteger, TLeftPar, TKind, TOpAssign
                              , flip TIntegerLiteral "1", TComma, TLen
                              , TOpAssign, flip TIntegerLiteral "1", TRightPar
                              , TDoubleColon , flip TId "kind", TComma
                              , flip TId "len", TEOF ]


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

      describe "Attribute" $ do
        it "lexes PARAMETER attribute" $
          shouldBe' (collectF90 "integer, parameter :: x") $
                    fmap ($u) [ TInteger, TComma, TParameter, TDoubleColon
                              , flip TId "x", TEOF ]

        it "lexes INTENT attribute" $
          shouldBe' (collectF90 "integer, intent (inout) :: x") $
                    fmap ($u) [ TInteger, TComma, TIntent, TLeftPar, TInOut
                              , TRightPar, TDoubleColon , flip TId "x", TEOF ]

        it "lexes DIMENSION attribute" $
          shouldBe' (collectF90 "double precision, dimension (3:10) :: x") $
                    fmap ($u) [ TDoublePrecision, TComma, TDimension, TLeftPar
                              , flip TIntegerLiteral "3", TColon
                              , flip TIntegerLiteral "10" , TRightPar
                              , TDoubleColon , flip TId "x", TEOF ]

        it "lexes variable declaration with multiple attributes" $
          shouldBe' (collectF90 "double precision, save, dimension(2), allocatable :: y") $
                    fmap ($u) [ TDoublePrecision, TComma, TSave, TComma
                              , TDimension, TLeftPar, flip TIntegerLiteral "2"
                              , TRightPar, TComma, TAllocatable, TDoubleColon
                              , flip TId "y", TEOF ]

        it "try to trick lexer into parsing variables as attributes (1)" $
          shouldBe' (collectF90 "integer save, dimension(10), target") $
                    fmap ($u) [ TInteger, flip TId "save", TComma
                              , flip TId "dimension", TLeftPar, flip TIntegerLiteral "10", TRightPar, TComma
                              , flip TId "target", TEOF ]

        it "try to trick lexer into parsing variables as attributes (2)" $
          shouldBe' (collectF90 "type(foo) save, dimension(10), target") $
                    fmap ($u) [ TType, TLeftPar, flip TId "foo", TRightPar, flip TId "save", TComma
                              , flip TId "dimension", TLeftPar, flip TIntegerLiteral "10", TRightPar, TComma
                              , flip TId "target", TEOF ]

        it "try to trick lexer into parsing variables as attributes (3)" $
          shouldBe' (collectF90 "allocate(type(foo) :: errmsg(stat, source), source=x)") $
                    fmap ($u) [ TAllocate, TLeftPar, TType, TLeftPar, flip TId "foo", TRightPar, TDoubleColon
                              , flip TId "errmsg", TLeftPar, flip TId "stat", TComma, flip TId "source", TRightPar
                              , TComma, TSource, TOpAssign, flip TId "x", TRightPar, TEOF ]

      describe "Character" $ do
        it "lexes single quote literal" $
          shouldBe' (collectF90 "character c = 'heL\"Lo ''daRLing'") $
                    fmap ($u) [ TCharacter, flip TId "c", TOpAssign
                              , flip TString "heL\"Lo 'daRLing", TEOF ]

        it "lexes double quote literal" $
          shouldBe' (collectF90 "character c = \"heL'Lo \"\"daRLing\"") $
                    fmap ($u) [ TCharacter, flip TId "c", TOpAssign
                              , flip TString "heL'Lo \"daRLing", TEOF ]

      describe "Module" $ do
        it "lexes module statement" $
          shouldBe' (collectF90 "module Hello_mod") $
                    fmap ($u) [ TModule, flip TId "hello_mod", TEOF ]

        it "lexes use statement" $
          shouldBe' (collectF90 "use Hello_mod, hello => hi") $
                    fmap ($u) [ TUse, flip TId "hello_mod", TComma
                              , flip TId "hello", TArrow, flip TId "hi", TEOF ]

        it "lexes use statement with only" $
          shouldBe' (collectF90 "use Hello_mod, only: a, b => c") $
                    fmap ($u) [ TUse, flip TId "hello_mod", TComma, TOnly
                              , TColon, flip TId "a", TComma, flip TId "b"
                              , TArrow, flip TId "c", TEOF ]

      describe "Label" $
        it "lexes simple label" $
          shouldBe' (collectF90 "010 print *, 'hello'") $
                    fmap ($u) [ flip TIntegerLiteral "010", TPrint, TStar, TComma
                              , flip TString "hello", TEOF ]

      describe "Conditional" $ do
        it "lexes logical if with array assignment" $
          shouldBe' (collectF90 "if (.true.) a(1) = 42") $
                    fmap ($u) [ TIf, TLeftPar, flip TLogicalLiteral ".true."
                              , TRightPar, flip TId "a", TLeftPar
                              , flip TIntegerLiteral "1", TRightPar, TOpAssign
                              , flip TIntegerLiteral "42", TEOF ]

        it "lexes block if statement" $
          shouldBe' (collectF90 "if (a > b) then") $
                    fmap ($u) [ TIf, TLeftPar, flip TId "a", TOpGT, flip TId "b"
                              , TRightPar, TThen, TEOF ]

        it "lexes arithmetic if statement" $
          shouldBe' (collectF90 "if (a) 10, 11, 12") $
                    fmap ($u) [ TIf, TLeftPar, flip TId "a", TRightPar
                              , flip TIntegerLiteral "10", TComma
                              , flip TIntegerLiteral "11", TComma
                              , flip TIntegerLiteral "12" , TEOF ]

        it "lexes logical if statement" $
          shouldBe' (collectF90 "if (a > b) print *, 'hello'") $
                    fmap ($u) [ TIf, TLeftPar, flip TId "a", TOpGT, flip TId "b"
                              , TRightPar, TPrint, TStar, TComma
                              , flip TString "hello", TEOF ]

      describe "Lexes numeric values" $ do
        it "lexes integer" $
          shouldBe' (collectF90 "i = 42") $
                    pseudoAssign $ flip TIntegerLiteral "42"

        describe "Real" $ do
          it "lexes real (1)" $
            shouldBe' (collectF90 "i = 10.5e2") $
                      pseudoAssign $ flip TRealLiteral "10.5e2"

          it "lexes real (2)" $
            shouldBe' (collectF90 "i = 10.") $
                      pseudoAssign $ flip TRealLiteral "10."

          it "lexes real (3)" $
            shouldBe' (collectF90 "i = .42") $
                      pseudoAssign $ flip TRealLiteral ".42"

          it "lexes real (3)" $
            shouldBe' (collectF90 "i = 42d-3") $
                      pseudoAssign $ flip TRealLiteral "42d-3"

          it "resolves disambiguity when xxx. follows relational operator" $
            shouldBe' (collectF90 "if (10.EQ. 20)") $
                      fmap ($u) [ TIf, TLeftPar, flip TIntegerLiteral "10"
                                , TOpEQ, flip TIntegerLiteral "20"
                                , TRightPar, TEOF ]

      describe "Continuation" $ do
        it "Single continuation char without space" $
          shouldBe' (collectF90 "i = &\n42") $
                    pseudoAssign $ flip TIntegerLiteral "42"

        it "Single continuation char with space" $
          shouldBe' (collectF90 "i = &   \n \t   42") $
                    pseudoAssign $ flip TIntegerLiteral "42"

        it "Double continuation (1)" $
          shouldBe' (collectF90 "i = &\n  & 42") $
                    pseudoAssign $ flip TIntegerLiteral "42"

        it "Double continuation (2)" $
          shouldBe' (collectF90 "i = 4&\n  &2") $
                    pseudoAssign $ flip TIntegerLiteral "42"

        it "Continuation with comment" $
          shouldBe' (collectF90 "i = 4&\n  ! hello\n  &2") $
                    pseudoAssign $ flip TIntegerLiteral "42"

        it "Continuation with inline comment" $
          shouldBe' (collectF90 "i = &  ! hi \n  42") $
                    pseudoAssign $ flip TIntegerLiteral "42"

      describe "Comment" $ do
        it "Full line comment" $
          shouldBe' (collectF90 "! = & ! hi \n") $
                    ($u) <$> [ flip TComment " = & ! hi ", TNewline , TEOF ]

        it "Inline comment" $
          shouldBe' (collectF90 "i = 10 ! = & ! hi \n") $
                    ($u) <$> [ flip TId "i", TOpAssign
                             , flip TIntegerLiteral "10"
                             , flip TComment " = & ! hi ", TNewline , TEOF ]
        it "Empty comment" $
          shouldBe' (collectF90 "!\n") $
                    ($u) <$> [ flip TComment "", TNewline , TEOF ]

      describe "Fortran95" $ do
        it "lexes value attribute" $ do
          shouldBe' (collectF03 "value :: a, b") $
                    fmap ($u) [ TValue, TDoubleColon, flip TId "a", TComma, flip TId "b", TEOF ]
          shouldBe' (collectF03 "integer, value :: a, b") $
                    fmap ($u) [ TInteger, TComma, TValue, TDoubleColon, flip TId "a", TComma, flip TId "b", TEOF ]

        it "lexes volatile attribute" $ do
          shouldBe' (collectF03 "volatile :: a, b") $
                    fmap ($u) [ TVolatile, TDoubleColon, flip TId "a", TComma, flip TId "b", TEOF ]
          shouldBe' (collectF03 "integer, volatile :: a, b") $
                    fmap ($u) [ TInteger, TComma, TVolatile, TDoubleColon, flip TId "a", TComma, flip TId "b", TEOF ]

      describe "Fortran2003" $ do
        it "lexes procedures" $
          shouldBe' (collectF03 "PROCEDURE(a), SAVE :: b => c()") $
            ($u) <$> [ TProcedure, TLeftPar, flip TId "a", TRightPar
                     , TComma, TSave, TDoubleColon
                     , flip TId "b", TArrow, flip TId "c", TLeftPar, TRightPar, TEOF ]

        it "lexes procedures with bind" $
          shouldBe' (collectF03 "PROCEDURE(a), BIND(C, NAME=\"d\") :: b => c()") $
            ($u) <$> [ TProcedure, TLeftPar, flip TId "a", TRightPar
                     , TComma, TBind, TLeftPar, TC, TComma, TName, TOpAssign, flip TString "d", TRightPar, TDoubleColon
                     , flip TId "b", TArrow, flip TId "c", TLeftPar, TRightPar, TEOF ]

        it "lexes functions with bind" $
          shouldBe' (collectF03 "FUNCTION f(a) RESULT(x) BIND(C, NAME=\"d\")") $
            ($u) <$> [ TFunction, flip TId "f", TLeftPar, flip TId "a", TRightPar
                     , TResult, TLeftPar, flip TId "x", TRightPar
                     , TBind, TLeftPar, TC, TComma, TName, TOpAssign, flip TString "d", TRightPar, TEOF ]

        it "lexes subroutines with bind" $
          shouldBe' (collectF03 "SUBROUTINE s(a) BIND(C, NAME=\"d\")") $
            ($u) <$> [ TSubroutine, flip TId "s", TLeftPar, flip TId "a", TRightPar
                     , TBind, TLeftPar, TC, TComma, TName, TOpAssign, flip TString "d", TRightPar, TEOF ]

        it "lexes class decl (name)" $
          shouldBe' (collectF03 "procedure (class(c))") $
                    fmap ($u) [ TProcedure, TLeftPar
                              , TClass, TLeftPar, flip TId "c", TRightPar, TRightPar, TEOF ]

        it "lexes class decl (*)" $
          shouldBe' (collectF03 "procedure (class(*))") $
                    fmap ($u) [ TProcedure, TLeftPar
                              , TClass, TLeftPar, TStar, TRightPar, TRightPar, TEOF ]

        it "lexes import statements" $
          shouldBe' (collectF03 "import :: a, b") $
                    fmap ($u) [ TImport, TDoubleColon, flip TId "a", TComma, flip TId "b", TEOF ]

        it "lexes asynchronous attribute" $ do
          shouldBe' (collectF03 "asynchronous :: a, b") $
                    fmap ($u) [ TAsynchronous, TDoubleColon, flip TId "a", TComma, flip TId "b", TEOF ]
          shouldBe' (collectF03 "integer, asynchronous :: a, b") $
                    fmap ($u) [ TInteger, TComma, TAsynchronous, TDoubleColon, flip TId "a", TComma, flip TId "b", TEOF ]

        it "lexes enums" $ do
          shouldBe' (collectF03 "enum, bind(c)") $ fmap ($u) [ TEnum, TComma, TBind, TLeftPar, TC, TRightPar, TEOF ]
          shouldBe' (collectF03 "enumerator :: a = 1, b") $
                    fmap ($u) [ TEnumerator, TDoubleColon, flip TId "a", TOpAssign, flip TIntegerLiteral "1"
                              , TComma, flip TId "b", TEOF ]
          shouldBe' (collectF03 "end enum") $ fmap ($u) [ TEndEnum, TEOF ]

        it "lexes flush" $ do
          shouldBe' (collectF03 "flush(unit=1)") $
            fmap ($u) [ TFlush, TLeftPar, TUnit, TOpAssign, flip TIntegerLiteral "1", TRightPar, TEOF ]
          shouldBe' (collectF03 "flush(unit=1,iomsg=x,iostat=y,err=z)") $
            fmap ($u) [ TFlush, TLeftPar, TUnit, TOpAssign, flip TIntegerLiteral "1", TComma
                      , TIOMsg, TOpAssign, flip TId "x", TComma
                      , TIOStat, TOpAssign, flip TId "y", TComma
                      , TErr, TOpAssign, flip TId "z", TRightPar, TEOF ]
