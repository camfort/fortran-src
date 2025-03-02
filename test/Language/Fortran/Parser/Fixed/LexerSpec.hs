module Language.Fortran.Parser.Fixed.LexerSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import TestUtil

import Language.Fortran.Parser.Fixed.Lexer
import Language.Fortran.Parser
import Language.Fortran.Parser.Monad ( ParseState, getAlex, evalParse )
import Language.Fortran.AST.Literal.Boz
import Language.Fortran.Version

import Data.List (isPrefixOf)
import qualified Data.ByteString.Char8 as B

initState :: QualifiedFortranVersion -> B.ByteString -> ParseState AlexInput
initState = initParseStateFixed "<unknown>"

collectFixedTokens :: QualifiedFortranVersion -> B.ByteString -> [Token]
collectFixedTokens qfv bs =
    collectTokens lexer' $ initState qfv bs

collectFixedTokens' :: FortranVersion -> String -> [Token]
collectFixedTokens' v = collectFixedTokens (VanillaVersion v) . B.pack

collectFixedTokensSafe :: QualifiedFortranVersion -> B.ByteString -> Maybe [Token]
collectFixedTokensSafe fv bs =
    collectTokensSafe lexer' $ initState fv bs

lex66 :: String -> Maybe Token
lex66 = collectToLex (VanillaVersion Fortran66)

safeLex66 :: String -> Maybe Token
safeLex66 = collectToLexSafe (VanillaVersion Fortran66)

lex77 :: String -> Maybe Token
lex77 = collectToLex (VanillaVersion Fortran77)

collectToLex :: QualifiedFortranVersion -> String -> Maybe Token
collectToLex version srcInput = dropUntil2 $ collectFixedTokens version (B.pack srcInput)
  where
    dropUntil2 [] = Nothing
    dropUntil2 [_] = Nothing
    dropUntil2 [a,_] = Just a
    dropUntil2 (_:xs) = dropUntil2 xs

collectToLexSafe :: QualifiedFortranVersion -> String -> Maybe Token
collectToLexSafe version srcInput = dropUntil2 $ collectFixedTokensSafe version (B.pack srcInput)
  where
    dropUntil2 (Just [a,_]) = Just a
    dropUntil2 (Just (_:xs)) = dropUntil2 $ Just xs
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
          resetSrcSpan (collectFixedTokens' Fortran77 "      c = 'x' // 'o'") `shouldBe` resetSrcSpan [TId u "c", TOpAssign u, TString u "x", TSlash u, TSlash u, TString u "o", TEOF u]

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
        resetSrcSpan (collectFixedTokens' Fortran66 "      e42 =") `shouldBe` resetSrcSpan [TId u "e42", TOpAssign u, TEOF u]

      it "lexes exponent" $
        resetSrcSpan (collectFixedTokens' Fortran66 "      a = 42 e42") `shouldBe` resetSrcSpan [TId u "a", TOpAssign u, TInt u "42", TExponent u "e42", TEOF u]

      it "lexes 'function foo()'" $
        resetSrcSpan (collectFixedTokens' Fortran66 "      function foo()") `shouldBe` resetSrcSpan [TFunction u, TId u "foo", TLeftPar u, TRightPar u, TEOF u]

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
        resetSrcSpan (collectFixedTokens' Fortran66 "      function end format") `shouldBe` resetSrcSpan [TFunction u, TId u "endfor", TId u "mat", TEOF u]

      it "lexes multiple comments in a line" $
        resetSrcSpan (collectFixedTokens' Fortran66 "csomething\ncsomething else\n\nc\ncc\n") `shouldBe`
          resetSrcSpan [TComment u "something", TNewline u, TComment u "something else", TNewline u, TNewline u, TComment u "", TNewline u, TComment u "c", TNewline u, TEOF u]

      it "lexes example1" $
        resetSrcSpan (collectFixedTokens' Fortran66 example1) `shouldBe` resetSrcSpan example1Expectation

      it "lexes end of file" $
        resetSrcSpan (lex66 "") `shouldBe` Nothing

      it "lexes '3 + 2'" $
        resetSrcSpan (collectFixedTokens' Fortran66 "      a = 3 + 2") `shouldBe` resetSrcSpan [TId u "a", TOpAssign u, TInt u "3", TOpPlus u , TInt u "2", TEOF u]

      it "should lex continuation lines properly" $
        resetSrcSpan (collectFixedTokens' Fortran66 continuationExample) `shouldBe` resetSrcSpan [ TType u "integer", TId u "ix", TNewline u, TId u "ix", TOpAssign u, TInt u "42", TNewline u, TEnd u, TNewline u, TEOF u ]

      it "lexes 'ASSIGN 100 TO FOO'" $
        resetSrcSpan (collectFixedTokens' Fortran66 "      ASSIGN 100 TO FOO") `shouldBe` resetSrcSpan [TAssign u, TInt u "100", TTo u, TId u "foo", TEOF u]

      it "lexes 'DO 100 dovar = 1, 10'" $
        resetSrcSpan (collectFixedTokens' Fortran66 "      DO 100 dovar = 1, 10")
          `shouldBe`
          resetSrcSpan [TDo u, TInt u "100", TId u "dovar", TOpAssign u, TInt u "1", TComma u, TInt u "10", TEOF u]

    describe "lexN" $
      it "`lexN 5` parses lexes next five characters" $
        (lexemeMatch . aiLexeme) (evalParse (lexN 5 >> getAlex) (initState (VanillaVersion Fortran66) (B.pack "helloWorld"))) `shouldBe` reverse "hello"

    describe "lexHollerith" $ do
      it "lexes Hollerith '7hmistral'" $
        resetSrcSpan (lex66 "      x = 7hmistral") `shouldBe` resetSrcSpan (Just $ THollerith u "mistral")

      it "becomes case sensitive" $
        resetSrcSpan (collectFixedTokens' Fortran66 "      format (5h a= 1)") `shouldBe` resetSrcSpan [ TFormat u, TBlob u "(5ha=1)", TEOF u ]

    it "lexes if statement '        IF (IY) 5,6,6'" $
      resetSrcSpan (collectFixedTokens' Fortran66 "      IF (IY) 5,6,6") `shouldBe` resetSrcSpan [TIf u, TLeftPar u, TId u "iy", TRightPar u, TInt u "5", TComma u, TInt u "6", TComma u, TInt u "6", TEOF u]

    it "lexes if then statement '      if (x) then'" $
      resetSrcSpan (collectFixedTokens' Fortran77 "      if (x) then") `shouldBe` resetSrcSpan [TIf u, TLeftPar u, TId u "x", TRightPar u, TThen u, TEOF u]

    it "lexes if variable decl '      INTEGER IF'" $  -- yes, really..
      resetSrcSpan (collectFixedTokens' Fortran77 "      INTEGER IF")
        `shouldBe` resetSrcSpan [TType u "integer", TId u "if", TEOF u]

    describe "Fortran 77 Legacy" $ do
      it "lexes inline comments" $
        resetSrcSpan (collectFixedTokens' Fortran77Legacy "      integer foo ! bar")
          `shouldBe` resetSrcSpan [TType u "integer", TId u "foo", TEOF u]

      it "lexes inline comments as blocks when possible" $
        resetSrcSpan (collectFixedTokens' Fortran77Legacy "\n      ! Block")
          `shouldBe` resetSrcSpan [TNewline u, TComment u " Block", TEOF u]

      it "lexes continuation lines separated by comments" $ do
        let src = unlines [ "      integer foo,"
                          , "C hello"
                          , "     +        bar"
                          ]
          in resetSrcSpan (collectFixedTokens' Fortran77Legacy src)
            `shouldBe` resetSrcSpan [TType u "integer", TId u "foo", TComma u, TId u "bar", TNewline u, TEOF u]
        let src = unlines [ "      integer foo, ! hello"
                          , "     +        bar"
                          ]
          in resetSrcSpan (collectFixedTokens' Fortran77Legacy src)
            `shouldBe` resetSrcSpan [TType u "integer", TId u "foo", TComma u, TId u "bar", TNewline u, TEOF u]
        let src = unlines [ "      integer foo,"
                          , ""
                          , "     +        bar"
                          ]
          in resetSrcSpan (collectFixedTokens' Fortran77Legacy src)
            `shouldBe` resetSrcSpan [TType u "integer", TId u "foo", TComma u, TId u "bar", TNewline u, TEOF u]
        let src = unlines [ "      integer foo,"
                          , "  " -- the space is intentional
                          , "     +        bar"
                          ]
          in resetSrcSpan (collectFixedTokens' Fortran77Legacy src)
            `shouldBe` resetSrcSpan [TType u "integer", TId u "foo", TComma u, TId u "bar", TNewline u, TEOF u]

      it "lexes the older TYPE statement" $
        resetSrcSpan (collectFixedTokens' Fortran77Legacy "      type *, 'hello'")
          `shouldBe` resetSrcSpan [TTypePrint u, TStar u, TComma u, TString u "hello", TEOF u]

      it "lexes width-specific type declarations" $ do
        resetSrcSpan (collectFixedTokens' Fortran77Legacy "      integer*4 i")
          `shouldBe` resetSrcSpan [TType u "integer", TStar u, TInt u "4", TId u "i", TEOF u]

        resetSrcSpan (collectFixedTokens' Fortran77Legacy "      integer*4 function foo()")
          `shouldBe` resetSrcSpan [TType u "integer", TStar u, TInt u "4", TFunction u, TId u "foo", TLeftPar u, TRightPar u, TEOF u]

        resetSrcSpan (collectFixedTokens' Fortran77Legacy "      character*4 s")
          `shouldBe` resetSrcSpan [TType u "character", TStar u, TInt u "4", TId u "s", TEOF u]

        resetSrcSpan (collectFixedTokens' Fortran77Legacy "      character*(*) s")
          `shouldBe` resetSrcSpan [TType u "character", TStar u, TLeftPar u, TStar u, TRightPar u, TId u "s", TEOF u]

        resetSrcSpan (collectFixedTokens' Fortran77Legacy "      character s*(*)")
          `shouldBe` resetSrcSpan [TType u "character", TId u "s", TStar u, TLeftPar u, TStar u, TRightPar u, TEOF u]

      it "lexes strings case-sensitively" $
        resetSrcSpan (collectFixedTokens' Fortran77Legacy "      c = 'Hello'")
          `shouldBe` resetSrcSpan [TId u "c", TOpAssign u, TString u "Hello", TEOF u]

      it "lexes strings delimited by '\"'" $
        resetSrcSpan (collectFixedTokens' Fortran77Legacy "      c = \"hello\"")
          `shouldBe` resetSrcSpan [TId u "c", TOpAssign u, TString u "hello", TEOF u]

      it "lexes Hollerith constants" $ do
        resetSrcSpan (collectFixedTokens' Fortran77Legacy "      x = 7hmistral")
          `shouldBe` resetSrcSpan [TId u "x", TOpAssign u, THollerith u "mistral", TEOF u]

        resetSrcSpan (collectFixedTokens' Fortran77Legacy "      x = 7hshort\n")
          `shouldBe` resetSrcSpan [TId u "x", TOpAssign u, THollerith u "short  ", TNewline u, TEOF u]

      it "lexes BOZ constants" $
        resetSrcSpan (collectFixedTokens' Fortran77Legacy "      integer i, j, k / b'0101', o'0755', z'ab01' /")
          `shouldBe` resetSrcSpan [ TType u "integer"
                                  , TId u "i", TComma u, TId u "j", TComma u, TId u "k"
                                  , TSlash u, TBozLiteral u (parseBoz "b'0101'")
                                  , TComma u, TBozLiteral u (parseBoz "o'0755'")
                                  , TComma u, TBozLiteral u (parseBoz "z'ab01'")
                                  , TSlash u , TEOF u ]

      it "lexes non-standard identifiers" $
        resetSrcSpan (collectFixedTokens' Fortran77Legacy "      integer _this_is_a_long_identifier$")
          `shouldBe` resetSrcSpan [TType u "integer", TId u "_this_is_a_long_identifier$", TEOF u]

      it "lexes ';' as a line-terminator" $
        resetSrcSpan (collectFixedTokens' Fortran77Legacy "      integer i; integer j")
          `shouldBe` resetSrcSpan [TType u "integer", TId u "i", TNewline u, TType u "integer", TId u "j", TEOF u]

      it "does not lex ';' as a line-terminator in first 6 columns" $
        safeLex66 "; integer i; integer j" `shouldBe` Nothing

      it "lexes subscripts in assignments" $
        resetSrcSpan (collectFixedTokens' Fortran77Legacy "      x(0,0) = 0")
          `shouldBe` resetSrcSpan [TId u "x", TLeftPar u, TInt u "0", TComma u, TInt u "0", TRightPar u, TOpAssign u, TInt u "0", TEOF u]

      it "lexes labeled DO WHILE blocks" $
        resetSrcSpan (collectFixedTokens' Fortran77Legacy "      do 10 while (.true.)")
          `shouldBe` resetSrcSpan [TDo u, TInt u "10", TWhile u, TLeftPar u, TBool u True, TRightPar u, TEOF u]

      it "lexes structure/union/map blocks" $ do
        let src = unlines [ "      structure /foo/"
                          , "        union"
                          , "          map"
                          , "            integer i"
                          , "            real r"
                          , "          end map"
                          , "        end union"
                          , "      end structure"]
        resetSrcSpan (collectFixedTokens' Fortran77Legacy src)
          `shouldBe` resetSrcSpan [ TStructure u, TSlash u, TId u "foo", TSlash u, TNewline u
                                  , TUnion u, TNewline u
                                  , TMap u, TNewline u
                                  , TType u "integer", TId u "i", TNewline u
                                  , TType u "real", TId u "r", TNewline u
                                  , TEndMap u, TNewline u
                                  , TEndUnion u, TNewline u
                                  , TEndStructure u, TNewline u
                                  , TEOF u ]

      it "lexes but skips comments after 72" $ do
        let src  = unlines [ "       l = r" <> replicate 65 ' ' <> "! comment after 72"
                           , "       r = l"
                           , replicate 72 ' ' <> "blank line with comment"]
        resetSrcSpan (collectFixedTokens' Fortran77Legacy src) `shouldBe`
          resetSrcSpan [ TId u "l", TOpAssign u, TId u "r", TNewline u
                       , TId u "r", TOpAssign u, TId u "l", TNewline u
                       , TNewline u, TEOF u]
      it "lexes comment overflow" $ do
        let src = unlines
              [ "      l = r" <> replicate 65 ' ' <>  "Comment overflowing 72 limit"
              , "      r = l"
              ]
        resetSrcSpan (collectFixedTokens' Fortran77Legacy src) `shouldBe`
          resetSrcSpan [ TId u "l", TOpAssign u, TId u "r", TNewline u
                       , TId u "r", TOpAssign u, TId u "l", TNewline u, TEOF u]
      it "lexes all comment line even with overflow" $ do
        let src = unlines [ replicate 80 'c'
                          , "      l = r" ]
        resetSrcSpan (collectFixedTokens' Fortran77Legacy src) `shouldBe`
          resetSrcSpan [ TComment u (replicate 79 'c'), TNewline u
                       , TId u "l", TOpAssign u, TId u "r", TNewline u, TEOF u]

example1 :: String
example1 = unlines [
  "      intEGerix",
  "1         iX= 42",
  " 200    ix =IX* ix",
  " 10   wrITe (*,*), ix",
  "        EnD" ]

continuationExample :: String
continuationExample = unlines [
  "      inte",
  "     .ger i",
  "     .x",
  "      ix = 4",
  "     .2",
  "      end"]

example1Expectation :: [Token]
example1Expectation = [
  TType u "integer", TId u "ix", TNewline u,
  TLabel u "1", TId u "ix", TOpAssign u, TInt u "42", TNewline u,
  TLabel u "200", TId u "ix", TOpAssign u, TId u "ix", TStar u, TId u "ix", TNewline u,
  TLabel u "10", TWrite u, TLeftPar u, TStar u, TComma u, TStar u, TRightPar u, TComma u, TId u "ix", TNewline u,
  TEnd u, TNewline u,
  TEOF u]
