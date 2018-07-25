module Language.Fortran.Parser.IncludeSpec where

import Test.Hspec
import TestUtil

import Language.Fortran.Parser.Fortran77
import qualified Data.ByteString.Char8 as B
import Language.Fortran.ParserMonad
import Language.Fortran.Lexer.FixedForm
import Language.Fortran.AST
import Language.Fortran.Util.Position

iParser :: [String] -> String -> IO (ParseResult AlexInput Token (ProgramFile A0))
iParser incs src = legacy77ParserWithIncludes incs (B.pack src) "<unknown>"

spec :: SpecWith ()
spec =
  describe "Include Test" $ do
    let source = unlines ["      program bar",
                          "      include 'foo.f'",
                          "      end"
                         ]
        incs = ["./test/Language/Fortran/Parser"]
        name = "bar"
        pf = ProgramFile mi77 [pu]
        puSpan = SrcSpan (Position 6 7 1) (Position 48 9 3)
        st1Span = SrcSpan (Position 24 7 2) (Position 38 21 2)
        expSpan = SrcSpan (Position 32 15 2) (Position 38 21 2)

        -- the expansion returns the span in the included file
        -- it should return the span at the inclusion
        st2Span = SrcSpan (Position 6 7 1) (Position 14 15 1)
        declSpan = SrcSpan (Position 6 7 1) (Position 14 15 1)
        typeSpan = SrcSpan (Position 6 7 1) (Position 12 13 1)
        blockSpan = SrcSpan (Position 14 15 1) (Position 14 15 1)
        varGen' str =  ExpValue () blockSpan $ ValVariable str

        pu = PUMain () puSpan (Just name) blocks Nothing
        blocks = [bl1]
        decl = DeclVariable () blockSpan (varGen' "a") Nothing Nothing
        typeSpec = TypeSpec () typeSpan TypeInteger Nothing
        st2 = StDeclaration () st2Span typeSpec Nothing (AList () blockSpan [decl])
        bl1 = BlStatement () st1Span Nothing st1
        st1 = StInclude () st1Span ex (Just [bl2])
        ex = ExpValue () expSpan (ValString "foo.f")
        bl2 = BlStatement () declSpan Nothing st2
    it "includes some files and expands them" $ do
      ps <- iParser incs source
      let pr = fromParseResultUnsafe ps
      pr `shouldBe` pf
