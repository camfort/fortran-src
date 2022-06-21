module Language.Fortran.Parser.Fixed.Fortran77.IncludeSpec where

import System.FilePath
import Test.Hspec
import TestUtil

import Language.Fortran.Parser ( f77lIncludes )
import Language.Fortran.AST
import Language.Fortran.Util.Position
import qualified Data.ByteString.Char8 as B

iParser :: [String] -> String -> IO (ProgramFile A0)
iParser incs = f77lIncludes incs mempty "<unknown>" . B.pack

makeSrcR :: (Int, Int, Int, String) -> (Int, Int, Int, String) -> SrcSpan
makeSrcR (i1, i2, i3, s) (j1, j2, j3, s') = SrcSpan (Position i1 i2 i3 s Nothing) (Position j1 j2 j3 s' Nothing)

spec :: SpecWith ()
spec =
  describe "Include Test" $ do
    let source = unlines ["      program bar",
                          "      include 'foo.f'",
                          "      end"
                         ]
        name = "bar"
        puSpan = makeSrcR (6,7,1,"<unknown>") (48,9,3,"<unknown>")
        st1Span = makeSrcR (24,7,2,"<unknown>") (38,21,2,"<unknown>")
        expSpan = makeSrcR (32,15,2,"<unknown>") (38,21,2,"<unknown>")
        pf inc = ProgramFile mi77' [pu]
         where
          -- the expansion returns the span in the included file
          -- it should return the span at the inclusion
          foo = inc </> "foo.f"
          st2Span = makeSrcR (6,7,1, foo) (14,15,1,foo)
          declSpan = makeSrcR (6,7,1,foo) (14,15,1,foo)
          typeSpan = makeSrcR (6,7,1,foo) (12,13,1,foo)
          blockSpan = makeSrcR (14,15,1,foo) (14,15,1,foo)
          varGen' str =  ExpValue () blockSpan $ ValVariable str

          pu = PUMain () puSpan (Just name) blocks Nothing
          blocks = [bl1]
          decl = Declarator () blockSpan (varGen' "a") ScalarDecl Nothing Nothing
          typeSpec = TypeSpec () typeSpan TypeInteger Nothing
          st2 = StDeclaration () st2Span typeSpec Nothing (AList () blockSpan [decl])
          bl1 = BlStatement () st1Span Nothing st1
          st1 = StInclude () st1Span ex (Just [bl2])
          ex = ExpValue () expSpan (ValString "foo.f")
          bl2 = BlStatement () declSpan Nothing st2
    it "includes some files and expands them" $ do
      let inc = "." </> "test-data" </> "f77-include"
      pfParsed <- iParser [inc] source
      pfParsed `shouldBe` pf inc
    it "includes without a newline behave the same" $ do 
      let inc = "." </> "test-data" </> "f77-include" </> "no-newline"
      pfParsed <- iParser [inc] source
      pfParsed `shouldBe` pf inc
