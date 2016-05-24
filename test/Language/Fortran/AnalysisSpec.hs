module Language.Fortran.AnalysisSpec where

import Test.Hspec
import TestUtil

import Language.Fortran.Parser.Fortran77
import Language.Fortran.Lexer.FixedForm (initParseState)
import Language.Fortran.ParserMonad (FortranVersion(..), evalParse)
import Language.Fortran.AST
import Language.Fortran.Analysis
import Data.Graph.Inductive
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List
import Data.Maybe

pParser :: String -> ProgramFile (Analysis ())
pParser source = initAnalysis $ extended77Parser source "<unknown>"

spec :: Spec
spec =
  describe "Analysis" $ do
    describe "anal1" $ do
      it "lhsExprs" $ do
        let pf = stripAnalysis $ pParser programAnal1
        lhsExprs pf `shouldBe'` programAnal1LhsExprs

programAnal1LhsExprs =
  [ ExpSubscript () u (ExpValue () u (ValVariable () "a")) (AList () u [ ixSinGen 1 ])
  , ExpSubscript () u (ExpValue () u (ValVariable () "a"))
      (AList () u [ IxSingle () u Nothing $
                             ExpSubscript () u (varGen "a")
                                          (AList () u [ ixSinGen 2 ])])
  , ExpValue () u (ValVariable () "f")
  , ExpSubscript () u (ExpValue () u (ValVariable () "a")) (AList () u [ ixSinGen 4 ])
  , ExpSubscript () u (ExpValue () u (ValVariable () "a")) (AList () u [ ixSinGen 6 ])
  , ExpSubscript () u (ExpValue () u (ValVariable () "a")) (AList () u [ ixSinGen 5 ]) ]

programAnal1 = unlines $ map (replicate 6 ' '++) [
      "program anal1"
    , "integer a, f"
    , "dimension a(10)"
    , "a(1) = f(a(6))"
    , "a(a(2)) = a(10)"
    , "call s(1)"
    , "call s(a(4))"
    , "call s(f(a(5)))"
    , "end"
    , "subroutine s(x)"
    , "integer x"
    , "end"
    , "function f(x)"
    , "integer x, f"
    , "f = x"
    , "end"
  ]

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl test-suite:spec"
-- End:
