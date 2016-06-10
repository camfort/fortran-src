module Language.Fortran.Analysis.DataFlowSpec where

import Test.Hspec
import TestUtil

import Language.Fortran.Parser.Fortran77
import Language.Fortran.Lexer.FixedForm (initParseState)
import Language.Fortran.ParserMonad (FortranVersion(..), evalParse)
import Language.Fortran.AST
import Language.Fortran.Analysis
import Language.Fortran.Analysis.Renaming
import Language.Fortran.Analysis.BBlocks
import Language.Fortran.Analysis.DataFlow
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Graph.Inductive
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Maybe
import Data.Data

pParser :: String -> ProgramFile (Analysis ())
pParser source = analyseBBlocks . snd . rename . analyseRenames . initAnalysis . resetSrcSpan $ extended77Parser source "<unknown>"

withParse :: Data a => String -> (ProgramFile (Analysis A0) -> a) -> a
withParse source f = underRenaming (f . analyseBBlocks) (extended77Parser source "<unknown>")

testGraph f p = fromJust . M.lookup (Named f) . withParse p $ genBBlockMap
testPfAndGraph f p = fmap (fromJust . M.lookup (Named f)) . withParse p $ \ pf -> (pf, genBBlockMap pf)

testGenDefMap p = genDefMap bm
  where
    pf = analyseBBlocks . initAnalysis . extended77Parser p $ "<unknown>"
    bm = genBlockMap pf

testBackEdges f p = bedges
  where
    gr     = testGraph f p
    domMap = dominators gr
    bedges = genBackEdgeMap domMap gr

spec :: Spec
spec =
  describe "Dataflow" $ do
    describe "loop4" $ do
      it "genBackEdgeMap" $ do
        testBackEdges "loop4" programLoop4 `shouldBe` (IM.fromList [(9, 6), (11, 2)])

      it "loopNodes" $ do
        let pf = pParser programLoop4
        let gr = fromJust . M.lookup (Named "loop4") $ genBBlockMap pf
        let domMap = dominators gr
        let bedges = genBackEdgeMap domMap gr
        S.fromList (loopNodes bedges gr) `shouldBe`
          S.fromList [IS.fromList [6, 9], IS.fromList [2, 5, 6, 7, 9, 11]]

      it "genDefMap" $
        testGenDefMap programLoop4 `shouldBe`
          M.fromList [("i",IS.fromList [4,13]),("j",IS.fromList [7,10]),("r",IS.fromList [2,9])]

      it "reachingDefinitions" $ do
        let pf = pParser programLoop4
        let gr = fromJust . M.lookup (Named "loop4") $ genBBlockMap pf
        let bm = genBlockMap pf
        let dm = genDefMap bm
        IM.lookup 9 (reachingDefinitions dm gr) `shouldBe`
          Just (IS.fromList [2,4,7,9,10,13], IS.fromList [4,9,10,13])

      it "flowsTo" $ do
        let pf = pParser programLoop4
        let gr = fromJust . M.lookup (Named "loop4") $ genBBlockMap pf
        let bm = genBlockMap pf
        let dm = genDefMap bm
        (S.fromList . edges . genFlowsToGraph bm dm gr $ reachingDefinitions dm gr) `shouldBe`
          S.fromList [(2,9),(2,16),(4,5),(4,9),(4,13),(7,8),(7,9),(7,10),(9,9),(9,16),(10,8),(10,9),(10,10),(13,5),(13,9),(13,13)]

    describe "rd3" $ do
      it "genBackEdgeMap" $ do
        testBackEdges "f" programRd3 `shouldBe` (IM.fromList [(3, 2)])

      -- it "loopNodes" $ do
      --   let (pf, gr) = testPfAndGraph "f" programRd3
      --   let domMap = dominators gr
      --   let bedges = genBackEdgeMap domMap gr
      --   S.fromList (loopNodes bedges gr) `shouldBe`
      --     S.fromList [IS.fromList [2, 3]]

      it "genDefMap" $ do
        testGenDefMap programRd3 `shouldBe`
          M.fromList [ ("_f_t#0",IS.fromList [14]),("a",IS.fromList [4]),("b",IS.fromList [3]),("f",IS.fromList [7]),("f[0]",IS.fromList [12]),("f[1]",IS.fromList [13,16]),("i",IS.fromList [6]),("x",IS.fromList [11]) ]

      it "reachingDefinitions" $ do
        let (pf, gr) = testPfAndGraph "f" programRd3
        let bm = genBlockMap pf
        let dm = genDefMap bm
        IM.lookup 3 (reachingDefinitions dm gr) `shouldBe`
          Just (IS.fromList [3,4,6,11], IS.fromList [3,4,6,11])

      it "flowsTo" $ do
        let (pf, gr) = testPfAndGraph "f" programRd3
        let bm = genBlockMap pf
        let dm = genDefMap bm
        (S.fromList . edges . genFlowsToGraph bm dm gr $ reachingDefinitions dm gr) `shouldBe`
          S.fromList [ (3,4),(4,3),(4,7),(6,3),(6,4),(7,12),(11,3),(11,13) ]

programLoop4 = unlines [
      "      program loop4"
    , "      integer r, i, j"
    , ""
    , "      r = 0"
    , ""
    , "c     outer loop"
    , "      i = 1"
    , " 10   if (i .gt. 10) goto 40"
    , ""
    , "c     inner loop"
    , "      j = 1"
    , " 20   if (j .gt. 5) goto 30"
    , "      r = r + i * j"
    , "      j = j + 1"
    , "      goto 20"
    , "c     inner loop end"
    , ""
    , " 30   i = i + 1"
    , "      goto 10"
    , "c     outer loop end"
    , ""
    , " 40   write (*,*) r"
    , "      end"
  ]

programRd3 = unlines [
      "      function f(x)"
    , "      integer i, a, b, x, f"
    , "      dimension a(10), b(10)"
    , ""
    , "      do 10 i = 2, 10"
    , "         b(i) = a(i-1) + x"
    , "         a(i) = b(i)"
    , " 10   continue"
    , "      f = a(10)"
    , "      end"
    , "      program rd3"
    , "      implicit none"
    , "      integer f"
    , ""
    , "      write (*,*) f(1)"
    , "      end"
    , ""
    ]

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl test-suite:spec"
-- End:
