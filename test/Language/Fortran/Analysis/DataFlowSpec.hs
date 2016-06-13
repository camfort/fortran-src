module Language.Fortran.Analysis.DataFlowSpec where

import Test.Hspec
import TestUtil

import Language.Fortran.Parser.Fortran77
import qualified Language.Fortran.Parser.Fortran90 as F90
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

data F77 = F77
data F90 = F90

class Parser t where
    parser :: t -> String -> String -> ProgramFile A0
instance Parser F77 where
    parser F77 = extended77Parser
instance Parser F90 where
    parser F90 = F90.fortran90Parser

pParser :: Parser t => t -> String -> ProgramFile (Analysis ())
pParser version source = analyseBBlocks . snd . rename . analyseRenames . initAnalysis
                 . resetSrcSpan $ parser version source "<unknown>"

withParse :: Data a => Parser t => t -> String -> (ProgramFile (Analysis A0) -> a) -> a
withParse version source f = underRenaming (f . analyseBBlocks) (parser version source "<unknown>")

testGraph version f p = fromJust . M.lookup (Named f) . withParse version p $ genBBlockMap
testPfAndGraph version f p = fmap (fromJust . M.lookup (Named f)) . withParse version p $ \ pf -> (pf, genBBlockMap pf)

testGenDefMap version p = genDefMap bm
  where
    pf = analyseBBlocks . initAnalysis $ parser version p $ "<unknown>"
    bm = genBlockMap pf

testBackEdges version f p = bedges
  where
    gr     = testGraph version f p
    domMap = dominators gr
    bedges = genBackEdgeMap domMap gr

spec :: Spec
spec =
  describe "Dataflow" $ do
  ----------------------------------------------
    describe "loop4" $ do
      it "genBackEdgeMap" $ do
        testBackEdges F77 "loop4" programLoop4 `shouldBe` (IM.fromList [(9, 6), (11, 2)])

      it "loopNodes" $ do
        let pf = pParser F77 programLoop4
        let gr = fromJust . M.lookup (Named "loop4") $ genBBlockMap pf
        let domMap = dominators gr
        let bedges = genBackEdgeMap domMap gr
        S.fromList (loopNodes bedges gr) `shouldBe`
          S.fromList [IS.fromList [6, 9], IS.fromList [2, 5, 6, 7, 9, 11]]

      it "genDefMap" $
        testGenDefMap F77 programLoop4 `shouldBe`
          M.fromList [("i",IS.fromList [4,13]),("j",IS.fromList [7,10]),("r",IS.fromList [2,9])]

      it "reachingDefinitions" $ do
        let pf = pParser F77 programLoop4
        let gr = fromJust . M.lookup (Named "loop4") $ genBBlockMap pf
        let bm = genBlockMap pf
        let dm = genDefMap bm
        IM.lookup 9 (reachingDefinitions dm gr) `shouldBe`
          Just (IS.fromList [2,4,7,9,10,13], IS.fromList [4,9,10,13])

      it "flowsTo" $ do
        let pf = pParser F77 programLoop4
        let gr = fromJust . M.lookup (Named "loop4") $ genBBlockMap pf
        let bm = genBlockMap pf
        let dm = genDefMap bm
        (S.fromList . edges . genFlowsToGraph bm dm gr $ reachingDefinitions dm gr) `shouldBe`
          S.fromList [(2,9),(2,16),(4,5),(4,9),(4,13),(7,8),(7,9),(7,10),(9,9),(9,16),(10,8),(10,9),(10,10),(13,5),(13,9),(13,13)]

  ----------------------------------------------
    let pf = pParser F90 programLoop4Alt
    let sgr = genSuperBBGr (genBBlockMap pf)
    let gr = superBBGrGraph sgr
    let domMap = dominators gr
    let bedges = genBackEdgeMap domMap gr
    let bm = genBlockMap pf
    let dm = genDefMap bm

    describe "loop4 alt (module)" $ do
      it "genBackEdgeMap" $ do
        testBackEdges F90 "loop4" programLoop4Alt `shouldBe` (IM.fromList [(5, 4), (6, 2)])

      it "loopNodes" $ do
        S.fromList (loopNodes bedges gr) `shouldBe`
          S.fromList [IS.fromList [6, 7], IS.fromList [4, 5, 6, 7, 8]]

      it "genDefMap" $
        testGenDefMap F90 programLoop4Alt `shouldBe`
          M.fromList [("i",IS.fromList [5,13]),("j",IS.fromList [7,9]),("r",IS.fromList [3,8])]

      it "reachingDefinitions" $ do
        IM.lookup 9 (reachingDefinitions dm gr) `shouldBe`
          Just (IS.fromList [3,5,7,8,9,13], IS.fromList [3,5,7,8,9,13])

      it "flowsTo" $ do
        (S.fromList . edges . genFlowsToGraph bm dm gr $ reachingDefinitions dm gr) `shouldBe`
          S.fromList [(3,8),(3,17),(5,8),(5,13),(7,8),(7,9),(8,8),(8,17),(9,8),(9,9),(13,8),(13,13)]

    -----------------------------------------------

    describe "rd3" $ do
      it "genBackEdgeMap" $ do
        testBackEdges F77 "f" programRd3 `shouldBe` (IM.fromList [(4, 2)])

      -- it "loopNodes" $ do
      --   let (pf, gr) = testPfAndGraph "f" programRd3
      --   let domMap = dominators gr
      --   let bedges = genBackEdgeMap domMap gr
      --   S.fromList (loopNodes bedges gr) `shouldBe`
      --     S.fromList [IS.fromList [2, 3]]

      it "genDefMap" $ do
        testGenDefMap F77 programRd3 `shouldBe`
          M.fromList [ ("_f_t#0",IS.fromList [14]),("a",IS.fromList [4]),("b",IS.fromList [3]),("f",IS.fromList [7]),("f[0]",IS.fromList [12]),("f[1]",IS.fromList [13,16]),("i",IS.fromList [6]),("x",IS.fromList [11]) ]

      it "reachingDefinitions" $ do
        let (pf, gr) = testPfAndGraph F77 "f" programRd3
        let bm = genBlockMap pf
        let dm = genDefMap bm
        IM.lookup 3 (reachingDefinitions dm gr) `shouldBe`
          Just (IS.fromList [3,4,6,11], IS.fromList [3,4,6,11])

      it "flowsTo" $ do
        let (pf, gr) = testPfAndGraph F77 "f" programRd3
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

programLoop4Alt = unlines [
      "      module loopMod"
    , "      implicit none"
    , "      contains"
    , "      subroutine loop4()"
    , "      integer r, i, j"
    , ""
    , "      r = 0"
    , ""
    , "!     outer loop"
    , "      i = 1"
    , "      do while (i .gt. 10)"
    , ""
    , "!     inner loop"
    , "      j = 1"
    , "      do while (j .gt. 5)"
    , "      r = r + i * j"
    , "      j = j + 1"
    , "      end do"
    , "!     inner loop end"
    , ""
    , "      i = i + 1"
    , "      end do"
    , "!     outer loop end"
    , ""
    , "      write (*,*) r"
    , "      end subroutine"
    , "      end module"
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
