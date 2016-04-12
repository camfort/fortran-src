module Forpar.Analysis.DataFlowSpec where

import Test.Hspec
import TestUtil

import Forpar.Parser.Fortran77
import Forpar.Lexer.FixedForm (initParseState)
import Forpar.ParserMonad (FortranVersion(..), evalParse)
import Forpar.AST
import Forpar.Analysis
import Forpar.Analysis.BBlocks
import Forpar.Analysis.DataFlow
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Graph.Inductive
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Maybe

pParser :: String -> ProgramFile (Analysis ())
pParser source = analyseBBlocks . initAnalysis $ extended77Parser source "<unknown>"

spec :: Spec
spec =
  describe "Dataflow" $ do
    describe "loop4" $ do
      it "genBackEdgeMap" $ do
        let pf = pParser programLoop4
        let gr = fromJust . M.lookup (Named "loop4") $ genBBlockMap pf
        let domMap = dominators gr
        let bedges = genBackEdgeMap domMap gr
        (bedges) `shouldBe` (IM.fromList [(9, 6), (11, 2)])

      it "loopNodes" $ do
        let pf = pParser programLoop4
        let gr = fromJust . M.lookup (Named "loop4") $ genBBlockMap pf
        let domMap = dominators gr
        let bedges = genBackEdgeMap domMap gr
        S.fromList (loopNodes bedges gr) `shouldBe`
          S.fromList [IS.fromList [6, 9], IS.fromList [2, 5, 6, 7, 9, 11]]

      it "genDefMap" $ do
        let pf = pParser programLoop4
        let gr = fromJust . M.lookup (Named "loop4") $ genBBlockMap pf
        let bm = genBlockMap pf
        genDefMap bm `shouldBe`
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
        (S.fromList . edges . flowsTo bm dm gr $ reachingDefinitions dm gr) `shouldBe`
          S.fromList [ (2,2),(2,9),(4,4),(4,9),(4,13),(7,7),(7,9),(7,10)
                     , (9,9),(10,9),(10,10),(13,9),(13,13) ]

    describe "rd3" $ do
      it "genBackEdgeMap" $ do
        let pf = pParser programRd3
        let gr = fromJust . M.lookup (Named "f") $ genBBlockMap pf
        let domMap = dominators gr
        let bedges = genBackEdgeMap domMap gr
        (bedges) `shouldBe` (IM.fromList [(3, 2)])

      it "loopNodes" $ do
        let pf = pParser programRd3
        let gr = fromJust . M.lookup (Named "f") $ genBBlockMap pf
        let domMap = dominators gr
        let bedges = genBackEdgeMap domMap gr
        S.fromList (loopNodes bedges gr) `shouldBe`
          S.fromList [IS.fromList [2, 3]]

      it "genDefMap" $ do
        let pf = pParser programRd3
        let gr = fromJust . M.lookup (Named "f") $ genBBlockMap pf
        let bm = genBlockMap pf
        genDefMap bm `shouldBe`
          M.fromList [ ("a",IS.fromList [7,9]),("b",IS.fromList [6,9])
                     , ("f",IS.fromList [10]),("i",IS.fromList [9]) ]

      it "reachingDefinitions" $ do
        let pf = pParser programRd3
        let gr = fromJust . M.lookup (Named "f") $ genBBlockMap pf
        let bm = genBlockMap pf
        let dm = genDefMap bm
        IM.lookup 3 (reachingDefinitions dm gr) `shouldBe`
          Just (IS.fromList [6,7,9], IS.fromList [6,7])

      it "flowsTo" $ do
        let pf = pParser programRd3
        let gr = fromJust . M.lookup (Named "f") $ genBBlockMap pf
        let bm = genBlockMap pf
        let dm = genDefMap bm
        (S.fromList . edges . flowsTo bm dm gr $ reachingDefinitions dm gr) `shouldBe`
          S.fromList [ (6,6),(6,7),(7,6),(7,7),(9,6),(9,7),(9,9) ]

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
      "      program rd3"
    , "      implicit none"
    , "      integer f"
    , ""
    , "      write (*,*) f(1)"
    , "      end"
    , ""
    , "      function f(x)"
    , "      integer i, a, b, x, f"
    , "      dimension a(10), b(10)"
    , ""
    , "      do 10 i = 2, 10"
    , "         b(i) = a(i-1) + x"
    , "         a(i) = b(i)"
    , " 10   continue"
    , "      f = a(10)"
    , "      end" ]
