module Forpar.Analysis.BBlocksSpec where

import Test.Hspec
import TestUtil

import Forpar.Parser.Fortran77
import Forpar.Lexer.FixedForm (initParseState)
import Forpar.ParserMonad (FortranVersion(..), evalParse)
import Forpar.AST
import Forpar.Analysis
import Forpar.Analysis.BBlocks
import qualified Data.Map as M
import qualified Data.IntSet as IS
import Data.Graph.Inductive
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Maybe

pParser :: String -> ProgramFile (Analysis ())
pParser source = analyseBBlocks . initAnalysis $ extended77Parser source "<unknown>"

spec :: Spec
spec =
  describe "Basic Blocks" $ do
    describe "loop4" $ do
      it "nodes and edges length" $ do
        let pf = pParser programLoop4
        let gr = fromJust . M.lookup (Named "loop4") $ genBBlockMap pf
        let ns = nodes gr
        let es = edges gr
        (length ns, length es) `shouldBe` (11, 12)
      it "branching nodes" $ do
        let pf = pParser programLoop4
        let gr = fromJust . M.lookup (Named "loop4") $ genBBlockMap pf
        (suc gr 2, suc gr 6) `shouldBe` ([3, 5], [7, 9])
      it "all reachable" $ do
        let pf = pParser programLoop4
        let gr = fromJust . M.lookup (Named "loop4") $ genBBlockMap pf
        let reached = IS.fromList $ dfs [0] gr
        let nodeSet = IS.fromList $ nodes gr
        reached `shouldBe` nodeSet
      it "all terminate" $ do
        let pf = pParser programLoop4
        let gr = fromJust . M.lookup (Named "loop4") $ genBBlockMap pf
        let reached = IS.fromList $ rdfs [-1] gr
        let nodeSet = IS.fromList $ nodes gr
        reached `shouldBe` nodeSet

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
