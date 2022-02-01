module Language.Fortran.Analysis.BBlocksSpec where

import Test.Hspec

import qualified Language.Fortran.Parser as Parser
import Language.Fortran.AST
import Language.Fortran.Analysis
import Language.Fortran.Analysis.BBlocks
import Language.Fortran.Analysis.Renaming
import qualified Data.Map as M
import qualified Data.IntSet as IS
import Data.Graph.Inductive
import Data.Maybe
import qualified Data.ByteString.Char8 as B

pParser :: String -> ProgramFile (Analysis ())
pParser source =
    case Parser.f77e "<unknown>" (B.pack source) of
      Left err -> error $ show err
      Right pf -> rename . analyseBBlocks . analyseRenames . initAnalysis $ pf

spec :: Spec
spec =
  describe "Basic Blocks" $ do
    describe "loop4" $ do
      let pf = pParser programLoop4
          gr = fromJust . M.lookup (Named "loop4") $ genBBlockMap pf
          ns = nodes $ bbgrGr gr
          es = edges $ bbgrGr gr
          nodeSet = IS.fromList ns
      it "nodes and edges length" $
        (length ns, length es) `shouldBe` (11, 12)
      it "branching nodes" $
        (IS.size (findSuccsBB gr [10]), IS.size (findSuccsBB gr [20])) `shouldBe` (2, 2)
      it "all reachable" $ do
        let reached = IS.fromList . dfs [0] $ bbgrGr gr
        reached `shouldBe` nodeSet
      it "all terminate" $ do
        let reached = IS.fromList . rdfs [-1] $ bbgrGr gr
        reached `shouldBe` nodeSet
    describe "if arith" $ do
      it "nodes and edges length" $ do
        let pf = pParser programArithIf
        let gr = fromJust . M.lookup (Named "arithif") $ genBBlockMap pf
        let ns = nodes $ bbgrGr gr
        let es = edges $ bbgrGr gr
        (length ns, length es) `shouldBe` (6, 7)
      it "branching nodes" $ do
        let pf = pParser programArithIf
        let gr = fromJust . M.lookup (Named "arithif") $ genBBlockMap pf
        (IS.size (findSuccsBB gr [10]), IS.size (findSuccsBB gr [20]), IS.size (findSuccsBB gr [30])) `shouldBe` (1, 1, 1)
      it "all reachable" $ do
        let pf = pParser programArithIf
        let gr = fromJust . M.lookup (Named "arithif") $ genBBlockMap pf
        let reached = IS.fromList . dfs [0] $ bbgrGr gr
        let nodeSet = IS.fromList . nodes $ bbgrGr gr
        reached `shouldBe` nodeSet
      it "all terminate" $ do
        let pf = pParser programArithIf
        let gr = fromJust . M.lookup (Named "arithif") $ genBBlockMap pf
        let reached = IS.fromList . rdfs [-1] $ bbgrGr gr
        let nodeSet = IS.fromList . nodes $ bbgrGr gr
        reached `shouldBe` nodeSet
    describe "gotos" $ do
      let pf = pParser programGotos
          gr = fromJust . M.lookup (Named "_gotos_1") $ genBBlockMap pf
          ns = nodes $ bbgrGr gr
          es = edges $ bbgrGr gr
          nodeSet = IS.fromList ns
      it "nodes and edges length" $ do
        (length ns, length es) `shouldBe` (10, 12)
      it "branching nodes" $
        (IS.size (findSuccsBB gr [10]), IS.size (findSuccsBB gr [20])) `shouldBe` (3, 1)
      it "all reachable" $ do
        let reached = IS.fromList . dfs [0] $ bbgrGr gr
        reached `shouldBe` nodeSet
      it "all terminate" $ do
        let reached = IS.fromList . rdfs [-1] $ bbgrGr gr
        reached `shouldBe` nodeSet
    describe "READ" $ do
      let pf = pParser programRead
          gr = fromJust . M.lookup (Named "reading_time") $ genBBlockMap pf
          ns = nodes $ bbgrGr gr
          es = edges $ bbgrGr gr
          nodeSet = IS.fromList ns
      it "nodes and edges length" $ do
        (length ns, length es) `shouldBe` (10, 11)
      it "branching nodes" $ do
        let succs l = IS.size $ findSuccsBB gr [l]
        (succs 10, succs 20, succs 40, succs 60) `shouldBe` (3, 1, 1, 1)
      it "all reachable" $ do
        let reached = IS.fromList . dfs [0] $ bbgrGr gr
        reached `shouldBe` nodeSet
      it "all terminate" $ do
        let reached = IS.fromList . rdfs [-1] $ bbgrGr gr
        reached `shouldBe` nodeSet
    describe "Leading zero labels" $ do
      let pf = pParser programZeroLabels
          gr = fromJust . M.lookup (Named "zero_labels") $ genBBlockMap pf
          ns = nodes $ bbgrGr gr
          es = edges $ bbgrGr gr
          nodeSet = IS.fromList ns
      it "nodes and edges length" $ do
        (length ns, length es) `shouldBe` (13, 15)
      it "branching nodes" $ do
        let succs l = IS.size $ findSuccsBB gr [l]
        (succs 10, succs 20, succs 40, succs 60, succs 80) `shouldBe` (4, 1, 1, 1, 1)
      it "all reachable" $ do
        let reached = IS.fromList . dfs [0] $ bbgrGr gr
        reached `shouldBe` nodeSet
      it "all terminate" $ do
        let reached = IS.fromList . rdfs [-1] $ bbgrGr gr
        reached `shouldBe` nodeSet
    describe "nested calls" $ do
      let pf = pParser programNestedCalls
          gr = fromJust . M.lookup (Named "nestedcall") $ genBBlockMap pf
          ns = nodes $ bbgrGr gr
          es = edges $ bbgrGr gr
          nodeSet = IS.fromList ns
      it "nodes and edges length" $ do
        (length ns, length es) `shouldBe` (10, 9)
      -- it "branching nodes" $
      --   (IS.size (findSuccsBB gr [10]), IS.size (findSuccsBB gr [20])) `shouldBe` (3, 1)
      it "all reachable" $ do
        let reached = IS.fromList . dfs [0] $ bbgrGr gr
        reached `shouldBe` nodeSet
      it "all terminate" $ do
        let reached = IS.fromList . rdfs [-1] $ bbgrGr gr
        reached `shouldBe` nodeSet
      it "straight-line program" $ do
        [ length (suc (bbgrGr gr) n) | n <- ns, n /= -1 ] `shouldSatisfy` all (== 1)

--------------------------------------------------
-- Label-finding helper functions to help write tests that are
-- insensitive to minor changes to the AST.

-- For each label in the list, find the corresponding basic block,
-- return as an IntSet.
findLabelsBB :: BBGr a -> [Int] -> IS.IntSet
findLabelsBB gr = IS.fromList . mapMaybe (flip findLabeledBBlock gr . show)

findLabelBB :: BBGr a -> Int -> Node
findLabelBB gr = (error "findLabelBB" `fromMaybe`) . flip findLabeledBBlock gr . show

-- For each label in the list, find the successors of the
-- corresponding basic block, return as an IntSet.
findSuccsBB :: BBGr a -> [Int] -> IS.IntSet
findSuccsBB gr = IS.fromList . concatMap (suc $ bbgrGr gr) . mapMaybe (flip findLabeledBBlock gr . show)

--------------------------------------------------
-- Test programs

programLoop4 :: String
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

programArithIf :: String
programArithIf = unlines [
    "      program arithif"
  , "      integer n"
  , "      n = 0"
  , "      if (n) 10, 20, 30"
  , " 10   write (*,*) 10"
  , " 20   write (*,*) 20"
  , " 30   write (*,*) 30"
  , "      end"]

programGotos :: String
programGotos = unlines [
    "      subroutine gotos(s)"
  , "       integer s"
  , "       character a"
  , "       a = 'H'"
  , " 10    goto (30, 40) s"
  , " 20    goto 999"
  , " 30    continue"
  , "       if (a .eq. 'G') then"
  , "        print *, 'almost there'"
  , "       endif"
  , " 40    continue"
  , "999    print *, 'all done'"
  , "      end" ]

programRead :: String
programRead = unlines [
    "      program reading_time"
  , "       integer i"
  , " 10    read(*, *, END=30, ERR=50) i"
  , " 20    goto 70"
  , " 30    print *, 'end'"
  , " 40    goto 70"
  , " 50    print *, 'err'"
  , " 60    goto 70"
  , " 70    print *, 'done'"
  , "       print *, i"
  , "      end" ]

programZeroLabels :: String
programZeroLabels = unlines [
    "      program zero_labels"
  , "       integer i"
  , "  10   goto (30, 50, 70) i"
  , "  20   goto 999"
  , "  30   print *, '30'"
  , "  40   goto 900"
  , " 050   print *, '050'"
  , "  60   goto 900"
  , " 070   print *, '070'"
  , "  80   goto 0900"
  , " 0900  print *, '0900'"
  , "  999  continue"
  , "      end" ]

programNestedCalls :: String
programNestedCalls = unlines [
    "      program nestedcall"
  , "        call foo(bar(baz(1)))"
  , "      end" ]


-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl test-suite:spec"
-- End:
