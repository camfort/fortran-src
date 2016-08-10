module Language.Fortran.Analysis.BBlocksSpec where

import Test.Hspec
import TestUtil

import Language.Fortran.Parser.Fortran77
import Language.Fortran.Lexer.FixedForm (initParseState)
import Language.Fortran.ParserMonad (FortranVersion(..), evalParse)
import Language.Fortran.AST
import Language.Fortran.Analysis
import Language.Fortran.Analysis.BBlocks
import Language.Fortran.Analysis.Renaming
import qualified Data.Map as M
import qualified Data.IntSet as IS
import Data.Graph.Inductive
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Maybe
import qualified Data.ByteString.Char8 as B

pParser :: String -> ProgramFile (Analysis ())
pParser source = analyseBBlocks . snd . rename . analyseRenames . initAnalysis $ extended77Parser (B.pack source) "<unknown>"

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
        (IS.size (findSuccsBB gr [10]), IS.size (findSuccsBB gr [20])) `shouldBe` (2, 2)
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
findSuccsBB gr = IS.fromList . concatMap (suc gr) . mapMaybe (flip findLabeledBBlock gr . show)

--------------------------------------------------
-- Test programs

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

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl test-suite:spec"
-- End:
