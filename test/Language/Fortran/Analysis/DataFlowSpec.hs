{-# LANGUAGE ScopedTypeVariables #-}
module Language.Fortran.Analysis.DataFlowSpec where

import Test.Hspec
import TestUtil

import Language.Fortran.Parser.Fortran77
import qualified Language.Fortran.Parser.Fortran90 as F90
import Language.Fortran.ParserMonad (fromParseResultUnsafe)
import Language.Fortran.AST
import Language.Fortran.Analysis
import Language.Fortran.Analysis.Renaming
import Language.Fortran.Analysis.BBlocks
import Language.Fortran.Analysis.DataFlow
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Graph.Inductive hiding (version, lab')
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Maybe
import Data.List
import Data.Data
import Data.Generics.Uniplate.Operations
import qualified Data.ByteString.Char8 as B

data F77 = F77
data F90 = F90

class Parser t where
    parser :: t -> String -> String -> ProgramFile A0
instance Parser F77 where
    parser F77 src file = fromParseResultUnsafe $ extended77Parser (B.pack src) file
instance Parser F90 where
    parser F90 src file = fromParseResultUnsafe $ F90.fortran90Parser (B.pack src) file

pParser :: Parser t => t -> String -> ProgramFile (Analysis ())
pParser version source = rename . analyseBBlocks . analyseRenames . initAnalysis
                                . resetSrcSpan $ parser version source "<unknown>"

withParse :: Data a => Parser t => t -> String -> (ProgramFile (Analysis A0) -> a) -> a
withParse version source f = underRenaming (f . analyseBBlocks) (parser version source "<unknown>")

testGraph :: Parser t => t -> String -> String -> BBGr (Analysis A0)
testGraph version f p = fromJust . M.lookup (Named f) . withParse version p $ genBBlockMap
testPfAndGraph :: Parser t => t -> String -> String -> (ProgramFile (Analysis A0), BBGr (Analysis A0))
testPfAndGraph version f p = fmap (fromJust . M.lookup (Named f)) . withParse version p $ \ pf -> (pf, genBBlockMap pf)

testGenDefMap :: Parser t => t -> String -> DefMap
testGenDefMap version = flip (withParse version) (genDefMap . genBlockMap . analyseBBlocks . initAnalysis)

testBackEdges :: Parser t => t -> String -> String -> BackEdgeMap
testBackEdges version f p = bedges
  where
    gr     = testGraph version f p
    domMap = dominators gr
    bedges = genBackEdgeMap domMap gr

spec :: Spec
spec =
  describe "Dataflow" $ do
    describe "loop4" $ do
      it "genBackEdgeMap" $ do
        let gr = testGraph F77 "loop4" programLoop4
        testBackEdges F77 "loop4" programLoop4 `shouldBe`
          IM.fromList [(findLabelBB gr 8, findLabelBB gr 10), (findLabelBB gr 7, findLabelBB gr 20)]

      it "loopNodes" $ do
        let pf = pParser F77 programLoop4
        let gr = fromJust . M.lookup (Named "loop4") $ genBBlockMap pf
        let domMap = dominators gr
        let bedges = genBackEdgeMap domMap gr
        S.fromList (loopNodes bedges gr) `shouldBe`
          S.fromList [findLabelsBB gr [5,6,7,20], IS.unions [findLabelsBB gr [4,5,6,7,8,10,20,30], findSuccsBB gr [20]]]

      it "genDefMap" $ do
        let pf = pParser F77 programLoop4
        testGenDefMap F77 programLoop4 `shouldBe`
          M.fromList [("i",findLabelsBl pf [3,30]),("j",findLabelsBl pf [4,6]),("r",findLabelsBl pf [2,5])]

      it "reachingDefinitions" $ do
        let pf = pParser F77 programLoop4
        let gr = fromJust . M.lookup (Named "loop4") $ genBBlockMap pf
        let bm = genBlockMap pf
        let dm = genDefMap bm
        IM.lookup (findLabelBB gr 5) (reachingDefinitions dm gr) `shouldBe`
          Just (findLabelsBl pf [2,3,4,5,6,30], findLabelsBl pf [3,4,5,6,30])

      it "flowsTo" $ do
        let pf = pParser F77 programLoop4
        let gr = fromJust . M.lookup (Named "loop4") $ genBBlockMap pf
        let bm = genBlockMap pf
        let dm = genDefMap bm
        (S.fromList . edges . genFlowsToGraph bm dm gr $ reachingDefinitions dm gr) `shouldBe`
          -- Find the flows of the assignment statements in the program.
          findLabelsBlEdges pf [(2,5),(2,40)            -- r = 0
                               ,(3,5),(3,10),(3,30)     -- i = 1
                               ,(4,5),(4,6),(4,20)      -- j = 1
                               ,(5,5),(5,40)            -- r = r + i * j
                               ,(6,5),(6,6),(6,20)      -- j = j + 1
                               ,(30,5),(30,10),(30,30)  -- i = i + 1
                               ]

  ----------------------------------------------

    describe "loop4 alt (module)" $ do
      it "genBackEdgeMap" $ do
        let gr = testGraph F90 "loop4" programLoop4Alt
        testBackEdges F90 "loop4" programLoop4Alt `shouldBe`
          IM.fromList [(findLabelBB gr 22, findLabelBB gr 20), (findLabelBB gr 31, findLabelBB gr 10)]

      it "loopNodes" $ do
        let pf = pParser F90 programLoop4Alt
            sgr = genSuperBBGr (genBBlockMap pf)
            gr = superBBGrGraph sgr
            domMap = dominators gr
            bedges = genBackEdgeMap domMap gr
        S.fromList (loopNodes bedges gr) `shouldBe`
          S.fromList [findLabelsBB gr [20,21,22], findLabelsBB gr [10,11,20,21,22,31,40]]

      it "genDefMap" $ do
        let pf = pParser F90 programLoop4Alt
        testGenDefMap F90 programLoop4Alt `shouldBe`
          M.fromList [("i",findLabelsBl pf [2,31]),("j",findLabelsBl pf [11,22]),("r",findLabelsBl pf [1,21])]

      it "reachingDefinitions" $ do
        let pf = pParser F90 programLoop4Alt
            sgr = genSuperBBGr (genBBlockMap pf)
            gr = superBBGrGraph sgr
            bm = genBlockMap pf
            dm = genDefMap bm
        IM.lookup (findLabelBB gr 21) (reachingDefinitions dm gr) `shouldBe`
          Just (findLabelsBl pf [1,2,11,21,22,31], findLabelsBl pf [2,11,21,22,31])

      it "flowsTo" $ do
        let pf = pParser F90 programLoop4Alt
            sgr = genSuperBBGr (genBBlockMap pf)
            gr = superBBGrGraph sgr
            bm = genBlockMap pf
            dm = genDefMap bm
        (S.fromList . edges . genFlowsToGraph bm dm gr $ reachingDefinitions dm gr) `shouldBe`
          -- Find the flows of the assignment statements in the program.
          findLabelsBlEdges pf [(1,21),(1,41)           -- r = 0
                               ,(2,10),(2,21),(2,31)    -- i = 1
                               ,(11,20),(11,21),(11,22) -- j = 1
                               ,(21,21),(21,41)         -- r = r + i * j
                               ,(22,20),(22,21),(22,22) -- j = j + 1
                               ,(31,10),(31,21),(31,31) -- i = i + 1
                               ]

    -----------------------------------------------

    describe "rd3" $ do
      it "genBackEdgeMap" $ do
        let gr = testGraph F77 "f" programRd3
        testBackEdges F77 "f" programRd3 `shouldBe` IM.singleton (findLabelBB gr 4) (findLabelBB gr 1)

      it "loopNodes" $ do
        let (_, gr) = testPfAndGraph F77 "f" programRd3
        let domMap = dominators gr
        let bedges = genBackEdgeMap domMap gr
        S.fromList (loopNodes bedges gr) `shouldBe`
          S.fromList [findLabelsBB gr [1,2,3,4]]

      it "reachingDefinitions" $ do
        let (pf, gr) = testPfAndGraph F77 "f" programRd3
        let bm = genBlockMap pf
        let dm = genDefMap bm
        IM.lookup (findLabelBB gr 5) (reachingDefinitions dm gr) `shouldBe`
          Just (IS.unions [findBBlockBl gr 0, findLabelsBl pf [1,2,3]]
               ,IS.unions [findBBlockBl gr 0, findLabelsBl pf [1,2,3,5]])

      it "flowsTo" $ do
        let (pf, gr) = testPfAndGraph F77 "f" programRd3
        let bm = genBlockMap pf
        let dm = genDefMap bm
        (S.fromList . edges . genFlowsToGraph bm dm gr $ reachingDefinitions dm gr) `shouldSatisfy`
          -- Find the flows of the assignment statements in the program.
          S.isSubsetOf (findLabelsBlEdges pf [(1,2),(1,3) -- do 4  i = 2, 10
                                             ,(2,3)       -- b(i) = a(i-1) + x
                                             ,(3,2),(3,5) -- a(i) = b(i)
                                             ])

    describe "rd4" $ do
      it "ivMapByASTBlock" $ do
        let (_, gr) = testPfAndGraph F77 "f" programRd4
        let domMap = dominators gr
        let bedges = genBackEdgeMap domMap gr
        let ivMap  = genInductionVarMapByASTBlock bedges gr
        (sort . map (\ x -> (head x, length x)) . group . sort . map S.size $ IM.elems ivMap) `shouldBe` [(1,3),(2,3)]

    describe "bug36" $ do
      let pf = pParser F90 programBug36
      let sgr = genSuperBBGr (genBBlockMap pf)
      let gr = superBBGrGraph sgr
      let domMap = dominators gr
      let bedges = genBackEdgeMap domMap gr
      it "loopNodes" $ do
        length (loopNodes bedges gr) `shouldBe` 2

    describe "funcflow1" $ do
      let pf = pParser F90 programFuncFlow1
      let sgr = genSuperBBGr (genBBlockMap pf)
      let gr = superBBGrGraph sgr
      let bm = genBlockMap pf
      let dm = genDefMap bm
      let rDefs = reachingDefinitions dm gr
      let flTo = genFlowsToGraph bm dm gr rDefs
      it "flowsTo" $ do
        (S.fromList . edges . trc $ flTo) `shouldSatisfy`
          -- Find the flows of the assignment statements in the program.
          S.isSubsetOf (findLabelsBlEdges pf [(1,2),(1,3),(3,2)])

    describe "funcflow2" $ do
      let pf = pParser F90 programFuncFlow2
      let sgr = genSuperBBGr (genBBlockMap pf)
      let gr = superBBGrGraph sgr
      let bm = genBlockMap pf
      let dm = genDefMap bm
      let rDefs = reachingDefinitions dm gr
      let flTo = genFlowsToGraph bm dm gr rDefs
      let domMap = dominators gr
      let bedges = genBackEdgeMap domMap gr
      let diMap = genDerivedInductionMap bedges gr
      let (iLabel, iName):_ = [ (fromJust (insLabel a), varName e)
                              | e@(ExpValue a _ (ValVariable _)) <- rhsExprs pf, srcName e == "i" ]
      let (jLabel, _):_ = [ (fromJust (insLabel a), varName e)
                              | e@(ExpValue a _ (ValVariable _)) <- lhsExprs pf, srcName e == "j" ]
      it "flowsTo" $ do
        (S.fromList . edges . trc $ flTo) `shouldSatisfy`
          -- Find the flows of the assignment statements in the program.
          S.isSubsetOf (findLabelsBlEdges pf [(1,2),(1,3),(3,2)])
      it "derivedInduction" $ do
        IM.lookup iLabel diMap `shouldBe` Just (IELinear iName 1 0)
        IM.lookup jLabel diMap `shouldBe` Just (IELinear iName 6 2)

    describe "defUse1" $ do
      let pf = pParser F90 programDefUse1
      let sgr = genSuperBBGr (genBBlockMap pf)
      let gr = superBBGrGraph sgr
      let bm = genBlockMap pf
      let dm = genDefMap bm
      let rDefs = reachingDefinitions dm gr
      let flTo = genFlowsToGraph bm dm gr rDefs
      let domMap = dominators gr
      let bedges = genBackEdgeMap domMap gr
      it "backEdges" $ do
        bedges `shouldBe` IM.fromList [(findLabelBB gr 5, findLabelBB gr 4)]
      it "flowsTo" $ do
        (S.fromList . edges $ flTo) `shouldBe`
          -- Find the flows of the assignment statements in the program.
          findLabelsBlEdges pf [(1,2),(1,3),(1,5),(2,3),(3,4),(4,5),(5,5)]

    describe "defUse2" $ do
      let pf = pParser F90 programDefUse2
      let sgr = genSuperBBGr (genBBlockMap pf)
      let gr = superBBGrGraph sgr
      let bm = genBlockMap pf
      let dm = genDefMap bm
      let rDefs = reachingDefinitions dm gr
      let flTo = genFlowsToGraph bm dm gr rDefs
      let domMap = dominators gr
      let bedges = genBackEdgeMap domMap gr
      it "backEdges" $ do
        bedges `shouldBe` IM.fromList [(findLabelBB gr 12, findLabelBB gr 11)]
      it "flowsTo" $ do
        (S.fromList . edges $ flTo) `shouldSatisfy`
          -- Find the flows of the assignment statements in the program.
          S.isSubsetOf (findLabelsBlEdges pf [(1,2),(1,3),(1,4),(2,3),(3,11),(4,4),(11,12),(12,12)])

    describe "other" $ do
      it "dominators on disconnected graph" $ do
        dominators (nmap (const []) (mkUGraph [0,1,3,4,5,6,7,8,9] [(0,3) ,(3,1) ,(5,6) ,(6,7) ,(7,4) ,(7,8) ,(8,7) ,(8,9) ,(9,8)] :: Gr () ())) `shouldBe` IM.fromList [(0,IS.fromList [0]),(1,IS.fromList [0,1,3]),(3,IS.fromList [0,3]),(4,IS.fromList [4,5,6,7]),(5,IS.fromList [5]),(6,IS.fromList [5,6]),(7,IS.fromList [5,6,7]),(8,IS.fromList [5,6,7,8]),(9,IS.fromList [5,6,7,8,9])]

--------------------------------------------------
-- Label-finding helper functions to help write tests that are
-- insensitive to minor changes to the AST.

-- For each Fortran label in the list, find the corresponding basic
-- block, return as an IntSet.
findLabelsBB :: BBGr a -> [Int] -> IS.IntSet
findLabelsBB gr = IS.fromList . mapMaybe (flip findLabeledBBlock gr . show)

findLabelBB :: BBGr a -> Int -> Node
findLabelBB gr = (error "findLabelBB" `fromMaybe`) . flip findLabeledBBlock gr . show

-- For each Fortran label in the list, find the successors of the
-- corresponding basic block, return as an IntSet.
findSuccsBB :: BBGr a -> [Int] -> IS.IntSet
findSuccsBB gr = IS.fromList . concatMap (suc gr) . mapMaybe (flip findLabeledBBlock gr . show)

-- For each Fortran label in the list, find the AST-block label numbers ('insLabel') associated
findLabelsBl :: forall a. Data a => ProgramFile (Analysis a) -> [Int] -> IS.IntSet
findLabelsBl pf labs = IS.fromList [ i | b <- universeBi pf :: [Block (Analysis a)]
                                       , ExpValue _ _ (ValInteger lab') <- maybeToList (getLabel b)
                                       , lab' `elem` labsS
                                       , let a = getAnnotation b
                                       , i <- maybeToList (insLabel a) ]
  where labsS = map show labs

-- Translate a list of edges given as Fortran labels into a set of
-- edges given as AST-block label numbers.
findLabelsBlEdges :: Data a => ProgramFile (Analysis a) -> [(Int, Int)] -> S.Set (Int, Int)
findLabelsBlEdges pf = S.fromList . map convEdge
  where
    convEdge (a, b)
      | a':_ <- IS.toList (findLabelsBl pf [a]) -- FIXME: inefficient
      , b':_ <- IS.toList (findLabelsBl pf [b]) = (a', b')
      | otherwise = error $ "findLabelsBlEdges (" ++ show a ++ "," ++ show b ++ ")"

-- Get the set of AST-block labels found in a given basic block
findBBlockBl :: BBGr (Analysis a) -> Int -> IS.IntSet
findBBlockBl gr = IS.fromList . mapMaybe (insLabel . getAnnotation) . concat . maybeToList . lab gr

--------------------------------------------------
-- Test programs

programLoop4 :: String
programLoop4 = unlines [
      "      program loop4"
    , " 1    integer r, i, j"
    , ""
    , " 2    r = 0"
    , ""
    , " 3    i = 1"
    , " 10   if (i .gt. 10) goto 40"
    , ""
    , " 4    j = 1"
    , " 20   if (j .gt. 5) goto 30"
    , " 5    r = r + i * j"
    , " 6    j = j + 1"
    , " 7    goto 20"
    , ""
    , " 30   i = i + 1"
    , " 8    goto 10"
    , ""
    , " 40   write (*,*) r"
    , "      end"
  ]

programLoop4Alt :: String
programLoop4Alt = unlines [
      "      module loopMod"
    , "      implicit none"
    , "      contains"
    , "      subroutine loop4()"
    , "      integer r, i, j"
    , ""
    , " 1    r = 0"
    , ""
--    , "!     outer loop"
    , " 2    i = 1"
    , " 10   do while (i .gt. 10)"
    , ""
--    , "!     inner loop"
    , " 11   j = 1"
    , " 20   do while (j .gt. 5)"
    , " 21   r = r + i * j"
    , " 22   j = j + 1"
    , "      end do"
--    , "!     inner loop end"
    , ""
    , " 31   i = i + 1"
    , "      end do"
--    , "!     outer loop end"
    , ""
    , " 41   write (*,*) r"
    , "      end subroutine"
    , "      end module"
  ]

programRd3 :: String
programRd3 = unlines [
      "      function f(x)"
    , "      integer i, a, b, x, f"
    , "      dimension a(10), b(10)"
    , ""
    , " 1    do 4  i = 2, 10"
    , " 2       b(i) = a(i-1) + x"
    , " 3       a(i) = b(i)"
    , " 4    continue"
    , " 5    f = a(10)"
    , "      end"
    , "      program rd3"
    , "      implicit none"
    , "      integer f"
    , ""
    , "      write (*,*) f(1)"
    , "      end"
    , ""
    ]

programRd4 :: String
programRd4 = unlines [
      "      function f(x)"
    , "      integer i, j, a, b, x, f"
    , "      dimension a(10), b(10)"
    , ""
    , "      do 10 i = 2, 10"
    , "      do 20 j = 2, 10"
    , "         b(i) = a(i-1) + x"
    , " 20   continue"
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

-- do not use line numbers
programBug36 :: String
programBug36 = unlines [
      "program foo"
    , "  implicit none"
    , "  integer :: i, j"
    , "  real, dimension(100) :: a, b"
    , "  do i=1,100"
    , "     do j=1,100"
    , "      a(i) = b(i) + b(1)"
    , "     end do"
    , "  end do"
    , "end program"
    ]

programFuncFlow1 :: String
programFuncFlow1 = unlines [
      "      program main"
    , "        integer :: i, j"
    , " 1      i = 1"
    , " 2      j = f(i)"
    , "      contains"
    , "        integer function f(k)"
    , "          integer :: k"
    , " 3        f = k + 1"
    , "        end function f"
    , "      end program main"
    ]

programFuncFlow2 :: String
programFuncFlow2 = unlines [
      "      program main"
    , "        integer :: i, j"
    , " 1      do i = 1, 10"
    , " 2         j = 2*f(3*i)"
    , "        end do"
    , "      contains"
    , "        integer function f(k)"
    , "          integer :: k"
    , " 3        f = k + 1"
    , "        end function f"
    , "      end program main"
    ]

programDefUse1 :: String
programDefUse1 = unlines [
      "program defUse1"
    , "1 integer :: x = 1"
    , "2 integer :: y = x + 1"
    , "3 integer :: z = x * y"
    , "4 do y=1,z"
    , "5  x = x + y"
    , "6 end do"
    , "end program defUse1"
    ]

programDefUse2 :: String
programDefUse2 = unlines [
      "program defUse2"
    , "1 integer :: x = 1"
    , "2 integer :: y = x + 1"
    , "3 integer :: z = x * y"
    , "4 call s(x)"
    , "contains"
    , "  subroutine s(a)"
    , "10  integer :: a"
    , "11  do y=1,z"
    , "12     a = a + y"
    , "13  end do"
    , "end subroutine s"
    , "end program defUse2"
    ]

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl test-suite:spec"
-- End:
