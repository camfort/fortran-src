module Language.Fortran.Analysis.RenamingSpec (spec) where

import Test.Hspec
import TestUtil

import Data.Map ((!), elems)
import qualified Data.Map as M
import Data.List

import Language.Fortran.AST
import Language.Fortran.Util.Position
import Language.Fortran.Parser.Fortran90 (fortran90Parser)
import Language.Fortran.Analysis
import Language.Fortran.Analysis.Renaming
import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations
import Data.Data

import Debug.Trace

testF90 pf = (resetSrcSpan . analyseRenames . initAnalysis) $ pf
extractNameMap' = extractNameMap . analyseRenames . initAnalysis
unrename' = unrename . renameAndStrip . analyseRenames . initAnalysis
renameAndStrip' x = renameAndStrip . analyseRenames . initAnalysis $ x

countUnrenamed e = length [ () | ExpValue (Analysis { uniqueName = Nothing }) _ (ValVariable {}) <- uniE_PF e ]
  where uniE_PF :: ProgramFile (Analysis ()) -> [Expression (Analysis ())]
        uniE_PF = universeBi

spec :: Spec
spec = do
  describe "Basic" $ do
    it "num-entries 1" $ do
      let entry = extractNameMap' ex3
      shouldBe ( length (filter (=="f1") (elems entry))
               , length (filter (=="a") (elems entry))
               , length (filter (=="b") (elems entry))
               , length (filter (=="d") (elems entry)) )
               ( 1, 2, 2, 2 )

    -- Test that every symbol that is supposed to be renamed is renamed.
    it "complete ex1" $ do
      countUnrenamed (analyseRenames . initAnalysis $ ex1) `shouldBe` 0
    it "complete ex2" $ do
      countUnrenamed (analyseRenames . initAnalysis $ ex2) `shouldBe` 0
    it "complete ex3" $ do
      countUnrenamed (analyseRenames . initAnalysis $ ex3) `shouldBe` 0
    it "complete ex4" $ do
      countUnrenamed (analyseRenames . initAnalysis $ ex4) `shouldBe` 0
    it "complete ex5" $ do
      countUnrenamed (analyseRenames . initAnalysis $ ex5) `shouldBe` 0
    it "complete ex6" $ do
      countUnrenamed (analyseRenames . initAnalysis $ ex6) `shouldBe` 0
    it "complete ex7" $ do
      countUnrenamed (analyseRenames . initAnalysis $ ex7) `shouldBe` 0
    it "complete ex8" $ do
      countUnrenamed (analyseRenames . initAnalysis $ ex8) `shouldBe` 0
    it "complete ex9" $ do
      countUnrenamed (analyseRenames . initAnalysis $ ex9) `shouldBe` 0
    it "complete ex10" $ do
      countUnrenamed (analyseRenames . initAnalysis $ ex10) `shouldBe` 0
    it "complete ex11" $ do
      countUnrenamed (analyseRenames . initAnalysis $ ex11) `shouldBe` 0
    it "complete ex12" $ do
      countUnrenamed (analyseRenames . initAnalysis $ ex12) `shouldBe` 0

    it "functions 1" $ do
      let entry = extractNameMap' ex3
      length (filter (=="f1") (elems entry)) `shouldBe'` 1

  describe "Identity" $ do
    it "unrename-rename 1" $ do
      let entry = unrename' ex1
      entry `shouldBe'` ex1

    it "unrename-rename 2" $ do
      let entry = unrename' ex2
      entry `shouldBe'` ex2

    it "unrename-rename 3" $ do
      let entry = unrename' ex3
      entry `shouldBe'` ex3

    it "unrename-rename 4" $ do
      let entry = unrename' ex4
      entry `shouldBe'` ex4

  describe "Shadowing" $ do
    it "shadowing ex7" $ do
      let entry = extractNameMap' ex7
      let keys = M.keys entry
      keys `shouldSatisfy` any (isPrefixOf "m1_foo_bar_s_x")
      keys `shouldSatisfy` all (not . isPrefixOf "m1_foo_bar_s_t_x")

    it "shadowing ex9" $ do
      let entry = extractNameMap' ex9
      length (filter (=="x") (elems entry)) `shouldBe` 2

--------------------------------------------------

ex1 = ProgramFile [ ([ ], ex1pu1) ] [ ]
ex1pu1 = PUFunction () u (Just $ TypeSpec () u TypeInteger Nothing) False "f1" Nothing Nothing [] Nothing

ex2 = ProgramFile [ ([ ], ex2pu1)] [ ]
ex2pu1 = PUMain () u (Just "main") ex2pu1bs Nothing
ex2pu1bs =
  [ BlStatement () u Nothing (StDeclaration () u (TypeSpec () u TypeInteger Nothing) Nothing (AList () u
      [ DeclVariable () u (varGen "a") Nothing Nothing
      , DeclArray () u (varGen "b") (AList () u [ DimensionDeclarator () u Nothing (Just $ intGen 1) ]) Nothing Nothing
      , DeclVariable () u (varGen "c") Nothing Nothing
      , DeclVariable () u (varGen "d") Nothing Nothing ]))
  , BlStatement () u Nothing (StDimension () u (AList () u
      [ DeclArray () u (varGen "a") (AList () u [ DimensionDeclarator () u Nothing (Just $ intGen 1 ) ]) Nothing Nothing ]))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpSubscript () u (varGen "a") (AList () u [ ixSinGen 1 ])) (intGen 1))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpSubscript () u (varGen "b") (AList () u [ ixSinGen 1 ])) (intGen 1))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpSubscript () u (varGen "c") (AList () u [ ixSinGen 1 ])) (intGen 1))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpSubscript () u (varGen "d") (AList () u [ ixSinGen 1 ])) (intGen 1)) ]

ex3 = ProgramFile [ ([ ], ex3pu1), ([ ], ex3pu2)] [ ]
ex3pu1 = PUMain () u (Just "main") ex3pu1bs Nothing
ex3pu1bs =
  [ BlStatement () u Nothing (StDeclaration () u (TypeSpec () u TypeInteger Nothing) Nothing (AList () u
      [ DeclVariable () u (varGen "a") Nothing Nothing
      , DeclArray () u (varGen "b") (AList () u [ DimensionDeclarator () u Nothing (Just $ intGen 1) ]) Nothing Nothing
      , DeclVariable () u (varGen "c") Nothing Nothing
      , DeclVariable () u (varGen "d") Nothing Nothing ]))
  , BlStatement () u Nothing (StDimension () u (AList () u
      [ DeclArray () u (varGen "a") (AList () u [ DimensionDeclarator () u Nothing (Just $ intGen 1 ) ]) Nothing Nothing ]))
  , BlStatement () u Nothing (StDeclaration () u (TypeSpec () u TypeInteger Nothing) Nothing (AList () u
      [ DeclVariable () u (varGen "c") Nothing Nothing ]))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpSubscript () u (varGen "a") (AList () u [ ixSinGen 1 ])) (intGen 1))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpSubscript () u (varGen "b") (AList () u [ ixSinGen 1 ])) (intGen 1))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpSubscript () u (varGen "c") (AList () u [ ixSinGen 1 ])) (intGen 1))
  , BlStatement () u Nothing (StExpressionAssign () u
      (varGen "d") (ExpBinary () u Addition (varGen "d") (intGen 1))) ]
ex3pu2 = PUFunction () u (Just $ TypeSpec () u TypeInteger Nothing) False "f1" (Just $ AList () u [ varGen "d", varGen "b"]) Nothing (ex3pu1bs ++ [ BlStatement () u Nothing (StExpressionAssign () u (varGen "f1") (varGen "d")) ]) Nothing

ex4 = ProgramFile [ ([ ], ex4pu1), ([ ], ex4pu2)] [ ]
ex4pu1 = PUMain () u (Just "main") ex4pu1bs Nothing
ex4pu1bs =
  [ BlStatement () u Nothing (StDeclaration () u (TypeSpec () u TypeInteger Nothing) Nothing (AList () u
      [ DeclVariable () u (varGen "f1") Nothing Nothing
      , DeclVariable () u (varGen "r") Nothing Nothing ]))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpValue () u (ValVariable "r"))
      (ExpFunctionCall () u (ExpValue () u (ValVariable "f1"))
                            (Just $ AList () u [ Argument () u Nothing $ intGen 1 ]))) ]
ex4pu2 = PUFunction () u (Just $ TypeSpec () u TypeInteger Nothing) False "f1" (Just $ AList () u [ varGen "x"]) Nothing [ BlStatement () u Nothing (StExpressionAssign () u (varGen "f1") (varGen "x")) ] Nothing

ex5 = ProgramFile [ ([], ex5pu1), ([], ex5pu2) ] []
ex5pu1 = PUMain () u (Just "main") ex5pu1bs Nothing
ex5pu1bs = []
ex5pu2 = PUModule () u "ex5mod" ex5pu2bs (Just [ex5pu2pu1])
ex5pu2bs = []
ex5pu2pu1 = PUFunction () u (Just $ TypeSpec () u TypeInteger Nothing) False "f1" (Just $ AList () u [ varGen "x"]) Nothing [ BlStatement () u Nothing (StExpressionAssign () u (varGen "f1") (varGen "x")) ] Nothing


ex6 = ProgramFile [ ([], ex6pu1), ([], ex6pu2) ] []
ex6pu1 = PUMain () u (Just "main") ex6pu1bs Nothing
ex6pu1bs = []
ex6pu2 = PUModule () u "ex6mod" ex6pu2bs (Just [ex6pu2pu1])
ex6pu2bs = []
ex6pu2pu1 = PUFunction () u (Just $ TypeSpec () u TypeInteger Nothing) False "f1" (Just $ AList () u [ varGen "x"]) Nothing [ BlStatement () u Nothing (StExpressionAssign () u (varGen "f1") (ExpFunctionCall () u (ExpValue () u (ValVariable "f1")) (Just $ AList () u [Argument () u Nothing (varGen "x")]))) ] (Just [ex5pu2pu1])

parseF90 = resetSrcSpan . flip fortran90Parser "" . unlines

ex7 = parseF90 [
    "program main"
  , "  implicit none"
  , "  integer :: x, y, z"
  , "  x = 1"
  , "  z = foo (3)"
  , "  print *, x, y, z"
  , "contains"
  , "  integer function foo (x)"
  , "    integer :: x"
  , "    foo = x"
  , "    y = 2"
  , "  end function foo"
  , "end program main"
  , "module m1"
  , "  implicit none"
  , "  integer :: x, y, z"
  , "contains"
  , "  integer function foo (x)"
  , "    integer :: x"
  , "    foo = x"
  , "    y = bar (3)"
  , "  contains"
  , "    integer function bar (x)"
  , "      integer :: x"
  , "      call s (x)"
  , "      bar = x"
  , "    contains"
  , "      subroutine s (x)"
  , "        integer :: x"
  , "        call t ()"
  , "      contains"
  , "        subroutine t ()"
  , "          x = x + 1"
  , "        end subroutine t"
  , "      end subroutine s"
  , "    end function bar"
  , "  end function foo"
  , "end module m1"

  ]

ex8 = resetSrcSpan . flip fortran90Parser "" $ unlines [
    "module m1"
  , "  implicit none"
  , "contains"
  , "  integer function foo ()"
  , "    foo = 0"
  , "  end function foo"
  , "end module m1"
  , ""
  , "module m2"
  , "  implicit none"
  , "contains"
  , "  integer function foo2 (x)"
  , "    use m1"
  , "    integer :: x"
  , "    foo2 = foo () + x"
  , "  end function foo2"
  , "end module m2"
  , ""
  , "module m3"
  , "  implicit none"
  , "contains"
  , "  integer function foo () result (r)"
  , "    r = 1"
  , "  end function foo"
  , "end module m3"
  , ""
  , "program main"
  , "  use m1"
  , "  integer :: x"
  , "  x = foo ()"
  , "end program main"
  ]

ex9 = resetSrcSpan . flip fortran90Parser "" $ unlines [
    "module m1"
  , "  implicit none"
  , "  integer :: x"
  , "contains"
  , "  integer function f () result (x)"
  , "    integer :: x"
  , "    x = 0"
  , "  end function f"
  , "  subroutine s ()"
  , "    x = 0"
  , "  end subroutine s"
  , "end module m1"
  ]

ex10 = ProgramFile [ ([ ], ex10pu1) ] [ ]
ex10pu1 = PUSubroutine () u False "s1" Nothing ex10pu1bs Nothing
ex10pu1bs =
  [ BlStatement () u Nothing (StEntry () u (ExpValue () u (ValVariable "e1")) Nothing Nothing)
  , BlStatement () u Nothing (StEntry () u (ExpValue () u (ValVariable "e2")) Nothing Nothing)
  , BlStatement () u Nothing (StEntry () u (ExpValue () u (ValVariable "e3")) Nothing Nothing) ]

ex11 = ProgramFile [ ([ ], ex11pu1) ] [ ]
ex11pu1 = PUFunction () u (Just (TypeSpec () u TypeInteger Nothing)) False "f1" Nothing (Just (varGen "r1")) ex11pu1bs Nothing
ex11pu1bs =
  [ BlStatement () u Nothing (StEntry () u (ExpValue () u (ValVariable "e1")) Nothing Nothing)
  , BlStatement () u Nothing (StEntry () u (ExpValue () u (ValVariable "e2")) Nothing Nothing)
  , BlStatement () u Nothing (StEntry () u (ExpValue () u (ValVariable "e3")) Nothing (Just (varGen "r2"))) ]

ex12 = resetSrcSpan . flip fortran90Parser "" $ unlines [
    "module m1"
  , "  implicit none"
  , "  integer :: z"
  , "contains"
  , "  integer function foo ()"
  , "    foo = 0"
  , "  end function foo"
  , "end module m1"
  , ""
  , "module m2"
  , "  implicit none"
  , "contains"
  , "  integer function foo2 (x)"
  , "    use m1"
  , "    integer :: x"
  , "    foo2 = foo () + x"
  , "  end function foo2"
  , "end module m2"
  , ""
  , "module m3"
  , "  implicit none"
  , "contains"
  , "  integer function foo () result (r)"
  , "    r = 1"
  , "  end function foo"
  , "end module m3"
  , ""
  , "program main"
  , "  use m1, only: z"
  , "  use m3, only: foo"
  , "  integer :: x"
  , "  x = foo ()"
  , "end program main"
  ]


-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl test-suite:spec"
-- End:
