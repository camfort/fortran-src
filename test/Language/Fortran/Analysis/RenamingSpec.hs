module Language.Fortran.Analysis.RenamingSpec (spec) where

import Test.Hspec
import TestUtil

import Data.Map ((!), elems)
import qualified Data.Map as M
import Data.List

import Language.Fortran.ParserMonad
import Language.Fortran.AST
import Language.Fortran.Util.Position
import qualified Language.Fortran.Parser.Fortran90 as F90
import Language.Fortran.Analysis
import Language.Fortran.Analysis.Renaming hiding (extractNameMap, underRenaming)
import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations
import Data.Data
import qualified Data.ByteString.Char8 as B

import Debug.Trace

testF90 pf = (resetSrcSpan . analyseRenames . initAnalysis) $ pf
extractNameMap' = extractNameMap . analyseRenames . initAnalysis
unrename' = stripAnalysis . unrename . rename . analyseRenames . initAnalysis
renameAndStrip' x = stripAnalysis . rename . analyseRenames . initAnalysis $ x

countUnrenamed e = length [ () | ExpValue (Analysis { uniqueName = Nothing }) _ (ValVariable {}) <- uniE_PF e ]
  where uniE_PF :: ProgramFile (Analysis ()) -> [Expression (Analysis ())]
        uniE_PF = universeBi

fortran90Parser src file = fromParseResultUnsafe $ F90.fortran90Parser (B.pack src) file

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

    it "complete exScope1" $ do
      countUnrenamed (analyseRenames . initAnalysis $ exScope1) `shouldBe` 0
    it "complete exScope2" $ do
      countUnrenamed (analyseRenames . initAnalysis $ exScope2) `shouldBe` 0

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
    it "exScope1 testing non-shadowing of subprogram names" $ do
      let entry = extractNameMap' exScope1
      let keys = M.keys entry
      length keys `shouldBe` 1

    it "exScope2 testing shadowing of variables" $ do
      let entry = extractNameMap' exScope2
      length (filter (=="x") (elems entry)) `shouldBe` 2

  describe "Ordering" $ do
    it "exScope3 testing out-of-order definitions" $ do
      let entry = extractNameMap' exScope3
      length (filter (=="f1") (elems entry)) `shouldBe` 1
      length (filter (=="f2") (elems entry)) `shouldBe` 1
      length (filter (=="s1") (elems entry)) `shouldBe` 1
      length (filter (=="s2") (elems entry)) `shouldBe` 1

--------------------------------------------------

ex1 = ProgramFile mi77 [ ex1pu1 ]
ex1pu1 = PUFunction () u (Just $ TypeSpec () u TypeInteger Nothing) (None () False) "f1" Nothing Nothing [] Nothing

ex2 = ProgramFile mi77 [ ex2pu1 ]
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

ex3 = ProgramFile mi77 [ ex3pu1, ex3pu2 ]
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
ex3pu2 = PUFunction () u (Just $ TypeSpec () u TypeInteger Nothing) (None () False) "f1" (Just $ AList () u [ varGen "d", varGen "b"]) Nothing (ex3pu1bs ++ [ BlStatement () u Nothing (StExpressionAssign () u (varGen "f1") (varGen "d")) ]) Nothing

ex4 = ProgramFile mi77 [ ex4pu1, ex4pu2 ]
ex4pu1 = PUMain () u (Just "main") ex4pu1bs Nothing
ex4pu1bs =
  [ BlStatement () u Nothing (StDeclaration () u (TypeSpec () u TypeInteger Nothing) Nothing (AList () u
      [ DeclVariable () u (varGen "f1") Nothing Nothing
      , DeclVariable () u (varGen "r") Nothing Nothing ]))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpValue () u (ValVariable "r"))
      (ExpFunctionCall () u (ExpValue () u (ValVariable "f1"))
                            (Just $ AList () u [ Argument () u Nothing $ intGen 1 ]))) ]
ex4pu2 = PUFunction () u (Just $ TypeSpec () u TypeInteger Nothing) (None () False) "f1" (Just $ AList () u [ varGen "x"]) Nothing [ BlStatement () u Nothing (StExpressionAssign () u (varGen "f1") (varGen "x")) ] Nothing

ex5 = ProgramFile mi77 [ ex5pu1, ex5pu2 ]
ex5pu1 = PUMain () u (Just "main") ex5pu1bs Nothing
ex5pu1bs = []
ex5pu2 = PUModule () u "ex5mod" ex5pu2bs (Just [ex5pu2pu1])
ex5pu2bs = []
ex5pu2pu1 = PUFunction () u (Just $ TypeSpec () u TypeInteger Nothing) (None () False) "f1" (Just $ AList () u [ varGen "x"]) Nothing [ BlStatement () u Nothing (StExpressionAssign () u (varGen "f1") (varGen "x")) ] Nothing


ex6 = ProgramFile mi77 [ ex6pu1, ex6pu2 ]
ex6pu1 = PUMain () u (Just "main") ex6pu1bs Nothing
ex6pu1bs = []
ex6pu2 = PUModule () u "ex6mod" ex6pu2bs (Just [ex6pu2pu1])
ex6pu2bs = []
ex6pu2pu1 = PUFunction () u (Just $ TypeSpec () u TypeInteger Nothing) (None () False) "f1" (Just $ AList () u [ varGen "x"]) Nothing [ BlStatement () u Nothing (StExpressionAssign () u (varGen "f1") (ExpFunctionCall () u (ExpValue () u (ValVariable "f1")) (Just $ AList () u [Argument () u Nothing (varGen "x")]))) ] (Just [ex5pu2pu1])

parseF90 = resetSrcSpan . flip fortran90Parser "" . unlines

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

ex10 = ProgramFile mi77 [ ex10pu1 ]
ex10pu1 = PUSubroutine () u False "s1" Nothing ex10pu1bs Nothing
ex10pu1bs =
  [ BlStatement () u Nothing (StEntry () u (ExpValue () u (ValVariable "e1")) Nothing Nothing)
  , BlStatement () u Nothing (StEntry () u (ExpValue () u (ValVariable "e2")) Nothing Nothing)
  , BlStatement () u Nothing (StEntry () u (ExpValue () u (ValVariable "e3")) Nothing Nothing) ]

ex11 = ProgramFile mi77 [ ex11pu1 ]
ex11pu1 = PUFunction () u (Just (TypeSpec () u TypeInteger Nothing)) (None () False) "f1" Nothing (Just (varGen "r1")) ex11pu1bs Nothing
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


exScope1 = resetSrcSpan . flip fortran90Parser "" $ unlines [
    "program scope1"
  -- local variables cannot take on the name of subprogram, therefore
  -- this declaration must be simply redeclaring the function x.
  , "  integer :: x"
  , ""
  , "  print *, x()" -- function use prior to definition is OK
  , ""
  , "end program scope1"
  , "integer function x ()"
  , "  x = 1"
  , "end function x"
  , ""
  ]

exScope2 = resetSrcSpan . flip fortran90Parser "" $ unlines [
    "module scope2"
  , "  integer :: x"
  , "contains"
  , "  subroutine s1 ()"
  , "    ! this is variable shadowing"
  , "    integer :: x"
  , "    x = 1"
  , "    print *, x"
  , "    call s2 ()"
  , "    print *, x"
  , "  contains"
  , "    subroutine s2"
  , "      ! reference to outside variable"
  , "      x = 2"
  , "    end subroutine s2"
  , "  end subroutine s1"
  , "end module scope2"
  , ""
  , "program main"
  , "  use scope2"
  , "  x = 0"
  , "  print *, x"
  , "  call s1 ()"
  , "  print *, x"
  , "  ! should print 0 at end"
  , "end program main"
  ]

exScope3 = resetSrcSpan . flip fortran90Parser "" $ unlines [
    "module m1"
  , "  implicit none"
  , "  integer :: x"
  , "contains"
  , ""
  , "  subroutine s1 ()"
  , "    call s2 (f1(x))"
  , "  end subroutine s1"
  , ""
  , "  integer function f1(x)"
  , "    integer :: x, f2"
  , "    f1 = f2(x)"
  , "  end function f1"
  , ""
  , ""
  , "end module m1"
  , ""
  , "program main"
  , "  use m1"
  , "  implicit none"
  , "  call s1()"
  , "  print *, f1(x)"
  , "end program main"
  , ""
  , "subroutine s2 (x)"
  , "  integer :: x, f2"
  , "  x = f2 (x)"
  , "end subroutine s2"
  , ""
  , "function f2(x)"
  , "  integer :: x, f2"
  , "  f2 = x + 1"
  , "end function f2"
  ]

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl test-suite:spec"
-- End:
