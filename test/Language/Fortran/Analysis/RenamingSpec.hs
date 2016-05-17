module Language.Fortran.Analysis.RenamingSpec (spec) where

import Test.Hspec
import TestUtil

import Data.Map ((!), elems)

import Language.Fortran.AST
import Language.Fortran.Util.Position
import Language.Fortran.Analysis
import Language.Fortran.Analysis.Renaming
import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations
import Data.Data

import Debug.Trace

spec :: Spec
spec = do
  describe "Basic" $ do
    it "num-entries 1" $ do
      let entry = extractNameMap . analyseRenames . initAnalysis $ ex3
      shouldBe ( length (filter (=="f1") (elems entry))
               , length (filter (=="a") (elems entry))
               , length (filter (=="b") (elems entry))
               , length (filter (=="d") (elems entry)) )
               ( 1, 2, 2, 2 )

    it "complete 1" $ do
      let uniV_PF :: ProgramFile (Analysis ()) -> [Value (Analysis ())]
          uniV_PF = universeBi
      let entry = analyseRenames . initAnalysis $ ex3
      [ n | ValVariable (Analysis { uniqueName = Nothing }) n <- uniV_PF entry ] `shouldSatisfy` null

    it "functions 1" $ do
      let entry = extractNameMap . analyseRenames . initAnalysis $ ex3
      length (filter (=="f1") (elems entry)) `shouldBe'` 1

  describe "Identity" $ do
    it "unrename-rename 1" $ do
      let entry = unrename . renameAndStrip . analyseRenames . initAnalysis $ ex1
      entry `shouldBe'` ex1

    it "unrename-rename 2" $ do
      let entry = unrename . renameAndStrip . analyseRenames . initAnalysis $ ex2
      entry `shouldBe'` ex2

    it "unrename-rename 3" $ do
      let entry = unrename . renameAndStrip . analyseRenames . initAnalysis $ ex3
      entry `shouldBe'` ex3

    it "unrename-rename 4" $ do
      let entry = unrename . renameAndStrip . analyseRenames . initAnalysis $ ex4
      entry `shouldBe'` ex4

  describe "Shadowing" $ do
    it "shadowing 1" $ do
      let entry = extractNameMap . analyseRenames . initAnalysis $ ex3
      length (filter (=="c") (elems entry)) `shouldBe'` 4

--------------------------------------------------

ex1 = ProgramFile [ ([ ], ex1pu1) ] [ ]
ex1pu1 = PUFunction () u (Just $ TypeSpec () u TypeInteger Nothing) False "f1" Nothing Nothing []

ex2 = ProgramFile [ ([ ], ex2pu1)] [ ]
ex2pu1 = PUMain () u (Just "main") ex2pu1bs Nothing
ex2pu1bs =
  [ BlStatement () u Nothing (StDeclaration () u (TypeSpec () u TypeInteger Nothing) Nothing (AList () u
      [ DeclVariable () u (varGen "a") Nothing Nothing
      , DeclArray () u (varGen "b") (AList () u [ DimensionDeclarator () u Nothing (Just $ intGen 1) ]) Nothing Nothing
      , DeclVariable () u (varGen "c") Nothing Nothing ]))
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
ex3pu2 = PUFunction () u (Just $ TypeSpec () u TypeInteger Nothing) False "f1" (Just $ AList () u [ varGen "d", varGen "b"]) Nothing (ex3pu1bs ++ [ BlStatement () u Nothing (StExpressionAssign () u (varGen "f1") (varGen "d")) ])

ex4 = ProgramFile [ ([ ], ex4pu1), ([ ], ex4pu2)] [ ]
ex4pu1 = PUMain () u (Just "main") ex4pu1bs Nothing
ex4pu1bs =
  [ BlStatement () u Nothing (StDeclaration () u (TypeSpec () u TypeInteger Nothing) Nothing (AList () u
      [ DeclVariable () u (varGen "f1") Nothing Nothing
      , DeclVariable () u (varGen "r") Nothing Nothing ]))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpValue () u (ValVariable () "r"))
      (ExpFunctionCall () u (ExpValue () u (ValVariable () "f1"))
                            (AList () u [ Argument () u Nothing $ intGen 1 ]))) ]
ex4pu2 = PUFunction () u (Just $ TypeSpec () u TypeInteger Nothing) False "f1" (Just $ AList () u [ varGen "x"]) Nothing [ BlStatement () u Nothing (StExpressionAssign () u (varGen "f1") (varGen "x")) ]

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl test-suite:spec"
-- End:
