module Forpar.Analysis.RenamingSpec where

import Test.Hspec

import Data.Map ((!), elems)

import Forpar.AST
import Forpar.Util.Position
import Forpar.Analysis
import Forpar.Analysis.Renaming
import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations
import Data.Data

import Debug.Trace

--------------------------------------------------

u = initSrcSpan
varGen :: String -> Expression ()
varGen str = ExpValue () u $ ValVariable () str
arrGen :: String -> Expression ()
arrGen str = ExpValue () u $ ValArray () str
intGen :: Integer -> Expression ()
intGen i = ExpValue () u $ ValInteger $ show i

--------------------------------------------------

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
      [ 1 | ValVariable (Analysis { uniqueName = Nothing }) _ <- uniV_PF entry ] `shouldSatisfy` null
      [ 1 | ValArray (Analysis { uniqueName = Nothing }) _ <- uniV_PF entry ]    `shouldSatisfy` null

  describe "Identity" $ do
    it "unrename-rename 1" $ do
      let entry = unrename . renameAndStrip . analyseRenames . initAnalysis $ ex1
      entry `shouldBe` ex1

    it "unrename-rename 2" $ do
      let entry = unrename . renameAndStrip . analyseRenames . initAnalysis $ ex2
      entry `shouldBe` ex2

    it "unrename-rename 3" $ do
      let entry = unrename . renameAndStrip . analyseRenames . initAnalysis $ ex3
      entry `shouldBe` ex3

  describe "Shadowing" $ do
    it "shadowing 1" $ do
      let entry = extractNameMap . analyseRenames . initAnalysis $ ex3
      length (filter (=="c") (elems entry)) `shouldBe` 4

--------------------------------------------------

ex1 = ProgramFile [ ([ ], ex1pu1) ] [ ]
ex1pu1 = PUFunction () u (Just $ TypeInteger () u) "f1" (AList () u []) []

ex2 = ProgramFile [ ([ ], ex2pu1)] [ ]
ex2pu1 = PUMain () u (Just "main") ex2pu1bs
ex2pu1bs =
  [ BlStatement () u Nothing (StDeclaration () u (TypeInteger () u) (AList () u
      [ DeclVariable () u (varGen "a")
      , DeclArray () u (arrGen "b") (AList () u [ DimensionDeclarator () u Nothing (intGen 1) ])
      , DeclVariable () u (varGen "c") ]))
  , BlStatement () u Nothing (StDimension () u (AList () u
      [ DeclArray () u (arrGen "a") (AList () u [ DimensionDeclarator () u Nothing (intGen 1 ) ]) ]))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpSubscript () u (arrGen "a") (AList () u [ intGen 1 ])) (intGen 1))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpSubscript () u (arrGen "b") (AList () u [ intGen 1 ])) (intGen 1))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpSubscript () u (arrGen "c") (AList () u [ intGen 1 ])) (intGen 1))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpSubscript () u (arrGen "d") (AList () u [ intGen 1 ])) (intGen 1)) ]

ex3 = ProgramFile [ ([ ], ex3pu1), ([ ], ex3pu2)] [ ]
ex3pu1 = PUMain () u (Just "main") ex3pu1bs
ex3pu1bs =
  [ BlStatement () u Nothing (StDeclaration () u (TypeInteger () u) (AList () u
      [ DeclVariable () u (varGen "a")
      , DeclArray () u (arrGen "b") (AList () u [ DimensionDeclarator () u Nothing (intGen 1) ])
      , DeclVariable () u (varGen "c")
      , DeclVariable () u (varGen "d") ]))
  , BlStatement () u Nothing (StDimension () u (AList () u
      [ DeclArray () u (arrGen "a") (AList () u [ DimensionDeclarator () u Nothing (intGen 1 ) ]) ]))
  , BlStatement () u Nothing (StDeclaration () u (TypeInteger () u) (AList () u
      [ DeclVariable () u (varGen "c") ]))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpSubscript () u (arrGen "a") (AList () u [ intGen 1 ])) (intGen 1))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpSubscript () u (arrGen "b") (AList () u [ intGen 1 ])) (intGen 1))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpSubscript () u (arrGen "c") (AList () u [ intGen 1 ])) (intGen 1))
  , BlStatement () u Nothing (StExpressionAssign () u
      (varGen "d") (ExpBinary () u Addition (varGen "d") (intGen 1))) ]
ex3pu2 = PUFunction () u (Just $ TypeInteger () u) "f1" (AList () u [ValVariable () "d", ValVariable () "b"]) (ex3pu1bs ++ [ BlStatement () u Nothing (StExpressionAssign () u (varGen "f1") (varGen "d")) ])
