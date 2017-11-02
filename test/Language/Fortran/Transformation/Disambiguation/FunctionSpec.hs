module Language.Fortran.Transformation.Disambiguation.FunctionSpec (spec) where

import Test.Hspec
import TestUtil

import Language.Fortran.Analysis
import Language.Fortran.Analysis.Renaming
import Language.Fortran.Analysis.Types
import Language.Fortran.AST
import Language.Fortran.Transformer
import Language.Fortran.Transformation.TransformMonad

disambiguateFunction :: ProgramFile () -> ProgramFile ()
disambiguateFunction = transform [ DisambiguateIntrinsic, DisambiguateFunction ]

spec :: Spec
spec = do
  describe "Function statement disambiguation" $
    it "disambiguates function statements in example 1" $ do
      let pf = disambiguateFunction $ resetSrcSpan ex1
      pf `shouldBe'` expectedEx1

  describe "Function call disambiguation" $
    it "disambiguates function calls in example 2" $ do
      let pf = disambiguateFunction $ resetSrcSpan ex2
      pf `shouldBe'` expectedEx2

  describe "Function call / Intrinsic disambiguation" $
    it "disambiguates function calls / intrinsics in example 3" $ do
      let pf = disambiguateFunction $ resetSrcSpan ex3
      pf `shouldBe'` expectedEx3

{-
- program Main
- integer a, b(1), c
- dimension a(1)
- a(1) = 1
- b(1) = 1
- c(x) = 1
- d(x) = 1
- end
-}
ex1 = ProgramFile mi77 [ ex1pu1 ]
ex1pu1 = PUMain () u (Just "main") ex1pu1bs Nothing
ex1pu1bs =
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
      (ExpSubscript () u (varGen "c") (AList () u [ IxSingle () u Nothing $ varGen "x" ])) (intGen 1))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpSubscript () u (varGen "d") (AList () u [ IxSingle () u Nothing $ varGen "x" ])) (intGen 1)) ]

expectedEx1 = ProgramFile mi77 [ expectedEx1pu1 ]
expectedEx1pu1 = PUMain () u (Just "main") expectedEx1pu1bs Nothing
expectedEx1pu1bs =
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
  , BlStatement () u Nothing (StFunction () u
      (ExpValue () u $ ValVariable "c") (AList () u [ varGen "x" ]) (intGen 1))
  , BlStatement () u Nothing (StFunction () u
      (ExpValue () u $ ValVariable "d") (AList () u [ varGen "x" ]) (intGen 1)) ]

{-
- program
- integer k(1)
- f(x) = 1
- i = 1 + f(1)
- l = k(1)
- j = y(1,1) + a
- end
-
- function y(i,j)
- end
-}
ex2 = ProgramFile mi77 [ ex2pu1, ex2pu2 ]
ex2pu1 = PUMain () u Nothing ex2pu1bs Nothing
ex2pu2 = PUFunction () u Nothing (None () u False) "y" (Just $ AList () u [ varGen "i", varGen "j" ]) Nothing [ ] Nothing
ex2pu1bs =
  [ BlStatement () u Nothing
      (StFunction () u
        (ExpValue () u (ValVariable "f"))
        (AList () u [ varGen "x" ])
        (intGen 1))
  , BlStatement () u Nothing
      (StExpressionAssign () u (varGen "i")
        (ExpBinary () u Addition
          (intGen 1)
          (ExpSubscript () u
                        (varGen "f")
                        (AList () u [ ixSinGen 1 ])))) ]

expectedEx2 = ProgramFile mi77 [ expectedEx2pu1, ex2pu2 ]
expectedEx2pu1 = PUMain () u Nothing expectedEx2pu1bs Nothing
expectedEx2pu1bs =
  [ BlStatement () u Nothing
      (StFunction () u
        (ExpValue () u (ValVariable "f"))
        (AList () u [ varGen "x" ])
        (intGen 1))
  , BlStatement () u Nothing
      (StExpressionAssign () u (varGen "i")
        (ExpBinary () u Addition
          (intGen 1)
          (ExpFunctionCall () u
            (ExpValue () u $ ValVariable "f")
            (Just $ AList () u [ Argument () u Nothing (intGen 1) ])))) ]


ex3 = ProgramFile mi77 [ ex3pu1, ex3pu2 ]
ex3pu1 = PUMain () u Nothing ex3pu1bs Nothing
ex3pu2 = PUFunction () u Nothing (None () u False) "y" (Just $ AList () u [ varGen "i", varGen "j" ]) Nothing [ ] Nothing
ex3pu1bs =
  [ BlStatement () u Nothing
      (StFunction () u
        (ExpValue () u (ValVariable "f"))
        (AList () u [ varGen "x" ])
        (intGen 1))
  , BlStatement () u Nothing
      (StExpressionAssign () u (varGen "i")
        (ExpSubscript () u (varGen "abs")
          (AList () u [
            (IxSingle () u Nothing (ExpSubscript () u (varGen "f") (AList () u [ ixSinGen 1 ])))]))) ]

expectedEx3 = ProgramFile mi77 [ expectedEx3pu1, ex3pu2 ]
expectedEx3pu1 = PUMain () u Nothing expectedEx3pu1bs Nothing
expectedEx3pu1bs =
  [ BlStatement () u Nothing
      (StFunction () u
        (ExpValue () u (ValVariable "f"))
        (AList () u [ varGen "x" ])
        (intGen 1))
  , BlStatement () u Nothing
      (StExpressionAssign () u (varGen "i")
        (ExpFunctionCall () u (ExpValue () u $ ValIntrinsic "abs")
          (Just $ AList () u [ Argument () u Nothing
            (ExpFunctionCall () u (ExpValue () u $ ValVariable "f")
                                  (Just $ AList () u [ Argument () u Nothing (intGen 1) ])) ]))) ]

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl test-suite:spec"
-- End:
