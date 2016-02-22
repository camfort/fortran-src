module Forpar.Transformation.Disambiguation.FunctionSpec (spec) where

import Test.Hspec
import TestUtil

import Forpar.AST
import Forpar.Transformer
import Forpar.Transformation.TransformMonad

disambiguateFunction :: ProgramFile () -> ProgramFile ()
disambiguateFunction = transform [ DisambiguateFunction ]

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
ex1 = ProgramFile [ ([ ], ex1pu1)] [ ]
ex1pu1 = PUMain () u (Just "main") ex1pu1bs
ex1pu1bs =
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
      (ExpSubscript () u (arrGen "c") (AList () u [ varGen "x" ])) (intGen 1))
  , BlStatement () u Nothing (StExpressionAssign () u 
      (ExpSubscript () u (arrGen "d") (AList () u [ varGen "x" ])) (intGen 1)) ]

expectedEx1 = ProgramFile [ ([ ], expectedEx1pu1) ] [ ]
expectedEx1pu1 = PUMain () u (Just "main") expectedEx1pu1bs
expectedEx1pu1bs =
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
  , BlStatement () u Nothing (StFunction () u 
      (ExpValue () u $ ValFunctionName "c") (AList () u [ varGen "x" ]) (intGen 1))
  , BlStatement () u Nothing (StFunction () u 
      (ExpValue () u $ ValFunctionName "d") (AList () u [ varGen "x" ]) (intGen 1)) ]

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
ex2 = ProgramFile [ ([ ], ex2pu1), ([ ], ex2pu2) ] [ ]
ex2pu1 = PUMain () u Nothing ex2pu1bs
ex2pu2 = PUFunction () u Nothing "y" (AList () u [ ValVariable "i", ValVariable "j" ]) [ ]
ex2pu1bs =
    {-
  [ BlStatement () u Nothing
      (StDeclaration () u 
        (TypeInteger () u) 
        (AList () u
          [ DeclArray () u (arrGen "k") 
              (AList () u
                [ DimensionDeclarator () u Nothing (intGen 1) ]) ]))
                -}
  [ BlStatement () u Nothing
      (StFunction () u 
        (ExpValue () u (ValFunctionName "f")) 
        (AList () u [ varGen "x"]) 
        (intGen 1))
  , BlStatement () u Nothing
      (StExpressionAssign () u (varGen "i") 
        (ExpBinary () u Addition 
          (intGen 1) 
          (ExpSubscript () u (arrGen "f") (AList () u [ intGen 1 ])))) ]
          {-
  , BlStatement () u Nothing
      (StExpressionAssign () u (varGen "l") 
        (ExpSubscript () u (arrGen "k") (AList () u [ intGen 1 ])))
  , BlStatement () u Nothing
      (StExpressionAssign () u (varGen "j") 
        (ExpBinary () u Addition 
          (ExpSubscript () u (arrGen "y") (AList () u [ intGen 1, intGen 1 ]))
          (varGen "a"))) ]
          -}

expectedEx2 = ProgramFile [ ([ ], expectedEx2pu1), ([ ], ex2pu2) ] [ ]
expectedEx2pu1 = PUMain () u Nothing expectedEx2pu1bs
expectedEx2pu1bs =
    {-
  [ BlStatement () u Nothing
      (StDeclaration () u 
        (TypeInteger () u) 
        (AList () u
          [ DeclArray () u (arrGen "k") 
              (AList () u
                [ DimensionDeclarator () u Nothing (intGen 1) ]) ]))
                -}
  [ BlStatement () u Nothing
      (StFunction () u 
        (ExpValue () u (ValFunctionName "f")) 
        (AList () u [ varGen "x"]) 
        (intGen 1))
  , BlStatement () u Nothing
      (StExpressionAssign () u (varGen "i") 
        (ExpBinary () u Addition 
          (intGen 1) 
          (ExpFunctionCall () u 
            (ExpValue () u $ ValFunctionName "f") 
            (AList () u [ intGen 1 ])))) ]
            {-
  , BlStatement () u Nothing
      (StExpressionAssign () u (varGen "l") 
        (ExpSubscript () u (arrGen "k") (AList () u [ intGen 1 ])))
  , BlStatement () u Nothing
      (StExpressionAssign () u (varGen "j") 
        (ExpBinary () u Addition 
          (ExpFunctionCall () u 
            (ExpValue () u $ ValFunctionName "y") 
            (AList () u [ intGen 1, intGen 1 ]))
          (varGen "a"))) ]
          -}
