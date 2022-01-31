module Language.Fortran.Transformation.Disambiguation.FunctionSpec (spec) where

import Test.Hspec
import TestUtil

import Language.Fortran.AST
import Language.Fortran.Transformation.Monad
import Language.Fortran.Transformation.Disambiguation.Function
import Language.Fortran.Transformation.Disambiguation.Intrinsic
import Data.Data

disambiguateFunction' :: Data a => ProgramFile a -> ProgramFile a
disambiguateFunction' = transformWith $ sequence_ [ disambiguateIntrinsic
                                                  , disambiguateFunction ]

transformWith :: Data a => Transform a () -> ProgramFile a -> ProgramFile a
transformWith = runTransform mempty mempty

spec :: Spec
spec = do
  describe "Function statement disambiguation" $
    it "disambiguates function statements in example 1" $ do
      let pf = disambiguateFunction' $ resetSrcSpan ex1
      pf `shouldBe'` expectedEx1

  describe "Function call disambiguation" $
    it "disambiguates function calls in example 2" $ do
      let pf = disambiguateFunction' $ resetSrcSpan ex2
      pf `shouldBe'` expectedEx2

  describe "Function call / Intrinsic disambiguation" $
    it "disambiguates function calls / intrinsics in example 3" $ do
      let pf = disambiguateFunction' $ resetSrcSpan ex3
      pf `shouldBe'` expectedEx3

  describe "Function call / Variable disambiguation" $
    it "disambiguates function calls in example 4" $ do
      let pf = disambiguateFunction' $ resetSrcSpan ex4
      pf `shouldBe'` expectedEx4

  describe "Implicit Function call / Variable disambiguation" $
    it "disambiguates function calls in example 5" $ do
      let pf = disambiguateFunction' $ resetSrcSpan ex5
      pf `shouldBe'` expectedEx5

  describe "Implicit array declaration with dimension disambiguation" $
    it "Should not disambiguation to a function call in example 6" $ do
      let pf = disambiguateFunction' $ resetSrcSpan ex6
      pf `shouldBe'` expectedEx6

{-
- program Main
- integer a, b(1), c, e
- dimension a(1)
- a(1) = 1
- b(1) = 1
- c(x) = 1
- d(x) = 1
- e() = 1
- end
-}
ex1 :: ProgramFile ()
ex1 = ProgramFile mi77 [ ex1pu1 ]
ex1pu1 :: ProgramUnit ()
ex1pu1 = PUMain () u (Just "main") ex1pu1bs Nothing
ex1pu1bs :: [Block ()]
ex1pu1bs =
  [ BlStatement () u Nothing (StDeclaration () u (TypeSpec () u TypeInteger Nothing) Nothing (AList () u
      [ declVarGen "a"
      , Declarator () u (varGen "b") (ArrayDecl (AList () u [ DimensionDeclarator () u Nothing (Just $ intGen 1) ])) Nothing Nothing
      , declVarGen "c" ]))
  , BlStatement () u Nothing (StDimension () u (AList () u
      [ Declarator () u (varGen "a") (ArrayDecl (AList () u [ DimensionDeclarator () u Nothing (Just $ intGen 1) ])) Nothing Nothing ]))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpSubscript () u (varGen "a") (AList () u [ ixSinGen 1 ])) (intGen 1))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpSubscript () u (varGen "b") (AList () u [ ixSinGen 1 ])) (intGen 1))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpSubscript () u (varGen "c") (AList () u [ IxSingle () u Nothing $ varGen "x" ])) (intGen 1))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpSubscript () u (varGen "d") (AList () u [ IxSingle () u Nothing $ varGen "x" ])) (intGen 1))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpFunctionCall () u (varGen "e") Nothing) (intGen 1)) ]

expectedEx1 :: ProgramFile ()
expectedEx1 = ProgramFile mi77 [ expectedEx1pu1 ]
expectedEx1pu1 :: ProgramUnit ()
expectedEx1pu1 = PUMain () u (Just "main") expectedEx1pu1bs Nothing
expectedEx1pu1bs :: [Block ()]
expectedEx1pu1bs =
  [ BlStatement () u Nothing (StDeclaration () u (TypeSpec () u TypeInteger Nothing) Nothing (AList () u
      [ declVarGen "a"
      , Declarator () u (varGen "b") (ArrayDecl (AList () u [ DimensionDeclarator () u Nothing (Just $ intGen 1) ])) Nothing Nothing
      , declVarGen "c" ]))
  , BlStatement () u Nothing (StDimension () u (AList () u
      [ Declarator () u (varGen "a") (ArrayDecl (AList () u [ DimensionDeclarator () u Nothing (Just $ intGen 1) ])) Nothing Nothing ]))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpSubscript () u (varGen "a") (AList () u [ ixSinGen 1 ])) (intGen 1))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpSubscript () u (varGen "b") (AList () u [ ixSinGen 1 ])) (intGen 1))
  , BlStatement () u Nothing (StFunction () u
      (ExpValue () u $ ValVariable "c") (AList () u [ varGen "x" ]) (intGen 1))
  , BlStatement () u Nothing (StFunction () u
      (ExpValue () u $ ValVariable "d") (AList () u [ varGen "x" ]) (intGen 1))
  , BlStatement () u Nothing (StFunction () u
      (ExpValue () u $ ValVariable "e") (AList () u []) (intGen 1)) ]

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
ex2 :: ProgramFile ()
ex2 = ProgramFile mi77 [ ex2pu1, ex2pu2 ]
ex2pu1 :: ProgramUnit ()
ex2pu1 = PUMain () u Nothing ex2pu1bs Nothing
ex2pu2 :: ProgramUnit ()
ex2pu2 = PUFunction () u Nothing emptyPrefixSuffix "y" (Just $ AList () u [ varGen "i", varGen "j" ]) Nothing [ ] Nothing
ex2pu1bs :: [Block ()]
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

expectedEx2 :: ProgramFile ()
expectedEx2 = ProgramFile mi77 [ expectedEx2pu1, ex2pu2 ]
expectedEx2pu1 :: ProgramUnit ()
expectedEx2pu1 = PUMain () u Nothing expectedEx2pu1bs Nothing
expectedEx2pu1bs :: [Block ()]
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
            (Just $ AList () u [ Argument () u Nothing (aintGen 1) ])))) ]


ex3 :: ProgramFile ()
ex3 = ProgramFile mi77 [ ex3pu1, ex3pu2 ]
ex3pu1 :: ProgramUnit ()
ex3pu1 = PUMain () u Nothing ex3pu1bs Nothing
ex3pu2 :: ProgramUnit ()
ex3pu2 = PUFunction () u Nothing emptyPrefixSuffix "y" (Just $ AList () u [ varGen "i", varGen "j" ]) Nothing [ ] Nothing
ex3pu1bs :: [Block ()]
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
            IxSingle () u Nothing (ExpSubscript () u (varGen "f") (AList () u [ ixSinGen 1 ]))]))) ]

expectedEx3 :: ProgramFile ()
expectedEx3 = ProgramFile mi77 [ expectedEx3pu1, ex3pu2 ]
expectedEx3pu1 :: ProgramUnit ()
expectedEx3pu1 = PUMain () u Nothing expectedEx3pu1bs Nothing
expectedEx3pu1bs :: [Block ()]
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
            (ArgExpr $ ExpFunctionCall () u (ExpValue () u $ ValVariable "f")
                                  (Just $ AList () u [ Argument () u Nothing (aintGen 1) ])) ]))) ]


{-
- program Main
- integer a, f
- a = f(1)
- end
-}

ex4 :: ProgramFile ()
ex4 = ProgramFile mi77 [ ex4pu1 ]
ex4pu1 :: ProgramUnit ()
ex4pu1 = PUMain () u (Just "main") ex4pu1bs Nothing
ex4pu1bs :: [Block ()]
ex4pu1bs =
  [ BlStatement () u Nothing (StDeclaration () u (TypeSpec () u TypeInteger Nothing) Nothing (AList () u
      [ declVarGen "a"
      , declVarGen "f" ]))
  , BlStatement () u Nothing
      (StExpressionAssign () u (varGen "a") (ExpSubscript () u (varGen "f") (AList () u [ ixSinGen 1 ]))) ]

expectedEx4 :: ProgramFile ()
expectedEx4 = ProgramFile mi77 [ expectedEx4pu1 ]
expectedEx4pu1 :: ProgramUnit ()
expectedEx4pu1 = PUMain () u (Just "main") expectedEx4pu1bs Nothing

expectedEx4pu1bs :: [Block ()]
expectedEx4pu1bs =
  [ BlStatement () u Nothing (StDeclaration () u (TypeSpec () u TypeInteger Nothing) Nothing (AList () u
      [ declVarGen "a"
      , declVarGen "f" ]))
  , BlStatement () u Nothing
      (StExpressionAssign () u (varGen "a")
       (ExpFunctionCall () u (ExpValue () u $ ValVariable "f")
                                  (Just $ AList () u [ Argument () u Nothing (aintGen 1) ] ))) ]

{-
- program Main
- integer a
- a = f(1)
- end
-}

ex5 :: ProgramFile ()
ex5 = ProgramFile mi77 [ ex5pu1 ]
ex5pu1 :: ProgramUnit ()
ex5pu1 = PUMain () u (Just "main") ex5pu1bs Nothing
ex5pu1bs :: [Block ()]
ex5pu1bs =
  [ BlStatement () u Nothing (StDeclaration () u (TypeSpec () u TypeInteger Nothing) Nothing (AList () u
      [ declVarGen "a" ]))
  , BlStatement () u Nothing
      (StExpressionAssign () u (varGen "a") (ExpSubscript () u (varGen "f") (AList () u [ ixSinGen 1 ]))) ]

expectedEx5 :: ProgramFile ()
expectedEx5 = ProgramFile mi77 [ expectedEx5pu1 ]
expectedEx5pu1 :: ProgramUnit ()
expectedEx5pu1 = PUMain () u (Just "main") expectedEx5pu1bs Nothing

expectedEx5pu1bs :: [Block ()]
expectedEx5pu1bs =
  [ BlStatement () u Nothing (StDeclaration () u (TypeSpec () u TypeInteger Nothing) Nothing (AList () u
      [ declVarGen "a" ]))
  , BlStatement () u Nothing
      (StExpressionAssign () u (varGen "a")
       (ExpFunctionCall () u (ExpValue () u $ ValVariable "f")
                                  (Just $ AList () u [ Argument () u Nothing (aintGen 1) ] ))) ]

{-
- program Main
- integer a
- dimension f(10)
- a = f(1)
- end
-}

ex6 :: ProgramFile ()
ex6 = ProgramFile mi77 [ ex6pu1 ]
ex6pu1 :: ProgramUnit ()
ex6pu1 = PUMain () u (Just "main") ex6pu1bs Nothing
ex6pu1bs :: [Block ()]
ex6pu1bs =
  [ BlStatement () u Nothing (StDeclaration () u (TypeSpec () u TypeInteger Nothing) Nothing (AList () u
      [ declVarGen "a"
      ]))
  , BlStatement () u Nothing (StDimension () u (AList () u
      [ Declarator () u (varGen "f") (ArrayDecl (AList () u [ DimensionDeclarator () u Nothing (Just $ intGen 10) ])) Nothing Nothing ]))
  , BlStatement () u Nothing
      (StExpressionAssign () u (varGen "a") (ExpSubscript () u (varGen "f") (AList () u [ ixSinGen 1 ]))) ]

expectedEx6 :: ProgramFile ()
expectedEx6 = ProgramFile mi77 [ expectedEx6pu1 ]
expectedEx6pu1 :: ProgramUnit ()
expectedEx6pu1 = PUMain () u (Just "main") expectedEx6pu1bs Nothing

expectedEx6pu1bs :: [Block ()]
expectedEx6pu1bs =
  [ BlStatement () u Nothing (StDeclaration () u (TypeSpec () u TypeInteger Nothing) Nothing (AList () u
      [ Declarator () u (varGen "a") ScalarDecl Nothing Nothing ]))
  , BlStatement () u Nothing (StDimension () u (AList () u
      [ Declarator () u (varGen "f") (ArrayDecl (AList () u [ DimensionDeclarator () u Nothing (Just $ intGen 10 ) ])) Nothing Nothing ]))
  , BlStatement () u Nothing
      (StExpressionAssign () u (varGen "a") (ExpSubscript () u (varGen "f") (AList () u [ ixSinGen 1 ]))) ]

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl test-suite:spec"
-- End:
