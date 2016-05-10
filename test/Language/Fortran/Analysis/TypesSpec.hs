module Language.Fortran.Analysis.TypesSpec where

import Test.Hspec
import TestUtil

import Data.Map ((!))

import Language.Fortran.AST
import Language.Fortran.Analysis.Types

import Debug.Trace

spec :: Spec
spec = do
  describe "Global type inference" $ do
    it "types integer returning function" $ do
      let entry = inferTypes ex1 ! Global ! "f1"
      entry `shouldBe` IDType (Just TypeInteger) (Just CTFunction)

    it "types multiples program units" $ do
      let mapping = inferTypes ex2 ! Global
      mapping ! "f1" `shouldBe` IDType (Just TypeInteger) (Just CTFunction)
      mapping ! "s1" `shouldBe` IDType Nothing (Just CTSubroutine)

    it "types ENTRY points within subprograms" $ do
      let mapping = inferTypes ex3 ! Global
      mapping ! "e1" `shouldBe` IDType Nothing (Just CTSubroutine)
      mapping ! "e2" `shouldBe` IDType Nothing (Just CTSubroutine)
      mapping ! "e3" `shouldBe` IDType Nothing (Just CTSubroutine)

  describe "Local type inference" $ do
    it "infers from type declarations" $ do
      let mapping = inferTypes ex4 ! Local NamelessMain
      mapping ! "x" `shouldBe` IDType (Just TypeInteger) Nothing
      mapping ! "y" `shouldBe` IDType (Just TypeInteger) (Just CTArray)
      mapping ! "c" `shouldBe` IDType (Just TypeCharacter) Nothing
      mapping ! "log" `shouldBe` IDType (Just TypeLogical) Nothing

    it "infers from dimension declarations" $ do
      let mapping = inferTypes ex5 ! (Local $ Named "bd")
      mapping ! "x" `shouldBe` IDType Nothing (Just CTArray)
      mapping ! "y" `shouldBe` IDType Nothing (Just CTArray)

    it "infers from function statements" $ do
      let mapping = inferTypes ex6 ! (Local $ Named "main")
      mapping ! "a" `shouldBe` IDType (Just TypeInteger) (Just CTArray)
      mapping ! "b" `shouldBe` IDType (Just TypeInteger) (Just CTArray)
      mapping ! "c" `shouldBe` IDType (Just TypeInteger) (Just CTFunction)
      mapping ! "d" `shouldBe` IDType Nothing (Just CTFunction)

ex1 = ProgramFile [ ([ ], ex1pu1) ] [ ]
ex1pu1 = PUFunction () u (Just $ TypeSpec () u TypeInteger Nothing) False "f1" (AList () u []) Nothing []

ex2 = ProgramFile [ ([ ], ex2pu1), ([] , ex1pu1) ] [ ]
ex2pu1 = PUSubroutine () u False "s1" (AList () u []) []

ex3 = ProgramFile [ ([ ], ex3pu1) ] [ ]
ex3pu1 = PUSubroutine () u False "s1" (AList () u []) ex3pu1bs
ex3pu1bs =
  [ BlStatement () u Nothing (StEntry () u (ExpValue () u (ValVariable () "e1")) Nothing Nothing)
  , BlStatement () u Nothing (StEntry () u (ExpValue () u (ValVariable () "e2")) Nothing Nothing)
  , BlStatement () u Nothing (StEntry () u (ExpValue () u (ValVariable () "e3")) Nothing Nothing) ]

ex4 = ProgramFile [ ([ ], ex4pu1) ] [ ]
ex4pu1 = PUMain () u Nothing ex4pu1bs Nothing
ex4pu1bs =
  [ BlStatement () u Nothing (StDeclaration () u (TypeSpec () u TypeInteger Nothing) Nothing
      (AList () u
        [ DeclVariable () u (varGen "x") Nothing Nothing
        , DeclArray () u (varGen "y")
            (AList () u [ DimensionDeclarator () u Nothing (Just $ intGen 10) ]) Nothing Nothing ]))
  , BlStatement () u Nothing (StDeclaration () u (TypeSpec () u TypeCharacter Nothing) Nothing
      (AList () u [ DeclVariable () u (varGen "c") Nothing Nothing ]))
  , BlStatement () u Nothing (StDeclaration () u (TypeSpec () u TypeLogical Nothing) Nothing
      (AList () u [ DeclVariable () u (varGen "log") Nothing Nothing ])) ]

ex5 = ProgramFile [ ([ ], ex5pu1) ] [ ]
ex5pu1 = PUBlockData () u (Just "bd") ex5pu1bs
ex5pu1bs =
  [ BlStatement () u Nothing (StDimension () u (AList () u
      [ DeclArray () u (varGen "x") (AList () u [ DimensionDeclarator () u Nothing (Just $ intGen 1) ]) Nothing Nothing
      , DeclArray () u (varGen "y") (AList () u [ DimensionDeclarator () u Nothing (Just $ intGen 1) ]) Nothing Nothing])) ]

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
ex6 = ProgramFile [ ([ ], ex6pu1)] [ ]
ex6pu1 = PUMain () u (Just "main") ex6pu1bs Nothing
ex6pu1bs =
  [ BlStatement () u Nothing (StDeclaration () u (TypeSpec () u TypeInteger Nothing) Nothing (AList () u
      [ DeclVariable () u (varGen "a") Nothing Nothing
      , DeclArray () u (varGen "b") (AList () u [ DimensionDeclarator () u Nothing (Just $ intGen 1) ]) Nothing Nothing
      , DeclVariable () u (varGen "c") Nothing Nothing ]))
  , BlStatement () u Nothing (StDimension () u (AList () u
      [ DeclArray () u (varGen "a") (AList () u [ DimensionDeclarator () u Nothing (Just $ intGen 1 ) ]) Nothing Nothing ]))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpSubscript () u (varGen "a") (fromList () [ ixSinGen 1 ])) (intGen 1))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpSubscript () u (varGen "b") (fromList () [ ixSinGen 1 ])) (intGen 1))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpSubscript () u (varGen "c") (fromList () [ ixSinGen 1 ])) (intGen 1))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpSubscript () u (varGen "d") (fromList () [ ixSinGen 1 ])) (intGen 1)) ]
