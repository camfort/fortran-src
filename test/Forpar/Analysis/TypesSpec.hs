module Forpar.Analysis.TypesSpec where

import Test.Hspec
import TestUtil

import Data.Map ((!))

import Forpar.AST
import Forpar.Analysis.Types

import Debug.Trace

spec :: Spec
spec = do
  describe "Global type inference" $ do
    it "types integer returning function" $ do
      let entry = inferTypes ex1 ! Global ! "f1"
      entry `shouldBe` IDType (Just VTInteger) (Just CTFunction) 

    it "types multiples program units" $ do
      let mapping = inferTypes ex2 ! Global 
      mapping ! "f1" `shouldBe` IDType (Just VTInteger) (Just CTFunction)
      mapping ! "s1" `shouldBe` IDType Nothing (Just CTSubroutine) 

    it "types ENTRY points within subprograms" $ do
      let mapping = inferTypes ex3 ! Global
      mapping ! "e1" `shouldBe` IDType Nothing (Just CTSubroutine) 
      mapping ! "e2" `shouldBe` IDType Nothing (Just CTSubroutine) 
      mapping ! "e3" `shouldBe` IDType Nothing (Just CTSubroutine) 

  describe "Local type inference" $ do
    it "infers from type declarations" $ do
      let mapping = inferTypes ex4 ! Local NamelessMain
      mapping ! "x" `shouldBe` IDType (Just VTInteger) Nothing
      mapping ! "y" `shouldBe` IDType (Just VTInteger) (Just CTArray)
      mapping ! "c" `shouldBe` IDType (Just VTCharacter) Nothing
      mapping ! "log" `shouldBe` IDType (Just VTLogical) Nothing

    it "infers from dimension declarations" $ do
      let mapping = inferTypes ex5 ! (Local $ Named "bd")
      mapping ! "x" `shouldBe` IDType Nothing (Just CTArray)
      mapping ! "y" `shouldBe` IDType Nothing (Just CTArray)

ex1 = ProgramFile [ ([ ], ex1pu1) ] [ ]
ex1pu1 = PUFunction () u (Just $ TypeInteger () u) "f1" (AList () u []) []

ex2 = ProgramFile [ ([ ], ex2pu1), ([] , ex1pu1) ] [ ]
ex2pu1 = PUSubroutine () u "s1" (AList () u []) []

ex3 = ProgramFile [ ([ ], ex3pu1) ] [ ]
ex3pu1 = PUSubroutine () u "s1" (AList () u []) ex3pu1bs
ex3pu1bs =
  [ BlStatement () u Nothing (StEntry () u (ExpValue () u (ValFunctionName "e1")) Nothing)
  , BlStatement () u Nothing (StEntry () u (ExpValue () u (ValFunctionName "e2")) Nothing)
  , BlStatement () u Nothing (StEntry () u (ExpValue () u (ValFunctionName "e3")) Nothing) ]

ex4 = ProgramFile [ ([ ], ex4pu1) ] [ ]
ex4pu1 = PUMain () u Nothing ex4pu1bs
ex4pu1bs =
  [ BlStatement () u Nothing (StDeclaration () u (TypeInteger () u) 
      (AList () u 
        [ DeclVariable () u $ varGen "x"
        , DeclArray () u (varGen "y") 
            (AList () u [ DimensionDeclarator () u Nothing (intGen 10) ]) ]))
  , BlStatement () u Nothing (StDeclaration () u (TypeCharacter () u Nothing)
      (AList () u [ DeclCharVariable () u (varGen "c") Nothing ]))
  , BlStatement () u Nothing (StDeclaration () u (TypeLogical () u) 
      (AList () u [ DeclVariable () u $ varGen "log" ])) ]

ex5 = ProgramFile [ ([ ], ex5pu1) ] [ ]
ex5pu1 = PUBlockData () u (Just "bd") ex5pu1bs 
ex5pu1bs =
  [ BlStatement () u Nothing (StDimension () u (AList () u
      [ DeclArray () u (arrGen "x") (AList () u [ DimensionDeclarator () u Nothing (intGen 1) ])
      , DeclArray () u (arrGen "y") (AList () u [ DimensionDeclarator () u Nothing (intGen 1) ]) ])) ]
