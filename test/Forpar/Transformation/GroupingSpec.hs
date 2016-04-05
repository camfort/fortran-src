module Forpar.Transformation.GroupingSpec where

import Test.Hspec
import TestUtil

import Forpar.Transformer
import Forpar.AST

groupIf = transform [ GroupIf ]

spec :: Spec
spec =
  describe "Block IF-THEN and related statements" $ do
    it "groups example1" $
      groupIf example1 `shouldBe'` expectedExample1

    it "groups example2" $
      groupIf example2 `shouldBe'` expectedExample2

-- if (.true.) then
-- end if
example1 = ProgramFile [ ([ ], PUMain () u (Just "example1") example1Blocks) ] []
example1Blocks = 
  [ BlStatement () u Nothing (StIfThen () u valTrue)
  , BlStatement () u Nothing (StEndif () u) ]

expectedExample1 = ProgramFile [ ([ ], PUMain () u (Just "example1") expectedExample1Blocks) ] [ ]
expectedExample1Blocks = [ BlIf () u Nothing [ Just valTrue ] [ [ BlStatement () u Nothing (StEndif () u) ] ] ]

-- if (.true.) then
--   integer x
--   if (.false.) then
--   endif
-- else if (.true.) then
-- else
--   if (.false.) then
--   endif
-- end if
example2 = ProgramFile [ ([ ], PUMain () u (Just "example2") example2Blocks) ] [ ]
example2Blocks = 
  [ BlStatement () u Nothing (StIfThen () u valTrue)
  , BlStatement () u Nothing (StDeclaration () u (TypeSpec () u TypeInteger Nothing) Nothing (AList () u [ DeclVariable () u (varGen "x") Nothing Nothing ]))
  , BlStatement () u Nothing (StIfThen () u valFalse)
  , BlStatement () u Nothing (StEndif () u)
  , BlStatement () u Nothing (StElsif () u valTrue)
  , BlStatement () u Nothing (StElse () u)
  , BlStatement () u Nothing (StIfThen () u valFalse)
  , BlStatement () u Nothing (StEndif () u)
  , BlStatement () u Nothing (StEndif () u) ]

expectedExample2 = ProgramFile [ ([ ], PUMain () u (Just "example2") expectedExample2Blocks) ] [ ]
expectedExample2Blocks = [ BlIf () u Nothing [ Just valTrue, Just valTrue, Nothing ] blockGroups ]
blockGroups =
  [ [ BlStatement () u Nothing (StDeclaration () u (TypeSpec () u TypeInteger Nothing) Nothing (AList () u [ DeclVariable () u (varGen "x") Nothing Nothing ]))
    , innerIf ]
  , [ ]
  , [ innerIf
    , BlStatement () u Nothing (StEndif () u) ] ]
innerIf = BlIf () u Nothing [ Just valFalse ] [ [ BlStatement () u Nothing (StEndif () u) ] ]
