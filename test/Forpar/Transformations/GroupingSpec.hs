module Forpar.Transformations.GroupingSpec where

import Test.Hspec
import TestUtil

import Forpar.Transformations.Grouping
import Forpar.AST

spec :: Spec
spec =
  describe "Block IF-THEN and related statements" $ do
    it "groups example1" $ do
      groupIf example1 `shouldBe'` expectedExample1

    it "groups example2" $ do
      groupIf example2 `shouldBe'` expectedExample2

-- if (.true.) then
-- end if
example1 = [ PUMain () u (Just "example1") example1Blocks ]
example1Blocks = 
  [ BlStatement () u Nothing (StIfThen () u (ExpValue () u ValTrue))
  , BlStatement () u Nothing (StEndif () u) ]

expectedExample1 = [ PUMain () u (Just "example1") expectedExample1Blocks ]
expectedExample1Blocks = [ BlIf () u Nothing [ Just $ ExpValue () u ValTrue ] [ [ BlStatement () u Nothing (StEndif () u) ] ] ]

-- if (.true.) then
--   integer x
--   if (.false.) then
--   endif
-- else if (.true.) then
-- else
--   if (.false.) then
--   endif
-- end if
example2 = [ PUMain () u (Just "example2") example1Blocks ]
example2Blocks = 
  [ BlStatement () u Nothing (StIfThen () u valTrue)
  , BlStatement () u Nothing (StDeclaration () u (TypeInteger () u) (AList () u [ DeclVariable () u (varGen "x") ]))
  , BlStatement () u Nothing (StIfThen () u valFalse)
  , BlStatement () u Nothing (StEndif () u)
  , BlStatement () u Nothing (StElsif () u valTrue)
  , BlStatement () u Nothing (StElse () u)
  , BlStatement () u Nothing (StIfThen () u valFalse)
  , BlStatement () u Nothing (StEndif () u)
  , BlStatement () u Nothing (StEndif () u) ]

expectedExample2 = [ PUMain () u (Just "example2") expectedExample1Blocks ]
expectedExample2Blocks = [ BlIf () u Nothing [ Just $ valTrue, Just $ valFalse, Nothing ] blockGroups ]
blockGroups =
  [ [ BlStatement () u Nothing (StDeclaration () u (TypeInteger () u) (AList () u [ DeclVariable () u (varGen "x") ]))
    , innerIf ]
  , [ ]
  , [ innerIf
    , BlStatement () u Nothing (StEndif () u) ] ]
innerIf = BlIf () u Nothing [ Just $ ExpValue () u ValFalse ] [ [ BlStatement () u Nothing (StEndif () u) ] ]
