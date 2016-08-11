module Language.Fortran.Transformation.GroupingSpec where

import Test.Hspec
import TestUtil

import Language.Fortran.Transformer
import Language.Fortran.AST

groupIf = transform [ GroupIf ]
groupDo = transform [ GroupLabeledDo ]

spec :: Spec
spec = do
  describe "Block IF-THEN and related statements" $ do
    it "groups example1" $
      groupIf example1 `shouldBe'` expectedExample1

    it "groups example2" $
      groupIf example2 `shouldBe'` expectedExample2

  describe "Block DO statements" $ do
    it "do group example1" $
      groupDo example1do `shouldBe` expectedExample1do

    it "do group example2 with common end-point" $
      groupDo example2do `shouldBe` expectedExample2do

-- if (.true.) then
-- end if
example1 = ProgramFile mi77 [ ([ ], PUMain () u (Just "example1") example1Blocks Nothing) ] []
example1Blocks =
  [ BlStatement () u Nothing (StIfThen () u Nothing valTrue)
  , BlStatement () u Nothing (StEndif () u Nothing) ]

expectedExample1 = ProgramFile mi77 [ ([ ], PUMain () u (Just "example1") expectedExample1Blocks Nothing) ] [ ]
expectedExample1Blocks = [ BlIf () u Nothing Nothing [ Just valTrue ] [ [ ] ] Nothing ]

-- if (.true.) then
--   integer x
--   if (.false.) then
--   endif
-- else if (.true.) then
-- else
--   if (.false.) then
--   endif
-- end if
example2 = ProgramFile mi77 [ ([ ], PUMain () u (Just "example2") example2Blocks Nothing) ] [ ]
example2Blocks =
  [ BlStatement () u Nothing (StIfThen () u Nothing valTrue)
  , BlStatement () u Nothing (StDeclaration () u (TypeSpec () u TypeInteger Nothing) Nothing (AList () u [ DeclVariable () u (varGen "x") Nothing Nothing ]))
  , BlStatement () u Nothing (StIfThen () u Nothing valFalse)
  , BlStatement () u Nothing (StEndif () u Nothing)
  , BlStatement () u Nothing (StElsif () u Nothing valTrue)
  , BlStatement () u Nothing (StElse () u Nothing)
  , BlStatement () u Nothing (StIfThen () u Nothing valFalse)
  , BlStatement () u Nothing (StEndif () u Nothing)
  , BlStatement () u Nothing (StEndif () u Nothing) ]

expectedExample2 = ProgramFile mi77 [ ([ ], PUMain () u (Just "example2") expectedExample2Blocks Nothing) ] [ ]
expectedExample2Blocks = [ BlIf () u Nothing Nothing [ Just valTrue, Just valTrue, Nothing ] blockGroups Nothing ]
blockGroups =
  [ [ BlStatement () u Nothing (StDeclaration () u (TypeSpec () u TypeInteger Nothing) Nothing (AList () u [ DeclVariable () u (varGen "x") Nothing Nothing ]))
    , innerIf ]
  , [ ]
  , [ innerIf ] ]
innerIf = BlIf () u Nothing Nothing [ Just valFalse ] [ [ ] ] Nothing


-- do 10 i = 0, 10
-- 10   continue
label10 = Just (ExpValue () u (ValInteger "10"))
example1do = ProgramFile mi77 [ ([ ], PUMain () u (Just "example1") example1doblocks Nothing) ] [ ]
example1doblocks =
  [ BlStatement () u Nothing
      (StDo () u Nothing (Just (ExpValue () u (ValInteger "10"))) dospec) ,
    BlStatement () u (Just (ExpValue () u (ValInteger "10"))) (StContinue () u) ]
dospec = (Just (DoSpecification () u
          (StExpressionAssign () u (ExpValue () u (ValVariable "i"))
                                   (ExpValue () u (ValInteger "0")))
                                   (ExpValue () u (ValInteger "10")) Nothing))

expectedExample1do = ProgramFile mi77 [ ([ ], PUMain () u (Just "example1") expectedExample1doBlocks Nothing) ] [ ]
expectedExample1doBlocks =
  [ BlDo () u Nothing Nothing label10 dospec
     [BlStatement () u label10 (StContinue () u)] Nothing ]


label20 = Just (ExpValue () u (ValInteger "20"))
-- do 10 i = 0, 10
-- do 10 i = 0, 10
-- 10   continue
-- do 20 i = 0, 10
-- 20   continue
example2do = ProgramFile mi77 [ ([ ], PUMain () u (Just "example2") example2doblocks Nothing) ] [ ]
example2doblocks =
  [ BlStatement () u Nothing
      (StDo () u Nothing (Just (ExpValue () u (ValInteger "10"))) dospec),
    BlStatement () u Nothing
      (StDo () u Nothing (Just (ExpValue () u (ValInteger "10"))) dospec),
    BlStatement () u (Just (ExpValue () u (ValInteger "10"))) (StContinue () u),
    BlStatement () u Nothing
      (StDo () u Nothing (Just (ExpValue () u (ValInteger "20"))) dospec) ,
    BlStatement () u (Just (ExpValue () u (ValInteger "20"))) (StContinue () u)]

expectedExample2do = ProgramFile mi77 [ ([ ], PUMain () u (Just "example2") expectedExample2doBlocks Nothing) ] [ ]
expectedExample2doBlocks =
  [ BlStatement () u Nothing
      (StDo () u Nothing (Just (ExpValue () u (ValInteger "10"))) dospec),
    BlStatement () u Nothing
      (StDo () u Nothing (Just (ExpValue () u (ValInteger "10"))) dospec),
    BlStatement () u (Just (ExpValue () u (ValInteger "10"))) (StContinue () u),
    BlDo () u Nothing Nothing label20 dospec
     [BlStatement () u (Just (ExpValue () u (ValInteger "20"))) (StContinue () u) ] Nothing ]
