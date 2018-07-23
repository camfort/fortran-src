module Language.Fortran.Transformation.GroupingSpec where

import Test.Hspec hiding (Selector)
import TestUtil
import Control.Exception (evaluate)
import Control.DeepSeq (force, NFData)

import Language.Fortran.Transformer
import Language.Fortran.AST
import Language.Fortran.Util.Position
import Language.Fortran.ParserMonad

groupIf = transform [ GroupIf ]
groupDo = transform [ GroupLabeledDo ]
groupForall = transform [ GroupForall ]

instance NFData MetaInfo
instance NFData FortranVersion
instance NFData SrcSpan
instance NFData Position
instance NFData BaseType
instance NFData UnaryOp
instance NFData BinaryOp
instance NFData Only
instance NFData Intent
instance (NFData a, NFData (t a)) => NFData (AList t a)
instance NFData a => NFData (ProgramFile a)
instance NFData a => NFData (ProgramUnit a)
instance NFData a => NFData (Block a)
instance NFData a => NFData (Expression a)
instance NFData a => NFData (TypeSpec a)
instance NFData a => NFData (Index a)
instance NFData a => NFData (Value a)
instance NFData a => NFData (Comment a)
instance NFData a => NFData (Statement a)
instance NFData a => NFData (DoSpecification a)
instance NFData a => NFData (Selector a)
instance NFData a => NFData (ForallHeader a)
instance NFData a => NFData (Argument a)
instance NFData a => NFData (Use a)
instance NFData a => NFData (Attribute a)
instance NFData a => NFData (CommonGroup a)
instance NFData a => NFData (ControlPair a)
instance NFData a => NFData (DataGroup a)
instance NFData a => NFData (DimensionDeclarator a)
instance NFData a => NFData (Declarator a)
instance NFData a => NFData (FormatItem a)
instance NFData a => NFData (ImpElement a)
instance NFData a => NFData (ImpList a)
instance NFData a => NFData (Namelist a)
instance NFData a => NFData (PUFunctionOpt a)
instance NFData a => NFData (StructureItem a)
instance NFData a => NFData (UnionMap a)

spec :: Spec
spec = do
  let name = Just "name"
  let endName = Just "endName"
  describe "Block FORALL statements" $ do
    it "groups unlabelled FORALL blocks" $ do
      groupForall (exampleForall Nothing Nothing) `shouldBe'` expectedForall Nothing
    it "groups unlabelled FORALL blocks" $ do
      groupForall (exampleForall name name) `shouldBe'` expectedForall name
    it "groups unlabelled FORALL blocks" $ do
      let lhs = (evaluate . force) (groupForall $ exampleForall name endName)
      lhs `shouldThrow` anyErrorCall
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

buildExampleProgram name blocks = ProgramFile mi77 [ PUMain () u (Just name) blocks Nothing ]

exampleComment = BlComment () u $ Comment "comment"
exampleHeader = ForallHeader [] Nothing
exampleForall name nameEnd = buildExampleProgram "forall" $
  [ BlStatement () u Nothing $ StForall () u name exampleHeader
  , exampleComment
  , BlStatement () u Nothing $ StEndForall () u nameEnd
  ]

expectedForall name  = buildExampleProgram "forall" $
    [BlForall () u Nothing name exampleHeader [exampleComment] Nothing]


-- if (.true.) then
-- end if
example1 = ProgramFile mi77 [ PUMain () u (Just "example1") example1Blocks Nothing ]
example1Blocks =
  [ BlStatement () u Nothing (StIfThen () u Nothing valTrue)
  , BlStatement () u Nothing (StEndif () u Nothing) ]

expectedExample1 = ProgramFile mi77 [ PUMain () u (Just "example1") expectedExample1Blocks Nothing ]
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
example2 = ProgramFile mi77 [ PUMain () u (Just "example2") example2Blocks Nothing ]
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

expectedExample2 = ProgramFile mi77 [ PUMain () u (Just "example2") expectedExample2Blocks Nothing ]
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
example1do = ProgramFile mi77 [ PUMain () u (Just "example1") example1doblocks Nothing ]
example1doblocks =
  [ BlStatement () u Nothing (StDo () u Nothing label10 dospec)
  , BlStatement () u label10 (StContinue () u) ]
dospec = Just (DoSpecification () u
           (StExpressionAssign () u (ExpValue () u (ValVariable "i"))
                                    (ExpValue () u (ValInteger "0")))
                                    (ExpValue () u (ValInteger "10")) Nothing)

expectedExample1do = ProgramFile mi77 [ PUMain () u (Just "example1") expectedExample1doBlocks Nothing ]
expectedExample1doBlocks =
  [ BlDo () u Nothing Nothing label10 dospec
     [BlStatement () u label10 (StContinue () u)] label10 ]

label20 = Just (ExpValue () u (ValInteger "20"))
-- do 10 i = 0, 10
-- do 10 i = 0, 10
-- 10   continue
-- do 20 i = 0, 10
-- 20   continue
example2do = ProgramFile mi77 [ PUMain () u (Just "example2") example2doblocks Nothing ]
example2doblocks =
  [ BlStatement () u Nothing (StDo () u Nothing label10 dospec)
  , BlStatement () u Nothing (StDo () u Nothing label10 dospec)
  , BlStatement () u label10 (StContinue () u)
  , BlStatement () u Nothing (StDo () u Nothing label20 dospec)
  , BlStatement () u label20 (StContinue () u)
  ]

expectedExample2do = ProgramFile mi77 [ PUMain () u (Just "example2") expectedExample2doBlocks Nothing ]
expectedExample2doBlocks =
  [ BlDo () u Nothing Nothing label10 dospec
      [ BlDo () u Nothing Nothing label10 dospec
          [ BlStatement () u label10 (StContinue () u) ] label10
      ] label10
  , BlDo () u Nothing Nothing label20 dospec
      [ BlStatement () u label20 (StContinue () u) ] label20
  ]
