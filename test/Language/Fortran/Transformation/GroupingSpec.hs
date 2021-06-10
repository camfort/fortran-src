{-# OPTIONS_GHC -Wno-orphans #-}
module Language.Fortran.Transformation.GroupingSpec where

import Test.Hspec hiding (Selector)
import TestUtil
import Control.Exception (evaluate)
import Control.DeepSeq (force)
import Data.ByteString.Char8 (ByteString, pack)

import Language.Fortran.Transformer
import Language.Fortran.AST
import Language.Fortran.Util.Position
import Language.Fortran.ParserMonad
import Language.Fortran.Parser.Fortran95
import Language.Fortran.Parser.Fortran77

groupDo :: ProgramFile () -> ProgramFile ()
groupDo = transform [ GroupLabeledDo ]
groupForall :: ProgramFile () -> ProgramFile ()
groupForall = transform [ GroupForall ]

spec :: Spec
spec = do
  let name = Just "name"
  let endName = Just "endName"
  describe "Block FORALL statements" $ do
    it "groups unlabelled FORALL blocks" $
      groupForall (exampleForall Nothing Nothing) `shouldBe'` expectedForall Nothing
    it "groups unlabelled FORALL blocks" $
      groupForall (exampleForall name name) `shouldBe'` expectedForall name
    it "groups unlabelled FORALL blocks" $ do
      let lhs = (evaluate . force) (groupForall $ exampleForall name endName)
      lhs `shouldThrow` anyErrorCall

  describe "Block DO statements" $ do
    it "do group example1" $
      groupDo example1do `shouldBe` expectedExample1do

    it "do group example2 with common end-point" $
      groupDo example2do `shouldBe` expectedExample2do

  describe "Block SrcSpan's" $ do
    it "Spans all a BlIf" $
      ifSpan `shouldBe` expectedIfSpan
    it "spans all a BlDo" $
      doSpan `shouldBe` expectedDoSpan
    it "spans all a BlDoWhile" $
      doWhileSpan `shouldBe` expectedDoWhileSpan

  describe "Inner block SrcSpan's" $ do
    it "Spans the inner blocks of an if including comments - 77" $
      ifInnerBlockSpan getSingleParsedBlock77 `shouldBe` expectedIfInnerBlockSpan
    it "Spans the inner blocks of an if including comments - 77 legacy" $
      ifInnerBlockSpan getSingleParsedBlock77Legacy `shouldBe` expectedIfInnerBlockSpan

buildExampleProgram :: Name -> [Block ()] -> ProgramFile ()
buildExampleProgram name blocks = ProgramFile mi77 [ PUMain () u (Just name) blocks Nothing ]

exampleComment :: Block ()
exampleComment = BlComment () u $ Comment "comment"
exampleHeader :: ForallHeader a
exampleHeader = ForallHeader [] Nothing
exampleForall :: Maybe String -> Maybe String -> ProgramFile ()
exampleForall name nameEnd = buildExampleProgram "forall"
  [ BlStatement () u Nothing $ StForall () u name exampleHeader
  , exampleComment
  , BlStatement () u Nothing $ StEndForall () u nameEnd
  ]

expectedForall :: Maybe String -> ProgramFile ()
expectedForall name  = buildExampleProgram "forall"
    [BlForall () u Nothing name exampleHeader [exampleComment] Nothing]

-- do 10 i = 0, 10
-- 10   continue
label10 :: Maybe (Expression ())
label10 = Just (ExpValue () u (ValInteger "10"))
example1do :: ProgramFile ()
example1do = ProgramFile mi77 [ PUMain () u (Just "example1") example1doblocks Nothing ]
example1doblocks :: [Block ()]
example1doblocks =
  [ BlStatement () u Nothing (StDo () u Nothing label10 dospec)
  , BlStatement () u label10 (StContinue () u) ]
dospec :: Maybe (DoSpecification ())
dospec = Just (DoSpecification () u
           (StExpressionAssign () u (ExpValue () u (ValVariable "i"))
                                    (ExpValue () u (ValInteger "0")))
                                    (ExpValue () u (ValInteger "10")) Nothing)

expectedExample1do :: ProgramFile ()
expectedExample1do = ProgramFile mi77 [ PUMain () u (Just "example1") expectedExample1doBlocks Nothing ]
expectedExample1doBlocks :: [Block ()]
expectedExample1doBlocks =
  [ BlDo () u Nothing Nothing label10 dospec
     [ ] label10 ]

label20 :: Maybe (Expression ())
label20 = Just (ExpValue () u (ValInteger "20"))
-- do 10 i = 0, 10
-- do 10 i = 0, 10
-- 10   continue
-- do 20 i = 0, 10
-- 20   continue
example2do :: ProgramFile ()
example2do = ProgramFile mi77 [ PUMain () u (Just "example2") example2doblocks Nothing ]
example2doblocks :: [Block ()]
example2doblocks =
  [ BlStatement () u Nothing (StDo () u Nothing label10 dospec)
  , BlStatement () u Nothing (StDo () u Nothing label10 dospec)
  , BlStatement () u label10 (StContinue () u)
  , BlStatement () u Nothing (StDo () u Nothing label20 dospec)
  , BlStatement () u label20 (StContinue () u)
  ]

expectedExample2do :: ProgramFile ()
expectedExample2do = ProgramFile mi77 [ PUMain () u (Just "example2") expectedExample2doBlocks Nothing ]
expectedExample2doBlocks :: [Block ()]
expectedExample2doBlocks =
  [ BlDo () u Nothing Nothing label10 dospec
      [ BlDo () u Nothing Nothing label10 dospec
          [ ] label10
      ] label10
  , BlDo () u Nothing Nothing label20 dospec
      [ ] label20
  ]

getSingleParsedBlock :: Show b => (ByteString -> String -> ParseResult a b (ProgramFile A0)) -> String -> Block A0
getSingleParsedBlock p c =
  let pf = fromRight . fromParseResult $ p (pack c) "foobar.f"
      ProgramFile _ ((PUSubroutine _ _ _ _ _ (b:_) _):_) = pf
  in  b

getSingleParsedBlock95 :: String -> Block A0
getSingleParsedBlock95 = getSingleParsedBlock fortran95Parser

getSingleParsedBlock77 :: String -> Block A0
getSingleParsedBlock77 = getSingleParsedBlock fortran77Parser

getSingleParsedBlock77Legacy :: String -> Block A0
getSingleParsedBlock77Legacy = getSingleParsedBlock legacy77Parser

type SimpleSpan = (Int, Int, Int, Int)

simplifySpan :: SrcSpan -> SimpleSpan
simplifySpan (SrcSpan b e) = (posLine b, posColumn b, posLine e, posColumn e)

ifSpanRaw :: String
ifSpanRaw = unlines [
    "      subroutine foobar"
  , "       if (.TRUE.) then"
  , "        print *, 'w00t'"
  , "       endif"
  , "      end" ]
ifSpan :: SimpleSpan
ifSpan =
  let BlIf _ s _ _ _ _ _ = getSingleParsedBlock95 ifSpanRaw
  in  simplifySpan s
expectedIfSpan :: SimpleSpan
expectedIfSpan = (2, 8, 4, 12)

doSpanRaw :: String
doSpanRaw = unlines [
    "      subroutine foobar2"
  , "       do ii = 2, 5"
  , "        if(ii .eq. 2) print *, ii"
  , "        if(ii .eq. 4) print *, ii"
  , "       end do"
  , "      end" ]
doSpan :: SimpleSpan
doSpan =
  let BlDo _ s _ _ _ _ _ _ = getSingleParsedBlock95 doSpanRaw
  in  simplifySpan s
expectedDoSpan :: SimpleSpan
expectedDoSpan = (2, 8, 5, 13)

doWhileSpanRaw :: String
doWhileSpanRaw = unlines [
    "      subroutine barfoo"
  , "       do while (.true.)"
  , "        print *, 'foooo'"
  , "       enddo"
  , "      end" ]
doWhileSpan :: SimpleSpan
doWhileSpan =
  let BlDoWhile _ s _ _ _ _ _ _ = getSingleParsedBlock95 doWhileSpanRaw
  in  simplifySpan s
expectedDoWhileSpan :: SimpleSpan
expectedDoWhileSpan = (2, 8, 4, 12)

ifInnerBlockSpanRaw :: String
ifInnerBlockSpanRaw = unlines [
    "      subroutine yeet"
  , "       if (.true.) then"
  , "c       very important comment"
  , "        print *, 'yeet'"
  , "c       even more important comment"
  , "       endif"
  , "      end" ]
ifInnerBlockSpan :: (String -> Block A0) -> SimpleSpan
ifInnerBlockSpan p =
  let BlIf _ _ _ _ _ bs _ = p ifInnerBlockSpanRaw
  in  simplifySpan $ getSpan bs
expectedIfInnerBlockSpan :: SimpleSpan
expectedIfInnerBlockSpan = (3, 1, 5, 35)
