{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fortran.PrettySpec where

import qualified Data.ByteString.Char8 as B
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (replace)

import Data.Data
import Data.Foldable
import Data.Generics.Uniplate.Operations
import Data.Maybe (catMaybes)
--import Data.DeriveTH

import Control.Monad (void)

import Language.Fortran.AST
import Language.Fortran.ParserMonad
import Language.Fortran.Pretty
import Language.Fortran.Parser.Any
import Language.Fortran.Util.Position
import Language.Fortran.Util.SecondParameter

import System.FilePath
import System.Directory
import Text.PrettyPrint
import Text.PrettyPrint.GenericPretty

import Test.Hspec
import TestUtil

--derive makeArbitrary ''Expression
--derive makeArbitrary ''Statement
--derive makeArbitrary ''Value

checkAll :: forall a b c . (Out c, Data c, Data a, Data b)
         => (b -> Maybe c) -> (c -> Spec) -> a -> Spec
checkAll restrict check t =
    describe ("Testing on " ++ show (length inputs) ++ " nodes")
      $ mapM_ check inputs
  where
    inputs = catMaybes [ restrict b | b <- universeBi t :: [b] ]

samplesBase :: FilePath
samplesBase = "test" </> "Language" </> "Fortran" </> "samples"

spec :: Spec
spec =
  describe "Pretty printer tests" $ do
    let path = samplesBase </> "simple.f90"
    contents <- runIO $ flexReadFile path
    let version = deduceVersion path
    let Just parserF = lookup version parserVersions
    let ast = void (parserF contents path)

{-
    describe "Size-related invariants (values in expressions)" $ do
     let ppr = prop_pprintsize :: FortranVersion -> Expression () -> Spec
     checkAll valueExpressions (ppr version) ast

    describe "Size-related invariants (do specifications)" $ do
     let ppr = prop_pprintsize :: FortranVersion -> DoSpecification () -> Spec
     checkAll Just (ppr version) ast

    describe "Size-related invariants (indices)" $ do
     let ppr = prop_pprintsize :: FortranVersion -> Index () -> Spec
     checkAll Just (ppr version) ast

    describe "Size-related invariants (expressions)" $ do
     let ppr = prop_pprintsize :: FortranVersion -> Expression () -> Spec
     checkAll Just (ppr version) ast

    describe "Size-related invariants (arguments)" $ do
     let ppr = prop_pprintsize :: FortranVersion -> Argument () -> Spec
     checkAll Just (ppr version) ast

    describe "Size-related invariants (statements)" $ do
     let ppr = prop_pprintsize :: FortranVersion -> Statement () -> Spec
     checkAll Just (ppr version) ast

    describe "Size-related invariants (dimension declarator)" $ do
     let ppr = prop_pprintsize :: FortranVersion -> DimensionDeclarator ()
            -> Spec
     checkAll Just (ppr version) ast

    describe "Size-related invariants (selector)" $ do
     let ppr = prop_pprintsize :: FortranVersion -> DimensionDeclarator ()
            -> Spec
     checkAll Just (ppr version) ast
-}

    describe "Dimension declarator" $ do
      it "Prints left bound dimension declarator" $ do
        let dd = DimensionDeclarator () u (Just $ intGen 42) Nothing
        pprint Fortran90 dd `shouldBe` "42:"

      it "Prints right bound dimension declarator" $ do
        let dd = DimensionDeclarator () u Nothing (Just $ intGen 42)
        pprint Fortran90 dd `shouldBe` "42"

      it "Prints bounded dimension declarator" $ do
        let dd = DimensionDeclarator () u (Just $ intGen 24) (Just $ intGen 42)
        pprint Fortran90 dd `shouldBe` "24:42"

    describe "Selector" $ do
      it "prints Fortran 77 selector" $ do
        let sel = Selector () u (Just $ intGen 42) Nothing
        pprint Fortran77 sel `shouldBe` "* (42)"

      it "prints Fortran 90 selector" $ do
        let sel = Selector () u (Just $ intGen 42) (Just $ intGen 24)
        pprint Fortran90 sel `shouldBe` "(len=42, kind=24)"

    describe "Use" $
      it "prints renaming" $ do
        let renaming = UseRename () u (varGen "x") (varGen "y")
        pprint Fortran90 renaming `shouldBe` "x => y"

    describe "Control pair" $
      it "prints named control pair" $ do
        let cp = ControlPair () u (Just "errno") (intGen 42)
        pprint Fortran77Extended cp `shouldBe` "errno=42"

    describe "Implicit list" $
      it "prints mixed implicit lists" $ do
        let typ = TypeSpec () u TypeInteger Nothing
        let impEls = [ ImpCharacter () u "x"
                     , ImpRange () u "a" "z"
                     , ImpCharacter () u "o" ]
        let impList = ImpList () u typ (AList () u impEls)
        pprint Fortran90 impList `shouldBe` "integer (x, a-z, o)"

    describe "Common group" $ do
      let globs = [ varGen "x", varGen "y", varGen "z" ]

      it "prints anonymous common group" $ do
        let group = CommonGroup () u Nothing (AList () u globs)
        pprint Fortran66 group `shouldBe` "//x, y, z"

      it "prints named common group" $ do
        let group = CommonGroup () u (Just $ varGen "my_g") (AList () u globs)
        pprint Fortran66 group `shouldBe` "/my_g/x, y, z"

    describe "Format item" $
      it "prints hollerith constant" $ do
        let ed = FIHollerith () u (ValHollerith "hello darling")
        pprint Fortran77 ed `shouldBe` "13hhello darling"

    describe "Statement" $ do
      describe "Declaration" $ do
        it "prints 90 style with attributes" $ do
          let sel = Selector () u (Just $ intGen 3) Nothing
          let typeSpec = TypeSpec () u TypeCharacter (Just sel)
          let attrs = [ AttrIntent () u In , AttrPointer () u ]
          let declList =
                [ DeclVariable () u (varGen "x") Nothing (Just $ intGen 42)
                , DeclVariable () u (varGen "y") (Just $ intGen 3) Nothing ]
          let st = StDeclaration () u typeSpec
                                      (Just $ AList () u attrs)
                                      (AList () u declList)
          let expect = "character (len=3), intent(in), pointer :: x = 42, y*3"
          pprint Fortran90 st `shouldBe` expect

        it "prints 77 style" $ do
          let typeSpec = TypeSpec () u TypeInteger Nothing
          let dds = [ DimensionDeclarator () u Nothing (Just $ intGen 10) ]
          let declList =
                [ DeclArray () u (varGen "x") (AList () u dds) Nothing Nothing ]
          let st = StDeclaration () u typeSpec Nothing (AList () u declList)
          pprint Fortran77 st `shouldBe` "integer x(10)"

      describe "Intent" $
        it "prints intent statement" $ do
          let exps = [ varGen "x", varGen "y" ]
          let st = StIntent () u In (AList () u exps)
          pprint Fortran90 st `shouldBe` "intent (in) :: x, y"

      describe "Save" $ do
        it "prints lone save statement" $ do
          let st = StSave () u Nothing
          pprint Fortran90 st `shouldBe` "save"

        let st = StSave () u (Just $ AList () u [ varGen "x", varGen "y" ])

        it "prints 90 style save statement with vars" $
          pprint Fortran90 st `shouldBe` "save :: x, y"

        it "prints 77 style save statement with vars" $
          pprint Fortran77Extended st `shouldBe` "save x, y"

      describe "Data" $ do
        let groups =
              [ DataGroup () u (AList () u [ varGen "x"])
                               (AList () u [ intGen 42 ])
              , DataGroup () u (AList () u [ varGen "y"])
                               (AList () u [ intGen 24 ]) ]
        let st = StData () u (AList () u groups)

        it "prints 90 style data statement with multiple groups" $
          pprint Fortran90 st `shouldBe` "data x/42/, y/24/"

        it "prints 77 style data statement with multiple groups" $
          pprint Fortran77Extended st `shouldBe` "data x/42/ y/24/"

      describe "Parameter" $
        it "prints vanilla statement" $ do
          let decls = [ DeclVariable () u (varGen "x") Nothing (Just $ intGen 42)
                      , DeclVariable () u (varGen "y") Nothing (Just $ intGen 24)
                      ]
          let st = StParameter () u (AList () u decls)
          pprint Fortran90 st `shouldBe` "parameter (x = 42, y = 24)"

      describe "Equivalence" $
        it "prints multiple equivalence groups" $ do
          let equivGroups = [ AList () u [ varGen "x", varGen "y" ]
                            , AList () u [ varGen "z" ] ]
          let st = StEquivalence () u (AList () u equivGroups)
          pprint Fortran90 st `shouldBe` "equivalence (x, y), (z)"

      describe "Equivalence" $
        it "prints entry point with arguments and result specified" $ do
          let aargs = AList () u [ varGen "x", varGen "y" ]
          let result = varGen "z"
          let entry = StEntry () u (varGen "func") (Just aargs) (Just result)
          pprint Fortran90 entry `shouldBe` "entry func (x, y) result (z)"

      describe "Do" $ do
        it "prints infinity do" $ do
          let stDo = StDo () u Nothing Nothing Nothing
          pprint Fortran90 stDo `shouldBe` "do"

        let doInit = StExpressionAssign () u (varGen "i") (intGen (-1))
        let doSpec = DoSpecification () u doInit (intGen 5) Nothing

        it "prints labeled do" $ do
          let stDo = StDo () u Nothing (Just $ intGen 42) (Just doSpec)
          pprint Fortran90 stDo `shouldBe` "do 42 i = -1, 5"

        it "prints named do" $ do
          let stDo = StDo () u (Just "mistral") Nothing (Just doSpec)
          pprint Fortran90 stDo `shouldBe` "mistral: do i = -1, 5"

      describe "If" $ do
        it "prints arithmetic if" $ do
          let arIf = StIfArithmetic () u (intGen 0)
                (intGen 10) (intGen 20) (intGen 30)
          pprint Fortran66 arIf `shouldBe` "if (0) 10, 20, 30"

        it "prints logical if" $ do
          let as = StExpressionAssign () u (varGen "x") (intGen 42)
          let logIf = StIfLogical () u valFalse as
          pprint Fortran90 logIf `shouldBe` "if (.false.) x = 42"

        it "prints named if-then" $ do
          let ifThen = StIfThen () u (Just "mistral") valTrue
          pprint Fortran90 ifThen `shouldBe` "mistral: if (.true.) then"

      describe "Case" $ do
        it "prints select case" $ do
          let sc = StSelectCase () u Nothing (varGen "x")
          pprint Fortran90 sc `shouldBe` "select case (x)"

        it "prints filled case" $ do
          let caseRanges =
                [ IxRange () u (Just $ intGen 0) (Just $ intGen 100) Nothing
                , IxSingle () u Nothing (intGen 10) ]
          let casee = StCase () u Nothing (Just $ AList () u caseRanges)
          pprint Fortran90 casee `shouldBe` "case (0:100, 10)"

        it "prints named default case" $ do
          let casee = StCase () u (Just "mistral") Nothing
          pprint Fortran90 casee `shouldBe` "case default mistral"

      describe "Function statement" $
        it "prints function statement" $ do
          let args = [ varGen "x", varGen "y" ]
          let fSt = StFunction () u (varGen "pi") (AList () u args) (varGen "x")
          pprint Fortran90 fSt `shouldBe` "pi(x, y) = x"

      describe "Stop" $
        it "prints stop with code" $
          pprint Fortran66 (StStop () u (Just $ intGen 1)) `shouldBe` "stop 1"

      describe "IO" $
        describe "Print" $
          it "prints vanilla print statement" $ do
            let st = StPrint () u starVal (Just $ AList () u [ intGen 42 ])
            pprint Fortran90 st `shouldBe` "print *, 42"

      describe "Allocation" $
        describe "Allocate" $
          it "prints allocate statement" $ do
            let pair = ControlPair () u (Just "stat") (varGen "s")
            let st = StAllocate () u (AList () u [ varGen "x" ]) (Just pair)
            pprint Fortran90 st `shouldBe` "allocate (x, stat=s)"

      describe "Where" $
        it "prints statement" $ do
          let stAssign = StExpressionAssign () u (varGen "x") (intGen 42)
          let stWhere = StWhere () u valTrue stAssign
          pprint Fortran90 stWhere `shouldBe` "where (.true.) x = 42"

      describe "Use" $
        it "prints exlusive use statement" $ do
          let aRenames = AList () u [ UseRename () u (varGen "x") (varGen "y") ]
          let st = StUse () u (varGen "my_mod") Exclusive (Just aRenames)
          pprint Fortran90 st `shouldBe` "use my_mod, only: x => y"

valueExpressions :: Expression () -> Maybe (Expression ())
valueExpressions e@ExpValue{} = Just e
valueExpressions _ = Nothing

prop_pprintsize :: (SecondParameter (e ()) SrcSpan, Pretty (e ()))
                   => FortranVersion -> e () -> Spec
prop_pprintsize v e = do
  let pp = render $ pprint v e
  let span = getSecondParameter e
  it ("line length on: " ++ pp) $ lineDistance span `shouldBe` 0
  it ("col length on: " ++ pp)  $ length pp `shouldBe` (columnDistance span + 1)

flexReadFile :: String -> IO B.ByteString
flexReadFile = fmap (encodeUtf8 . decodeUtf8With (replace ' ')) . B.readFile
