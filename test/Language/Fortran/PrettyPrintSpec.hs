{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fortran.PrettyPrintSpec where

import Prelude hiding (mod)

import Data.Data
import Data.Generics.Uniplate.Operations
import Data.Maybe (catMaybes)

import Language.Fortran.AST as LFA
import Language.Fortran.ParserMonad
import Language.Fortran.PrettyPrint

import System.FilePath
import Text.PrettyPrint hiding ((<>))
import Text.PrettyPrint.GenericPretty

import Test.Hspec
import TestUtil

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
    describe "Dimension declarator" $ do
      it "Prints left bound dimension declarator" $ do
        let dd = DimensionDeclarator () u (Just $ intGen 42) Nothing
        pprint Fortran90 dd Nothing `shouldBe` "42:"

      it "Prints right bound dimension declarator" $ do
        let dd = DimensionDeclarator () u Nothing (Just $ intGen 42)
        pprint Fortran90 dd Nothing `shouldBe` "42"

      it "Prints bounded dimension declarator" $ do
        let dd = DimensionDeclarator () u (Just $ intGen 24) (Just $ intGen 42)
        pprint Fortran90 dd Nothing `shouldBe` "24:42"

    describe "Selector" $ do
      it "prints Fortran 77 selector" $ do
        let sel = Selector () u (Just $ intGen 42) Nothing
        pprint Fortran77 sel Nothing `shouldBe` "*42"
        let ksel = Selector () u Nothing (Just $ intGen 4)
        pprint Fortran77 ksel Nothing `shouldBe` "*4"
        let vsel = Selector () u (Just $ varGen "v") Nothing
        pprint Fortran77 vsel Nothing `shouldBe` "*(v)"

      it "prints Fortran 90 selector" $ do
        let sel = Selector () u (Just $ intGen 42) (Just $ intGen 24)
        pprint Fortran90 sel Nothing `shouldBe` "(len=42, kind=24)"

    describe "Use" $
      it "prints renaming" $ do
        let renaming = UseRename () u (varGen "x") (varGen "y")
        pprint Fortran90 renaming Nothing `shouldBe` "x => y"

    describe "Control pair" $
      it "prints named control pair" $ do
        let cp = ControlPair () u (Just "errno") (intGen 42)
        pprint Fortran77Extended cp Nothing `shouldBe` "errno=42"

    describe "Implicit list" $
      it "prints mixed implicit lists" $ do
        let typ = TypeSpec () u TypeInteger Nothing
        let impEls = [ ImpCharacter () u "x"
                     , ImpRange () u "a" "z"
                     , ImpCharacter () u "o" ]
        let impList = ImpList () u typ (AList () u impEls)
        pprint Fortran90 impList Nothing `shouldBe` "integer (x, a-z, o)"

    describe "Common group" $ do
      let globs = [ varGen "x", varGen "y", varGen "z" ]

      it "prints anonymous common group" $ do
        let group = CommonGroup () u Nothing (AList () u globs)
        pprint Fortran66 group Nothing `shouldBe` "//x, y, z"

      it "prints named common group" $ do
        let group = CommonGroup () u (Just $ varGen "my_g") (AList () u globs)
        pprint Fortran66 group Nothing `shouldBe` "/my_g/x, y, z"

    describe "Format item" $
      it "prints hollerith constant" $ do
        let ed = FIHollerith () u (ValHollerith "hello darling")
        pprint Fortran77 ed Nothing `shouldBe` "13hhello darling"

    describe "Flush statement" $
      it "prints flush statement" $ do
        let f = StFlush () u (AList () u [ FSUnit () u (intGen 1), FSIOStat () u (varGen "x")
                                         , FSIOMsg () u (varGen "y"), FSErr () u (varGen "z") ])
        pprint Fortran2003 f Nothing `shouldBe` "flush (unit=1, iostat=x, iomsg=y, err=z)"

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
          let expect = "character(len=3), intent(in), pointer :: x = 42, y*3"
          pprint Fortran90 st Nothing `shouldBe` expect

        it "prints 77 style" $ do
          let typeSpec = TypeSpec () u TypeInteger Nothing
          let dds = [ DimensionDeclarator () u Nothing (Just $ intGen 5) ]
          let declList =
                [ DeclArray () u (varGen "x") (AList () u dds) Nothing
                            (Just . initGen $ map intGen [1..5])
                ]
          let st = StDeclaration () u typeSpec Nothing (AList () u declList)
          pprint Fortran77 st Nothing `shouldBe` "integer x(5)/1, 2, 3, 4, 5/"

      describe "Intent" $
        it "prints intent statement" $ do
          let exps = [ varGen "x", varGen "y" ]
          let st = StIntent () u In (AList () u exps)
          pprint Fortran90 st Nothing `shouldBe` "intent (in) :: x, y"

      describe "Save" $ do
        it "prints lone save statement" $ do
          let st = StSave () u Nothing
          pprint Fortran90 st Nothing `shouldBe` "save"

        let st = StSave () u (Just $ AList () u [ varGen "x", varGen "y" ])

        it "prints 90 style save statement with vars" $
          pprint Fortran90 st Nothing `shouldBe` "save :: x, y"

        it "prints 77 style save statement with vars" $
          pprint Fortran77Extended st Nothing `shouldBe` "save x, y"

      describe "Data" $ do
        let groups =
              [ DataGroup () u (AList () u [ varGen "x"])
                               (AList () u [ intGen 42 ])
              , DataGroup () u (AList () u [ varGen "y"])
                               (AList () u [ intGen 24 ]) ]
        let st = StData () u (AList () u groups)

        it "prints 90 style data statement with multiple groups" $
          pprint Fortran90 st Nothing `shouldBe` "data x/42/, y/24/"

        it "prints 77 style data statement with multiple groups" $
          pprint Fortran77Extended st Nothing `shouldBe` "data x/42/ y/24/"

      describe "Parameter" $
        it "prints vanilla statement" $ do
          let decls = [ DeclVariable () u (varGen "x") Nothing (Just $ intGen 42)
                      , DeclVariable () u (varGen "y") Nothing (Just $ intGen 24)
                      ]
          let st = StParameter () u (AList () u decls)
          pprint Fortran90 st Nothing `shouldBe` "parameter (x = 42, y = 24)"

      describe "Equivalence" $
        it "prints multiple equivalence groups" $ do
          let equivGroups = [ AList () u [ varGen "x", varGen "y" ]
                            , AList () u [ varGen "z" ] ]
          let st = StEquivalence () u (AList () u equivGroups)
          pprint Fortran90 st Nothing `shouldBe` "equivalence (x, y), (z)"

      describe "Equivalence" $
        it "prints entry point with arguments and result specified" $ do
          let aargs = AList () u [ varGen "x", varGen "y" ]
          let result = varGen "z"
          let entry = StEntry () u (varGen "func") (Just aargs) (Just result)
          pprint Fortran90 entry Nothing `shouldBe` "entry func (x, y) result (z)"

      describe "Do" $ do
        it "prints infinity do" $ do
          let stDo = StDo () u Nothing Nothing Nothing
          pprint Fortran90 stDo Nothing `shouldBe` "do"

        let doInit = StExpressionAssign () u (varGen "i") (intGen (-1))
        let doSpec = DoSpecification () u doInit (intGen 5) Nothing

        it "prints labeled do" $ do
          let stDo = StDo () u Nothing (Just $ intGen 42) (Just doSpec)
          pprint Fortran90 stDo Nothing `shouldBe` "do 42 i = -1, 5"

        it "prints named do" $ do
          let stDo = StDo () u (Just "mistral") Nothing (Just doSpec)
          pprint Fortran90 stDo Nothing `shouldBe` "mistral: do i = -1, 5"

      describe "If" $ do
        it "prints arithmetic if" $ do
          let arIf = StIfArithmetic () u (intGen 0)
                (intGen 10) (intGen 20) (intGen 30)
          pprint Fortran66 arIf Nothing `shouldBe` "if (0) 10, 20, 30"

        it "prints logical if" $ do
          let as = StExpressionAssign () u (varGen "x") (intGen 42)
          let logIf = StIfLogical () u valFalse as
          pprint Fortran90 logIf Nothing `shouldBe` "if (.false.) x = 42"

      describe "Case" $ do
        it "prints select case" $ do
          let sc = StSelectCase () u Nothing (varGen "x")
          pprint Fortran90 sc Nothing `shouldBe` "select case (x)"

        it "prints filled case" $ do
          let caseRanges =
                [ IxRange () u (Just $ intGen 0) (Just $ intGen 100) Nothing
                , IxSingle () u Nothing (intGen 10) ]
          let casee = StCase () u Nothing (Just $ AList () u caseRanges)
          pprint Fortran90 casee Nothing `shouldBe` "case (0:100, 10)"

        it "prints named default case" $ do
          let casee = StCase () u (Just "mistral") Nothing
          pprint Fortran90 casee Nothing `shouldBe` "case default mistral"

      describe "Function statement" $
        it "prints function statement" $ do
          let args = [ varGen "x", varGen "y" ]
          let fSt = StFunction () u (varGen "pi") (AList () u args) (varGen "x")
          pprint Fortran90 fSt Nothing `shouldBe` "pi(x, y) = x"

      describe "Stop" $
        it "prints stop with code" $
          pprint Fortran66 (StStop () u (Just $ intGen 1)) Nothing `shouldBe` "stop 1"

      describe "IO" $
        describe "Print" $
          it "prints vanilla print statement" $ do
            let st = StPrint () u starVal (Just $ AList () u [ intGen 42 ])
            pprint Fortran90 st Nothing `shouldBe` "print *, 42"

      describe "Allocation" $
        describe "Allocate" $ do
          it "prints allocate statement" $ do
            let stat = AOStat () u (varGen "s")
            let st = StAllocate () u Nothing (AList () u [ varGen "x" ]) (Just (AList () u [stat]))
            pprint Fortran90 st Nothing `shouldBe` "allocate (x, stat=s)"
          it "prints allocate statement with type spec" $ do
            let stat = AOStat () u (varGen "s")
            let sel = Selector () u (Just (intGen 30)) Nothing
            let ty = TypeSpec () u TypeCharacter (Just sel)
            let st = StAllocate () u (Just ty) (AList () u [ varGen "x" ]) (Just (AList () u [stat]))
            pprint Fortran2003 st Nothing `shouldBe` "allocate (character(len=30) :: x, stat=s)"

      describe "Where" $
        it "prints statement" $ do
          let stAssign = StExpressionAssign () u (varGen "x") (intGen 42)
          let stWhere = StWhere () u valTrue stAssign
          pprint Fortran90 stWhere Nothing `shouldBe` "where (.true.) x = 42"

      describe "Use" $ do
        it "prints exclusive use statement" $ do
          let aRenames = AList () u [ UseRename () u (varGen "x") (varGen "y") ]
          let st = StUse () u (varGen "my_mod") Nothing Exclusive (Just aRenames)
          pprint Fortran90 st Nothing `shouldBe` "use my_mod, only: x => y"

        it "prints intrinsic use statement" $ do
          let aRenames = AList () u [ UseRename () u (varGen "x") (varGen "y") ]
          let st = StUse () u (varGen "my_mod") (Just ModIntrinsic) Exclusive (Just aRenames)
          pprint Fortran2003 st Nothing `shouldBe` "use, intrinsic :: my_mod, only: x => y"

        it "prints non_intrinsic use statement" $ do
          let aRenames = AList () u [ UseRename () u (varGen "x") (varGen "y") ]
          let st = StUse () u (varGen "my_mod") (Just ModNonIntrinsic) Exclusive (Just aRenames)
          pprint Fortran2003 st Nothing `shouldBe` "use, non_intrinsic :: my_mod, only: x => y"

    let decrementRHS = ExpBinary () u Subtraction (varGen "i") (intGen 1)
    let st1 = StPrint () u starVal (Just $ AList () u [ varGen "i" ])
    let st2 = StExpressionAssign () u (varGen "i") decrementRHS
    let body = [ BlStatement () u Nothing st1 , BlStatement () u Nothing st2 ]

    describe "Blocks" $ do
      describe "Comment" $ do
        let blComment = BlComment () u (Comment " si vis pacem para bellum")

        it "prints 90 style comment" $
          pprint Fortran90 blComment Nothing `shouldBe` "! si vis pacem para bellum\n"

        it "prints 66 style comment" $
          pprint Fortran66 blComment Nothing `shouldBe` "c si vis pacem para bellum\n"

      describe "Statement" $
        it "prints vanilla print" $ do
          let st = StPrint () u starVal Nothing
          let bl = BlStatement () u (Just $ intGen 42) st
          pprint Fortran90 bl Nothing `shouldBe` "42 print *\n"

      describe "Interface" $
        it "prints interface block" pending

      describe "Do While" $ do
        it "prints simple do while loop" $ do
          let cond = ExpBinary () u LFA.GT (varGen "i") (intGen 42)
          let bl = BlDoWhile () u Nothing (Just "my_block") Nothing cond body Nothing
          let expect = unlines [ "my_block: do while ((i > 42))"
                               , "print *, i"
                               , "i = (i - 1)"
                               , "end do my_block" ]
          pprint Fortran90 bl Nothing `shouldBe` text expect

        it "prints a labelled do while loop" $ do
          let cond = ExpBinary () u LFA.GT (varGen "i") (intGen 42)
          let bl = BlDoWhile () u Nothing Nothing (Just (intGen 10)) cond body Nothing
          let expect = unlines [ "do 10 while ((i > 42))"
                               , "print *, i"
                               , "i = (i - 1)" ]
          pprint Fortran90 bl Nothing `shouldBe` text expect

      describe "Do" $ do
        let iAssign = StExpressionAssign () u (varGen "i") (intGen 1)
        let doSpec = DoSpecification () u iAssign (intGen 9) (Just (intGen 2))

        it "prints 90 style do loop" $ do
          let bl = BlDo () u Nothing Nothing Nothing (Just doSpec) body Nothing
          let expect = unlines [ "do i = 1, 9, 2"
                               , "print *, i"
                               , "i = (i - 1)"
                               , "end do" ]
          pprint Fortran90 bl Nothing `shouldBe` text expect

        it "prints named infinite do loop" $ do
          let bl = BlDo () u Nothing (Just "joker") Nothing Nothing body Nothing
          let expect = unlines [ "joker: do"
                               , "print *, i"
                               , "i = (i - 1)"
                               , "end do joker" ]
          pprint Fortran90 bl Nothing `shouldBe` text expect

        it "prints named labeled do loop" $ do
          let bl = BlDo () u Nothing (Just "joker") (Just $ intGen 42) (Just doSpec) body (Just $ intGen 42)
          let expect = unlines [ "joker: do 42 i = 1, 9, 2"
                               , "print *, i"
                               , "i = (i - 1)"
                               , "42 end do joker" ]
          pprint Fortran90 bl Nothing `shouldBe` text expect

        it "prints vanilla labeled do loop" $ do
          let body2 = body ++ [ BlStatement () u (Just $ intGen 42) (StContinue () u) ]
          let bl = BlDo () u Nothing Nothing (Just $ intGen 42) (Just doSpec) body2 (Just $ intGen 42)
          let expect = unlines [ "      do 42 i = 1, 9, 2"
                               , "        print *, i"
                               , "        i = (i - 1)"
                               , "42      continue" ]
          pprint Fortran77 bl (Just 6) `shouldBe` text expect

      describe "If" $ do
        it "prints vanilla structured if" $ do
          let bl = BlIf () u Nothing Nothing [ Just valTrue ] [ body ] Nothing
          let expect = unlines [ "if (.true.) then"
                               , "print *, i"
                               , "i = (i - 1)"
                               , "end if" ]
          pprint Fortran90 bl Nothing `shouldBe` text expect

        it "prints multiple condition named structured if" $ do
          let conds = [ Just valTrue, Just valFalse, Just valTrue, Nothing ]
          let bodies = replicate 4 body
          let bl = BlIf () u Nothing (Just "mistral") conds bodies Nothing
          let expect = unlines [ "mistral: if (.true.) then"
                               , "  print *, i"
                               , "  i = (i - 1)"
                               , "else if (.false.) then"
                               , "  print *, i"
                               , "  i = (i - 1)"
                               , "else if (.true.) then"
                               , "  print *, i"
                               , "  i = (i - 1)"
                               , "else"
                               , "  print *, i"
                               , "  i = (i - 1)"
                               , "end if mistral" ]
          pprint Fortran90 bl (Just 0) `shouldBe` text expect

      describe "Case" $
        it "prints complicated structured if" $ do
          let range = IxRange () u (Just $ intGen 2) (Just $ intGen 4) Nothing
          let cases = [ Just (AList () u [range])
                      , Just (AList () u [ IxSingle () u Nothing (intGen 7) ])
                      , Nothing ]
          let bodies = replicate 3 body
          let bl = BlCase () u Nothing Nothing (varGen "x") cases bodies (Just (intGen 42))
          let expect = unlines [ "select case (x)"
                               , "  case (2:4)"
                               , "    print *, i"
                               , "    i = (i - 1)"
                               , "  case (7)"
                               , "    print *, i"
                               , "    i = (i - 1)"
                               , "  case default"
                               , "    print *, i"
                               , "    i = (i - 1)"
                               , "42 end select" ]
          pprint Fortran90 bl (Just 0) `shouldBe` text expect

    describe "Program units" $ do
      describe "Main" $ do
        it "prints 90 style main without sub programs" $ do
          let main = PUMain () u (Just "main") body Nothing
          let expect = unlines [ "program main"
                               , "print *, i"
                               , "i = (i - 1)"
                               , "end program main" ]
          pprint Fortran90 main Nothing `shouldBe` text expect

        it "prints 66 style main" $ do
          let main = PUMain () u Nothing body Nothing
          let expect = unlines [ "      print *, i"
                               , "      i = (i - 1)"
                               , "      end" ]
          pprint Fortran66 main (Just 0) `shouldBe` text expect

      describe "Module" $ do
        it "prints module without sub programs" $ do
          let mod = PUModule () u "my_mod" body Nothing
          let expect = unlines [ "module my_mod"
                               , "  print *, i"
                               , "  i = (i - 1)"
                               , "end module my_mod" ]
          pprint Fortran90 mod (Just 0) `shouldBe` text expect

        it "prints module with sub programs" $ do
          let sub = PUSubroutine () u emptyPrefixSuffix "sub" Nothing body Nothing
          let mod = PUModule () u "my_mod" body (Just [ sub ])
          let expect = unlines [ "   module my_mod"
                               , "     print *, i"
                               , "     i = (i - 1)"
                               , ""
                               , "     contains"
                               , ""
                               , "     subroutine sub"
                               , "       print *, i"
                               , "       i = (i - 1)"
                               , "     end subroutine sub"
                               , "   end module my_mod" ]
          pprint Fortran90 mod (Just 3) `shouldBe` text expect

      describe "Subroutine" $ do
        it "prints recursive subroutine with args without sub programs" $ do
          let args = AList () u [ varGen "x", varGen "y", varGen "z" ]
          let sub = PUSubroutine () u (Just (AList () u [PfxRecursive () u]), emptySuffixes) "sub" (Just args) body Nothing
          let expect = unlines [ "recursive subroutine sub(x, y, z)"
                               , "print *, i"
                               , "i = (i - 1)"
                               , "end subroutine sub" ]
          pprint Fortran90 sub Nothing `shouldBe` text expect

        it "prints 66 style subroutine without args" $ do
          let mod = PUSubroutine () u emptyPrefixSuffix "sub" Nothing body Nothing
          let expect = unlines [ "      subroutine sub"
                               , "        print *, i"
                               , "        i = (i - 1)"
                               , "      end" ]
          pprint Fortran66 mod Nothing `shouldBe` text expect

      describe "Function" $ do
        let tSpec = Just $ TypeSpec () u TypeInteger Nothing

        it "prints function with args with result without sub programs" $ do
          let args = AList () u [ varGen "x", varGen "y", varGen "z" ]
          let res = Just $ varGen "i"
          let fun = PUFunction () u tSpec emptyPrefixSuffix "f" (Just args) res body Nothing
          let expect = unlines [ "  integer function f(x, y, z) result(i)"
                               , "    print *, i"
                               , "    i = (i - 1)"
                               , "  end function f" ]
          pprint Fortran90 fun (Just 2) `shouldBe` text expect

    describe "Program file" $
      it "prints simple program file" $ do
        let body' = [ BlStatement () u Nothing (StContinue () u) ]
        let pu = PUModule () u "my_mod" body' Nothing
        let com = PUComment () u (Comment "hello!")
        let pf = ProgramFile mi77 [com, pu, com, pu, com, com]
        let expect = unlines [ "!hello!"
                             , "module my_mod"
                             , "  continue"
                             , "end module my_mod"
                             , "!hello!"
                             , "module my_mod"
                             , "  continue"
                             , "end module my_mod"
                             , "!hello!"
                             , "!hello!" ]
        pprint Fortran90 pf (Just 0) `shouldBe` text expect

    describe "Continuation reformatting" $ do
      it "continuates a too-long 77 style statement" $ do
        let input  = "      integer, parameter :: "
                  <> "very_awfully_really_quite_pointlessly_long_s"
                  <> "illy_variable_name = 1"
            expect = "      integer, parameter :: "
                  <> "very_awfully_really_quite_pointlessly_long_s"
                  <> "&\n     &"
                  <> "illy_variable_name = 1"
        reformatMixedFormInsertContinuations input `shouldBe` expect

      it "does not continuate a long mixed-form comment line" $ do
        let input  = "      ! a very long, long comment that ends up"
                  <> " exceeding both the F77 and F90 maximum line lengths"
                  <> " (~72 and ~132 respectively), but should not be"
                  <> " continuated (since they're ignored by compilers anyway)"
        reformatMixedFormInsertContinuations input `shouldBe` input

      it "does not continuate a long F77 comment line" $ do
        let input  = "c F77 comments begin with a 'c' in the first column"
                  <> " position, so they're easy to handle"
        reformatMixedFormInsertContinuations input `shouldBe` input

      it "adds continuations idempotently" $ do
        let input  = "      integer, parameter :: "
                  <> "very_awfully_really_quite_pointlessly_long_s"
                  <> "illy_variable_name = 1"
            pass1 = reformatMixedFormInsertContinuations input
            pass2 = reformatMixedFormInsertContinuations pass1
        pass2 `shouldBe` pass1

      it "does not continuate lines exactly 72 columns" $ do
        let input  = "      integer, parameter :: "
                  <> "variable_id_making_line_exactly_72_chars = 1"
        reformatMixedFormInsertContinuations input `shouldBe` input

valueExpressions :: Expression () -> Maybe (Expression ())
valueExpressions e@ExpValue{} = Just e
valueExpressions _ = Nothing
