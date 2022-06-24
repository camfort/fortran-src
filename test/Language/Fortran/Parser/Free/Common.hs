{-| Common tests for free-form Fortran.

Fortran standards F90 and beyond are a lot more consistent than the
previous 2. As such, there is lots of shared parsing, and lots of shared
tests. This module encodes such shared/common tests, where no difference
in behaviour between parsers is be expected.
-}

module Language.Fortran.Parser.Free.Common ( specFreeCommon ) where

import TestUtil
import Test.Hspec

import Language.Fortran.AST
import Language.Fortran.AST.Literal.Real

specFreeCommon :: (String -> Block A0) -> (String -> Statement A0) -> (String -> Expression A0) -> Spec
specFreeCommon bParser sParser eParser =
  describe "Common Fortran 90+ (free form) tests" $ do
    describe "Literals" $ do
      describe "Logical" $ do
        it "parses logical literal without kind parameter" $ do
          eParser ".true." `shouldBe'` valTrue

        it "parses logical literal with kind parameter" $ do
          let kp = KindParamVar () u "kind"
          eParser ".false._kind" `shouldBe'` valFalse' kp

        it "parses mixed-case logical literal" $ do
          eParser ".tRUe." `shouldBe'` valTrue

      -- Main parse testing is performed in @Language.Fortran.AST.RealLitSpec@.
      -- Here we mainly want to test kind parameter and sign behaviour.
      describe "Real" $ do
        let realLitExp r mkp = ExpValue () u (ValReal (parseRealLit r) mkp)
        it "parses various REAL literals" $ do
          eParser "1."      `shouldBe'` realLitExp "1."    Nothing
          eParser ".1e20_8" `shouldBe'` realLitExp ".1e20" (Just (KindParamInt () u "8"))

        it "parses \"negative\" real literal (unary op)" $ do
          let lit = "-1.0d-1_k8"
              kp  = KindParamVar () u "k8"
           in eParser lit `shouldBe'` ExpUnary () u Minus (realLitExp "1.0d-1" (Just kp))

      describe "Kind parameters" $ do
        it "parses var kind parameter with underscore" $ do
          let lit = "123_a_1"
              kp  = KindParamVar () u "a_1"
           in eParser lit `shouldBe'` ExpValue () u (ValInteger "123" (Just kp))

        it "fails to parse invalid kind parameter with underscore after numeric" $ do
          {- TODO can't test here because we push parse errors into runtime ones
          let lit = "123_4_8"
           in eParser lit `shouldBe'` undefined -- should error "last parsed was the 2nd underscore"
          -}
          pending

      -- Check various real/int literal forms and some kind parameters.
      describe "Complex" $ do
        let kp = Just . KindParamInt () u
        it "parses a complex literal via positive reals" $ do
          let cr = ComplexPartReal () u (parseRealLit "1.0E0") (kp "8")
              ci = ComplexPartReal () u (parseRealLit "0.2E1") Nothing
          eParser "(1.0E0_8, 0.2E1)" `shouldBe'` complexGen cr ci
        it "parses a complex literal via positive mixed lits" $ do
          let cr = ComplexPartInt  () u "1"                  Nothing
              ci = ComplexPartReal () u (parseRealLit "2D0") Nothing
          eParser "(1, 2D0)" `shouldBe'` complexGen cr ci
        it "parses a complex literal via negative ints" $ do
          let cr = ComplexPartInt  () u "-1" Nothing
              ci = ComplexPartInt  () u "-2" Nothing
          eParser "(-1, -2)" `shouldBe'` complexGen cr ci
        it "parses a complex literal via mixed sign mixed lits with kind parameters" $ do
          let cr = ComplexPartReal () u (parseRealLit "-1.2") (kp "8")
              ci = ComplexPartInt  () u "0"                    Nothing
          eParser "(-1.2_8, 0)" `shouldBe'` complexGen cr ci
        it "parses a complex literal via named constants" $ do
          let cr = ComplexPartNamed () u "a"
              ci = ComplexPartNamed () u "b_something"
          eParser "(a, b_something)" `shouldBe'` complexGen cr ci

    describe "Statement" $ do
      describe "Declaration" $ do
        it "parses scalar declaration with nonstandard kind param (non-CHAR)" $ do
          let stStr    = "integer x*8"
              expected = StDeclaration () u typeSpec Nothing decls
              typeSpec = TypeSpec () u TypeInteger Nothing
              decls    = AList () u
                [ declVariable () u (varGen "x") (Just (intGen 8)) Nothing ]
          sParser stStr `shouldBe'` expected

        it "parses array declaration with nonstandard kind param (non-CHAR)" $ do
          let stStr    = "integer x(2)*8"
              expected = StDeclaration () u typeSpec Nothing decls
              typeSpec = TypeSpec () u TypeInteger Nothing
              decls    = AList () u
                [ declArray () u (varGen "x") dims (Just (intGen 8)) Nothing ]
              dims     = AList () u
                [ DimensionDeclarator () u Nothing (Just (intGen 2)) ]
          sParser stStr `shouldBe'` expected

        it "parses array declaration with nonstandard kind param (non-CHAR) and nonstandard dimension/charlen order" $ do
          let stStr    = "integer x*8(2)"
              expected = StDeclaration () u typeSpec Nothing decls
              typeSpec = TypeSpec () u TypeInteger Nothing
              decls    = AList () u
                [ declArray () u (varGen "x") dims (Just (intGen 8)) Nothing ]
              dims     = AList () u
                [ DimensionDeclarator () u Nothing (Just (intGen 2)) ]
          sParser stStr `shouldBe'` expected

      describe "Function call" $ do
        it "parses a simple function call" $ do
          let stStr    = "call double(i, i)"
              expected = StCall () u (varGen "double") (Just args)
              args     = AList () u [arg, arg]
              arg      = Argument () u Nothing (ArgExpr (varGen "i"))
          sParser stStr `shouldBe'` expected

        it "parses a parenthesized variable as a special indirect/copied variable reference" $ do
          let stStr    = "call double((i), i)"
              expected = StCall () u (varGen "double") (Just args)
              args     = AList () u [ genArg (ArgExprVar () u "i")
                                    , genArg (ArgExpr (varGen "i")) ]
              genArg   = Argument () u Nothing
          sParser stStr `shouldBe'` expected

      describe "Implicit" $ do
        it "parses implicit none" $ do
          let st = StImplicit () u Nothing
          sParser "implicit none" `shouldBe'` st

        it "parses implicit with single" $ do
          let typeSpec = TypeSpec () u TypeCharacter Nothing
              impEls = [ ImpElement () u 'k' Nothing ]
              impLists = [ ImpList () u typeSpec (fromList () impEls) ]
              st = StImplicit () u (Just $ fromList () impLists)
          sParser "implicit character (k)" `shouldBe'` st

        it "parses implicit with range" $ do
          let typeSpec = TypeSpec () u TypeLogical Nothing
              impEls = [ ImpElement () u 'x' (Just 'z') ]
              impLists = [ ImpList () u typeSpec (fromList () impEls) ]
              st = StImplicit () u (Just $ fromList () impLists)
          sParser "implicit logical (x-z)" `shouldBe'` st

        it "parses implicit statement" $ do
          let typeSpec1 = TypeSpec () u TypeCharacter Nothing
              typeSpec2 = TypeSpec () u TypeInteger Nothing
              impEls1 = [ ImpElement () u 's' Nothing, ImpElement () u 'a' Nothing ]
              impEls2 = [ ImpElement () u 'x' (Just 'z') ]
              impLists = [ ImpList () u typeSpec1 (fromList () impEls1)
                         , ImpList () u typeSpec2 (fromList () impEls2) ]
              st = StImplicit () u (Just $ fromList () impLists)
          sParser "implicit character (s, a), integer (x-z)" `shouldBe'` st

    describe "Block" $ do
      describe "Case" $ do
        let printArgs str = Just $ AList () u [ExpValue () u $ ValString str]
            printStmt = StPrint () u (ExpValue () u ValStar) . printArgs
            ind2 = AList () u . pure $ IxSingle () u Nothing $ intGen 2
            ind3Plus = AList () u . pure $ IxRange () u (Just $ intGen 3) Nothing Nothing

        it "unlabelled case block (with inline comments to be stripped)" $ do
          let printBlock = BlStatement () u Nothing . printStmt
              clauses = [(ind2, [printBlock "foo"]), (ind3Plus, [printBlock "bar"])]
              caseDef = Just [printBlock "baz"]
          let src = unlines [ "select case (x) ! comment select"
                            , "! full line before first case (unrepresentable)"
                            , "case (2) ! comment case 1"
                            , "print *, 'foo'"
                            , "case (3:) ! comment case 2"
                            , "print *, 'bar'"
                            , "case default ! comment case 3"
                            , "print *, 'baz'"
                            , "end select ! comment end"
                            ]
              block = BlCase () u Nothing Nothing (varGen "x") clauses caseDef Nothing
          bParser src `shouldBe'` block

        it "labelled case block (with inline comments to be stripped" $ do
          let printBlock label = BlStatement () u (Just $ intGen label) . printStmt
              clauses = [(ind2, [printBlock 30 "foo"]), (ind3Plus, [printBlock 50 "bar"])]
              caseDef = Just [printBlock 70 "baz"]
          let src = unlines [ "10 mylabel: select case (x) ! comment select"
                            , "20 case (2) ! comment case 1"
                            , "30 print *, 'foo'"
                            , "40 case (3:) ! comment case 2"
                            , "50 print *, 'bar'"
                            , "60 case default ! comment case 3"
                            , "70 print *, 'baz'"
                            , "80 end select mylabel ! comment end"
                            ]
              block = BlCase () u
                             (Just $ intGen 10) (Just "mylabel") (varGen "x")
                             clauses caseDef
                             (Just $ intGen 80)
          bParser src `shouldBe'` block

      describe "If" $ do
        let stPrint = StPrint () u starVal (Just $ fromList () [ ExpValue () u (ValString "foo")])
            inner   = [BlStatement () u Nothing stPrint]
        it "parser if block" $
          let ifBlockSrc = unlines [ "if (.false.) then", "print *, 'foo'", "end if"]
              ifBlock = BlIf () u Nothing Nothing ((valFalse, inner) :| []) Nothing Nothing
          in  bParser ifBlockSrc `shouldBe'` ifBlock

        it "parses named if block" $ do
          let ifBlockSrc = unlines [ "mylabel : if (.true.) then", "print *, 'foo'", "end if mylabel"]
              ifBlock = BlIf () u Nothing (Just "mylabel") ((valTrue, inner) :| []) Nothing Nothing
          bParser ifBlockSrc `shouldBe'` ifBlock

        it "parses if-else block with inline comments (stripped)" $
          let ifBlockSrc = unlines [ "if (.false.) then ! comment if", "print *, 'foo'", "else ! comment else", "print *, 'foo'", "end if ! comment end"]
              ifBlock = BlIf () u Nothing Nothing ((valFalse, inner) :| []) (Just inner) Nothing
          in  bParser ifBlockSrc `shouldBe'` ifBlock

        it "parses logical if statement" $ do
          let assignment = StExpressionAssign () u (varGen "a") (varGen "b")
              stIf = StIfLogical () u valTrue assignment
          sParser "if (.true.) a = b" `shouldBe'` stIf

        it "parses arithmetic if statement" $ do
          let stIf = StIfArithmetic () u (varGen "x") (intGen 1)
                                                      (intGen 2)
                                                      (intGen 3)
          sParser "if (x) 1, 2, 3" `shouldBe'` stIf
