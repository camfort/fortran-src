module Forpar.Parser.Fortran77Spec where

import Test.Hspec
import TestUtil

import Forpar.Parser.Fortran77
import Forpar.Lexer.FixedForm (initParseState)
import Forpar.ParserMonad (FortranVersion(..), evalParse)
import Forpar.AST

eParser :: String -> Expression ()
eParser sourceCode =
  case evalParse statementParser parseState of
    (StExpressionAssign _ _ _ e) -> e
  where
    paddedSourceCode = "      a = " ++ sourceCode
    parseState =  initParseState paddedSourceCode Fortran77 "<unknown>"

sParser :: String -> Statement ()
sParser sourceCode =
  evalParse statementParser $ initParseState sourceCode Fortran77 "<unknown>"

pParser :: String -> ProgramFile ()
pParser source = fortran77Parser source "<unknown>"

spec :: Spec
spec =
  describe "Fortran 77 Parser" $ do
    describe "IO" $ do
      it "parses 'print *, 9000" $ do
        let expectedSt = StPrint () u starVal $ Just (AList () u [ intGen 9000 ])
        sParser "      print *, 9000" `shouldBe'` expectedSt

      it "parses 'write (UNIT=6, FORMAT=*)" $ do
        let cp1 = ControlPair () u (Just "unit") (intGen 6)
        let cp2 = ControlPair () u (Just "format") starVal
        let expectedSt = StWrite () u (AList () u [cp1, cp2]) Nothing
        sParser "      write (UNIT=6, FORMAT=*)" `shouldBe'` expectedSt

      it "parses 'endfile i" $
        sParser "      endfile i" `shouldBe'` StEndfile2 () u (varGen "i")

      it "parses 'read *, (x, y(i), i = 1, 10, 2)'" $ do
        let stAssign = StExpressionAssign () u (varGen "i") (intGen 1)
        let doSpec = DoSpecification () u stAssign (intGen 10) (Just $ intGen 2)
        let impliedDoVars = AList () u [ varGen "x", ExpSubscript () u (varGen "y") (AList () u [ IxSingle () u $ varGen "i" ])]
        let impliedDo = ExpImpliedDo () u impliedDoVars doSpec
        let iolist = AList () u [ impliedDo ]
        let expectedSt = StRead2 () u starVal (Just iolist)
        sParser "      read *, (x, y(i), i = 1, 10, 2)" `shouldBe'` expectedSt

    it "parses '(x, y(i), i = 1, 10, 2)'" $ do
      let stAssign = StExpressionAssign () u (varGen "i") (intGen 1)
      let doSpec = DoSpecification () u stAssign (intGen 10) (Just $ intGen 2)
      let impliedDoVars = AList () u [ varGen "x", ExpSubscript () u (varGen "y") (AList () u [ IxSingle () u $ varGen "i" ])]
      let impliedDo = ExpImpliedDo () u impliedDoVars doSpec
      eParser "(x, y(i), i = 1, 10, 2)" `shouldBe'` impliedDo

    it "parses main program unit" $ do
      let decl = DeclVariable () u (varGen "x") Nothing Nothing
      let st = StDeclaration () u (TypeSpec () u TypeInteger Nothing) Nothing (AList () u [ decl ])
      let bl = BlStatement () u Nothing st
      let pu = ProgramFile [ ([ ], PUMain () u (Just "hello") [ bl ]) ] [ ]
      pParser exampleProgram1 `shouldBe'` pu

    it "parses block data unit" $ do
      let decl = DeclVariable () u (varGen "x") Nothing Nothing
      let st = StDeclaration () u (TypeSpec () u TypeInteger Nothing) Nothing (AList () u [ decl ])
      let bl = BlStatement () u Nothing st
      let pu = ProgramFile [ ([ ], PUBlockData () u (Just "hello") [ bl ]) ] [ ]
      pParser exampleProgram2 `shouldBe'` pu

    it "parses 'intrinsic cosh, sin'" $ do
      let fun1 = ExpValue () u (ValFunctionName "cosh")
      let fun2 = ExpValue () u (ValFunctionName "sin")
      let st = StIntrinsic () u (AList () u [ fun1, fun2 ])
      sParser "      intrinsic cosh, sin" `shouldBe'` st

    it "parses 'intrinsic real" $ do
      let fun = ExpValue () u (ValFunctionName "real")
      let st = StIntrinsic () u (AList () u [ fun ])
      sParser "      intrinsic real" `shouldBe'` st

    describe "CHARACTER" $ do
      it "parses character literal assignment" $ do
        let rhs = ExpValue () u (ValString "hello 'baby")
        let st = StExpressionAssign () u (varGen "xyz") rhs
        sParser "      xyz = 'hello ''baby'" `shouldBe'` st

      it "string concatenation" $ do
        let str1 = ExpValue () u (ValString "hello ")
        let str2 = ExpValue () u (ValString "world")
        let exp = ExpBinary () u Concatenation str1 str2
        eParser "'hello ' // 'world'" `shouldBe'` exp

    describe "Subscript like" $ do
      it "parses vanilla subscript" $ do
        let exp = ExpSubscript () u (varGen "a") (AList () u [ IxSingle () u $ varGen "x", IxSingle () u $ intGen 2, IxSingle () u $ intGen 3 ])
        eParser "a(x, 2, 3)" `shouldBe'` exp

      it "parses array declarator" $ do
        let dimDecls = [ DimensionDeclarator () u (Just $ intGen 1) (intGen 2)
                       , DimensionDeclarator () u Nothing (intGen 15)
                       , DimensionDeclarator () u (Just $ varGen "x") starVal ]
        let decl = DeclArray () u (varGen "a") (AList () u dimDecls) Nothing Nothing
        let st = StDeclaration () u (TypeSpec () u TypeInteger Nothing) Nothing (AList () u [ decl ])
        sParser "      integer a(1:2, 15, x:*)" `shouldBe'` st

      it "parses character substring" $ do
        let indicies = [ ixSinGen 1, ixSinGen 2, ixSinGen 3 ]
        let subExp = ExpSubscript () u (varGen "a")  (AList () u indicies)
        let range = IxRange () u Nothing (Just $ intGen 10) Nothing
        let exp = ExpSubscript () u subExp (AList () u [ range ])
        eParser "a(1, 2, 3)(:10)" `shouldBe'` exp

      it "parses simpler substring" $ do
        let exp = ExpSubscript () u (varGen "a") (AList () u [ ixRanGen 5 10 ])
        eParser "a(5:10)" `shouldBe'` exp

      it "parses simpler substring" $ do
        let range = IxRange () u (Just $ intGen 5) Nothing Nothing
        let exp = ExpSubscript () u (varGen "a") (AList () u [ range ])
        eParser "a(5:)" `shouldBe'` exp

    describe "GOTO" $ do
      it "parses computed GOTO with integer expression" $ do
        let exp = ExpBinary () u Multiplication (intGen 42) (intGen 24)
        let st = StGotoComputed () u (AList () u [labelGen 10, labelGen 20, labelGen 30]) exp
        sParser "      GOTO (10, 20, 30), 42 * 24" `shouldBe'` st

      let gotoSt = StGotoAssigned () u (varGen "v") (AList () u [labelGen 10, labelGen 20, labelGen 30])
      it "parses assigned GOTO with comma" $
        sParser "      GOTO v, (10, 20, 30)" `shouldBe'` gotoSt

      it "parses assigned GOTO without comma" $
        sParser "      GOTO v (10, 20, 30)" `shouldBe'` gotoSt

    describe "IMPLICIT" $ do
      it "parses 'implicit none'" $ do
        let st = resetSrcSpan $ StImplicit () u Nothing
        sParser "      implicit none" `shouldBe'` st

      it "parses 'implicit character*30 (a, b, c), integer (a-z, l)" $ do
        let impEls = [ImpCharacter () u "a", ImpCharacter () u "b", ImpCharacter () u "c"]
        let selector = Selector () u (Just $ intGen 30) Nothing
        let imp1 = ImpList () u (TypeSpec () u TypeCharacter (Just selector)) $ AList () u impEls
        let imp2 = ImpList () u (TypeSpec () u TypeInteger Nothing) $ AList () u [ImpRange () u "a" "z", ImpCharacter () u "l"]
        let st = StImplicit () u $ Just $ AList () u [imp1, imp2]
        sParser "      implicit character*30 (a, b, c), integer (a-z, l)" `shouldBe'` st

    it "parses 'parameter (pi = 3.14, b = 'X' // 'O', d = k) '" $ do
      let sts = [ DeclVariable () u (varGen "pi") Nothing (Just $ realGen 3.14)
                , let e = ExpBinary () u Concatenation (strGen "x") (strGen "o")
                  in DeclVariable () u (varGen "b") Nothing (Just e)
                , DeclVariable () u (varGen "d") Nothing (Just $ varGen "k") ]
      let st = StParameter () u (AList () u sts)
      sParser "      parameter (pi = 3.14, b = 'X' // 'O', d = k)" `shouldBe'` st

    it "parses 'pause 'hello world''" $ do
      let st = StPause () u $ Just $ strGen "hello world"
      sParser "      pause 'hello world'" `shouldBe'` st

    describe "SAVE" $ do
      it "parses 'save /cb/, var, /key/'" $ do
        let saveArgs = [ cbNameGen "cb", varGen "var", cbNameGen "key" ]
        let st = StSave () u (Just $ AList () u saveArgs)
        sParser "      save /cb/, var, /key/" `shouldBe'` st

      it "parses 'save'" $
        sParser "      save" `shouldBe'` StSave () u Nothing

    it "parses '.true. .eqv. f(42) .neqv. x'" $ do
      let arg2 = ExpSubscript () u (varGen "f") $ AList () u [ ixSinGen 42 ]
      let arg3 = varGen "x"
      let subexp = ExpBinary () u Equivalent valTrue arg2
      let exp = ExpBinary () u NotEquivalent subexp arg3
      eParser ".true. .eqv. f(42) .neqv. x" `shouldBe'` exp

    it "parses 'entry me (a,b,*)'" $ do
      let func = ExpValue () u (ValFunctionName "me")
      let args = [ varGen "a", varGen "b", starVal ]
      let st = StEntry () u func (Just $ AList () u args)
      sParser "      entry me (a,b,*)" `shouldBe'` st

exampleProgram1 = unlines
  [ "      program hello"
  , "      integer x"
  , "      end" ]

exampleProgram2 = unlines
  [ "      block data hello"
  , "      integer x"
  , "      end" ]

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl test-suite:spec"
-- End:
