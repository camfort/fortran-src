module Forpar.Parser.Fortran77Spec where

import Test.Hspec

import Forpar.Parser.Fortran77
import Forpar.Lexer.FixedForm (initParseState)
import Forpar.ParserMonad (FortranVersion(..), evalParse)
import Forpar.AST

u = undefined

eParser :: String -> Expression ()
eParser sourceCode = 
  evalParse expressionParser $ initParseState sourceCode Fortran77 "<unknown>"

sParser :: String -> Statement ()
sParser sourceCode = 
  evalParse statementParser $ initParseState sourceCode Fortran77 "<unknown>"

pParser :: String -> [ProgramUnit ()]
pParser source = fortran77Parser source "<unknown>"

varGen :: String -> Expression ()
varGen str = ExpValue () u $ ValVariable str

parGen :: String -> Expression ()
parGen str = ExpValue () u $ ValParameter str

intGen :: Integer -> Expression ()
intGen i = ExpValue () u $ ValInteger $ show i

strGen :: String -> Expression ()
strGen str = ExpValue () u $ ValString $ str

labelGen :: Integer -> Expression ()
labelGen i = ExpValue () u $ ValLabel $ show i

arrGen :: String -> Expression ()
arrGen str = ExpValue () u $ ValArray str

starVal :: Expression ()
starVal = ExpValue () u ValStar

spec :: Spec
spec = 
  describe "Fortran 77 Parser" $ do
    it "parses main program unit" $ do
      let decl = DeclVariable () u (varGen "x")
      let st = StDeclaration () u (TypeInteger () u) (AList () u [ decl ])
      let bl = BlStatement () u st []
      let pu = resetSrcSpan $ PUMain () u (Just "hello") [(Nothing, bl)] []
      resetSrcSpan (pParser exampleProgram1) `shouldBe` [pu]

    it "parses block data unit" $ do
      let decl = DeclVariable () u (varGen "x")
      let st = StDeclaration () u (TypeInteger () u) (AList () u [ decl ])
      let bl = BlStatement () u st []
      let pu = resetSrcSpan $ PUBlockData () u (Just "hello") [(Nothing, bl)] []
      resetSrcSpan (pParser exampleProgram2) `shouldBe` [pu]

    it "parses 'intrinsic cosh, sin'" $ do
      let fun1 = ExpValue () u (ValFunctionName "cosh")
      let fun2 = ExpValue () u (ValFunctionName "sin")
      let st = resetSrcSpan $ StIntrinsic () u (AList () u [ fun1, fun2 ])
      resetSrcSpan (sParser $ "      intrinsic cosh, sin") `shouldBe` st

    describe "CHARACTER" $ do
      it "parses character literal assignment" $ do
        let rhs = ExpValue () u (ValString "hello 'baby")
        let st = resetSrcSpan $ StExpressionAssign () u (varGen "xyz") rhs
        resetSrcSpan (sParser $ "      xyz = 'hello ''baby'") `shouldBe` st

      it "string concatination" $ do
        let str1 = ExpValue () u (ValString "hello ")
        let str2 = ExpValue () u (ValString "world")
        let exp = resetSrcSpan $ ExpBinary () u Concatination str1 str2
        resetSrcSpan (eParser $ "      'hello ' // 'world'") `shouldBe` exp

    describe "Subscript like" $ do
      it "parses vanilla subscript" $ do
        let exp = resetSrcSpan $ ExpSubscript () u (arrGen "a") (AList () u [ varGen "x", intGen 2, intGen 3 ])
        resetSrcSpan (eParser $ "      a(x, 2, 3)") `shouldBe` exp

      it "parses array declarator" $ do
        let dimDecls = [ DimensionDeclarator () u (Just $ intGen 1) (intGen 2)
                       , DimensionDeclarator () u Nothing (intGen 15)
                       , DimensionDeclarator () u (Just $ varGen "x") starVal ]
        let decl = DeclArray () u (arrGen "a") (AList () u dimDecls)
        let st = resetSrcSpan $ StDeclaration () u (TypeInteger () u) (AList () u [ decl ])
        resetSrcSpan (sParser $ "      integer a(1:2, 15, x:*)") `shouldBe` st

      it "parses character substring" $ do
        let indicies = [ intGen 1, intGen 2, intGen 3 ]
        let subExp = ExpSubscript () u (arrGen "a")  (AList () u indicies)
        let exp = resetSrcSpan $ ExpSubstring () u subExp Nothing (Just $ intGen 10)
        resetSrcSpan (eParser $ "      a(1, 2, 3)(:10)") `shouldBe` exp

      it "parses simpler substring" $ do
        let exp = resetSrcSpan $ ExpSubstring () u (arrGen "a") (Just $ intGen 5) (Just $ intGen 10)
        resetSrcSpan (eParser $ "      a(5:10)") `shouldBe` exp

      it "parses simpler substring" $ do
        let exp = resetSrcSpan $ ExpSubstring () u (arrGen "a") (Just $ intGen 5) Nothing
        resetSrcSpan (eParser $ "      a(5:)") `shouldBe` exp

    describe "GOTO" $ do
      it "parses computed GOTO with integer expression" $ do
        let exp = ExpBinary () u Multiplication (intGen 42) (intGen 24)
        let st = resetSrcSpan $ StGotoComputed () u (AList () u [labelGen 10, labelGen 20, labelGen 30]) exp
        resetSrcSpan (sParser "      GOTO (10, 20, 30), 42 * 24") `shouldBe` st

      let gotoSt = resetSrcSpan $ StGotoAssigned () u (varGen "v") (AList () u [labelGen 10, labelGen 20, labelGen 30])
      it "parses assigned GOTO with comma" $ do
        resetSrcSpan (sParser "      GOTO v, (10, 20, 30)") `shouldBe` gotoSt

      it "parses assigned GOTO without comma" $ do
        resetSrcSpan (sParser "      GOTO v (10, 20, 30)") `shouldBe` gotoSt

    describe "IMPLICIT" $ do
      it "parses 'implicit none'" $ do
        let st = resetSrcSpan $ StImplicit () u Nothing
        resetSrcSpan (sParser "      implicit none") `shouldBe` st

      it "parses 'implicit character*30 (a, b, c), integer (a-z, l)" $ do
        let imp1 = ImpList () u (TypeCharacter () u (Just $ intGen 30)) $ AList () u [ImpCharacter () u "a", ImpCharacter () u "b", ImpCharacter () u "c"]
        let imp2 = ImpList () u (TypeInteger () u) $ AList () u [ImpRange () u "a" "z", ImpCharacter () u "l"]
        let st = resetSrcSpan $ StImplicit () u $ Just $ AList () u [imp1, imp2]
        resetSrcSpan (sParser "      implicit character*30 (a, b, c), integer (a-z, l)") `shouldBe` st

    it "parses 'parameter (pi = 3.14, b = 'X' // 'O', d = k) '" $ do
      let sts = [ StExpressionAssign () u (parGen "pi") (ExpValue () u (ValReal "3.14"))
                , StExpressionAssign () u (parGen "b") (ExpBinary () u Concatination (strGen "x") (strGen "o"))
                , StExpressionAssign () u (parGen "d") (parGen "k") ] 
      let st = resetSrcSpan $ StParameter () u (AList () u sts)
      resetSrcSpan (sParser "      parameter (pi = 3.14, b = 'X' // 'O', d = k)") `shouldBe` st

exampleProgram1 = unlines
  [ "      program hello"
  , "      integer x"
  , "      end" ]

exampleProgram2 = unlines
  [ "      block data hello"
  , "      integer x"
  , "      end" ]
