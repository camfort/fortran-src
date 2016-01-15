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

intGen :: Integer -> Expression ()
intGen i = ExpValue () u $ ValInteger $ show i

labelGen :: Integer -> Expression ()
labelGen i = ExpValue () u $ ValLabel $ show i

spec :: Spec
spec = 
  describe "Fortran 77 Parser" $ do
    it "parses main program unit" $ do
      let st = StDeclaration () u (TypeInteger () u) (AList () u [varGen "x"])
      let bl = BlStatement () u st []
      let pu = resetSrcSpan $ PUMain () u (Just "hello") [(Nothing, bl)] []
      resetSrcSpan (pParser exampleProgram1) `shouldBe` [pu]

    it "parses block data unit" $ do
      let st = StDeclaration () u (TypeInteger () u) (AList () u [varGen "x"])
      let bl = BlStatement () u st []
      let pu = resetSrcSpan $ PUBlockData () u (Just "hello") [(Nothing, bl)] []
      resetSrcSpan (pParser exampleProgram2) `shouldBe` [pu]

    it "parses 'intrinsic cosh, sin'" $ do
      let fun1 = ExpValue () u (ValFunctionName "cosh")
      let fun2 = ExpValue () u (ValFunctionName "sin")
      let st = resetSrcSpan $ StIntrinsic () u (AList () u [ fun1, fun2 ])
      resetSrcSpan (sParser $ "      intrinsic cosh, sin") `shouldBe` st

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

exampleProgram1 = unlines
  [ "      program hello"
  , "      integer x"
  , "      end" ]

exampleProgram2 = unlines
  [ "      block data hello"
  , "      integer x"
  , "      end" ]
