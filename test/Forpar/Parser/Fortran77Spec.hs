module Forpar.Parser.Fortran77Spec where

import Test.Hspec

import Forpar.Parser.Fortran77
import Forpar.Lexer.FixedForm (initParseState)
import Forpar.ParserMonad (FortranVersion(..), evalParse)
import Forpar.AST

u = undefined

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

cbNameGen :: String -> Expression ()
cbNameGen str = ExpValue () u $ ValCommonName str

starVal :: Expression ()
starVal = ExpValue () u ValStar

shouldBe' a b = resetSrcSpan a `shouldBe` resetSrcSpan b

spec :: Spec
spec = 
  describe "Fortran 77 Parser" $ do
    it "parses main program unit" $ do
      let decl = DeclVariable () u (varGen "x")
      let st = StDeclaration () u (TypeInteger () u) (AList () u [ decl ])
      let bl = BlStatement () u st []
      let pu = PUMain () u (Just "hello") [(Nothing, bl)] []
      pParser exampleProgram1 `shouldBe'` [pu]

    it "parses block data unit" $ do
      let decl = DeclVariable () u (varGen "x")
      let st = StDeclaration () u (TypeInteger () u) (AList () u [ decl ])
      let bl = BlStatement () u st []
      let pu = PUBlockData () u (Just "hello") [(Nothing, bl)] []
      pParser exampleProgram2 `shouldBe'` [pu]

    it "parses 'intrinsic cosh, sin'" $ do
      let fun1 = ExpValue () u (ValFunctionName "cosh")
      let fun2 = ExpValue () u (ValFunctionName "sin")
      let st = StIntrinsic () u (AList () u [ fun1, fun2 ])
      sParser "      intrinsic cosh, sin" `shouldBe'` st

    describe "CHARACTER" $ do
      it "parses character literal assignment" $ do
        let rhs = ExpValue () u (ValString "hello 'baby")
        let st = StExpressionAssign () u (varGen "xyz") rhs
        sParser "      xyz = 'hello ''baby'" `shouldBe'` st

      it "string concatination" $ do
        let str1 = ExpValue () u (ValString "hello ")
        let str2 = ExpValue () u (ValString "world")
        let exp = ExpBinary () u Concatination str1 str2
        eParser "'hello ' // 'world'" `shouldBe'` exp

    describe "Subscript like" $ do
      it "parses vanilla subscript" $ do
        let exp = ExpSubscript () u (arrGen "a") (AList () u [ varGen "x", intGen 2, intGen 3 ])
        eParser "a(x, 2, 3)" `shouldBe'` exp

      it "parses array declarator" $ do
        let dimDecls = [ DimensionDeclarator () u (Just $ intGen 1) (intGen 2)
                       , DimensionDeclarator () u Nothing (intGen 15)
                       , DimensionDeclarator () u (Just $ varGen "x") starVal ]
        let decl = DeclArray () u (arrGen "a") (AList () u dimDecls)
        let st = StDeclaration () u (TypeInteger () u) (AList () u [ decl ])
        sParser "      integer a(1:2, 15, x:*)" `shouldBe'` st

      it "parses character substring" $ do
        let indicies = [ intGen 1, intGen 2, intGen 3 ]
        let subExp = ExpSubscript () u (arrGen "a")  (AList () u indicies)
        let exp = ExpSubstring () u subExp Nothing (Just $ intGen 10)
        eParser "a(1, 2, 3)(:10)" `shouldBe'` exp

      it "parses simpler substring" $ do
        let exp = ExpSubstring () u (arrGen "a") (Just $ intGen 5) (Just $ intGen 10)
        eParser "a(5:10)" `shouldBe'` exp

      it "parses simpler substring" $ do
        let exp = ExpSubstring () u (arrGen "a") (Just $ intGen 5) Nothing
        eParser "a(5:)" `shouldBe'` exp

    describe "GOTO" $ do
      it "parses computed GOTO with integer expression" $ do
        let exp = ExpBinary () u Multiplication (intGen 42) (intGen 24)
        let st = StGotoComputed () u (AList () u [labelGen 10, labelGen 20, labelGen 30]) exp
        sParser "      GOTO (10, 20, 30), 42 * 24" `shouldBe'` st

      let gotoSt = StGotoAssigned () u (varGen "v") (AList () u [labelGen 10, labelGen 20, labelGen 30])
      it "parses assigned GOTO with comma" $ do
        sParser "      GOTO v, (10, 20, 30)" `shouldBe'` gotoSt

      it "parses assigned GOTO without comma" $ do
        sParser "      GOTO v (10, 20, 30)" `shouldBe'` gotoSt

    describe "IMPLICIT" $ do
      it "parses 'implicit none'" $ do
        let st = resetSrcSpan $ StImplicit () u Nothing
        sParser "      implicit none" `shouldBe'` st

      it "parses 'implicit character*30 (a, b, c), integer (a-z, l)" $ do
        let impEls = [ImpCharacter () u "a", ImpCharacter () u "b", ImpCharacter () u "c"]
        let imp1 = ImpList () u (TypeCharacter () u (Just $ intGen 30)) $ AList () u impEls
        let imp2 = ImpList () u (TypeInteger () u) $ AList () u [ImpRange () u "a" "z", ImpCharacter () u "l"]
        let st = StImplicit () u $ Just $ AList () u [imp1, imp2]
        sParser "      implicit character*30 (a, b, c), integer (a-z, l)" `shouldBe'` st

    it "parses 'parameter (pi = 3.14, b = 'X' // 'O', d = k) '" $ do
      let sts = [ StExpressionAssign () u (parGen "pi") (ExpValue () u (ValReal "3.14"))
                , StExpressionAssign () u (parGen "b") (ExpBinary () u Concatination (strGen "x") (strGen "o"))
                , StExpressionAssign () u (parGen "d") (parGen "k") ] 
      let st = StParameter () u (AList () u sts)
      sParser "      parameter (pi = 3.14, b = 'X' // 'O', d = k)" `shouldBe'` st

    it "parses 'pause 'hello world''" $ do
      let st = StPause () u $ Just $ strGen "hello world"
      sParser "      pause 'hello world'" `shouldBe'` st

    describe "SAVE" $ do
      it "parses 'save /cb/, var, /key/'" $ do
        let saveArgs = [ cbNameGen "cb", varGen "var", cbNameGen "key" ]
        let st = StSave () u (AList () u saveArgs)
        sParser "      save /cb/, var, /key/" `shouldBe'` st

      it "parses 'save'" $ do
        sParser "      save" `shouldBe'` StSave () u (AList () u [])

    it "parses '.true. .eqv. f(42) .neqv. x'" $ do
      let arg1 = ExpValue () u ValTrue
      let arg2 = ExpSubscript () u (arrGen "f") $ AList () u [ intGen 42 ]
      let arg3 = varGen "x"
      let subexp = ExpBinary () u Equivalent arg1 arg2
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
