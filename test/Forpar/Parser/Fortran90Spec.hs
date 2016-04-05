module Forpar.Parser.Fortran90Spec (spec) where

import TestUtil
import Test.Hspec

import Forpar.AST
import Forpar.ParserMonad
import Forpar.Lexer.FreeForm
import Forpar.Parser.Fortran90

eParser :: String -> Expression ()
eParser sourceCode = 
  case evalParse statementParser parseState of
    (StExpressionAssign _ _ _ e) -> e
  where
    paddedSourceCode = "      a = " ++ sourceCode
    parseState =  initParseState paddedSourceCode Fortran90 "<unknown>"

sParser :: String -> Statement ()
sParser sourceCode = 
  evalParse statementParser $ initParseState sourceCode Fortran90 "<unknown>"

spec :: Spec
spec = 
  describe "Fortran 90 Parser" $ do
    describe "Expression" $ do
      it "parses logial literals with kind" $ do
        let expected = ExpValue () u (ValLogical ".true._kind")
        eParser ".true._kind" `shouldBe'` expected

      it "parses array initialisation exp" $ do
        let list = AList () u [ intGen 1, intGen 2, intGen 3, intGen 4 ]
        eParser "(/ 1, 2, 3, 4 /)" `shouldBe'` ExpInitialisation () u list

      describe "Custom operator" $ do
        let unOp = UnCustom ".inverse."
        let unExp = ExpUnary () u unOp $ intGen 42

        it "parses unary custom operator" $
          eParser ".inverse. 42" `shouldBe'` unExp

        let binOp = BinCustom ".xor."
        it "parses binary custom operator" $ do
          let expected = ExpBinary () u binOp (intGen 24) (intGen 42)
          eParser "24 .xor. 42" `shouldBe'` expected

        it "parses mixed unary custom operator" $ do
          let binExp = ExpBinary () u binOp unExp (intGen 24)
          eParser ".inverse. 42 .xor. 24" `shouldBe'` binExp
    describe "Statement" $ do
      it "parses declaration with attributes" $ do
        let typeSpec = TypeSpec () u TypeReal Nothing
        let attrs = AList () u [ AttrExternal () u
                               , AttrIntent () u Out
                               , AttrDimension () u $ AList () u 
                                  [ DimensionDeclarator () u 
                                      (Just $ intGen 3) (intGen 10)
                                  ]
                               ]
        let declarators = AList () u
              [ DeclVariable () u (varGen "x") Nothing Nothing
              , DeclVariable () u (varGen "y") Nothing Nothing ]
        let expected = StDeclaration () u typeSpec (Just attrs) declarators
        let stStr = "real, external, intent (out), dimension (3:10) :: x, y"
        sParser stStr `shouldBe'` expected

      it "parses declaration with old syntax" $ do
        let typeSpec = TypeSpec () u TypeLogical Nothing
        let declarators = AList () u
              [ DeclVariable () u (varGen "x") Nothing Nothing
              , DeclVariable () u (varGen "y") Nothing Nothing ]
        let expected = StDeclaration () u typeSpec Nothing declarators
        let stStr = "logical x, y"
        sParser stStr `shouldBe'` expected

      it "parses declaration with initialisation" $ do
        let typeSpec = TypeSpec () u TypeComplex Nothing
        let init = ExpValue () u (ValComplex (intGen 24) (realGen 42.0))
        let declarators = AList () u
              [ DeclVariable () u (varGen "x") Nothing (Just init) ]
        let expected = StDeclaration () u typeSpec Nothing declarators
        let stStr = "complex :: x = (24, 42.0)"
        sParser stStr `shouldBe'` expected

      it "parses declaration of custom type" $ do
        let typeSpec = TypeSpec () u (TypeCustom "meinetype") Nothing
        let declarators = AList () u
              [ DeclVariable () u (varGen "x") Nothing Nothing ]
        let expected = StDeclaration () u typeSpec Nothing declarators
        let stStr = "type (MeineType) :: x"
        sParser stStr `shouldBe'` expected

      it "parses declaration type with kind selector" $ do
        let selector = Selector () u Nothing (Just $ varGen "hello")
        let typeSpec = TypeSpec () u TypeInteger (Just selector)
        let declarators = AList () u
              [ DeclVariable () u (varGen "x") Nothing Nothing ]
        let expected = StDeclaration () u typeSpec Nothing declarators
        let stStr = "integer (hello) :: x"
        sParser stStr `shouldBe'` expected
