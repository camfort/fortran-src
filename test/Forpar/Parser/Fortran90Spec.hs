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
