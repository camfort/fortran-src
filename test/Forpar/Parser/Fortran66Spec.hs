module Forpar.Parser.Fortran66Spec(spec) where

import Test.Hspec

import Control.Monad.State.Lazy
import Prelude hiding (LT)

import Forpar.Parser.Fortran66
import Forpar.Lexer.FixedForm
import Forpar.ParserMonad
import Forpar.AST

import Debug.Trace
import Data.Typeable

evalExpressionParser :: String -> Expression ()
evalExpressionParser sourceCode = 
  evalParse expressionParser $ initParseState sourceCode Fortran66 "<unknown>"

evalStatementParser :: String -> Statement ()
evalStatementParser sourceCode = 
  evalParse statementParser $ initParseState sourceCode Fortran66 "<unknown>"

intGen :: Integer -> Expression ()
intGen i = ExpValue () u $ ValInteger $ show i

labelGen :: Integer -> Expression ()
labelGen i = ExpValue () u $ ValLabel $ show i

varGen :: String -> Expression ()
varGen str = ExpValue () u $ ValVariable str

arrGen :: String -> Expression ()
arrGen str = ExpValue () u $ ValArray str

u = undefined

spec :: Spec
spec = 
  describe "Fortran 66 Parser" $ do
    describe "Expressions" $ do
      describe "Arithmetic expressions" $ do
        it "parses '3'" $ do
          let expectedExp = resetSrcSpan $ intGen 3
          resetSrcSpan (evalExpressionParser "      3") `shouldBe` expectedExp

        it "parses '-3'" $ do
          let expectedExp = resetSrcSpan $ ExpUnary () u Minus $ intGen 3
          resetSrcSpan (evalExpressionParser "      -3") `shouldBe` expectedExp

        it "parses '3 + 2'" $ do
          let expectedExp = resetSrcSpan $ ExpBinary () u Addition (intGen 3) (intGen 2)
          resetSrcSpan (evalExpressionParser "      3 + 2") `shouldBe` expectedExp

        it "parses '3 + -2'" $ do
          let expectedExp = resetSrcSpan $ ExpBinary () u Addition (intGen 3) (ExpUnary () u Minus (intGen 2))
          resetSrcSpan (evalExpressionParser "      3 + -2") `shouldBe` expectedExp

        it "parses '3 + -2 + 42'" $ do
          let expectedExp = resetSrcSpan $ ExpBinary () u Addition (ExpBinary () u Addition (intGen 3) (ExpUnary () u Minus (intGen 2))) (intGen 42)
          resetSrcSpan (evalExpressionParser "      3 + -2 + 42") `shouldBe` expectedExp

        it "parses 'f(y, 24)'" $ do
          let expectedExp = resetSrcSpan $ ExpSubscript () u (arrGen "f") (AList () u [ExpValue () u (ValVariable "y"), intGen 24])
          resetSrcSpan (evalExpressionParser "      f(y, 24)") `shouldBe` expectedExp

        it "parses '3 + 4 * 12'" $ do
          let expectedExp = resetSrcSpan $ ExpBinary () u Addition (intGen 3) (ExpBinary () u Multiplication (intGen 4) (intGen 12))
          resetSrcSpan (evalExpressionParser "      3 + 4 * 12") `shouldBe` expectedExp

      describe "Logical expressions" $ do
        it "parses '.true. .and. .false.'" $ do
          let expectedExp = resetSrcSpan $ ExpBinary () u And (ExpValue () u (ValTrue)) (ExpValue () u (ValFalse)) 
          resetSrcSpan (evalExpressionParser "      .true. .and. .false.") `shouldBe` expectedExp

      describe "Relational expressions" $ do
        it "parses '(3 * 2) .lt. 42'" $ do
          let expectedExp = resetSrcSpan $ ExpBinary () u LT (ExpBinary () u Multiplication (intGen 3) (intGen 2)) (intGen 42)
          resetSrcSpan (evalExpressionParser "      (3 * 2) .lt. 42") `shouldBe` expectedExp

      describe "Other expressions" $ do
        it "parses 'a(2 * x - 3, 10)'" $ do
          let firstEl = ExpBinary () u Subtraction (ExpBinary () u Multiplication (intGen 2) (varGen "x")) (intGen 3)
              expectedExp = resetSrcSpan $ ExpSubscript () u (arrGen "a") (AList () u [firstEl, intGen 10])
          resetSrcSpan (evalExpressionParser "      a(2 * x - 3, 10)") `shouldBe` expectedExp

    describe "Statements" $ do
      it "parses 'EXTERNAL f, g, h'" $ do
        let procGen s = ExpValue () u (ValFunctionName s) 
        let expectedSt = resetSrcSpan $ StExternal () u (AList () u [procGen "f", procGen "g", procGen "h"])
        resetSrcSpan (evalStatementParser "      EXTERNAL f, g, h") `shouldBe` expectedSt

      it "parses 'COMMON a, b'" $ do
        let expectedSt = resetSrcSpan $ StCommon () u (AList () u [CommonGroup () u Nothing (AList () u [ExpValue () u (ValVariable "a"), ExpValue () u (ValVariable "b")])])
        resetSrcSpan (evalStatementParser "      COMMON a, b") `shouldBe` expectedSt

      it "parses 'COMMON // a, b /hello/ x, y, z'" $ do
        let expectedSt = resetSrcSpan $ StCommon () u (AList () u [CommonGroup () u Nothing (AList () u [ExpValue () u (ValVariable "a"), ExpValue () u (ValVariable "b")]), CommonGroup () u (Just "hello") (AList () u [ExpValue () u (ValVariable "x"), ExpValue () u (ValVariable "y"), ExpValue () u (ValVariable "z")])])
        resetSrcSpan (evalStatementParser "      COMMON // a, b /hello/ x, y, z") `shouldBe` expectedSt

      it "parses 'EQUIVALENCE (a,b), (x,y,z)'" $ do
        let valGen s = ExpValue () u (ValVariable s)
            expectedSt = resetSrcSpan $ StEquivalence () u (AList () u [AList () u [valGen "a", valGen "b"], AList () u [valGen "x", valGen "y", valGen "z"]])
        resetSrcSpan (evalStatementParser "      EQUIVALENCE (a,b), (x,y,z)") `shouldBe` expectedSt

      it "parses 'DATA a/1,2,3/,x/42/'" $ do
        let valGen s = ExpValue () u (ValVariable s)
            expectedSt = resetSrcSpan $ StData () u $ AList () u [DataGroup () u (AList () u [valGen "a"]) (AList () u [intGen 1, intGen 2, intGen 3]), DataGroup () u (AList () u [valGen "x"]) (AList () u [intGen 42])]
        resetSrcSpan (evalStatementParser "      DATA a/1,2,3/, x/42/") `shouldBe` expectedSt

      describe "FORMAT" $ do
        it "parses 'FORMAT ()'" $ do
          let expectedSt = resetSrcSpan $ StFormat () u $ AList () u []
          resetSrcSpan (evalStatementParser "      FORMAT ()") `shouldBe` expectedSt

        it "parses 'FORMAT (///)'" $ do
          let formatList = [FIDelimiter () u, FIDelimiter () u, FIDelimiter () u]
              expectedSt = resetSrcSpan $ StFormat () u $ AList () u formatList
          resetSrcSpan (evalStatementParser "      FORMAT (///)") `shouldBe` expectedSt

        it "parses 'FORMAT (2i5/5hhello)'" $ do
          let formatList = [FIFieldDescriptorAIL () u (Just 2) 'i' 5, FIDelimiter () u, FIHollerith () u (ValHollerith "hello")]
              expectedSt = resetSrcSpan $ StFormat () u $ AList () u formatList
          resetSrcSpan (evalStatementParser "      FORMAT (2i5/5hhello)") `shouldBe` expectedSt

        it "parses 'FORMAT (/(i5))'" $ do
          let formatList = [FIDelimiter () u, FIFormatList () u Nothing (AList () u [FIFieldDescriptorAIL () u Nothing 'i' 5])]
              expectedSt = resetSrcSpan $ StFormat () u $ AList () u formatList
          resetSrcSpan (evalStatementParser "      FORMAT (/(i5))") `shouldBe` expectedSt

      describe "CALL" $ do
        it "parses 'CALL me" $ do
          let expectedSt = resetSrcSpan $ StCall () u (ExpValue () u (ValSubroutineName "me")) Nothing
          resetSrcSpan (evalStatementParser "      CALL me") `shouldBe` expectedSt

        it "parses 'CALL me(baby)" $ do
          let args = AList () u [varGen "baby"]
          let expectedSt = resetSrcSpan $ StCall () u (ExpValue () u (ValSubroutineName "me")) $ Just args
          resetSrcSpan (evalStatementParser "      CALL me(baby)") `shouldBe` expectedSt

      it "parses 'stop'" $ do
        let expectedSt = resetSrcSpan $ StStop () u Nothing
        resetSrcSpan (evalStatementParser "      stop") `shouldBe` expectedSt

      it "parses 'integer i, j(2,2), k'" $ do
        let declarators = [varGen "i", ExpSubscript () u (arrGen "j") (AList () u [intGen 2, intGen 2]), varGen "k"] 
            expectedSt = resetSrcSpan $ StDeclaration () u TypeInteger $ AList () u declarators
        resetSrcSpan (evalStatementParser "      integer i, j(2,2), k") `shouldBe` expectedSt

      describe "WRITE" $ do
        it "parses 'write (6)'" $ do
          let expectedSt = resetSrcSpan $ StWrite () u (intGen 6) Nothing Nothing
          resetSrcSpan (evalStatementParser "      write (6)") `shouldBe` expectedSt

        it "parses 'write (6) i'" $ do
          let expectedSt = resetSrcSpan $ StWrite () u (intGen 6) Nothing (Just $ AList () u [IOExpression $ varGen "i"])
          resetSrcSpan (evalStatementParser "      write (6) i") `shouldBe` expectedSt

        it "parses 'write (6,10) i'" $ do
          let expectedSt = resetSrcSpan $ StWrite () u (intGen 6) (Just $ labelGen 10) (Just $ AList () u [IOExpression $ varGen "i"])
          resetSrcSpan (evalStatementParser "      write (6,10) i") `shouldBe` expectedSt

      describe "IF" $ do
        it "parses 'if (10 .LT. x) write (6,10) i'" $ do
          let writeSt = StWrite () u (intGen 6) (Just $ labelGen 10) (Just $ AList () u [IOExpression $ varGen "i"])
          let cond = ExpBinary () u LT (intGen 10) (varGen "x")
          let expectedSt = resetSrcSpan $ StIfLogical () u cond writeSt
          resetSrcSpan (evalStatementParser "      if (10 .LT. x) write (6,10) i") `shouldBe` expectedSt

        it "parses 'if (10 - 5) 10, 20, 30'" $ do
          let cond = ExpBinary () u Subtraction (intGen 10) (intGen 5)
          let expectedSt = resetSrcSpan $ StIfArithmetic () u cond (labelGen 10) (labelGen 20) (labelGen 30)
          resetSrcSpan (evalStatementParser "      if (10 - 5) 10, 20, 30") `shouldBe` expectedSt

      describe "ASSIGNMENT" $ do
        it "parses 'f = 1'" $ do
          let expectedSt = resetSrcSpan $ StExpressionAssign () u (varGen "f") (intGen 1)
          resetSrcSpan (evalStatementParser "      f = 1") `shouldBe` expectedSt

        it "parses 'f = a(1,2)'" $ do
          let rhs = ExpSubscript () u (ExpValue () u (ValArray "a")) (AList () u [intGen 1, intGen 2])
          let expectedSt = resetSrcSpan $ StExpressionAssign () u (varGen "f") rhs
          resetSrcSpan (evalStatementParser "      f = a(1,2)") `shouldBe` expectedSt

      it "parses 'do 42 i = 10, 1, 1'" $ do
        let st = StExpressionAssign () u (varGen "i") (intGen 10)
        let expectedSt = resetSrcSpan $ StDo () u (labelGen 42) st (intGen 1) (Just $ intGen 1)
        resetSrcSpan (evalStatementParser "      do 42 i = 10, 1, 1") `shouldBe` expectedSt
