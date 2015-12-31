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
  evalState expressionParser $ initParseState sourceCode Fortran66 "<unknown>"

evalStatementParser :: String -> Statement ()
evalStatementParser sourceCode = 
  evalState statementParser $ initParseState sourceCode Fortran66 "<unknown>"

intGen :: Integer -> Expression ()
intGen i = ExpValue () u $ ValInteger $ show i

u = undefined

spec :: Spec
spec = 
  describe "Fortran 66 Parser" $ do
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
        let expectedExp = resetSrcSpan $ ExpSubscript () u (ValArray "f") (AList () u [ExpValue () u (ValVariable "y"), intGen 24])
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

    describe "Statements" $ do
      it "parses 'EXTERNAL f, g, h'" $ do
        let procGen s = ExpValue () u (ValFunctionName s) 
        let expectedSt = resetSrcSpan $ StExternal (AList () u [procGen "f", procGen "g", procGen "h"])
        resetSrcSpan (evalStatementParser "      EXTERNAL f, g, h") `shouldBe` expectedSt

      it "parses 'COMMON a, b'" $ do
        let expectedSt = resetSrcSpan $ StCommon (AList () u [CommonGroup () u Nothing (AList () u [ExpValue () u (ValVariable "a"), ExpValue () u (ValVariable "b")])])
        resetSrcSpan (evalStatementParser "      COMMON a, b") `shouldBe` expectedSt

      it "parses 'COMMON // a, b /hello/ x, y, z'" $ do
        let expectedSt = resetSrcSpan $ StCommon (AList () u [CommonGroup () u Nothing (AList () u [ExpValue () u (ValVariable "a"), ExpValue () u (ValVariable "b")]), CommonGroup () u (Just "hello") (AList () u [ExpValue () u (ValVariable "x"), ExpValue () u (ValVariable "y"), ExpValue () u (ValVariable "z")])])
        resetSrcSpan (evalStatementParser "      COMMON // a, b /hello/ x, y, z") `shouldBe` expectedSt

      it "parses 'EQUIVALENCE (a,b), (x,y,z)'" $ do
        let valGen s = ExpValue () u (ValVariable s)
            expectedSt = resetSrcSpan $ StEquivalence (AList () u [AList () u [valGen "a", valGen "b"], AList () u [valGen "x", valGen "y", valGen "z"]])
        resetSrcSpan (evalStatementParser "      EQUIVALENCE (a,b), (x,y,z)") `shouldBe` expectedSt

      it "parses 'DATA a/1,2,3/,x/42/'" $ do
        let valGen s = ExpValue () u (ValVariable s)
            expectedSt = resetSrcSpan $ StData $ AList () u [DataGroup () u (AList () u [valGen "a"]) (AList () u [intGen 1, intGen 2, intGen 3]), DataGroup () u (AList () u [valGen "x"]) (AList () u [intGen 42])]
        resetSrcSpan (evalStatementParser "      DATA a/1,2,3/, x/42/") `shouldBe` expectedSt

      describe "FORMAT" $ do
        it "parses 'FORMAT ()'" $ do
          let expectedSt = resetSrcSpan $ StFormat $ AList () u []
          resetSrcSpan (evalStatementParser "      FORMAT ()") `shouldBe` expectedSt

        it "parses 'FORMAT (///)'" $ do
          let formatList = [FIDelimiter () u, FIDelimiter () u, FIDelimiter () u]
              expectedSt = resetSrcSpan $ StFormat $ AList () u formatList
          resetSrcSpan (evalStatementParser "      FORMAT (///)") `shouldBe` expectedSt

        it "parses 'FORMAT (2i5/5hhello)'" $ do
          let formatList = [FIFieldDescriptorAIL () u (Just 2) 'i' 5, FIDelimiter () u, FIHollerith () u (ValHollerith "hello")]
              expectedSt = resetSrcSpan $ StFormat $ AList () u formatList
          resetSrcSpan (evalStatementParser "      FORMAT (2i5/5hhello)") `shouldBe` expectedSt

{- ignore for now
        it "parses 'FORMAT (/(i5))'" $ do
          let formatList = [FIDelimiter () u, FIFormatList () u Nothing (AList () u [FIFieldDescriptorAIL () u Nothing 'i' 5])]
              expectedSt = resetSrcSpan $ StFormat $ AList () u formatList
          resetSrcSpan (evalStatementParser "      FORMAT (/(i5))") `shouldBe` expectedSt
-}
