module Forpar.Parser.Fortran66Spec where

import Test.Hspec

import Control.Monad.State.Lazy

import Forpar.Parser.Fortran66
import Forpar.Lexer.FixedForm
import Forpar.ParserMonad
import Forpar.AST

import Debug.Trace
import Data.Typeable

evalExpressionParser :: String -> Expression ()
evalExpressionParser sourceCode = 
  evalState expressionParser $ initParseState sourceCode Fortran66 "<unknown>"

intGen :: Integer -> Expression ()
intGen i = ExpValue () undefined $ ValInteger $ show i

spec :: Spec
spec = 
  describe "Fortran 66 Parser" $ do
    it "parses '3'" $ do
      let expectedExp = resetSrcSpan $ intGen 3
      resetSrcSpan (evalExpressionParser "      3") `shouldBe` expectedExp

    it "parses '-3'" $ do
      let expectedExp = resetSrcSpan $ ExpUnary () undefined Minus $ intGen 3
      resetSrcSpan (evalExpressionParser "      -3") `shouldBe` expectedExp

    it "parses '3 + 2'" $ do
      let expectedExp = resetSrcSpan $ ExpBinary () undefined Addition (intGen 3) (intGen 2)
      evalExpressionParser "      3 + 2" `shouldBe` expectedExp
