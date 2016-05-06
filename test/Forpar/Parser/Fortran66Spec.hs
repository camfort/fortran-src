module Forpar.Parser.Fortran66Spec(spec) where

import Test.Hspec
import TestUtil

import Control.Monad.State.Lazy
import Prelude hiding (LT)

import Forpar.Parser.Fortran66
import Forpar.Lexer.FixedForm
import Forpar.ParserMonad
import Forpar.AST

import Data.Typeable

eParser :: String -> Expression ()
eParser sourceCode =
  case evalParse statementParser parseState of
    (StExpressionAssign _ _ _ e) -> e
  where
    paddedSourceCode = "      a = " ++ sourceCode
    parseState =  initParseState paddedSourceCode Fortran66 "<unknown>"

sParser :: String -> Statement ()
sParser sourceCode =
  evalParse statementParser $ initParseState sourceCode Fortran66 "<unknown>"

spec :: Spec
spec =
  describe "Fortran 66 Parser" $ do
    describe "Expressions" $ do
      describe "Arithmetic expressions" $ do
        describe "Real numbers" $ do
          it "parses 'hello" $ do
            let expectedExp = varGen "hello"
            eParser "hello" `shouldBe'` expectedExp

          it "parses '3.14" $ do
            let expectedExp = ExpValue () u (ValReal "3.14")
            eParser "3.14" `shouldBe'` expectedExp

          it "parses '.14" $ do
            let expectedExp = ExpValue () u (ValReal ".14")
            eParser ".14" `shouldBe'` expectedExp

          it "parses '3." $ do
            let expectedExp = ExpValue () u (ValReal "3.")
            eParser "3." `shouldBe'` expectedExp

          it "parses '3E12" $ do
            let expectedExp = ExpValue () u (ValReal "3e12")
            eParser "3E12" `shouldBe'` expectedExp

          it "parses '3.14d12" $ do
            let expectedExp = ExpValue () u (ValReal "3.14d12")
            eParser "3.14d12" `shouldBe'` expectedExp

          it "parses '.14d+1" $ do
            let expectedExp = ExpValue () u (ValReal ".14d+1")
            eParser ".14d+1" `shouldBe'` expectedExp

        it "parses '3'" $ do
          let expectedExp = intGen 3
          eParser "3" `shouldBe'` expectedExp

        it "parses '-3'" $ do
          let expectedExp = ExpUnary () u Minus $ intGen 3
          eParser "-3" `shouldBe'` expectedExp

        it "parses '3 + 2'" $ do
          let expectedExp = ExpBinary () u Addition (intGen 3) (intGen 2)
          eParser "3 + 2" `shouldBe'` expectedExp

        it "parses '3 + -2'" $ do
          let expectedExp = ExpBinary () u Addition (intGen 3) (ExpUnary () u Minus (intGen 2))
          eParser "3 + -2" `shouldBe'` expectedExp

        it "parses '3 + -2 + 42'" $ do
          let expectedExp = ExpBinary () u Addition (ExpBinary () u Addition (intGen 3) (ExpUnary () u Minus (intGen 2))) (intGen 42)
          eParser "3 + -2 + 42" `shouldBe'` expectedExp

        it "parses 'f(y, 24)'" $ do
          let subs = [ IxSingle () u $ varGen "y", ixSinGen 24 ]
          let expectedExp = ExpSubscript () u (varGen "f") (fromList () subs)
          eParser "f(y, 24)" `shouldBe'` expectedExp

        it "parses '3 + 4 * 12'" $ do
          let expectedExp = ExpBinary () u Addition (intGen 3) (ExpBinary () u Multiplication (intGen 4) (intGen 12))
          eParser "3 + 4 * 12" `shouldBe'` expectedExp

      describe "Logical expressions" $
        it "parses '.true. .and. .false.'" $ do
          let expectedExp = ExpBinary () u And valTrue valFalse
          eParser ".true. .and. .false." `shouldBe'` expectedExp

      describe "Relational expressions" $
        it "parses '(3 * 2) .lt. 42'" $ do
          let expectedExp = ExpBinary () u LT (ExpBinary () u Multiplication (intGen 3) (intGen 2)) (intGen 42)
          eParser "(3 * 2) .lt. 42" `shouldBe'` expectedExp

      describe "Other expressions" $
        it "parses 'a(2 * x - 3, 10)'" $ do
          let firstEl = ExpBinary () u Subtraction (ExpBinary () u Multiplication (intGen 2) (varGen "x")) (intGen 3)
              expectedExp = ExpSubscript () u (varGen "a") (AList () u [ IxSingle () u firstEl, ixSinGen 10])
          eParser "a(2 * x - 3, 10)" `shouldBe'` expectedExp

    describe "Statements" $ do
      it "parses 'EXTERNAL f, g, h'" $ do
        let procGen s = ExpValue () u (ValVariable () s)
        let expectedSt = StExternal () u (AList () u [procGen "f", procGen "g", procGen "h"])
        sParser "      EXTERNAL f, g, h" `shouldBe'` expectedSt

      it "parses 'COMMON a, b'" $ do
        let comGr = CommonGroup () u Nothing (AList () u [ varGen "a", varGen "b" ])
        let st = StCommon () u (AList () u [ comGr ])
        sParser "      COMMON a, b" `shouldBe'` st

      it "parses 'COMMON // a, b /hello/ x, y, z'" $ do
        let comGrs = [ CommonGroup () u Nothing (AList () u [ varGen "a", varGen "b" ])
                     , CommonGroup () u (Just $ varGen "hello") (AList () u [ varGen "x", varGen "y", varGen "z" ]) ]
        let st = StCommon () u (AList () u comGrs)
        sParser "      COMMON // a, b /hello/ x, y, z" `shouldBe'` st

      it "parses 'EQUIVALENCE (a,b), (x,y,z)'" $ do
        let ls = [ AList () u [varGen "a", varGen "b"]
                 , AList () u [varGen "x", varGen "y", varGen "z"] ]
        let st = StEquivalence () u (AList () u ls)
        sParser "      EQUIVALENCE (a,b), (x,y,z)" `shouldBe'` st

      it "parses 'DATA a/1,2,3/,x/42/'" $ do
        let dGrs = [ DataGroup () u (AList () u [varGen "a"]) (AList () u [intGen 1, intGen 2, intGen 3])
                   , DataGroup () u (AList () u [varGen "x"]) (AList () u [intGen 42]) ]
        let st = StData () u $ AList () u dGrs
        sParser "      DATA a/1,2,3/, x/42/" `shouldBe'` st

      describe "FORMAT" $ do
        it "parses 'FORMAT ()'" $ do
          let expectedSt = StFormat () u $ AList () u []
          sParser "      FORMAT ()" `shouldBe'` expectedSt

        it "parses 'FORMAT (///)'" $ do
          let formatList = [FIDelimiter () u, FIDelimiter () u, FIDelimiter () u]
              expectedSt = StFormat () u $ AList () u formatList
          sParser "      FORMAT (///)" `shouldBe'` expectedSt

        it "parses 'FORMAT (2i5/5hhello)'" $ do
          let formatList = [FIFieldDescriptorAIL () u (Just 2) 'i' 5, FIDelimiter () u, FIHollerith () u (ValHollerith "hello")]
              expectedSt = StFormat () u $ AList () u formatList
          sParser "      FORMAT (2i5/5hhello)" `shouldBe'` expectedSt

        it "parses 'FORMAT (/(i5))'" $ do
          let formatList = [FIDelimiter () u, FIFormatList () u Nothing (AList () u [FIFieldDescriptorAIL () u Nothing 'i' 5])]
              expectedSt = StFormat () u $ AList () u formatList
          sParser "      FORMAT (/(i5))" `shouldBe'` expectedSt

      describe "CALL" $ do
        it "parses 'CALL me" $ do
          let expectedSt = StCall () u (ExpValue () u (ValVariable () "me")) Nothing
          sParser "      CALL me" `shouldBe'` expectedSt

        it "parses 'CALL me(baby)" $ do
          let args = AList () u [ Argument () u Nothing $ varGen "baby" ]
          let expectedSt = StCall () u (ExpValue () u (ValVariable () "me")) $ Just args
          sParser "      CALL me(baby)" `shouldBe'` expectedSt

      it "parses 'stop'" $ do
        let expectedSt = StStop () u Nothing
        sParser "      stop" `shouldBe'` expectedSt

      it "parses 'integer i, j(2,2), k'" $ do
        let dimDecls = replicate 2 $ DimensionDeclarator () u Nothing (Just $ intGen 2)
            declarators = [ DeclVariable () u (varGen "i") Nothing Nothing
                          , DeclArray () u (varGen "j") (AList () u dimDecls) Nothing Nothing
                          , DeclVariable () u (varGen "k") Nothing Nothing ]
            st = StDeclaration () u (TypeSpec () u TypeInteger Nothing) Nothing $ AList () u declarators
        sParser "      integer i, j(2,2), k" `shouldBe'` st

      let controlPairs = AList () u [ ControlPair () u Nothing (intGen 6), ControlPair () u Nothing (labelGen 10) ]
      let writeSt = StWrite () u controlPairs (Just $ AList () u [ varGen "i" ])

      describe "WRITE" $ do
        it "parses 'write (6)'" $ do
          let expectedSt = StWrite () u (AList () u [ ControlPair () u Nothing (intGen 6) ]) Nothing
          sParser "      write (6)" `shouldBe'` expectedSt

        it "parses 'write (6) i'" $ do
          let expectedSt = StWrite () u (AList () u [ ControlPair () u Nothing (intGen 6) ]) (Just $ AList () u [ varGen "i" ])
          sParser "      write (6) i" `shouldBe'` expectedSt

        it "parses 'write (6,10) i'" $
          sParser "      write (6,10) i" `shouldBe'` writeSt

      describe "IF" $ do
        it "parses 'if (10 .LT. x) write (6,10) i'" $ do
          let cond = ExpBinary () u LT (intGen 10) (varGen "x")
          let expectedSt = StIfLogical () u cond writeSt
          sParser "      if (10 .LT. x) write (6,10) i" `shouldBe'` expectedSt

        it "parses 'if (10 - 5) 10, 20, 30'" $ do
          let cond = ExpBinary () u Subtraction (intGen 10) (intGen 5)
          let expectedSt = StIfArithmetic () u cond (labelGen 10) (labelGen 20) (labelGen 30)
          sParser "      if (10 - 5) 10, 20, 30" `shouldBe'` expectedSt

        it "parses 'IF (IY) 5,6,6" $ do
          let expectedSt = StIfArithmetic () u (varGen "iy") (labelGen 5) (labelGen 6) (labelGen 6)
          sParser "      IF (IY) 5,6,6" `shouldBe'` expectedSt

      describe "ASSIGNMENT" $ do
        it "parses 'f = 1'" $ do
          let expectedSt = StExpressionAssign () u (varGen "f") (intGen 1)
          sParser "      f = 1" `shouldBe'` expectedSt

        it "parses 'f = a(1,2)'" $ do
          let indicies = fromList () [ ixSinGen 1, ixSinGen 2 ]
          let rhs = ExpSubscript () u (varGen "a") indicies
          let expectedSt = StExpressionAssign () u (varGen "f") rhs
          sParser "      f = a(1,2)" `shouldBe'` expectedSt

      it "parses 'do 42 i = 10, 1, 1'" $ do
        let st = StExpressionAssign () u (varGen "i") (intGen 10)
        let doSpec = DoSpecification () u st (intGen 1) (Just $ intGen 1)
        let expectedSt = StDo () u Nothing (Just $ labelGen 42) doSpec
        sParser "      do 42 i = 10, 1, 1" `shouldBe'` expectedSt
